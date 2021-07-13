/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2021 The Stockfish developers (see AUTHORS file)

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstring>   // For std::memset
#include <iostream>
#include <sstream>

#include "evaluate.h"
#include "misc.h"
#include "movegen.h"
#include "movepick.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "timeman.h"
#include "tt.h"
#include "uci.h"
#include "syzygy/tbprobe.h"

namespace Stockfish {

namespace Search {

  LimitsType Limits;
}

namespace Tablebases {

  int Cardinality;
  bool RootInTB;
  bool UseRule50;
  Depth ProbeDepth;
}

namespace TB = Tablebases;

using std::string;
using Eval::evaluate;
using namespace Search;

namespace {

  // Different node types, used as a template parameter
  enum NodeType { NonPV, PV, Root };

  constexpr uint64_t TtHitAverageWindow     = 4096;
  constexpr uint64_t TtHitAverageResolution = 1024;

  // Futility margin
  Value futility_margin(Depth d, bool improving) {
    return Value(214 * (d - improving));
  }

  // Reductions lookup table, initialized at startup
  int Reductions[MAX_MOVES]; // [depth or moveNumber]

  Depth reduction(bool i, Depth d, int mn) {
    int r = Reductions[d] * Reductions[mn];
    return (r + 534) / 1024 + (!i && r > 904);
  }

  constexpr int futility_move_count(bool improving, Depth depth) {
    return (3 + depth * depth) / (2 - improving);
  }

  // History and stats update bonus, based on depth
  int stat_bonus(Depth d) {
    return d > 14 ? 73 : 6 * d * d + 229 * d - 215;
  }

  // Add a small random component to draw evaluations to avoid 3-fold blindness
  Value value_draw(Thread* thisThread) {
    return VALUE_DRAW + Value(2 * (thisThread->nodes & 1) - 1);
  }

  // Skill structure is used to implement strength limit
  struct Skill {
    explicit Skill(int l) : level(l) {}
    bool enabled() const { return level < 20; }
    bool time_to_pick(Depth depth) const { return depth == 1 + level; }
    Move pick_best(size_t multiPV);

    int level;
    Move best = MOVE_NONE;
  };

  template <NodeType nodeType>
  Value search(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth, bool cutNode);

  template <NodeType nodeType>
  Value qsearch(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth = 0);

  Value value_to_tt(Value v, int ply);
  Value value_from_tt(Value v, int ply, int r50c);
  void update_pv(Move* pv, Move move, Move* childPv);
  void update_continuation_histories(Stack* ss, Piece pc, Square to, int bonus);
  void update_quiet_stats(const Position& pos, Stack* ss, Move move, int bonus, int depth);
  void update_all_stats(const Position& pos, Stack* ss, Move bestMove, Value bestValue, Value beta, Square prevSq,
                        Move* quietsSearched, int quietCount, Move* capturesSearched, int captureCount, Depth depth);

  // perft() is our utility to verify move generation. All the leaf nodes up
  // to the given depth are generated and counted, and the sum is returned.
  template<bool Root>
  uint64_t perft(Position& pos, Depth depth) {

    StateInfo st;
    ASSERT_ALIGNED(&st, Eval::NNUE::CacheLineSize);

    uint64_t cnt, nodes = 0;
    const bool leaf = (depth == 2);

    for (const auto& m : MoveList<LEGAL>(pos))
    {
        if (Root && depth <= 1)
            cnt = 1, nodes++;
        else
        {
            pos.do_move(m, st);
            cnt = leaf ? MoveList<LEGAL>(pos).size() : perft<false>(pos, depth - 1);
            nodes += cnt;
            pos.undo_move(m);
        }
        if (Root)
            sync_cout << UCI::move(m, pos.is_chess960()) << ": " << cnt << sync_endl;
    }
    return nodes;
  }

} // namespace


/// Search::init() is called at startup to initialize various lookup tables

void Search::init() {

  for (int i = 1; i < MAX_MOVES; ++i)
      Reductions[i] = int(21.9 * std::log(i));
}


/// Search::clear() resets search state to its initial value

void Search::clear() {

  Threads.main()->wait_for_search_finished();

  Time.availableNodes = 0;
  TT.clear();
  Threads.clear();
  Tablebases::init(Options["SyzygyPath"]); // Free mapped files
}


/// MainThread::search() is started when the program receives the UCI 'go'
/// command. It searches from the root position and outputs the "bestmove".

void MainThread::search() {

  if (Limits.perft)
  {
      nodes = perft<true>(rootPos, Limits.perft);
      sync_cout << "\nNodes searched: " << nodes << "\n" << sync_endl;
      return;
  }

  Color us = rootPos.side_to_move();
  Time.init(Limits, us, rootPos.game_ply());
  TT.new_search();

  Eval::NNUE::verify();

  if (rootMoves.empty())
  {
      rootMoves.emplace_back(MOVE_NONE);
      sync_cout << "info depth 0 score "
                << UCI::value(rootPos.checkers() ? -VALUE_MATE : VALUE_DRAW)
                << sync_endl;
  }
  else
  {
      Threads.start_searching(); // start non-main threads
      Thread::search();          // main thread start searching
  }

  // When we reach the maximum depth, we can arrive here without a raise of
  // Threads.stop. However, if we are pondering or in an infinite search,
  // the UCI protocol states that we shouldn't print the best move before the
  // GUI sends a "stop" or "ponderhit" command. We therefore simply wait here
  // until the GUI sends one of those commands.

  while (!Threads.stop && (ponder || Limits.infinite))
  {} // Busy wait for a stop or a ponder reset

  // Stop the threads if not already stopped (also raise the stop if
  // "ponderhit" just reset Threads.ponder).
  Threads.stop = true;

  // Wait until all threads have finished
  Threads.wait_for_search_finished();

  // When playing in 'nodes as time' mode, subtract the searched nodes from
  // the available ones before exiting.
  if (Limits.npmsec)
      Time.availableNodes += Limits.inc[us] - Threads.nodes_searched();

  Thread* bestThread = this;

  if (   int(Options["MultiPV"]) == 1
      && !Limits.depth
      && !(Skill(Options["Skill Level"]).enabled() || int(Options["UCI_LimitStrength"]))
      && rootMoves[0].pv[0] != MOVE_NONE)
      bestThread = Threads.get_best_thread();

  bestPreviousScore = bestThread->rootMoves[0].score;

  // Send again PV info if we have a new best thread
  if (bestThread != this)
      sync_cout << UCI::pv(bestThread->rootPos, bestThread->completedDepth, -VALUE_INFINITE, VALUE_INFINITE) << sync_endl;

  sync_cout << "bestmove " << UCI::move(bestThread->rootMoves[0].pv[0], rootPos.is_chess960());

  if (bestThread->rootMoves[0].pv.size() > 1 || bestThread->rootMoves[0].extract_ponder_from_tt(rootPos))
      std::cout << " ponder " << UCI::move(bestThread->rootMoves[0].pv[1], rootPos.is_chess960());

  std::cout << sync_endl;
}


/// Thread::search() is the main iterative deepening loop. It calls search()
/// repeatedly with increasing depth until the allocated thinking time has been
/// consumed, the user stops the search, or the maximum search depth is reached.

void Thread::search() {

  // To allow access to (ss-7) up to (ss+2), the stack must be oversized.
  // The former is needed to allow update_continuation_histories(ss-1, ...),
  // which accesses its argument at ss-6, also near the root.
  // The latter is needed for statScore and killer initialization.
  Stack stack[MAX_PLY+10], *ss = stack+7;
  Move  pv[MAX_PLY+1];
  Value bestValue, alpha, beta, delta;
  Move  lastBestMove = MOVE_NONE;
  Depth lastBestMoveDepth = 0;
  MainThread* mainThread = (this == Threads.main() ? Threads.main() : nullptr);
  double timeReduction = 1, totBestMoveChanges = 0;
  Color us = rootPos.side_to_move();
  int iterIdx = 0;

  std::memset(ss-7, 0, 10 * sizeof(Stack));
  for (int i = 7; i > 0; i--)
      (ss-i)->continuationHistory = &this->continuationHistory[0][0][NO_PIECE][0]; // Use as a sentinel

  for (int i = 0; i <= MAX_PLY + 2; ++i)
      (ss+i)->ply = i;

  ss->pv = pv;

  bestValue = delta = alpha = -VALUE_INFINITE;
  beta = VALUE_INFINITE;

  if (mainThread)
  {
      if (mainThread->bestPreviousScore == VALUE_INFINITE)
          for (int i = 0; i < 4; ++i)
              mainThread->iterValue[i] = VALUE_ZERO;
      else
          for (int i = 0; i < 4; ++i)
              mainThread->iterValue[i] = mainThread->bestPreviousScore;
  }

  std::copy(&lowPlyHistory[2][0], &lowPlyHistory.back().back() + 1, &lowPlyHistory[0][0]);
  std::fill(&lowPlyHistory[MAX_LPH - 2][0], &lowPlyHistory.back().back() + 1, 0);

  size_t multiPV = size_t(Options["MultiPV"]);

  // Pick integer skill levels, but non-deterministically round up or down
  // such that the average integer skill corresponds to the input floating point one.
  // UCI_Elo is converted to a suitable fractional skill level, using anchoring
  // to CCRL Elo (goldfish 1.13 = 2000) and a fit through Ordo derived Elo
  // for match (TC 60+0.6) results spanning a wide range of k values.
  PRNG rng(now());
  double floatLevel = Options["UCI_LimitStrength"] ?
                      std::clamp(std::pow((Options["UCI_Elo"] - 1346.6) / 143.4, 1 / 0.806), 0.0, 20.0) :
                        double(Options["Skill Level"]);
  int intLevel = int(floatLevel) +
                 ((floatLevel - int(floatLevel)) * 1024 > rng.rand<unsigned>() % 1024  ? 1 : 0);
  Skill skill(intLevel);

  // When playing with strength handicap enable MultiPV search that we will
  // use behind the scenes to retrieve a set of possible moves.
  if (skill.enabled())
      multiPV = std::max(multiPV, (size_t)4);

  multiPV = std::min(multiPV, rootMoves.size());
  ttHitAverage = TtHitAverageWindow * TtHitAverageResolution / 2;

  trend = SCORE_ZERO;

  int searchAgainCounter = 0;

  // Iterative deepening loop until requested to stop or the target depth is reached
  while (   ++rootDepth < MAX_PLY
         && !Threads.stop
         && !(Limits.depth && mainThread && rootDepth > Limits.depth))
  {
      // Age out PV variability metric
      if (mainThread)
          totBestMoveChanges /= 2;

      // Save the last iteration's scores before first PV line is searched and
      // all the move scores except the (new) PV are set to -VALUE_INFINITE.
      for (RootMove& rm : rootMoves)
          rm.previousScore = rm.score;

      size_t pvFirst = 0;
      pvLast = 0;

      if (!Threads.increaseDepth)
         searchAgainCounter++;

      // MultiPV loop. We perform a full root search for each PV line
      for (pvIdx = 0; pvIdx < multiPV && !Threads.stop; ++pvIdx)
      {
          if (pvIdx == pvLast)
          {
              pvFirst = pvLast;
              for (pvLast++; pvLast < rootMoves.size(); pvLast++)
                  if (rootMoves[pvLast].tbRank != rootMoves[pvFirst].tbRank)
                      break;
          }

          // Reset UCI info selDepth for each depth and each PV line
          selDepth = 0;

          // Reset aspiration window starting size
          if (rootDepth >= 4)
          {
              Value prev = rootMoves[pvIdx].previousScore;
              delta = Value(17);
              alpha = std::max(prev - delta,-VALUE_INFINITE);
              beta  = std::min(prev + delta, VALUE_INFINITE);

              // Adjust trend based on root move's previousScore (dynamic contempt)
              int tr = 113 * prev / (abs(prev) + 147);

              trend = (us == WHITE ?  make_score(tr, tr / 2)
                                   : -make_score(tr, tr / 2));
          }

          // Start with a small aspiration window and, in the case of a fail
          // high/low, re-search with a bigger window until we don't fail
          // high/low anymore.
          int failedHighCnt = 0;
          while (true)
          {
              Depth adjustedDepth = std::max(1, rootDepth - failedHighCnt - searchAgainCounter);
              bestValue = Stockfish::search<Root>(rootPos, ss, alpha, beta, adjustedDepth, false);

              // Bring the best move to the front. It is critical that sorting
              // is done with a stable algorithm because all the values but the
              // first and eventually the new best one are set to -VALUE_INFINITE
              // and we want to keep the same order for all the moves except the
              // new PV that goes to the front. Note that in case of MultiPV
              // search the already searched PV lines are preserved.
              std::stable_sort(rootMoves.begin() + pvIdx, rootMoves.begin() + pvLast);

              // If search has been stopped, we break immediately. Sorting is
              // safe because RootMoves is still valid, although it refers to
              // the previous iteration.
              if (Threads.stop)
                  break;

              // When failing high/low give some update (without cluttering
              // the UI) before a re-search.
              if (   mainThread
                  && multiPV == 1
                  && (bestValue <= alpha || bestValue >= beta)
                  && Time.elapsed() > 3000)
                  sync_cout << UCI::pv(rootPos, rootDepth, alpha, beta) << sync_endl;

              // In case of failing low/high increase aspiration window and
              // re-search, otherwise exit the loop.
              if (bestValue <= alpha)
              {
                  beta = (alpha + beta) / 2;
                  alpha = std::max(bestValue - delta, -VALUE_INFINITE);

                  failedHighCnt = 0;
                  if (mainThread)
                      mainThread->stopOnPonderhit = false;
              }
              else if (bestValue >= beta)
              {
                  beta = std::min(bestValue + delta, VALUE_INFINITE);
                  ++failedHighCnt;
              }
              else
                  break;

              delta += delta / 4 + 5;

              assert(alpha >= -VALUE_INFINITE && beta <= VALUE_INFINITE);
          }

          // Sort the PV lines searched so far and update the GUI
          std::stable_sort(rootMoves.begin() + pvFirst, rootMoves.begin() + pvIdx + 1);

          if (    mainThread
              && (Threads.stop || pvIdx + 1 == multiPV || Time.elapsed() > 3000))
              sync_cout << UCI::pv(rootPos, rootDepth, alpha, beta) << sync_endl;
      }

      if (!Threads.stop)
          completedDepth = rootDepth;

      if (rootMoves[0].pv[0] != lastBestMove) {
         lastBestMove = rootMoves[0].pv[0];
         lastBestMoveDepth = rootDepth;
      }

      // Have we found a "mate in x"?
      if (   Limits.mate
          && bestValue >= VALUE_MATE_IN_MAX_PLY
          && VALUE_MATE - bestValue <= 2 * Limits.mate)
          Threads.stop = true;

      if (!mainThread)
          continue;

      // If skill level is enabled and time is up, pick a sub-optimal best move
      if (skill.enabled() && skill.time_to_pick(rootDepth))
          skill.pick_best(multiPV);

      // Do we have time for the next iteration? Can we stop searching now?
      if (    Limits.use_time_management()
          && !Threads.stop
          && !mainThread->stopOnPonderhit)
      {
          double fallingEval = (318 + 6 * (mainThread->bestPreviousScore - bestValue)
                                    + 6 * (mainThread->iterValue[iterIdx] - bestValue)) / 825.0;
          fallingEval = std::clamp(fallingEval, 0.5, 1.5);

          // If the bestMove is stable over several iterations, reduce time accordingly
          timeReduction = lastBestMoveDepth + 9 < completedDepth ? 1.92 : 0.95;
          double reduction = (1.47 + mainThread->previousTimeReduction) / (2.32 * timeReduction);

          // Use part of the gained time from a previous stable move for the current move
          for (Thread* th : Threads)
          {
              totBestMoveChanges += th->bestMoveChanges;
              th->bestMoveChanges = 0;
          }
          double bestMoveInstability = 1.073 + std::max(1.0, 2.25 - 9.9 / rootDepth)
                                              * totBestMoveChanges / Threads.size();
          double totalTime = Time.optimum() * fallingEval * reduction * bestMoveInstability;

          // Cap used time in case of a single legal move for a better viewer experience in tournaments
          // yielding correct scores and sufficiently fast moves.
          if (rootMoves.size() == 1)
              totalTime = std::min(500.0, totalTime);

          // Stop the search if we have exceeded the totalTime
          if (Time.elapsed() > totalTime)
          {
              // If we are allowed to ponder do not stop the search now but
              // keep pondering until the GUI sends "ponderhit" or "stop".
              if (mainThread->ponder)
                  mainThread->stopOnPonderhit = true;
              else
                  Threads.stop = true;
          }
          else if (   Threads.increaseDepth
                   && !mainThread->ponder
                   && Time.elapsed() > totalTime * 0.58)
                   Threads.increaseDepth = false;
          else
                   Threads.increaseDepth = true;
      }

      mainThread->iterValue[iterIdx] = bestValue;
      iterIdx = (iterIdx + 1) & 3;
  }

  if (!mainThread)
      return;

  mainThread->previousTimeReduction = timeReduction;

  // If skill level is enabled, swap best PV line with the sub-optimal one
  if (skill.enabled())
      std::swap(rootMoves[0], *std::find(rootMoves.begin(), rootMoves.end(),
                skill.best ? skill.best : skill.pick_best(multiPV)));
}


namespace {

  // search<>() is the main search function for both PV and non-PV nodes

  template <NodeType nodeType>
  Value search(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth, bool cutNode) {

    constexpr bool PvNode = nodeType != NonPV;
    constexpr bool rootNode = nodeType == Root;
    const Depth maxNextDepth = rootNode ? depth : depth + 1;

    // Check if we have an upcoming move which draws by repetition, or
    // if the opponent had an alternative move earlier to this position.
    if (   !rootNode
        && pos.rule50_count() >= 3
        && alpha < VALUE_DRAW
        && pos.has_game_cycle(ss->ply))
    {
        alpha = value_draw(pos.this_thread());
        if (alpha >= beta)
            return alpha;
    }

    // Dive into quiescence search when the depth reaches zero
    if (depth <= 0)
        return qsearch<PvNode ? PV : NonPV>(pos, ss, alpha, beta);

    assert(-VALUE_INFINITE <= alpha && alpha < beta && beta <= VALUE_INFINITE);
    assert(PvNode || (alpha == beta - 1));
    assert(0 < depth && depth < MAX_PLY);
    assert(!(PvNode && cutNode));

    Move pv[MAX_PLY+1], capturesSearched[32], quietsSearched[64];
    StateInfo st;
    ASSERT_ALIGNED(&st, Eval::NNUE::CacheLineSize);

    TTEntry* tte;
    Key posKey;
    Move ttMove, move, excludedMove, bestMove;
    Depth extension, newDepth;
    Value bestValue, value, ttValue, eval, maxValue, probCutBeta;
    bool givesCheck, improving, didLMR, priorCapture;
    bool captureOrPromotion, doFullDepthSearch, moveCountPruning,
         ttCapture, singularQuietLMR;
    Piece movedPiece;
    int moveCount, captureCount, quietCount;

    // Step 1. Initialize node
    Thread* thisThread = pos.this_thread();
    ss->inCheck        = pos.checkers();
    priorCapture       = pos.captured_piece();
    Color us           = pos.side_to_move();
    moveCount          = captureCount = quietCount = ss->moveCount = 0;
    bestValue          = -VALUE_INFINITE;
    maxValue           = VALUE_INFINITE;

    // Check for the available remaining time
    if (thisThread == Threads.main())
        static_cast<MainThread*>(thisThread)->check_time();

    // Used to send selDepth info to GUI (selDepth counts from 1, ply from 0)
    if (PvNode && thisThread->selDepth < ss->ply + 1)
        thisThread->selDepth = ss->ply + 1;

    if (!rootNode)
    {
        // Step 2. Check for aborted search and immediate draw
        if (   Threads.stop.load(std::memory_order_relaxed)
            || pos.is_draw(ss->ply)
            || ss->ply >= MAX_PLY)
            return (ss->ply >= MAX_PLY && !ss->inCheck) ? evaluate(pos)
                                                        : value_draw(pos.this_thread());

        // Step 3. Mate distance pruning. Even if we mate at the next move our score
        // would be at best mate_in(ss->ply+1), but if alpha is already bigger because
        // a shorter mate was found upward in the tree then there is no need to search
        // because we will never beat the current alpha. Same logic but with reversed
        // signs applies also in the opposite condition of being mated instead of giving
        // mate. In this case return a fail-high score.
        alpha = std::max(mated_in(ss->ply), alpha);
        beta = std::min(mate_in(ss->ply+1), beta);
        if (alpha >= beta)
            return alpha;
    }

    assert(0 <= ss->ply && ss->ply < MAX_PLY);

    (ss+1)->ttPv         = false;
    (ss+1)->excludedMove = bestMove = MOVE_NONE;
    (ss+2)->killers[0]   = (ss+2)->killers[1] = MOVE_NONE;
    ss->doubleExtensions = (ss-1)->doubleExtensions;
    Square prevSq        = to_sq((ss-1)->currentMove);

    // Initialize statScore to zero for the grandchildren of the current position.
    // So statScore is shared between all grandchildren and only the first grandchild
    // starts with statScore = 0. Later grandchildren start with the last calculated
    // statScore of the previous grandchild. This influences the reduction rules in
    // LMR which are based on the statScore of parent position.
    if (!rootNode)
        (ss+2)->statScore = 0;

    // Step 4. Transposition table lookup. We don't want the score of a partial
    // search to overwrite a previous full search TT value, so we use a different
    // position key in case of an excluded move.
    excludedMove = ss->excludedMove;
    posKey = excludedMove == MOVE_NONE ? pos.key() : pos.key() ^ make_key(excludedMove);
    tte = TT.probe(posKey, ss->ttHit);
    ttValue = ss->ttHit ? value_from_tt(tte->value(), ss->ply, pos.rule50_count()) : VALUE_NONE;
    ttMove =  rootNode ? thisThread->rootMoves[thisThread->pvIdx].pv[0]
            : ss->ttHit    ? tte->move() : MOVE_NONE;
    if (!excludedMove)
        ss->ttPv = PvNode || (ss->ttHit && tte->is_pv());

    // Update low ply history for previous move if we are near root and position is or has been in PV
    if (   ss->ttPv
        && depth > 12
        && ss->ply - 1 < MAX_LPH
        && !priorCapture
        && is_ok((ss-1)->currentMove))
        thisThread->lowPlyHistory[ss->ply - 1][from_to((ss-1)->currentMove)] << stat_bonus(depth - 5);

    // thisThread->ttHitAverage can be used to approximate the running average of ttHit
    thisThread->ttHitAverage =   (TtHitAverageWindow - 1) * thisThread->ttHitAverage / TtHitAverageWindow
                                + TtHitAverageResolution * ss->ttHit;

    // At non-PV nodes we check for an early TT cutoff
    if (  !PvNode
        && ss->ttHit
        && tte->depth() >= depth
        && ttValue != VALUE_NONE // Possible in case of TT access race
        && (ttValue >= beta ? (tte->bound() & BOUND_LOWER)
                            : (tte->bound() & BOUND_UPPER)))
    {
        // If ttMove is quiet, update move sorting heuristics on TT hit
        if (ttMove)
        {
            if (ttValue >= beta)
            {
                // Bonus for a quiet ttMove that fails high
                if (!pos.capture_or_promotion(ttMove))
                    update_quiet_stats(pos, ss, ttMove, stat_bonus(depth), depth);

                // Extra penalty for early quiet moves of the previous ply
                if ((ss-1)->moveCount <= 2 && !priorCapture)
                    update_continuation_histories(ss-1, pos.piece_on(prevSq), prevSq, -stat_bonus(depth + 1));
            }
            // Penalty for a quiet ttMove that fails low
            else if (!pos.capture_or_promotion(ttMove))
            {
                int penalty = -stat_bonus(depth);
                thisThread->mainHistory[us][from_to(ttMove)] << penalty;
                update_continuation_histories(ss, pos.moved_piece(ttMove), to_sq(ttMove), penalty);
            }
        }

        // Partial workaround for the graph history interaction problem
        // For high rule50 counts don't produce transposition table cutoffs.
        if (pos.rule50_count() < 90)
            return ttValue;
    }

    // Step 5. Tablebases probe
    if (!rootNode && TB::Cardinality)
    {
        int piecesCount = pos.count<ALL_PIECES>();

        if (    piecesCount <= TB::Cardinality
            && (piecesCount <  TB::Cardinality || depth >= TB::ProbeDepth)
            &&  pos.rule50_count() == 0
            && !pos.can_castle(ANY_CASTLING))
        {
            TB::ProbeState err;
            TB::WDLScore wdl = Tablebases::probe_wdl(pos, &err);

            // Force check of time on the next occasion
            if (thisThread == Threads.main())
                static_cast<MainThread*>(thisThread)->callsCnt = 0;

            if (err != TB::ProbeState::FAIL)
            {
                thisThread->tbHits.fetch_add(1, std::memory_order_relaxed);

                int drawScore = TB::UseRule50 ? 1 : 0;

                // use the range VALUE_MATE_IN_MAX_PLY to VALUE_TB_WIN_IN_MAX_PLY to score
                value =  wdl < -drawScore ? VALUE_MATED_IN_MAX_PLY + ss->ply + 1
                       : wdl >  drawScore ? VALUE_MATE_IN_MAX_PLY - ss->ply - 1
                                          : VALUE_DRAW + 2 * wdl * drawScore;

                Bound b =  wdl < -drawScore ? BOUND_UPPER
                         : wdl >  drawScore ? BOUND_LOWER : BOUND_EXACT;

                if (    b == BOUND_EXACT
                    || (b == BOUND_LOWER ? value >= beta : value <= alpha))
                {
                    tte->save(posKey, value_to_tt(value, ss->ply), ss->ttPv, b,
                              std::min(MAX_PLY - 1, depth + 6),
                              MOVE_NONE, VALUE_NONE);

                    return value;
                }

                if (PvNode)
                {
                    if (b == BOUND_LOWER)
                        bestValue = value, alpha = std::max(alpha, bestValue);
                    else
                        maxValue = value;
                }
            }
        }
    }

    CapturePieceToHistory& captureHistory = thisThread->captureHistory;

    // Step 6. Static evaluation of the position
    if (ss->inCheck)
    {
        // Skip early pruning when in check
        ss->staticEval = eval = VALUE_NONE;
        improving = false;
        goto moves_loop;
    }
    else if (ss->ttHit)
    {
        // Never assume anything about values stored in TT
        ss->staticEval = eval = tte->eval();
        if (eval == VALUE_NONE)
            ss->staticEval = eval = evaluate(pos);

        // Randomize draw evaluation
        if (eval == VALUE_DRAW)
            eval = value_draw(thisThread);

        // Can ttValue be used as a better position evaluation?
        if (    ttValue != VALUE_NONE
            && (tte->bound() & (ttValue > eval ? BOUND_LOWER : BOUND_UPPER)))
            eval = ttValue;
    }
    else
    {
        // In case of null move search use previous static eval with a different sign
        // and addition of two tempos
        if ((ss-1)->currentMove != MOVE_NULL)
            ss->staticEval = eval = evaluate(pos);
        else
            ss->staticEval = eval = -(ss-1)->staticEval;

        // Save static evaluation into transposition table
        tte->save(posKey, VALUE_NONE, ss->ttPv, BOUND_NONE, DEPTH_NONE, MOVE_NONE, eval);
    }

    // Use static evaluation difference to improve quiet move ordering
    if (is_ok((ss-1)->currentMove) && !(ss-1)->inCheck && !priorCapture)
    {
        int bonus = std::clamp(-depth * 4 * int((ss-1)->staticEval + ss->staticEval), -1000, 1000);
        thisThread->mainHistory[~us][from_to((ss-1)->currentMove)] << bonus;
    }

    // Set up improving flag that is used in various pruning heuristics
    // We define position as improving if static evaluation of position is better
    // Than the previous static evaluation at our turn
    // In case of us being in check at our previous move we look at move prior to it
    improving =  (ss-2)->staticEval == VALUE_NONE
               ? ss->staticEval > (ss-4)->staticEval || (ss-4)->staticEval == VALUE_NONE
               : ss->staticEval > (ss-2)->staticEval;

    // Step 7. Futility pruning: child node (~50 Elo)
    if (   !PvNode
        &&  depth < 9
        &&  eval - futility_margin(depth, improving) >= beta
        &&  eval < VALUE_KNOWN_WIN) // Do not return unproven wins
        return eval;

    // Step 8. Null move search with verification search (~40 Elo)
    if (   !PvNode
        && (ss-1)->currentMove != MOVE_NULL
        && (ss-1)->statScore < 23767
        &&  eval >= beta
        &&  eval >= ss->staticEval
        &&  ss->staticEval >= beta - 20 * depth - 22 * improving + 168 * ss->ttPv + 159
        && !excludedMove
        &&  pos.non_pawn_material(us)
        && (ss->ply >= thisThread->nmpMinPly || us != thisThread->nmpColor))
    {
        assert(eval - beta >= 0);

        // Null move dynamic reduction based on depth and value
        Depth R = (1090 + 81 * depth) / 256 + std::min(int(eval - beta) / 205, 3);

        ss->currentMove = MOVE_NULL;
        ss->continuationHistory = &thisThread->continuationHistory[0][0][NO_PIECE][0];

        pos.do_null_move(st);

        Value nullValue = -search<NonPV>(pos, ss+1, -beta, -beta+1, depth-R, !cutNode);

        pos.undo_null_move();

        if (nullValue >= beta)
        {
            // Do not return unproven mate or TB scores
            if (nullValue >= VALUE_TB_WIN_IN_MAX_PLY)
                nullValue = beta;

            if (thisThread->nmpMinPly || (abs(beta) < VALUE_KNOWN_WIN && depth < 14))
                return nullValue;

            assert(!thisThread->nmpMinPly); // Recursive verification is not allowed

            // Do verification search at high depths, with null move pruning disabled
            // for us, until ply exceeds nmpMinPly.
            thisThread->nmpMinPly = ss->ply + 3 * (depth-R) / 4;
            thisThread->nmpColor = us;

            Value v = search<NonPV>(pos, ss, beta-1, beta, depth-R, false);

            thisThread->nmpMinPly = 0;

            if (v >= beta)
                return nullValue;
        }
    }

    probCutBeta = beta + 209 - 44 * improving;

    // Step 9. ProbCut (~4 Elo)
    // If we have a good enough capture and a reduced search returns a value
    // much above beta, we can (almost) safely prune the previous move.
    if (   !PvNode
        &&  depth > 4
        &&  abs(beta) < VALUE_TB_WIN_IN_MAX_PLY
        // if value from transposition table is lower than probCutBeta, don't attempt probCut
        // there and in further interactions with transposition table cutoff depth is set to depth - 3
        // because probCut search has depth set to depth - 4 but we also do a move before it
        // so effective depth is equal to depth - 3
        && !(   ss->ttHit
             && tte->depth() >= depth - 3
             && ttValue != VALUE_NONE
             && ttValue < probCutBeta))
    {
        assert(probCutBeta < VALUE_INFINITE);

        MovePicker mp(pos, ttMove, probCutBeta - ss->staticEval, &captureHistory);
        int probCutCount = 0;
        bool ttPv = ss->ttPv;
        ss->ttPv = false;

        while (   (move = mp.next_move()) != MOVE_NONE
               && probCutCount < 2 + 2 * cutNode)
            if (move != excludedMove && pos.legal(move))
            {
                assert(pos.capture_or_promotion(move));
                assert(depth >= 5);

                captureOrPromotion = true;
                probCutCount++;

                ss->currentMove = move;
                ss->continuationHistory = &thisThread->continuationHistory[ss->inCheck]
                                                                          [captureOrPromotion]
                                                                          [pos.moved_piece(move)]
                                                                          [to_sq(move)];

                pos.do_move(move, st);

                // Perform a preliminary qsearch to verify that the move holds
                value = -qsearch<NonPV>(pos, ss+1, -probCutBeta, -probCutBeta+1);

                // If the qsearch held, perform the regular search
                if (value >= probCutBeta)
                    value = -search<NonPV>(pos, ss+1, -probCutBeta, -probCutBeta+1, depth - 4, !cutNode);

                pos.undo_move(move);

                if (value >= probCutBeta)
                {
                    // if transposition table doesn't have equal or more deep info write probCut data into it
                    if ( !(ss->ttHit
                       && tte->depth() >= depth - 3
                       && ttValue != VALUE_NONE))
                        tte->save(posKey, value_to_tt(value, ss->ply), ttPv,
                            BOUND_LOWER,
                            depth - 3, move, ss->staticEval);
                    return value;
                }
            }
         ss->ttPv = ttPv;
    }

    // Step 10. If the position is not in TT, decrease depth by 2
    if (   PvNode
        && depth >= 6
        && !ttMove)
        depth -= 2;

moves_loop: // When in check, search starts from here

    ttCapture = ttMove && pos.capture_or_promotion(ttMove);

    // Step 11. A small Probcut idea, when we are in check
    probCutBeta = beta + 409;
    if (   ss->inCheck
        && !PvNode
        && depth >= 4
        && ttCapture
        && (tte->bound() & BOUND_LOWER)
        && tte->depth() >= depth - 3
        && ttValue >= probCutBeta
        && abs(ttValue) <= VALUE_KNOWN_WIN
        && abs(beta) <= VALUE_KNOWN_WIN
       )
        return probCutBeta;


    const PieceToHistory* contHist[] = { (ss-1)->continuationHistory, (ss-2)->continuationHistory,
                                          nullptr                   , (ss-4)->continuationHistory,
                                          nullptr                   , (ss-6)->continuationHistory };

    Move countermove = thisThread->counterMoves[pos.piece_on(prevSq)][prevSq];

    MovePicker mp(pos, ttMove, depth, &thisThread->mainHistory,
                                      &thisThread->lowPlyHistory,
                                      &captureHistory,
                                      contHist,
                                      countermove,
                                      ss->killers,
                                      ss->ply);

    value = bestValue;
    singularQuietLMR = moveCountPruning = false;
    bool doubleExtension = false;

    // Indicate PvNodes that will probably fail low if the node was searched
    // at a depth equal or greater than the current depth, and the result of this search was a fail low.
    bool likelyFailLow =    PvNode
                         && ttMove
                         && (tte->bound() & BOUND_UPPER)
                         && tte->depth() >= depth;

    // Step 12. Loop through all pseudo-legal moves until no moves remain
    // or a beta cutoff occurs.
    while ((move = mp.next_move(moveCountPruning)) != MOVE_NONE)
    {
      assert(is_ok(move));

      if (move == excludedMove)
          continue;

      // At root obey the "searchmoves" option and skip moves not listed in Root
      // Move List. As a consequence any illegal move is also skipped. In MultiPV
      // mode we also skip PV moves which have been already searched and those
      // of lower "TB rank" if we are in a TB root position.
      if (rootNode && !std::count(thisThread->rootMoves.begin() + thisThread->pvIdx,
                                  thisThread->rootMoves.begin() + thisThread->pvLast, move))
          continue;

      // Check for legality
      if (!rootNode && !pos.legal(move))
          continue;

      ss->moveCount = ++moveCount;

      if (rootNode && thisThread == Threads.main() && Time.elapsed() > 3000)
          sync_cout << "info depth " << depth
                    << " currmove " << UCI::move(move, pos.is_chess960())
                    << " currmovenumber " << moveCount + thisThread->pvIdx << sync_endl;
      if (PvNode)
          (ss+1)->pv = nullptr;

      extension = 0;
      captureOrPromotion = pos.capture_or_promotion(move);
      movedPiece = pos.moved_piece(move);
      givesCheck = pos.gives_check(move);

      // Calculate new depth for this move
      newDepth = depth - 1;

      // Step 13. Pruning at shallow depth (~200 Elo)
      if (  !rootNode
          && pos.non_pawn_material(us)
          && bestValue > VALUE_TB_LOSS_IN_MAX_PLY)
      {
          // Skip quiet moves if movecount exceeds our FutilityMoveCount threshold
          moveCountPruning = moveCount >= futility_move_count(improving, depth);

          // Reduced depth of the next LMR search
          int lmrDepth = std::max(newDepth - reduction(improving, depth, moveCount), 0);

          if (   captureOrPromotion
              || givesCheck)
          {
              // Capture history based pruning when the move doesn't give check
              if (   !givesCheck
                  && lmrDepth < 1
                  && captureHistory[movedPiece][to_sq(move)][type_of(pos.piece_on(to_sq(move)))] < 0)
                  continue;

              // SEE based pruning
              if (!pos.see_ge(move, Value(-218) * depth)) // (~25 Elo)
                  continue;
          }
          else
          {
              // Continuation history based pruning (~20 Elo)
              if (   lmrDepth < 5
                  && (*contHist[0])[movedPiece][to_sq(move)] < CounterMovePruneThreshold
                  && (*contHist[1])[movedPiece][to_sq(move)] < CounterMovePruneThreshold)
                  continue;

              // Futility pruning: parent node (~5 Elo)
              if (   lmrDepth < 7
                  && !ss->inCheck
                  && ss->staticEval + 174 + 157 * lmrDepth <= alpha
                  &&  (*contHist[0])[movedPiece][to_sq(move)]
                    + (*contHist[1])[movedPiece][to_sq(move)]
                    + (*contHist[3])[movedPiece][to_sq(move)]
                    + (*contHist[5])[movedPiece][to_sq(move)] / 3 < 28255)
                  continue;

              // Prune moves with negative SEE (~20 Elo)
              if (!pos.see_ge(move, Value(-(30 - std::min(lmrDepth, 18)) * lmrDepth * lmrDepth)))
                  continue;
          }
      }

      // Step 14. Extensions (~75 Elo)

      // Singular extension search (~70 Elo). If all moves but one fail low on a
      // search of (alpha-s, beta-s), and just one fails high on (alpha, beta),
      // then that move is singular and should be extended. To verify this we do
      // a reduced search on all the other moves but the ttMove and if the
      // result is lower than ttValue minus a margin, then we will extend the ttMove.
      if (   !rootNode
          &&  depth >= 7
          &&  move == ttMove
          && !excludedMove // Avoid recursive singular search
       /* &&  ttValue != VALUE_NONE Already implicit in the next condition */
          &&  abs(ttValue) < VALUE_KNOWN_WIN
          && (tte->bound() & BOUND_LOWER)
          &&  tte->depth() >= depth - 3)
      {
          Value singularBeta = ttValue - 2 * depth;
          Depth singularDepth = (depth - 1) / 2;

          ss->excludedMove = move;
          value = search<NonPV>(pos, ss, singularBeta - 1, singularBeta, singularDepth, cutNode);
          ss->excludedMove = MOVE_NONE;

          if (value < singularBeta)
          {
              extension = 1;
              singularQuietLMR = !ttCapture;

              // Avoid search explosion by limiting the number of double extensions to at most 3
              if (   !PvNode
                  && value < singularBeta - 93
                  && ss->doubleExtensions < 3)
              {
                  extension = 2;
                  doubleExtension = true;
              }
          }

          // Multi-cut pruning
          // Our ttMove is assumed to fail high, and now we failed high also on a reduced
          // search without the ttMove. So we assume this expected Cut-node is not singular,
          // that multiple moves fail high, and we can prune the whole subtree by returning
          // a soft bound.
          else if (singularBeta >= beta)
              return singularBeta;

          // If the eval of ttMove is greater than beta we try also if there is another
          // move that pushes it over beta, if so also produce a cutoff.
          else if (ttValue >= beta)
          {
              ss->excludedMove = move;
              value = search<NonPV>(pos, ss, beta - 1, beta, (depth + 3) / 2, cutNode);
              ss->excludedMove = MOVE_NONE;

              if (value >= beta)
                  return beta;
          }
      }
      else if (   givesCheck
               && depth > 6
               && abs(ss->staticEval) > Value(100))
          extension = 1;

      // Add extension to new depth
      newDepth += extension;
      ss->doubleExtensions = (ss-1)->doubleExtensions + (extension == 2);

      // Speculative prefetch as early as possible
      prefetch(TT.first_entry(pos.key_after(move)));

      // Update the current move (this must be done after singular extension search)
      ss->currentMove = move;
      ss->continuationHistory = &thisThread->continuationHistory[ss->inCheck]
                                                                [captureOrPromotion]
                                                                [movedPiece]
                                                                [to_sq(move)];

      // Step 15. Make the move
      pos.do_move(move, st, givesCheck);
bool CC = false, C = false;
      // Step 16. Late moves reduction / extension (LMR, ~200 Elo)
      // We use various heuristics for the sons of a node after the first son has
      // been searched. In general we would like to reduce them, but there are many
      // cases where we extend a son if it has good chances to be "interesting".
      if (    depth >= 3
          &&  moveCount > 1 + 2 * rootNode
          && (  !captureOrPromotion
              || (cutNode && (ss-1)->moveCount > 1)
              || !ss->ttPv)
          && (!PvNode || ss->ply > 1 || thisThread->id() % 4 != 3))
      {
          Depth r = reduction(improving, depth, moveCount);

	  CC = true;
	  //C = cutNode && (captureOrPromotion || ss->inCheck || !ttCapture);
	  //C = cutNode && givesCheck && bool(type_of(movedPiece) & 4);
	  /*
	  C = !givesCheck && !ss->inCheck && improving && !likelyFailLow
              && bool(type_of(movedPiece) & 2) && bool(type_of(movedPiece) & 4)
              && !bool(pos.count<ALL_PIECES>() & 2) && bool(pos.count<ALL_PIECES>() & 4) && !bool(pos.count<ALL_PIECES>() & 8) && !bool(pos.count<ALL_PIECES>() & 16); 
	 [0] Total 30668089 Hits 1534899 hit rate (%) 5.00487
	[0] Total 232303 Mean 8.3206
       [0] Total 30668089 CramersV(x,y) = 0.0132852 error% =5.6363	
	  C = !givesCheck && !ss->inCheck && improving && !likelyFailLow
              && type_of(movedPiece) == KING
	      && pos.count<ALL_PIECES>() >= 4 
	      && pos.count<ALL_PIECES>() <= 5 ;
       [0] Total 30668089 Hits 1534899 hit rate (%) 5.00487
       [0] Total 232303 Mean 8.3206
       [0] Total 30668089 CramersV(x,y) = 0.0132852 error% =5.6363
	  */
	  /*
	  C = !givesCheck && !ss->inCheck && improving && !likelyFailLow
              && type_of(movedPiece) == KING
	      && pos.count<ALL_PIECES>() <= 5 ;
	      [0] Total 30668089 Hits 1534899 hit rate (%) 5.00487
	      [0] Total 257365 Mean 8.11804
	      [0] Total 30668089 CramersV(x,y) = 0.0131346 error% =5.70782
	      */
	  //C = PvNode && !captureOrPromotion && ss->inCheck && likelyFailLow && type_of(movedPiece) == BISHOP && relative_rank(us, to_sq(move)) > RANK_4;
	  //C = cutNode || ss->inCheck;
	  /*
	  C = cutNode && !likelyFailLow && !ttCapture;
	  [0] Total 30081378 Hits 1491170 hit rate (%) 4.95712
	  [0] Total 9489172 Mean 9.25085
	  [0] Total 30081378 CramersV(x,y) = 0.134284 error% =30.6658
*/
	  /*
	  C = cutNode && !givesCheck && !likelyFailLow && !more_than_one(pos.checkers())
		            && !singularQuietLMR && move == ss->killers[1] &&  !(int(depth) & 1)                                                    && !(moveCount & 1) && !(moveCount & 2) && !(moveCount & 64);
[0] Total 30081378 Hits 1491170 hit rate (%) 4.95712
[0] Total 4422 Mean 11.3071
[0] Total 30081378 CramersV(x,y) = 0.00354724 error% =4.9685
			    */
	  /*
	  C = cutNode && !givesCheck && !likelyFailLow && !more_than_one(pos.checkers())
		            && !singularQuietLMR && move == ss->killers[1] &&  !(int(depth) & 1)                                                    && !(moveCount & 1) && !(moveCount & 2);
	  [0] Total 30081378 Hits 1491170 hit rate (%) 4.95712
		  [0] Total 4422 Mean 11.3071
			  [0] Total 30081378 CramersV(x,y) = 0.00354724 error% =4.9685
				  */
	  /*
	  C = (PvNode || cutNode) && !likelyFailLow && !doubleExtension && !singularQuietLMR && moveCount < 4;
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 27529844 Hits 866086 hit rate (%) 3.14599
	   * [11] Total 3456916 Hits 681556 hit rate (%) 19.7157
	   * [100] Total 30986760 Hits 3456916 hit rate (%) 11.1561
	   * [0] Total 30986760 CramersV(x,y) = 0.239477 error% =11.7516
	   * */

	  /*
	  C = PvNode && !likelyFailLow && !doubleExtension && !singularQuietLMR && moveCount < 4;
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	  [10] Total 30794106 Hits 1525160 hit rate (%) 4.95277
	  [11] Total 192654 Hits 22482 hit rate (%) 11.6696
	  [100] Total 30986760 Hits 192654 hit rate (%) 0.62173
	  [0] Total 30986760 CramersV(x,y) = 0.0242377 error% =5.47115
	   * */
	  /*
	  C = cutNode && !likelyFailLow && !doubleExtension && !singularQuietLMR && moveCount < 4;
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 27722498 Hits 888568 hit rate (%) 3.20522
	   * [11] Total 3264262 Hits 659074 hit rate (%) 20.1906
	   * [100] Total 30986760 Hits 3264262 hit rate (%) 10.5344
	   * [0] Total 30986760 CramersV(x,y) = 0.239379 error% =11.275
	   * */
	  /*
	  C = cutNode && !likelyFailLow && !doubleExtension && moveCount < 4;
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 27670553 Hits 882712 hit rate (%) 3.19008
	   * [11] Total 3316207 Hits 664930 hit rate (%) 20.0509
	   * [100] Total 30986760 Hits 3316207 hit rate (%) 10.702
	   * [0] Total 30986760 CramersV(x,y) = 0.239283 error% =11.4048
	   * */
	  /*
	  C = cutNode && !likelyFailLow && !singularQuietLMR && moveCount < 4;
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 27712502 Hits 888289 hit rate (%) 3.20537
	   * [11] Total 3274258 Hits 659353 hit rate (%) 20.1375
	   * [100] Total 30986760 Hits 3274258 hit rate (%) 10.5666
	   * [0] Total 30986760 CramersV(x,y) = 0.23895 error% =11.3055
	   * */
	  /*
            #C = cutNode && !doubleExtension && !singularQuietLMR && moveCount < 4;
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 27722498 Hits 888568 hit rate (%) 3.20522
	   * [11] Total 3264262 Hits 659074 hit rate (%) 20.1906
	   * [100] Total 30986760 Hits 3264262 hit rate (%) 10.5344
	   * [0] Total 30986760 CramersV(x,y) = 0.239379 error% =11.275
	   * */
	  /*
	  C = cutNode && !doubleExtension && !singularQuietLMR;
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 20707509 Hits 586556 hit rate (%) 2.83258
	   * [11] Total 10279251 Hits 961086 hit rate (%) 9.34977
	   * [100] Total 30986760 Hits 10279251 hit rate (%) 33.173
	   * [0] Total 30986760 CramersV(x,y) = 0.140866 error% =31.9644
	   * */
	    /*
            C = cutNode && moveCount < 4;
	     * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	     * [10] Total 27642250 Hits 881197 hit rate (%) 3.18786
	     * [11] Total 3344510 Hits 666445 hit rate (%) 19.9265
	     * [100] Total 30986760 Hits 3344510 hit rate (%) 10.7934
	     * [0] Total 30986760 CramersV(x,y) = 0.238438 error% =11.4864
	     * */

		/*
	  	C = moveCount > 63;
		 * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
		 * [10] Total 30986756 Hits 1547642 hit rate (%) 4.99453
		 * [11] Total 4 Hits 0 hit rate (%) 0
		 * [100] Total 30986760 Hits 4 hit rate (%) 1.29087e-05
		 * [0] Total 30986760 CramersV(x,y) = -8.23787e-05 error% =4.99454
		 * */
		/*
	  	C = move == ss->killers[0] || move == ss->killers[1] || move == countermove;
		 * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
		 * [10] Total 28953436 Hits 1138419 hit rate (%) 3.9319
		 * [11] Total 2033324 Hits 409223 hit rate (%) 20.1258
		 * [100] Total 30986760 Hits 2033324 hit rate (%) 6.56191
		 * [0] Total 30986760 CramersV(x,y) = 0.18408 error% =8.91516
		 * */
		/*
	  	C = move == ss->killers[0];
		 * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
		 * [10] Total 30332984 Hits 1395646 hit rate (%) 4.60108
		 * [11] Total 653776 Hits 151996 hit rate (%) 23.2489
		 * [100] Total 30986760 Hits 653776 hit rate (%) 2.10986
		 * [0] Total 30986760 CramersV(x,y) = 0.123028 error% =6.12334
		 * */
		/*
	  	C = move == ss->killers[1];
		 * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
		 * [10] Total 30434717 Hits 1442355 hit rate (%) 4.73918
		 * [11] Total 552043 Hits 105287 hit rate (%) 19.0722
		 * [100] Total 30986760 Hits 552043 hit rate (%) 1.78154
		 * [0] Total 30986760 CramersV(x,y) = 0.0870387 error% =6.09651
		 * */
		/*
	  	C = move == countermove;
		 * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
		 * [10] Total 30018863 Hits 1348418 hit rate (%) 4.4919
		 * [11] Total 967897 Hits 199224 hit rate (%) 20.5832
		 * [100] Total 30986760 Hits 967897 hit rate (%) 3.12358
		 * [0] Total 30986760 CramersV(x,y) = 0.1285 error% =6.83224
		 * */
		/*
	  	C = move == ss->killers[0] || move == countermove;
		 * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
		 * [10] Total 29442474 Hits 1225063 hit rate (%) 4.16087
		 * [11] Total 1544286 Hits 322579 hit rate (%) 20.8886
		 * [100] Total 30986760 Hits 1544286 hit rate (%) 4.9837
		 * [0] Total 30986760 CramersV(x,y) = 0.167105 error% =7.89618
		 * */
		//BEST it=400 hit=0.0733355 support=0.100678 T=0.0193299 msteps=1 => !c18*!c22*!c25*c30
		/*
		C = !(int(depth) & 4) && !(moveCount & 4) && !(moveCount & 32) && bool(pos.count<ALL_PIECES>() & 1);
		 * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
		 * [10] Total 27867072 Hits 1318858 hit rate (%) 4.73268
		 * [11] Total 3119688 Hits 228784 hit rate (%) 7.33355
		 * [100] Total 30986760 Hits 3119688 hit rate (%) 10.0678
		 * [0] Total 30986760 CramersV(x,y) = 0.0359272 error% =13.5857
		 * */
	  /*
	  C = cutNode && !doubleExtension && !moveCountPruning && move == ss->killers[0] && (moveCount & 2) && !(moveCount & 8);
	   * BEST it=600 hit=0.355263 support=0.0110199 T=0.00709326 msteps=1 => c0*!c9*!c10*c13*c21*!c23
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 30645289 Hits 1426330 hit rate (%) 4.65432
	   * [11] Total 341471 Hits 121312 hit rate (%) 35.5263
	   * [100] Total 30986760 Hits 341471 hit rate (%) 1.10199
	   * [0] Total 30986760 CramersV(x,y) = 0.147954 error% =5.31352
	   * */
	  /*
	  C = cutNode && !doubleExtension && !moveCountPruning && move == ss->killers[0];
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	  [10] Total 30633746 Hits 1423538 hit rate (%) 4.64696
	  [11] Total 353014 Hits 124104 hit rate (%) 35.1555
	  [100] Total 30986760 Hits 353014 hit rate (%) 1.13924
	  [0] Total 30986760 CramersV(x,y) = 0.148635 error% =5.33276
	 */
	  /*
	  C = cutNode && !moveCountPruning && move == ss->killers[0];
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
[10] Total 30628684 Hits 1423077 hit rate (%) 4.64622
[11] Total 358076 Hits 124565 hit rate (%) 34.7873
[100] Total 30986760 Hits 358076 hit rate (%) 1.15558
[0] Total 30986760 CramersV(x,y) = 0.147881 error% =5.34612
	   * */
	  /*
	  C = cutNode && !doubleExtension && move == ss->killers[0];
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
[10] Total 30633288 Hits 1423470 hit rate (%) 4.64681
[11] Total 353472 Hits 124172 hit rate (%) 35.1292
[100] Total 30986760 Hits 353472 hit rate (%) 1.14072
[0] Total 30986760 CramersV(x,y) = 0.148603 error% =5.33379
	  */

	  /*
	  C = cutNode && !doubleExtension && !extension && move == ss->killers[0] && (moveCount & 2) && !(moveCount & 8);
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 30649572 Hits 1428227 hit rate (%) 4.65986
	   * [11] Total 337188 Hits 119415 hit rate (%) 35.415
	   * [100] Total 30986760 Hits 337188 hit rate (%) 1.08817
	   * [0] Total 30986760 CramersV(x,y) = 0.146476 error% =5.31195
	   * */
	  /*
	  C = cutNode && !doubleExtension && !extension && move == ss->killers[0] && (moveCount & 2);
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 30649340 Hits 1428197 hit rate (%) 4.6598
	   * [11] Total 337420 Hits 119445 hit rate (%) 35.3995
	   * [100] Total 30986760 Hits 337420 hit rate (%) 1.08892
	   * [0] Total 30986760 CramersV(x,y) = 0.146453 error% =5.3125
	   * */
	  /*
	  C = cutNode && !doubleExtension && !extension && move == ss->killers[0] && !(moveCount & 8);
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	  [10] Total 30638574 Hits 1425643 hit rate (%) 4.6531
	  [11] Total 348186 Hits 121999 hit rate (%) 35.0385
	  [100] Total 30986760 Hits 348186 hit rate (%) 1.12366
	  [0] Total 30986760 CramersV(x,y) = 0.14703 error% =5.33076
	  */
	  /*
	  C = cutNode && !doubleExtension && !extension && (moveCount & 2) && !(moveCount & 8);
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	  [10] Total 25988788 Hits 825811 hit rate (%) 3.17757
	  [11] Total 4997972 Hits 721831 hit rate (%) 14.4425
	  [100] Total 30986760 Hits 4997972 hit rate (%) 16.1294
	  [0] Total 30986760 CramersV(x,y) = 0.190204 error% =16.4649
*/
	  /*
	  C = cutNode && !doubleExtension && move == ss->killers[0] && (moveCount & 2) && !(moveCount & 8);
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	  [10] Total 30645137 Hits 1426296 hit rate (%) 4.65423
	  [11] Total 341623 Hits 121346 hit rate (%) 35.5204
	  [100] Total 30986760 Hits 341623 hit rate (%) 1.10248
	  [0] Total 30986760 CramersV(x,y) = 0.147959 error% =5.3138
	  */
	  /*
	  C = cutNode && !extension && move == ss->killers[0] && (moveCount & 2);
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	  [10] Total 30644798 Hits 1427781 hit rate (%) 4.65913
	  [11] Total 341962 Hits 119861 hit rate (%) 35.051
	  [100] Total 30986760 Hits 341962 hit rate (%) 1.10357
	  [0] Total 30986760 CramersV(x,y) = 0.145756 error% =5.32447
	  */
	  /*
	  C = !doubleExtension && !extension && move == ss->killers[0] && !(moveCount & 8);
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	  [10] Total 30352653 Hits 1398902 hit rate (%) 4.60883
	  [11] Total 634107 Hits 148740 hit rate (%) 23.4566
	  [100] Total 30986760 Hits 634107 hit rate (%) 2.04638
	  [0] Total 30986760 CramersV(x,y) = 0.122502 error% =6.08088
	  */
	  /*
	  C = cutNode && move == ss->killers[0];
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	  [10] Total 30628223 Hits 1423009 hit rate (%) 4.64607
	  [11] Total 358537 Hits 124633 hit rate (%) 34.7615
	  [100] Total 30986760 Hits 358537 hit rate (%) 1.15707
	  [0] Total 30986760 CramersV(x,y) = 0.14785 error% =5.34716
	  */

	/*
	  C = cutNode && !ss->inCheck && !ttCapture && !singularQuietLMR && !(ss->ttPv && !PvNode) && move == countermove && !(int(depth) & 8) && (moveCount & 2) && !(moveCount & 32); 
	 * BEST it=1400 hit=0.350634 support=0.00998556 T=0.000128621 msteps=1 => c0*!c4*!c7*!c11*!c12*c15*!c19*c21*!c25
	 * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	 * [10] Total 30677340 Hits 1438992 hit rate (%) 4.69073
	 * [11] Total 309420 Hits 108650 hit rate (%) 35.1141
	 * [100] Total 30986760 Hits 309420 hit rate (%) 0.998556
	 * [0] Total 30986760 CramersV(x,y) = 0.138865 error% =5.29181
 	* */

	  /*
	   * LMR
	   *   SA36_1
	   *  c0   cutNode,
	   *  c1   PvNode || cutNode,  // PvNode = 00, cutNode = 01, allNode = 10
	   *  c2   captureOrPromotion,                           
	   *  c3   givesCheck,
	   *  c4   ss->inCheck,
	   *  c5   improving,
	   *  c6   likelyFailLow,
	   *  c7   ttCapture,
	   *  c8   more_than_one(pos.checkers()),
	   *  c9   doubleExtension,
	   *  c10  moveCountPruning,
	   *  c11  singularQuietLMR,
	   *  c12  ss->ttPv && !PvNode,
	   *  c13  move == ss->killers[0],
	   *  c14  move == ss->killers[1],
	   *  c15  move == countermove,
	   *  c16  bool(int(depth) & 1),
	   *  c17  bool(int(depth) & 2),
	   *  c18  bool(int(depth) & 4),
	   *  c19  bool(int(depth) & 8),
	   *  c20  bool(moveCount & 1),
	   *  c21  bool(moveCount & 2),
	   *  c22  bool(moveCount & 4),
	   *  c23  bool(moveCount & 8),
	   *  c24  bool(moveCount & 16),
	   *  c25  bool(moveCount & 32),
	   *  c26  bool(moveCount & 64)
	   *  c27  bool(type_of(movedPiece) & 1),
	   *  c28  bool(type_of(movedPiece) & 2),
	   *  c29  bool(type_of(movedPiece) & 4),
	   *  c30  bool(pos.count<ALL_PIECES>() & 1),
	   *  c31  bool(pos.count<ALL_PIECES>() & 2),
	   *  c32  bool(pos.count<ALL_PIECES>() & 4),
	   *  c33  bool(pos.count<ALL_PIECES>() & 8),
	   *  c34  bool(pos.count<ALL_PIECES>() & 16)
	   *  c35  bool(extension)
	   * */
	  /*
	  C = improving && !likelyFailLow && !doubleExtension && !moveCountPruning && !singularQuietLMR && move == countermove && !(int(depth) & 8)  && !(moveCount & 8) && !(moveCount & 16);
	   * BEST it=1900 hit=0.316886 support=0.0102391 T=1.04918e-05 msteps=1 => c5*!c6*!c9*!c10*!c11*c15*!c19*!c23*!c24
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 30669485 Hits 1447102 hit rate (%) 4.71838
	   * [11] Total 317275 Hits 100540 hit rate (%) 31.6886
	   * [100] Total 30986760 Hits 317275 hit rate (%) 1.02391
	   * [0] Total 30986760 CramersV(x,y) = 0.12464 error% =5.36951
	   * */
	  /*
	  C = improving && !likelyFailLow && !doubleExtension && !moveCountPruning && !singularQuietLMR && move == countermove;
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
[10] Total 30622148 Hits 1439491 hit rate (%) 4.70082
[11] Total 364612 Hits 108151 hit rate (%) 29.6619
[100] Total 30986760 Hits 364612 hit rate (%) 1.17667
[0] Total 30986760 CramersV(x,y) = 0.123566 error% =5.47315

*/
	  /*
	  C = improving && move == countermove;
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 30599889 Hits 1437099 hit rate (%) 4.69642
	   * [11] Total 386871 Hits 110543 hit rate (%) 28.5736
	   * [100] Total 30986760 Hits 386871 hit rate (%) 1.2485
	   * [0] Total 30986760 CramersV(x,y) = 0.121711 error% =5.52955
	   * */
	  /*
	  C = !likelyFailLow && move == countermove;
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 30047698 Hits 1351362 hit rate (%) 4.49739
	   * [11] Total 939062 Hits 196280 hit rate (%) 20.9017
	   * [100] Total 30986760 Hits 939062 hit rate (%) 3.03053
	   * [0] Total 30986760 CramersV(x,y) = 0.129096 error% =6.75819
	   * */
	  /*
	  C = !moveCountPruning && move == countermove;
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
[10] Total 30021026 Hits 1348503 hit rate (%) 4.49186
[11] Total 965734 Hits 199139 hit rate (%) 20.6205
[100] Total 30986760 Hits 965734 hit rate (%) 3.1166
[0] Total 30986760 CramersV(x,y) = 0.128659 error% =6.82581

*/
	  /*
	  C = !singularQuietLMR && move == countermove;
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
[10] Total 30034637 Hits 1350085 hit rate (%) 4.49509
[11] Total 952123 Hits 197557 hit rate (%) 20.7491
[100] Total 30986760 Hits 952123 hit rate (%) 3.07268
[0] Total 30986760 CramersV(x,y) = 0.128772 error% =6.7921

*/
	  /*
	  C = depth < 8 && move == countermove;
[0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
[10] Total 30178498 Hits 1364823 hit rate (%) 4.5225
[11] Total 808262 Hits 182819 hit rate (%) 22.6188
[100] Total 30986760 Hits 808262 hit rate (%) 2.60841
[0] Total 30986760 CramersV(x,y) = 0.132409 error% =6.42296
*/
	  /*
	  C = depth < 8 && improving && move == countermove;
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
[10] Total 30658206 Hits 1445710 hit rate (%) 4.71557
[11] Total 328554 Hits 101932 hit rate (%) 31.0244
[100] Total 30986760 Hits 328554 hit rate (%) 1.0603
[0] Total 30986760 CramersV(x,y) = 0.123703 error% =5.39692
*/
	  /*
C = !captureOrPromotion && move == ss->killers[1] && ss->statScore > 0 && ss->ttPv && pos.rule50_count() <= 11;
[0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
[10] Total 30954397 Hits 1544282 hit rate (%) 4.98889
[11] Total 32363 Hits 3360 hit rate (%) 10.3822
[100] Total 30986760 Hits 32363 hit rate (%) 0.104441
[0] Total 30986760 CramersV(x,y) = 0.00799733 error% =5.07728
*/

	  /*
	   * BEST it=1500 hit=0.394984 support=0.00103576 T=7.79146e-05 msteps=1 => c5*!c6*!c7*!c9*!c10*!c12*c14*c17*!c18*!c19*c21*!c24*!c25*!c28
	   * */

	  /*
	   * BEST it=1600 hit=0.481371 support=0.00115372 T=4.71983e-05 msteps=1 => c0*c1*c3*!c4*!c7*!c11*!c12*c16*!c19*c21*!c22*!c23*!c24*!c25*!c26*c27*!c28*c29
	   * */

	  /*
	   C = cutNode && !singularQuietLMR && !ss->ttPv &&  move == ss->killers[0] && move == countermove && !(int(depth) & 8) && !(moveCount & 4);
	   * BEST it=1600 hit=0.591595 support=0.00101366 T=4.71983e-05 msteps=1 => c0*!c11*!c12*c13*!c14*c15*!c19*!c22
	   * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   * [10] Total 30955350 Hits 1529060 hit rate (%) 4.93957
	   * [11] Total 31410 Hits 18582 hit rate (%) 59.1595
	   * [100] Total 30986760 Hits 31410 hit rate (%) 0.101366
	   * [0] Total 30986760 CramersV(x,y) = 0.0792069 error% =4.97596
	   * */

	   /*
	   C = cutNode && !singularQuietLMR && !ss->ttPv &&  move == ss->killers[0] && move == countermove;
	    * [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	    * [10] Total 30951280 Hits 1527031 hit rate (%) 4.93366
	    * [11] Total 35480 Hits 20611 hit rate (%) 58.0919
	    * [100] Total 30986760 Hits 35480 hit rate (%) 0.114501
	    * [0] Total 30986760 CramersV(x,y) = 0.0825285 error% =4.976
	    * */

	  /*
	   * BEST it=1600 hit=0.472238 support=0.00104387 T=4.71983e-05 msteps=1 => c0*c5*!c7*!c9*!c10*!c11*!c12*c15*c16*!c18*!c19*!c20*!c22*!c23*!c26*!c28
	   * */

	  /*
	   BEST it=1100 hit=0.170308 support=0.0997138 T=0.000578611 msteps=1 => c0*c5*!c11*!c23*!c24*!c25;
	  C = cutNode && improving && !singularQuietLMR && !(moveCount & 8) && !(moveCount & 16) && !(moveCount & 32);
	   [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   [10] Total 27896951 Hits 1019913 hit rate (%) 3.656
	   [11] Total 3089809 Hits 527729 hit rate (%) 17.0797
	   [100] Total 30986760 Hits 3089809 hit rate (%) 9.97138
	   [0] Total 30986760 CramersV(x,y) = 0.184637 error% =11.5598
	   */
	  /*
	  C = cutNode && improving && !singularQuietLMR && moveCount < 8;
	  [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	  [10] Total 27896953 Hits 1019913 hit rate (%) 3.656
	  [11] Total 3089807 Hits 527729 hit rate (%) 17.0797
	  [100] Total 30986760 Hits 3089807 hit rate (%) 9.97138
	  [0] Total 30986760 CramersV(x,y) = 0.184637 error% =11.5597
	  */
/*
 * BEST it=4200 hit=0.132891 support=0.0960639 T=1.03261e-10 msteps=1 => c16*c17*!c18*!c19*!c22*!c23
 *
 * BEST it=4300 hit=0.20067 support=0.0997177 T=6.25525e-11 msteps=1 => c1*!c4*!c9*!c12*!c22*!c23*!c24*!c25
 *
 * BEST it=4100 hit=0.116038 support=0.100284 T=1.70462e-10 msteps=1 => !c6*!c9*!c10*!c11*!c12*!c19*c21*!c23*!c24*!c25*!c28*c29
 *
 * BEST it=4700 hit=0.170308 support=0.0997138 T=8.4232e-12 msteps=1 => c0*c1*c5*!c10*!c11*!c23*!c24*!c25*!c26
 * */
	  /* MINIMUM
	   *
	   * BEST it=800 hit=0.995774 support=0.0106762 T=0.00260293 msteps=1 => !c0*c19*!c21*c23*!c25*!c27*c29*!c30
	   C = !cutNode && (int(depth) & 8) && !(moveCount & 2) && (moveCount & 8) && !(moveCount & 32) && !(type_of(movedPiece) & 1)
	       &&  (type_of(movedPiece) & 4) && !(pos.count<ALL_PIECES>() & 1);
	       [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	       [10] Total 30655938 Hits 1546244 hit rate (%) 5.04386
	       [11] Total 330822 Hits 1398 hit rate (%) 0.422584
	       [100] Total 30986760 Hits 330822 hit rate (%) 1.06762
	       [0] Total 30986760 CramersV(x,y) = -0.0218031 error% =6.05313

           * BEST it=2500 hit=0.997375 support=0.0108797 T=5.18441e-07 msteps=1 => !c0*!c1*!c3*!c4*!c11*!c13*!c15*c17*c19*c21*c22
	   C = !PvNode && !cutNode && !givesCheck && !ss->inCheck && !singularQuietLMR && move != ss->killers[0] && move != countermove
	       && (int(depth) & 2) && (int(depth) & 8) && (moveCount & 2) && (moveCount & 4);
	   *
	   * BEST it=800 hit=0.995603 support=0.0156191 T=0.00260293 msteps=1 => !c0*!c2*!c5*!c7*!c9*!c12*!c13*!c15*!c18*c19*c21*c22
	   * BEST it=2500 hit=0.995603 support=0.0156191 T=5.18441e-07 msteps=1 => !c0*!c2*!c5*!c7*!c9*!c12*!c13*!c15*!c18*c19*c21*c22
	   *
	   * BEST it=800 hit=0.993803 support=0.0105825 T=0.00260293 msteps=1 => !c3*!c4*!c5*!c13*c16*c19*c20*!c26*!c28*c29
	   * BEST it=2500 hit=0.995546 support=0.0100862 T=5.18441e-07 msteps=1 => !c1*!c2*!c3*!c5*!c10*!c11*!c14*!c15*c17*c21*c22*!c25*!c26*c30
	   *
	   * BEST it=800 hit=0.997737 support=0.0123518 T=0.00260293 msteps=1 => !c0*!c3*!c5*!c8*!c10*!c12*!c18*c19*c22*c23*!c25
	   * BEST it=2500 hit=0.997737 support=0.0123518 T=5.18441e-07 msteps=1 => !c0*!c3*!c5*!c8*!c10*!c12*!c18*c19*c22*c23*!c25
	   * */
	  /*
	   C = !PvNode && !cutNode && !givesCheck && !ss->inCheck && !singularQuietLMR && move != ss->killers[0] && move != countermove
	       && (int(depth) & 2) && (int(depth) & 8) && (moveCount & 2) && (moveCount & 4);
	       [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	       [10] Total 30649632 Hits 1546757 hit rate (%) 5.04658
	       [11] Total 337128 Hits 885 hit rate (%) 0.262512
	       [100] Total 30986760 Hits 337128 hit rate (%) 1.08797
	       [0] Total 30986760 CramersV(x,y) = -0.0227829 error% =6.07679
	       */
	  /*
	   C = !cutNode && (int(depth) & 8) && !(moveCount & 2) && (moveCount & 8) && !(moveCount & 32) && (type_of(movedPiece) == ROOK || type_of(movedPiece) == KING);
	   [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   [10] Total 30405609 Hits 1545181 hit rate (%) 5.08189
	   [11] Total 581151 Hits 2461 hit rate (%) 0.42347
	   [100] Total 30986760 Hits 581151 hit rate (%) 1.87548
	   [0] Total 30986760 CramersV(x,y) = -0.029011 error% =6.85412
	  */
	  /*
	   C = !cutNode && depth >= 8 && (type_of(movedPiece) == ROOK || type_of(movedPiece) == KING);
	   [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   [10] Total 28246640 Hits 1521850 hit rate (%) 5.38772
	   [11] Total 2740120 Hits 25792 hit rate (%) 0.941273
	   [100] Total 30986760 Hits 2740120 hit rate (%) 8.84287
	   [0] Total 30986760 CramersV(x,y) = -0.057954 error% =13.6709
	   */
	  /*
	   C = !cutNode && depth >= 8 && moveCount >= 8 && (type_of(movedPiece) == ROOK || type_of(movedPiece) == KING);
	   [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   [10] Total 29279229 Hits 1542022 hit rate (%) 5.26661
	   [11] Total 1707531 Hits 5620 hit rate (%) 0.32913
	   [100] Total 30986760 Hits 1707531 hit rate (%) 5.51052
	   [0] Total 30986760 CramersV(x,y) = -0.0517216 error% =10.4688
	   */
	  /*
	   C = !cutNode && depth >= 8 && moveCount >= 8 && moveCount < 32 && (type_of(movedPiece) == ROOK || type_of(movedPiece) == KING);
	   [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   [10] Total 29420663 Hits 1542235 hit rate (%) 5.24201
	   [11] Total 1566097 Hits 5407 hit rate (%) 0.345253
	   [100] Total 30986760 Hits 1566097 hit rate (%) 5.05408
	   [0] Total 30986760 CramersV(x,y) = -0.0492433 error% =10.0137
	   */
	  /*
	   C = !cutNode && depth >= 8 && moveCount >= 8 && type_of(movedPiece) == KING;
	   [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   [10] Total 30464068 Hits 1546495 hit rate (%) 5.07646
	   [11] Total 522692 Hits 1147 hit rate (%) 0.219441
	   [100] Total 30986760 Hits 522692 hit rate (%) 1.68682
	   [0] Total 30986760 CramersV(x,y) = -0.0287137 error% =6.67395
	   */
	  /*
	   C = !cutNode && depth >= 8 && moveCount >= 8 && type_of(movedPiece) == ROOK;
	   [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   [10] Total 29801921 Hits 1543169 hit rate (%) 5.17809
	   [11] Total 1184839 Hits 4473 hit rate (%) 0.37752
	   [100] Total 30986760 Hits 1184839 hit rate (%) 3.82369
	   [0] Total 30986760 CramersV(x,y) = -0.0422617 error% =8.78935
	   */
/*
 * BEST it=2000 hit=0.283787 support=0.0205507 T=6.35564e-06 msteps=1 => c0*!c4*c5*!c9*!c10*!c11*!c12*c21*!c22*!c23*!c24*!c25*!c26*c27
  C = cutNode && !ss->inCheck && improving && !doubleExtension && !moveCountPruning && !singularQuietLMR && !ss->ttPv && (moveCount & 2) && !(moveCount & 4) && !(moveCount & 8)
	   && !(moveCount & 16) && !bool(moveCount & 32) && !(moveCount & 64) && (type_of(movedPiece) & 1);
	   [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
[10] Total 30349961 Hits 1366927 hit rate (%) 4.50388
[11] Total 636799 Hits 180715 hit rate (%) 28.3787
[100] Total 30986760 Hits 636799 hit rate (%) 2.05507
[0] Total 30986760 CramersV(x,y) = 0.155497 error% =5.88319

 * */

  CC = true;
	  /*
  CC = depth >= 10;
	   * LMR
	   *   SA36_1
	   *  c0   cutNode,
	   *  c1   PvNode || cutNode,  // PvNode = 00, cutNode = 01, allNode = 10
	   *  c2   captureOrPromotion,                           
	   *  c3   givesCheck,
	   *  c4   ss->inCheck,
	   *  c5   improving,
	   *  c6   likelyFailLow,
	   *  c7   ttCapture,
	   *  c8   more_than_one(pos.checkers()),
	   *  c9   doubleExtension,
	   *  c10  moveCountPruning,
	   *  c11  singularQuietLMR,
	   *  c12  ss->ttPv && !PvNode,
	   *  c13  move == ss->killers[0],
	   *  c14  move == ss->killers[1],
	   *  c15  move == countermove,
	   *  c16  bool(int(depth) & 1),
	   *  c17  bool(int(depth) & 2),
	   *  c18  bool(int(depth) & 4),
	   *  c19  bool(int(depth) & 8),
	   *  c20  bool(moveCount & 1),
	   *  c21  bool(moveCount & 2),
	   *  c22  bool(moveCount & 4),
	   *  c23  bool(moveCount & 8),
	   *  c24  bool(moveCount & 16),
	   *  c25  bool(moveCount & 32),
	   *  c26  bool(moveCount & 64)
	   *  c27  bool(type_of(movedPiece) & 1),
	   *  c28  bool(type_of(movedPiece) & 2),
	   *  c29  bool(type_of(movedPiece) & 4),
	   *  c30  bool(pos.count<ALL_PIECES>() & 1),
	   *  c31  bool(pos.count<ALL_PIECES>() & 2),
	   *  c32  bool(pos.count<ALL_PIECES>() & 4),
	   *  c33  bool(pos.count<ALL_PIECES>() & 8),
	   *  c34  bool(pos.count<ALL_PIECES>() & 16)
	   *  c35  bool(extension)
	   * */
	  /*
	   * BEST it=1600 hit=0.181278 support=0.00987941 T=4.71983e-05 msteps=1 => c0*!c2*!c6*!c7*!c11*c19*!c22*!c23*!c24*!c25*!c26
	   * BEST it=12900 hit=0.181278 support=0.00987941 T=1.18779e-29 msteps=1 => c0*!c2*!c6*!c7*!c11*c19*!c22*!c23*!c24*!c25*!c26
	   *
	   * BEST it=1700 hit=0.195637 support=0.0100839 T=2.85914e-05 msteps=1 => c0*!c6*!c20*c21*!c22*!c23*!c24*!c25   
	   C = cutNode && !likelyfailLow && !(moveCount & 1) && (moveCount & 2) && !(moveCount & 4) && !(moveCount & 8)
	       && !(moveCount & 16) && !(moveCount & 32);
	       BEST it=12800 hit=0.195637 support=0.0100839 T=1.96079e-29 msteps=1 => c0*!c6*!c20*c21*!c22*!c23*!c24*!c25
	   *
	   * BEST it=1900 hit=0.106508 support=0.0104801 T=1.04918e-05 msteps=1 => c13*!c30
	   * BEST it=12800 hit=0.171967 support=0.0105836 T=1.96079e-29 msteps=1 => c0*!c2*!c4*!c7*!c8*!c9*!c10*!c18*c21*!c22*!c23*!c24*!c25*!c26
	   *
	   * BEST it=2600 hit=0.132898 support=0.0099032 T=3.14056e-07 msteps=1 => c0*!c9*!c12*c19*!c20*c21*!c22*!c25
	   * BEST it=12900 hit=0.132898 support=0.0099032 T=1.18779e-29 msteps=1 => c0*!c9*!c12*c19*!c20*c21*!c22*!c25
* */
  /*
	   C = cutNode && !likelyFailLow && !(moveCount & 1) && (moveCount & 2) && !(moveCount & 4) && !(moveCount & 8)
	       && !(moveCount & 16) && !(moveCount & 32);
	       [0] Total 2899971 Hits 24759 hit rate (%) 0.853767
	       [10] Total 2870728 Hits 19038 hit rate (%) 0.663177
	       [11] Total 29243 Hits 5721 hit rate (%) 19.5637
	       [100] Total 2899971 Hits 29243 hit rate (%) 1.00839
	       [0] Total 2899971 CramersV(x,y) = 0.205248 error% =1.4676
	       */
  /*
	   C = cutNode && !likelyFailLow && moveCount <= 2;
	   [0] Total 2899971 Hits 24759 hit rate (%) 0.853767
	   [10] Total 2870728 Hits 19038 hit rate (%) 0.663177
	   [11] Total 29243 Hits 5721 hit rate (%) 19.5637
	   [100] Total 2899971 Hits 29243 hit rate (%) 1.00839
	   [0] Total 2899971 CramersV(x,y) = 0.205248 error% =1.4676
	   */
  /*
  CC = true;
	   C = depth >= 10 && cutNode && !likelyFailLow && moveCount <= 2;
	   [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   [10] Total 30957517 Hits 1541921 hit rate (%) 4.98076
	   [11] Total 29243 Hits 5721 hit rate (%) 19.5637
	   [100] Total 30986760 Hits 29243 hit rate (%) 0.0943726
	   [0] Total 30986760 CramersV(x,y) = 0.0205561 error% =5.05197
	   */
  /*
  CC = true;
	   C = depth >= 10 && cutNode && moveCount <= 2;
	   [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
	   [10] Total 30957517 Hits 1541921 hit rate (%) 4.98076
	   [11] Total 29243 Hits 5721 hit rate (%) 19.5637
	   [100] Total 30986760 Hits 29243 hit rate (%) 0.0943726
	   [0] Total 30986760 CramersV(x,y) = 0.0205561 error% =5.05197
	   */
  /*
     C = move == countermove && cutNode && !ss->inCheck && !ttCapture && (moveCount & 1) && !(moveCount & 32) && (pos.count<ALL_PIECES>() & 16) && !extension && !ss->ttHit;
     [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
     [10] Total 30952840 Hits 1538497 hit rate (%) 4.97046
     [11] Total 33920 Hits 9145 hit rate (%) 26.9605
     [100] Total 30986760 Hits 33920 hit rate (%) 0.109466
     [0] Total 30986760 CramersV(x,y) = 0.0333816 error% =5.04497
     */
  /*
     C = move == countermove && cutNode && !ss->inCheck && !ttCapture && moveCount < 32 && pos.count<ALL_PIECES>() >= 16 && !extension && !ss->ttHit;
     [0] Total 30986760 Hits 1547642 hit rate (%) 4.99453
     [10] Total 30871723 Hits 1510748 hit rate (%) 4.89363
     [11] Total 115037 Hits 36894 hit rate (%) 32.0714
     [100] Total 30986760 Hits 115037 hit rate (%) 0.371246
     [0] Total 30986760 CramersV(x,y) = 0.075878 error% =5.12764
     */
          if (PvNode)
              r--;

          // Decrease reduction if the ttHit running average is large (~0 Elo)
          if (thisThread->ttHitAverage > 537 * TtHitAverageResolution * TtHitAverageWindow / 1024)
              r--;

          // Decrease reduction if position is or has been on the PV
          // and node is not likely to fail low. (~3 Elo)
          if (   ss->ttPv
              && !likelyFailLow)
              r -= 2;

          // Increase reduction at root and non-PV nodes when the best move does not change frequently
          if (   (rootNode || !PvNode)
              && thisThread->bestMoveChanges <= 2)
              r++;

          // Decrease reduction if opponent's move count is high (~1 Elo)
          if ((ss-1)->moveCount > 13)
              r--;

          // Decrease reduction if ttMove has been singularly extended (~1 Elo)
          if (singularQuietLMR)
              r--;

          // Increase reduction for cut nodes (~3 Elo)
          if (cutNode && move != ss->killers[0])
              r += 2;

          if (!captureOrPromotion)
          {
              // Increase reduction if ttMove is a capture (~3 Elo)
              if (ttCapture)
                  r++;

              ss->statScore =  thisThread->mainHistory[us][from_to(move)]
                             + (*contHist[0])[movedPiece][to_sq(move)]
                             + (*contHist[1])[movedPiece][to_sq(move)]
                             + (*contHist[3])[movedPiece][to_sq(move)]
                             - 4923;

              // Decrease/increase reduction for moves with a good/bad history (~30 Elo)
              if (!ss->inCheck)
                  r -= ss->statScore / 14721;
          }

          // In general we want to cap the LMR depth search at newDepth. But if
          // reductions are really negative and movecount is low, we allow this move
          // to be searched deeper than the first move, unless ttMove was extended by 2.
          Depth d = std::clamp(newDepth - r, 1, newDepth + (r < -1 && moveCount <= 5 && !doubleExtension));

          value = -search<NonPV>(pos, ss+1, -(alpha+1), -alpha, d, true);

          // If the son is reduced and fails high it will be re-searched at full depth
          doFullDepthSearch = value > alpha && d < newDepth;
          didLMR = true;
      }
      else
      {
          doFullDepthSearch = !PvNode || moveCount > 1;
          didLMR = false;
      }

      // Step 17. Full depth search when LMR is skipped or fails high
      if (doFullDepthSearch)
      {
          value = -search<NonPV>(pos, ss+1, -(alpha+1), -alpha, newDepth, !cutNode);

          // If the move passed LMR update its stats
          if (didLMR && !captureOrPromotion)
          {
              int bonus = value > alpha ?  stat_bonus(newDepth)
                                        : -stat_bonus(newDepth);

              update_continuation_histories(ss, movedPiece, to_sq(move), bonus);
          }
      }

      // For PV nodes only, do a full PV search on the first move or after a fail
      // high (in the latter case search only if value < beta), otherwise let the
      // parent node fail low with value <= alpha and try another move.
      if (PvNode && (moveCount == 1 || (value > alpha && (rootNode || value < beta))))
      {
          (ss+1)->pv = pv;
          (ss+1)->pv[0] = MOVE_NONE;

          value = -search<PV>(pos, ss+1, -beta, -alpha,
                              std::min(maxNextDepth, newDepth), false);
      }

      // Step 18. Undo move
      pos.undo_move(move);

      assert(value > -VALUE_INFINITE && value < VALUE_INFINITE);

      // Step 19. Check for a new best move
      // Finished searching the move. If a stop occurred, the return value of
      // the search cannot be trusted, and we return immediately without
      // updating best move, PV and TT.
      if (Threads.stop.load(std::memory_order_relaxed))
          return VALUE_ZERO;

      if (rootNode)
      {
          RootMove& rm = *std::find(thisThread->rootMoves.begin(),
                                    thisThread->rootMoves.end(), move);

          // PV move or new best move?
          if (moveCount == 1 || value > alpha)
          {
              rm.score = value;
              rm.selDepth = thisThread->selDepth;
              rm.pv.resize(1);

              assert((ss+1)->pv);

              for (Move* m = (ss+1)->pv; *m != MOVE_NONE; ++m)
                  rm.pv.push_back(*m);

              // We record how often the best move has been changed in each
              // iteration. This information is used for time management and LMR
              if (moveCount > 1)
                  ++thisThread->bestMoveChanges;
          }
          else
              // All other moves but the PV are set to the lowest value: this
              // is not a problem when sorting because the sort is stable and the
              // move position in the list is preserved - just the PV is pushed up.
              rm.score = -VALUE_INFINITE;
      }

      if(CC)
      {
	      bool T = value > alpha;
	      dbg_cramer_of(C, T);
	      dbg_hit_on(T, 0);
	      dbg_hit_on(T, 10+C);
	      dbg_hit_on(C, 100);
	      //dbg_mean_of(C, T, 1);
      }

      if (value > bestValue)
      {
          bestValue = value;

          if (value > alpha)
          {
              bestMove = move;

              if (PvNode && !rootNode) // Update pv even in fail-high case
                  update_pv(ss->pv, move, (ss+1)->pv);

              if (PvNode && value < beta) // Update alpha! Always alpha < beta
                  alpha = value;
              else
              {
                  assert(value >= beta); // Fail high
                  break;
              }
          }
      }

      // If the move is worse than some previously searched move, remember it to update its stats later
      if (move != bestMove)
      {
          if (captureOrPromotion && captureCount < 32)
              capturesSearched[captureCount++] = move;

          else if (!captureOrPromotion && quietCount < 64)
              quietsSearched[quietCount++] = move;
      }
    }

    // The following condition would detect a stop only after move loop has been
    // completed. But in this case bestValue is valid because we have fully
    // searched our subtree, and we can anyhow save the result in TT.
    /*
       if (Threads.stop)
        return VALUE_DRAW;
    */

    // Step 20. Check for mate and stalemate
    // All legal moves have been searched and if there are no legal moves, it
    // must be a mate or a stalemate. If we are in a singular extension search then
    // return a fail low score.

    assert(moveCount || !ss->inCheck || excludedMove || !MoveList<LEGAL>(pos).size());

    if (!moveCount)
        bestValue = excludedMove ? alpha :
                    ss->inCheck  ? mated_in(ss->ply)
                                 : VALUE_DRAW;

    // If there is a move which produces search value greater than alpha we update stats of searched moves
    else if (bestMove)
        update_all_stats(pos, ss, bestMove, bestValue, beta, prevSq,
                         quietsSearched, quietCount, capturesSearched, captureCount, depth);

    // Bonus for prior countermove that caused the fail low
    else if (   (depth >= 3 || PvNode)
             && !priorCapture)
        update_continuation_histories(ss-1, pos.piece_on(prevSq), prevSq, stat_bonus(depth));

    if (PvNode)
        bestValue = std::min(bestValue, maxValue);

    // If no good move is found and the previous position was ttPv, then the previous
    // opponent move is probably good and the new position is added to the search tree.
    if (bestValue <= alpha)
        ss->ttPv = ss->ttPv || ((ss-1)->ttPv && depth > 3);
    // Otherwise, a counter move has been found and if the position is the last leaf
    // in the search tree, remove the position from the search tree.
    else if (depth > 3)
        ss->ttPv = ss->ttPv && (ss+1)->ttPv;

    // Write gathered information in transposition table
    if (!excludedMove && !(rootNode && thisThread->pvIdx))
        tte->save(posKey, value_to_tt(bestValue, ss->ply), ss->ttPv,
                  bestValue >= beta ? BOUND_LOWER :
                  PvNode && bestMove ? BOUND_EXACT : BOUND_UPPER,
                  depth, bestMove, ss->staticEval);

    assert(bestValue > -VALUE_INFINITE && bestValue < VALUE_INFINITE);

    return bestValue;
  }


  // qsearch() is the quiescence search function, which is called by the main search
  // function with zero depth, or recursively with further decreasing depth per call.
  template <NodeType nodeType>
  Value qsearch(Position& pos, Stack* ss, Value alpha, Value beta, Depth depth) {

    static_assert(nodeType != Root);
    constexpr bool PvNode = nodeType == PV;

    assert(alpha >= -VALUE_INFINITE && alpha < beta && beta <= VALUE_INFINITE);
    assert(PvNode || (alpha == beta - 1));
    assert(depth <= 0);

    Move pv[MAX_PLY+1];
    StateInfo st;
    ASSERT_ALIGNED(&st, Eval::NNUE::CacheLineSize);

    TTEntry* tte;
    Key posKey;
    Move ttMove, move, bestMove;
    Depth ttDepth;
    Value bestValue, value, ttValue, futilityValue, futilityBase, oldAlpha;
    bool pvHit, givesCheck, captureOrPromotion;
    int moveCount;

    if (PvNode)
    {
        oldAlpha = alpha; // To flag BOUND_EXACT when eval above alpha and no available moves
        (ss+1)->pv = pv;
        ss->pv[0] = MOVE_NONE;
    }

    Thread* thisThread = pos.this_thread();
    bestMove = MOVE_NONE;
    ss->inCheck = pos.checkers();
    moveCount = 0;

    // Check for an immediate draw or maximum ply reached
    if (   pos.is_draw(ss->ply)
        || ss->ply >= MAX_PLY)
        return (ss->ply >= MAX_PLY && !ss->inCheck) ? evaluate(pos) : VALUE_DRAW;

    assert(0 <= ss->ply && ss->ply < MAX_PLY);

    // Decide whether or not to include checks: this fixes also the type of
    // TT entry depth that we are going to use. Note that in qsearch we use
    // only two types of depth in TT: DEPTH_QS_CHECKS or DEPTH_QS_NO_CHECKS.
    ttDepth = ss->inCheck || depth >= DEPTH_QS_CHECKS ? DEPTH_QS_CHECKS
                                                  : DEPTH_QS_NO_CHECKS;
    // Transposition table lookup
    posKey = pos.key();
    tte = TT.probe(posKey, ss->ttHit);
    ttValue = ss->ttHit ? value_from_tt(tte->value(), ss->ply, pos.rule50_count()) : VALUE_NONE;
    ttMove = ss->ttHit ? tte->move() : MOVE_NONE;
    pvHit = ss->ttHit && tte->is_pv();

    if (  !PvNode
        && ss->ttHit
        && tte->depth() >= ttDepth
        && ttValue != VALUE_NONE // Only in case of TT access race
        && (ttValue >= beta ? (tte->bound() & BOUND_LOWER)
                            : (tte->bound() & BOUND_UPPER)))
        return ttValue;

    // Evaluate the position statically
    if (ss->inCheck)
    {
        ss->staticEval = VALUE_NONE;
        bestValue = futilityBase = -VALUE_INFINITE;
    }
    else
    {
        if (ss->ttHit)
        {
            // Never assume anything about values stored in TT
            if ((ss->staticEval = bestValue = tte->eval()) == VALUE_NONE)
                ss->staticEval = bestValue = evaluate(pos);

            // Can ttValue be used as a better position evaluation?
            if (    ttValue != VALUE_NONE
                && (tte->bound() & (ttValue > bestValue ? BOUND_LOWER : BOUND_UPPER)))
                bestValue = ttValue;
        }
        else
            // In case of null move search use previous static eval with a different sign
            // and addition of two tempos
            ss->staticEval = bestValue =
            (ss-1)->currentMove != MOVE_NULL ? evaluate(pos)
                                             : -(ss-1)->staticEval;

        // Stand pat. Return immediately if static value is at least beta
        if (bestValue >= beta)
        {
            // Save gathered info in transposition table
            if (!ss->ttHit)
                tte->save(posKey, value_to_tt(bestValue, ss->ply), false, BOUND_LOWER,
                          DEPTH_NONE, MOVE_NONE, ss->staticEval);

            return bestValue;
        }

        if (PvNode && bestValue > alpha)
            alpha = bestValue;

        futilityBase = bestValue + 155;
    }

    const PieceToHistory* contHist[] = { (ss-1)->continuationHistory, (ss-2)->continuationHistory,
                                          nullptr                   , (ss-4)->continuationHistory,
                                          nullptr                   , (ss-6)->continuationHistory };

    // Initialize a MovePicker object for the current position, and prepare
    // to search the moves. Because the depth is <= 0 here, only captures,
    // queen promotions, and other checks (only if depth >= DEPTH_QS_CHECKS)
    // will be generated.
    MovePicker mp(pos, ttMove, depth, &thisThread->mainHistory,
                                      &thisThread->captureHistory,
                                      contHist,
                                      to_sq((ss-1)->currentMove));

    // Loop through the moves until no moves remain or a beta cutoff occurs
    while ((move = mp.next_move()) != MOVE_NONE)
    {
      assert(is_ok(move));

      givesCheck = pos.gives_check(move);
      captureOrPromotion = pos.capture_or_promotion(move);

      moveCount++;

      // Futility pruning and moveCount pruning
      if (    bestValue > VALUE_TB_LOSS_IN_MAX_PLY
          && !givesCheck
          &&  futilityBase > -VALUE_KNOWN_WIN
          &&  type_of(move) != PROMOTION)
      {

          if (moveCount > 2)
              continue;

          futilityValue = futilityBase + PieceValue[EG][pos.piece_on(to_sq(move))];

          if (futilityValue <= alpha)
          {
              bestValue = std::max(bestValue, futilityValue);
              continue;
          }

          if (futilityBase <= alpha && !pos.see_ge(move, VALUE_ZERO + 1))
          {
              bestValue = std::max(bestValue, futilityBase);
              continue;
          }
      }

      // Do not search moves with negative SEE values
      if (    bestValue > VALUE_TB_LOSS_IN_MAX_PLY
          && !pos.see_ge(move))
          continue;

      // Speculative prefetch as early as possible
      prefetch(TT.first_entry(pos.key_after(move)));

      // Check for legality just before making the move
      if (!pos.legal(move))
      {
          moveCount--;
          continue;
      }

      ss->currentMove = move;
      ss->continuationHistory = &thisThread->continuationHistory[ss->inCheck]
                                                                [captureOrPromotion]
                                                                [pos.moved_piece(move)]
                                                                [to_sq(move)];

      // Continuation history based pruning
      if (  !captureOrPromotion
          && bestValue > VALUE_TB_LOSS_IN_MAX_PLY
          && (*contHist[0])[pos.moved_piece(move)][to_sq(move)] < CounterMovePruneThreshold
          && (*contHist[1])[pos.moved_piece(move)][to_sq(move)] < CounterMovePruneThreshold)
          continue;

      // Make and search the move
      pos.do_move(move, st, givesCheck);
      value = -qsearch<nodeType>(pos, ss+1, -beta, -alpha, depth - 1);
      pos.undo_move(move);

      assert(value > -VALUE_INFINITE && value < VALUE_INFINITE);

      // Check for a new best move
      if (value > bestValue)
      {
          bestValue = value;

          if (value > alpha)
          {
              bestMove = move;

              if (PvNode) // Update pv even in fail-high case
                  update_pv(ss->pv, move, (ss+1)->pv);

              if (PvNode && value < beta) // Update alpha here!
                  alpha = value;
              else
                  break; // Fail high
          }
       }
    }

    // All legal moves have been searched. A special case: if we're in check
    // and no legal moves were found, it is checkmate.
    if (ss->inCheck && bestValue == -VALUE_INFINITE)
    {
        assert(!MoveList<LEGAL>(pos).size());

        return mated_in(ss->ply); // Plies to mate from the root
    }

    // Save gathered info in transposition table
    tte->save(posKey, value_to_tt(bestValue, ss->ply), pvHit,
              bestValue >= beta ? BOUND_LOWER :
              PvNode && bestValue > oldAlpha  ? BOUND_EXACT : BOUND_UPPER,
              ttDepth, bestMove, ss->staticEval);

    assert(bestValue > -VALUE_INFINITE && bestValue < VALUE_INFINITE);

    return bestValue;
  }


  // value_to_tt() adjusts a mate or TB score from "plies to mate from the root" to
  // "plies to mate from the current position". Standard scores are unchanged.
  // The function is called before storing a value in the transposition table.

  Value value_to_tt(Value v, int ply) {

    assert(v != VALUE_NONE);

    return  v >= VALUE_TB_WIN_IN_MAX_PLY  ? v + ply
          : v <= VALUE_TB_LOSS_IN_MAX_PLY ? v - ply : v;
  }


  // value_from_tt() is the inverse of value_to_tt(): it adjusts a mate or TB score
  // from the transposition table (which refers to the plies to mate/be mated from
  // current position) to "plies to mate/be mated (TB win/loss) from the root". However,
  // for mate scores, to avoid potentially false mate scores related to the 50 moves rule
  // and the graph history interaction, we return an optimal TB score instead.

  Value value_from_tt(Value v, int ply, int r50c) {

    if (v == VALUE_NONE)
        return VALUE_NONE;

    if (v >= VALUE_TB_WIN_IN_MAX_PLY)  // TB win or better
    {
        if (v >= VALUE_MATE_IN_MAX_PLY && VALUE_MATE - v > 99 - r50c)
            return VALUE_MATE_IN_MAX_PLY - 1; // do not return a potentially false mate score

        return v - ply;
    }

    if (v <= VALUE_TB_LOSS_IN_MAX_PLY) // TB loss or worse
    {
        if (v <= VALUE_MATED_IN_MAX_PLY && VALUE_MATE + v > 99 - r50c)
            return VALUE_MATED_IN_MAX_PLY + 1; // do not return a potentially false mate score

        return v + ply;
    }

    return v;
  }


  // update_pv() adds current move and appends child pv[]

  void update_pv(Move* pv, Move move, Move* childPv) {

    for (*pv++ = move; childPv && *childPv != MOVE_NONE; )
        *pv++ = *childPv++;
    *pv = MOVE_NONE;
  }


  // update_all_stats() updates stats at the end of search() when a bestMove is found

  void update_all_stats(const Position& pos, Stack* ss, Move bestMove, Value bestValue, Value beta, Square prevSq,
                        Move* quietsSearched, int quietCount, Move* capturesSearched, int captureCount, Depth depth) {

    int bonus1, bonus2;
    Color us = pos.side_to_move();
    Thread* thisThread = pos.this_thread();
    CapturePieceToHistory& captureHistory = thisThread->captureHistory;
    Piece moved_piece = pos.moved_piece(bestMove);
    PieceType captured = type_of(pos.piece_on(to_sq(bestMove)));

    bonus1 = stat_bonus(depth + 1);
    bonus2 = bestValue > beta + PawnValueMg ? bonus1                                 // larger bonus
                                            : std::min(bonus1, stat_bonus(depth));   // smaller bonus

    if (!pos.capture_or_promotion(bestMove))
    {
        // Increase stats for the best move in case it was a quiet move
        update_quiet_stats(pos, ss, bestMove, bonus2, depth);

        // Decrease stats for all non-best quiet moves
        for (int i = 0; i < quietCount; ++i)
        {
            thisThread->mainHistory[us][from_to(quietsSearched[i])] << -bonus2;
            update_continuation_histories(ss, pos.moved_piece(quietsSearched[i]), to_sq(quietsSearched[i]), -bonus2);
        }
    }
    else
        // Increase stats for the best move in case it was a capture move
        captureHistory[moved_piece][to_sq(bestMove)][captured] << bonus1;

    // Extra penalty for a quiet early move that was not a TT move or
    // main killer move in previous ply when it gets refuted.
    if (   ((ss-1)->moveCount == 1 + (ss-1)->ttHit || ((ss-1)->currentMove == (ss-1)->killers[0]))
        && !pos.captured_piece())
            update_continuation_histories(ss-1, pos.piece_on(prevSq), prevSq, -bonus1);

    // Decrease stats for all non-best capture moves
    for (int i = 0; i < captureCount; ++i)
    {
        moved_piece = pos.moved_piece(capturesSearched[i]);
        captured = type_of(pos.piece_on(to_sq(capturesSearched[i])));
        captureHistory[moved_piece][to_sq(capturesSearched[i])][captured] << -bonus1;
    }
  }


  // update_continuation_histories() updates histories of the move pairs formed
  // by moves at ply -1, -2, -4, and -6 with current move.

  void update_continuation_histories(Stack* ss, Piece pc, Square to, int bonus) {

    for (int i : {1, 2, 4, 6})
    {
        // Only update first 2 continuation histories if we are in check
        if (ss->inCheck && i > 2)
            break;
        if (is_ok((ss-i)->currentMove))
            (*(ss-i)->continuationHistory)[pc][to] << bonus;
    }
  }


  // update_quiet_stats() updates move sorting heuristics

  void update_quiet_stats(const Position& pos, Stack* ss, Move move, int bonus, int depth) {

    // Update killers
    if (ss->killers[0] != move)
    {
        ss->killers[1] = ss->killers[0];
        ss->killers[0] = move;
    }

    Color us = pos.side_to_move();
    Thread* thisThread = pos.this_thread();
    thisThread->mainHistory[us][from_to(move)] << bonus;
    update_continuation_histories(ss, pos.moved_piece(move), to_sq(move), bonus);

    // Penalty for reversed move in case of moved piece not being a pawn
    if (type_of(pos.moved_piece(move)) != PAWN)
        thisThread->mainHistory[us][from_to(reverse_move(move))] << -bonus;

    // Update countermove history
    if (is_ok((ss-1)->currentMove))
    {
        Square prevSq = to_sq((ss-1)->currentMove);
        thisThread->counterMoves[pos.piece_on(prevSq)][prevSq] = move;
    }

    // Update low ply history
    if (depth > 11 && ss->ply < MAX_LPH)
        thisThread->lowPlyHistory[ss->ply][from_to(move)] << stat_bonus(depth - 7);
  }

  // When playing with strength handicap, choose best move among a set of RootMoves
  // using a statistical rule dependent on 'level'. Idea by Heinz van Saanen.

  Move Skill::pick_best(size_t multiPV) {

    const RootMoves& rootMoves = Threads.main()->rootMoves;
    static PRNG rng(now()); // PRNG sequence should be non-deterministic

    // RootMoves are already sorted by score in descending order
    Value topScore = rootMoves[0].score;
    int delta = std::min(topScore - rootMoves[multiPV - 1].score, PawnValueMg);
    int weakness = 120 - 2 * level;
    int maxScore = -VALUE_INFINITE;

    // Choose best move. For each move score we add two terms, both dependent on
    // weakness. One is deterministic and bigger for weaker levels, and one is
    // random. Then we choose the move with the resulting highest score.
    for (size_t i = 0; i < multiPV; ++i)
    {
        // This is our magic formula
        int push = (  weakness * int(topScore - rootMoves[i].score)
                    + delta * (rng.rand<unsigned>() % weakness)) / 128;

        if (rootMoves[i].score + push >= maxScore)
        {
            maxScore = rootMoves[i].score + push;
            best = rootMoves[i].pv[0];
        }
    }

    return best;
  }

} // namespace


/// MainThread::check_time() is used to print debug info and, more importantly,
/// to detect when we are out of available time and thus stop the search.

void MainThread::check_time() {

  if (--callsCnt > 0)
      return;

  // When using nodes, ensure checking rate is not lower than 0.1% of nodes
  callsCnt = Limits.nodes ? std::min(1024, int(Limits.nodes / 1024)) : 1024;

  static TimePoint lastInfoTime = now();

  TimePoint elapsed = Time.elapsed();
  TimePoint tick = Limits.startTime + elapsed;

  if (tick - lastInfoTime >= 1000)
  {
      lastInfoTime = tick;
      dbg_print();
  }

  // We should not stop pondering until told so by the GUI
  if (ponder)
      return;

  if (   (Limits.use_time_management() && (elapsed > Time.maximum() - 10 || stopOnPonderhit))
      || (Limits.movetime && elapsed >= Limits.movetime)
      || (Limits.nodes && Threads.nodes_searched() >= (uint64_t)Limits.nodes))
      Threads.stop = true;
}


/// UCI::pv() formats PV information according to the UCI protocol. UCI requires
/// that all (if any) unsearched PV lines are sent using a previous search score.

string UCI::pv(const Position& pos, Depth depth, Value alpha, Value beta) {

  std::stringstream ss;
  TimePoint elapsed = Time.elapsed() + 1;
  const RootMoves& rootMoves = pos.this_thread()->rootMoves;
  size_t pvIdx = pos.this_thread()->pvIdx;
  size_t multiPV = std::min((size_t)Options["MultiPV"], rootMoves.size());
  uint64_t nodesSearched = Threads.nodes_searched();
  uint64_t tbHits = Threads.tb_hits() + (TB::RootInTB ? rootMoves.size() : 0);

  for (size_t i = 0; i < multiPV; ++i)
  {
      bool updated = rootMoves[i].score != -VALUE_INFINITE;

      if (depth == 1 && !updated && i > 0)
          continue;

      Depth d = updated ? depth : std::max(1, depth - 1);
      Value v = updated ? rootMoves[i].score : rootMoves[i].previousScore;

      if (v == -VALUE_INFINITE)
          v = VALUE_ZERO;

      bool tb = TB::RootInTB && abs(v) < VALUE_MATE_IN_MAX_PLY;
      v = tb ? rootMoves[i].tbScore : v;

      if (ss.rdbuf()->in_avail()) // Not at first line
          ss << "\n";

      ss << "info"
         << " depth "    << d
         << " seldepth " << rootMoves[i].selDepth
         << " multipv "  << i + 1
         << " score "    << UCI::value(v);

      if (Options["UCI_ShowWDL"])
          ss << UCI::wdl(v, pos.game_ply());

      if (!tb && i == pvIdx)
          ss << (v >= beta ? " lowerbound" : v <= alpha ? " upperbound" : "");

      ss << " nodes "    << nodesSearched
         << " nps "      << nodesSearched * 1000 / elapsed;

      if (elapsed > 1000) // Earlier makes little sense
          ss << " hashfull " << TT.hashfull();

      ss << " tbhits "   << tbHits
         << " time "     << elapsed
         << " pv";

      for (Move m : rootMoves[i].pv)
          ss << " " << UCI::move(m, pos.is_chess960());
  }

  return ss.str();
}


/// RootMove::extract_ponder_from_tt() is called in case we have no ponder move
/// before exiting the search, for instance, in case we stop the search during a
/// fail high at root. We try hard to have a ponder move to return to the GUI,
/// otherwise in case of 'ponder on' we have nothing to think on.

bool RootMove::extract_ponder_from_tt(Position& pos) {

    StateInfo st;
    ASSERT_ALIGNED(&st, Eval::NNUE::CacheLineSize);

    bool ttHit;

    assert(pv.size() == 1);

    if (pv[0] == MOVE_NONE)
        return false;

    pos.do_move(pv[0], st);
    TTEntry* tte = TT.probe(pos.key(), ttHit);

    if (ttHit)
    {
        Move m = tte->move(); // Local copy to be SMP safe
        if (MoveList<LEGAL>(pos).contains(m))
            pv.push_back(m);
    }

    pos.undo_move(pv[0]);
    return pv.size() > 1;
}

void Tablebases::rank_root_moves(Position& pos, Search::RootMoves& rootMoves) {

    RootInTB = false;
    UseRule50 = bool(Options["Syzygy50MoveRule"]);
    ProbeDepth = int(Options["SyzygyProbeDepth"]);
    Cardinality = int(Options["SyzygyProbeLimit"]);
    bool dtz_available = true;

    // Tables with fewer pieces than SyzygyProbeLimit are searched with
    // ProbeDepth == DEPTH_ZERO
    if (Cardinality > MaxCardinality)
    {
        Cardinality = MaxCardinality;
        ProbeDepth = 0;
    }

    if (Cardinality >= popcount(pos.pieces()) && !pos.can_castle(ANY_CASTLING))
    {
        // Rank moves using DTZ tables
        RootInTB = root_probe(pos, rootMoves);

        if (!RootInTB)
        {
            // DTZ tables are missing; try to rank moves using WDL tables
            dtz_available = false;
            RootInTB = root_probe_wdl(pos, rootMoves);
        }
    }

    if (RootInTB)
    {
        // Sort moves according to TB rank
        std::stable_sort(rootMoves.begin(), rootMoves.end(),
                  [](const RootMove &a, const RootMove &b) { return a.tbRank > b.tbRank; } );

        // Probe during search only if DTZ is not available and we are winning
        if (dtz_available || rootMoves[0].tbScore <= VALUE_DRAW)
            Cardinality = 0;
    }
    else
    {
        // Clean up if root_probe() and root_probe_wdl() have failed
        for (auto& m : rootMoves)
            m.tbRank = 0;
    }
}

} // namespace Stockfish
