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

#include <cassert>
#include <cmath>
#include <iostream>
#include <sstream>
#include <string>

#include "evaluate.h"
#include "movegen.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "timeman.h"
#include "tt.h"
#include "uci.h"
#include "syzygy/tbprobe.h"

using namespace std;

namespace Stockfish {

extern vector<string> setup_bench(const Position&, istream&);

namespace {

  // FEN string of the initial position, normal chess
  const char* StartFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";


  // position() is called when engine receives the "position" UCI command.
  // The function sets up the position described in the given FEN string ("fen")
  // or the starting position ("startpos") and then makes the moves given in the
  // following move list ("moves").

  void position(Position& pos, istringstream& is, StateListPtr& states) {

    Move m;
    string token, fen;

    is >> token;

    if (token == "startpos")
    {
        fen = StartFEN;
        is >> token; // Consume "moves" token if any
    }
    else if (token == "fen")
        while (is >> token && token != "moves")
            fen += token + " ";
    else
        return;

    states = StateListPtr(new std::deque<StateInfo>(1)); // Drop old and create a new one
    pos.set(fen, Options["UCI_Chess960"], &states->back(), Threads.main());

    // Parse move list (if any)
    while (is >> token && (m = UCI::to_move(pos, token)) != MOVE_NONE)
    {
        states->emplace_back();
        pos.do_move(m, states->back());
    }
  }

  // trace_eval() prints the evaluation for the current position, consistent with the UCI
  // options set so far.

  void trace_eval(Position& pos) {

    StateListPtr states(new std::deque<StateInfo>(1));
    Position p;
    p.set(pos.fen(), Options["UCI_Chess960"], &states->back(), Threads.main());

    Eval::NNUE::verify();

    sync_cout << "\n" << Eval::trace(p) << sync_endl;
  }


  // setoption() is called when engine receives the "setoption" UCI command. The
  // function updates the UCI option ("name") to the given value ("value").

  void setoption(istringstream& is) {

    string token, name, value;

    is >> token; // Consume "name" token

    // Read option name (can contain spaces)
    while (is >> token && token != "value")
        name += (name.empty() ? "" : " ") + token;

    // Read option value (can contain spaces)
    while (is >> token)
        value += (value.empty() ? "" : " ") + token;

    if (Options.count(name))
        Options[name] = value;
    else
        sync_cout << "No such option: " << name << sync_endl;
  }


  // go() is called when engine receives the "go" UCI command. The function sets
  // the thinking time and other parameters from the input string, then starts
  // the search.

  void go(Position& pos, istringstream& is, StateListPtr& states) {

    Search::LimitsType limits;
    string token;
    bool ponderMode = false;

    limits.startTime = now(); // As early as possible!

    while (is >> token)
        if (token == "searchmoves") // Needs to be the last command on the line
            while (is >> token)
                limits.searchmoves.push_back(UCI::to_move(pos, token));

        else if (token == "wtime")     is >> limits.time[WHITE];
        else if (token == "btime")     is >> limits.time[BLACK];
        else if (token == "winc")      is >> limits.inc[WHITE];
        else if (token == "binc")      is >> limits.inc[BLACK];
        else if (token == "movestogo") is >> limits.movestogo;
        else if (token == "depth")     is >> limits.depth;
        else if (token == "nodes")     is >> limits.nodes;
        else if (token == "movetime")  is >> limits.movetime;
        else if (token == "mate")      is >> limits.mate;
        else if (token == "perft")     is >> limits.perft;
        else if (token == "infinite")  limits.infinite = 1;
        else if (token == "ponder")    ponderMode = true;

    Threads.start_thinking(pos, states, limits, ponderMode);
  }


  // bench() is called when engine receives the "bench" command. Firstly
  // a list of UCI commands is setup according to bench parameters, then
  // it is run one by one printing a summary at the end.

  void bench(Position& pos, istream& args, StateListPtr& states) {

    string token;
    uint64_t num, nodes = 0, cnt = 1;

    vector<string> list = setup_bench(pos, args);
    //num = count_if(list.begin(), list.end(), [](string s) { return s.find("go ") == 0 || s.find("eval") == 0; });

    /*
     * SA80
	        c0  cutNode, 
		c1  PvNode,
		    captureOrPromotion, 
		    givesCheck, 
		    ss->inCheck, 
		c5  improving, 
		    likelyFailLow, 
		    ttCapture,
		    more_than_one(pos.checkers()),
		    doubleExtension,
		c10 moveCountPruning,
	            singularQuietLMR,
	            ss->ttPv && !PvNode,
		    move == ss->killers[0],    
		c14 move == ss->killers[1],    
	        c15 move == countermove,
		    bool(int(depth) & 1),
		    bool(int(depth) & 2),
		    bool(int(depth) & 4),
		    bool(int(depth) & 8),
		c20 bool(moveCount & 1),
		    bool(moveCount & 2),
		    bool(moveCount & 4),
		    bool(moveCount & 8),
		    bool(moveCount & 16),
		    bool(moveCount & 32),
		c26 bool(moveCount & 64),
		c27 bool(type_of(movedPiece) & 1),
		    bool(type_of(movedPiece) & 2),
		    bool(type_of(movedPiece) & 4),
		c30 bool(pos.count<ALL_PIECES>() & 1),
		    bool(pos.count<ALL_PIECES>() & 2),
		    bool(pos.count<ALL_PIECES>() & 4),
		c33 bool(pos.count<ALL_PIECES>() & 8),
		    bool(pos.count<ALL_PIECES>() & 16),
		    bool(extension),

		    ss->ttHit,
		    bool(ttMove),
		c37 bool(bestMove),
		    eval >= beta,
		c39 ss->staticEval >= beta,
		c40 (ss-1)->inCheck,
		    type_of(move) == PROMOTION,
		    bool((ss-1)->moveCount & 1),
		    bool((ss-1)->moveCount & 2),
		    bool((ss-1)->moveCount & 4),
		    bool((ss-1)->moveCount & 8),
		    bool((ss-1)->moveCount & 16),
	        c48 bool((ss-1)->moveCount & 32),
		    priorCapture,

                c50 bool(thisThread->mainHistory[us][from_to(move)] & (1 <<  0)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 <<  1)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 <<  2)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 <<  3)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 <<  4)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 <<  5)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 <<  6)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 <<  7)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 <<  8)),
               c59  bool(thisThread->mainHistory[us][from_to(move)] & (1 <<  9)),
               c60  bool(thisThread->mainHistory[us][from_to(move)] & (1 << 10)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 << 11)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 << 12)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 << 13)),
                    bool(thisThread->mainHistory[us][from_to(move)] & (1 << 14)),

               c65  bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  0)),
                    bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  1)),
               c67  bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  2)),
                    bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  3)),
                    bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  4)),
              c70   bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  5)),
                    bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  6)),
                    bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  7)),
                    bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  8)),
                    bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  9)),
                c75 bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  10)),
                    bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  11)),
                    bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  12)),
                    bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  13)),
                c79 bool(thisThread->captureHistory[movedPiece][to_sq(move)][type_of(pos.captured_piece())] & (1 <<  14))
     * */
    constexpr bool MAXIMIZE = false;
    constexpr bool USE_CRAMER = true;
    constexpr bool USE_CRAMER_AND_HIT = false;
    double val = 0, support = 0;

    double bestVal = 0;
    double bestSupport = 0;
    double baseVal = 0;
    double cVal[F_N];

    auto doEval = [&]() {
	val = 0;
	double w = get_hit(10);
	auto wFunc = [&](double freq) { return 1; };

	double get_hit1 = MAXIMIZE ? get_hit(1) : 1 - get_hit(1);
	double get_hit0 = MAXIMIZE ? get_hit(0) : 1 - get_hit(0);

        if(USE_CRAMER_AND_HIT)
        {
            double valc = get_cramer();
            double v1 = get_hit1;
            double v0 = get_hit0;
            double valh = v1 >= v0 ? v1 : -v0;

	    val = (valc + valh) / 2;
        }	 
	else if(USE_CRAMER)
        {
            val = get_cramer();
        }	 
	else // HIT
	{
            double v1 = get_hit1;
            double v0 = get_hit0;
            val = v1 >= v0 ? v1 : -v0;
	}

	support = (val >= 0 ? get_hit(10) : 1 - get_hit(10) );
    };

    std::cerr << "N: " << F_N << std::endl;

    func.init();
    const std::string measure = (USE_CRAMER_AND_HIT ? "cramer+hit" : USE_CRAMER ? "cramer" : "hit");

        for (const auto& cmd : list)
        {
            istringstream is(cmd);
            is >> skipws >> token;

            if (token == "go" || token == "eval")
            {
            //cerr << "\nPosition: " << cnt++ << '/' << num << " (" << pos.fen() << ")" << endl;
                if (token == "go")
                {
                   go(pos, is, states);
                   Threads.main()->wait_for_search_finished();
                   nodes += Threads.nodes_searched();
		   //dbg_print();
                }
                else
                   trace_eval(pos);
            }
            else if (token == "setoption")  setoption(is);
            else if (token == "position")   position(pos, is, states);
            else if (token == "ucinewgame") { Search::clear(); } // Search::clear() may take some while
        }

    std::cerr << "Sample size: " << samples.size() << std::endl;

    // evaluate base condition CC
    func.evaluate();
    doEval();
    cerr << "BASE val=" << val << " support=" << support << std::endl;
    baseVal = val;
    //func.print(cerr);

    // evaluate addidinc different conditions
    for(int c = 0; c < F_N; ++c)
    {
        func.initCondition(c, true);
        func.evaluate();
        doEval();
        cVal[c] = val;

	if(std::abs(val) > std::abs(bestVal) || (std::abs(val) == std::abs(bestVal) && support > bestSupport))
	{
            bestVal = val;
            bestSupport = support;
            cerr << "=>c" << c << " val=" << val << " support=" << support << std::endl;
	}
	else
	{
            cerr << "c" << c << " val=" << val << " support=" << support << std::endl;
	}
    }

    /*
    for(int it = 0;true;++it)
    {



	if(it == 0 || std::abs(val) > std::abs(curVal))
	{
		curVal = val;
	        if(!SIMULATED_ANNEALING || it == 0 || Greater(std::abs(curVal), std::abs(bestVal)))
		{
		    best = func;
		    bestVal = curVal;
		    bestSupport = support;
	            if(SIMULATED_ANNEALING)
		        cerr << "!it=" << it << " " << measure << "=" << bestVal << " support=" << support << " T=" << T << " msteps=" << steps << " => ";
		    else
		        cerr << "it=" << it << " " << measure << "=" << bestVal << " support=" << support << " msteps=" << steps << " => ";
                    best.print(cerr);
		    fails = 0;
		}
		else if(SIMULATED_ANNEALING)
		{
		    cerr << "+it=" << it << " " << measure << "=" << curVal << " support=" << support << " T=" << T << " msteps=" << steps << " => ";
                    func.print(cerr);
		}
	}
	else if(SIMULATED_ANNEALING)
        {
		double p = std::exp(-std::abs(std::abs(curVal) - std::abs(val))/T);
		double r = std::rand()/double(RAND_MAX);
		if(r <= p) // accept
		{
		    curVal = val;
		    cerr << "-it=" << it << " " << measure << "=" << curVal << " support=" << support << " T=" << T << " p=" << p << " msteps=" << steps << " => ";
                    func.print(cerr);
		}
		else //reject
		{
		cerr << "=it=" << it << " " << measure << "=" << val << " support=" << support << " T=" << T << " p=" << p << " msteps=" << steps << " => ";
                    func.print(cerr);
		    func = tmp;
		}
	}
	else if(HILL_CLIMBING)
	{
		func = tmp;
		fails++;
	}

	if(it && it % 100 == 0)
	{
	            if(SIMULATED_ANNEALING)
		        cerr << "BEST it=" << it << " " << measure << "=" << bestVal << " support=" << bestSupport << " T=" << T << " msteps=" << steps << " => ";
		    else
		        cerr << "BEST it=" << it << " " << measure << "=" << bestVal << " support=" << bestSupport << " msteps=" << steps << " => ";
                    best.print(cerr);
	}
    }
    */


  }

  // The win rate model returns the probability (per mille) of winning given an eval
  // and a game-ply. The model fits rather accurately the LTC fishtest statistics.
  int win_rate_model(Value v, int ply) {

     // The model captures only up to 240 plies, so limit input (and rescale)
     double m = std::min(240, ply) / 64.0;

     // Coefficients of a 3rd order polynomial fit based on fishtest data
     // for two parameters needed to transform eval to the argument of a
     // logistic function.
     double as[] = {-3.68389304,  30.07065921, -60.52878723, 149.53378557};
     double bs[] = {-2.0181857,   15.85685038, -29.83452023,  47.59078827};
     double a = (((as[0] * m + as[1]) * m + as[2]) * m) + as[3];
     double b = (((bs[0] * m + bs[1]) * m + bs[2]) * m) + bs[3];

     // Transform eval to centipawns with limited range
     double x = std::clamp(double(100 * v) / PawnValueEg, -2000.0, 2000.0);

     // Return win rate in per mille (rounded to nearest)
     return int(0.5 + 1000 / (1 + std::exp((a - x) / b)));
  }

} // namespace


/// UCI::loop() waits for a command from stdin, parses it and calls the appropriate
/// function. Also intercepts EOF from stdin to ensure gracefully exiting if the
/// GUI dies unexpectedly. When called with some command line arguments, e.g. to
/// run 'bench', once the command is executed the function returns immediately.
/// In addition to the UCI ones, also some additional debug commands are supported.

void UCI::loop(int argc, char* argv[]) {

  Position pos;
  string token, cmd;
  StateListPtr states(new std::deque<StateInfo>(1));

  pos.set(StartFEN, false, &states->back(), Threads.main());

  for (int i = 1; i < argc; ++i)
      cmd += std::string(argv[i]) + " ";

  do {
      if (argc == 1 && !getline(cin, cmd)) // Block here waiting for input or EOF
          cmd = "quit";

      istringstream is(cmd);

      token.clear(); // Avoid a stale if getline() returns empty or blank line
      is >> skipws >> token;

      if (    token == "quit"
          ||  token == "stop")
          Threads.stop = true;

      // The GUI sends 'ponderhit' to tell us the user has played the expected move.
      // So 'ponderhit' will be sent if we were told to ponder on the same move the
      // user has played. We should continue searching but switch from pondering to
      // normal search.
      else if (token == "ponderhit")
          Threads.main()->ponder = false; // Switch to normal search

      else if (token == "uci")
          sync_cout << "id name " << engine_info(true)
                    << "\n"       << Options
                    << "\nuciok"  << sync_endl;

      else if (token == "setoption")  setoption(is);
      else if (token == "go")         go(pos, is, states);
      else if (token == "position")   position(pos, is, states);
      else if (token == "ucinewgame") Search::clear();
      else if (token == "isready")    sync_cout << "readyok" << sync_endl;

      // Additional custom non-UCI commands, mainly for debugging.
      // Do not use these commands during a search!
      else if (token == "flip")     pos.flip();
      else if (token == "bench")    bench(pos, is, states);
      else if (token == "d")        sync_cout << pos << sync_endl;
      else if (token == "eval")     trace_eval(pos);
      else if (token == "compiler") sync_cout << compiler_info() << sync_endl;
      else if (token == "export_net")
      {
          std::optional<std::string> filename;
          std::string f;
          if (is >> skipws >> f)
              filename = f;
          Eval::NNUE::save_eval(filename);
      }
      else if (!token.empty() && token[0] != '#')
          sync_cout << "Unknown command: " << cmd << sync_endl;

  } while (token != "quit" && argc == 1); // Command line args are one-shot
}


/// UCI::value() converts a Value to a string suitable for use with the UCI
/// protocol specification:
///
/// cp <x>    The score from the engine's point of view in centipawns.
/// mate <y>  Mate in y moves, not plies. If the engine is getting mated
///           use negative values for y.

string UCI::value(Value v) {

  assert(-VALUE_INFINITE < v && v < VALUE_INFINITE);

  stringstream ss;

  if (abs(v) < VALUE_MATE_IN_MAX_PLY)
      ss << "cp " << v * 100 / PawnValueEg;
  else
      ss << "mate " << (v > 0 ? VALUE_MATE - v + 1 : -VALUE_MATE - v) / 2;

  return ss.str();
}


/// UCI::wdl() report WDL statistics given an evaluation and a game ply, based on
/// data gathered for fishtest LTC games.

string UCI::wdl(Value v, int ply) {

  stringstream ss;

  int wdl_w = win_rate_model( v, ply);
  int wdl_l = win_rate_model(-v, ply);
  int wdl_d = 1000 - wdl_w - wdl_l;
  ss << " wdl " << wdl_w << " " << wdl_d << " " << wdl_l;

  return ss.str();
}


/// UCI::square() converts a Square to a string in algebraic notation (g1, a7, etc.)

std::string UCI::square(Square s) {
  return std::string{ char('a' + file_of(s)), char('1' + rank_of(s)) };
}


/// UCI::move() converts a Move to a string in coordinate notation (g1f3, a7a8q).
/// The only special case is castling, where we print in the e1g1 notation in
/// normal chess mode, and in e1h1 notation in chess960 mode. Internally all
/// castling moves are always encoded as 'king captures rook'.

string UCI::move(Move m, bool chess960) {

  Square from = from_sq(m);
  Square to = to_sq(m);

  if (m == MOVE_NONE)
      return "(none)";

  if (m == MOVE_NULL)
      return "0000";

  if (type_of(m) == CASTLING && !chess960)
      to = make_square(to > from ? FILE_G : FILE_C, rank_of(from));

  string move = UCI::square(from) + UCI::square(to);

  if (type_of(m) == PROMOTION)
      move += " pnbrqk"[promotion_type(m)];

  return move;
}


/// UCI::to_move() converts a string representing a move in coordinate notation
/// (g1f3, a7a8q) to the corresponding legal Move, if any.

Move UCI::to_move(const Position& pos, string& str) {

  if (str.length() == 5) // Junior could send promotion piece in uppercase
      str[4] = char(tolower(str[4]));

  for (const auto& m : MoveList<LEGAL>(pos))
      if (str == UCI::move(m, pos.is_chess960()))
          return m;

  return MOVE_NONE;
}

} // namespace Stockfish
