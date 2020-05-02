/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2020 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#include "bitboard.h"
#include "pawns.h"
#include "position.h"
#include "thread.h"
#include "material.h"

namespace {

  constexpr bool USE_FOR_TUNING = false;

  int IBackwardMG;
  int IBackwardEG;
  int IDoubledMG;
  int IDoubledEG;
  int IIsolatedMG;
  int IIsolatedEG;
  int IWeakLeverMG;
  int IWeakLeverEG;
  int IWeakUnopposedMG;
  int IWeakUnopposedEG;
  int IConnected[RANK_NB];

  int IBlockedStormMG;
  int IBlockedStormEG;
  
  int IDoubledMGS;
  int IDoubledEGS;
  
  int IDoubledMGU;
  int IDoubledEGU;
  
  

  #define V Value
  #define S(mg, eg) make_score(mg, eg)
  
  constexpr Score DoubledS       = S(0, 0);
  constexpr Score DoubledU       = S(11, 56);

  // Pawn penalties
  constexpr Score Backward      = S( 9, 24);
  constexpr Score BlockedStorm  = S(82, 82);
  constexpr Score Doubled       = S(11, 56);
  constexpr Score Isolated      = S( 5, 15);
  constexpr Score WeakLever     = S( 0, 56);
  constexpr Score WeakUnopposed = S(13, 27);

  // Connected pawn bonus
  constexpr int Connected[RANK_NB] = { 0, 7, 8, 12, 29, 48, 86 };

  // Strength of pawn shelter for our king by [distance from edge][rank].
  // RANK_1 = 0 is used for files where we have no pawn, or pawn is behind our king.
  constexpr Value ShelterStrength[int(FILE_NB) / 2][RANK_NB] = {
    { V( -6), V( 81), V( 93), V( 58), V( 39), V( 18), V(  25) },
    { V(-43), V( 61), V( 35), V(-49), V(-29), V(-11), V( -63) },
    { V(-10), V( 75), V( 23), V( -2), V( 32), V(  3), V( -45) },
    { V(-39), V(-13), V(-29), V(-52), V(-48), V(-67), V(-166) }
  };

  // Danger of enemy pawns moving toward our king by [distance from edge][rank].
  // RANK_1 = 0 is used for files where the enemy has no pawn, or their pawn
  // is behind our king. Note that UnblockedStorm[0][1-2] accommodate opponent pawn
  // on edge, likely blocked by our king.
  constexpr Value UnblockedStorm[int(FILE_NB) / 2][RANK_NB] = {
    { V( 85), V(-289), V(-166), V(97), V(50), V( 45), V( 50) },
    { V( 46), V( -25), V( 122), V(45), V(37), V(-10), V( 20) },
    { V( -6), V(  51), V( 168), V(34), V(-2), V(-22), V(-14) },
    { V(-15), V( -11), V( 101), V( 4), V(11), V(-15), V(-29) }
  };

  #undef S
  #undef V

  template<Color Us>
  Score evaluate(const Position& pos, Pawns::Entry* e) {

    constexpr Color     Them = ~Us;
    constexpr Direction Up   = pawn_push(Us);

    Phase phase = Material::probe(pos)->game_phase();
    Score PBackward = make_score(Tuning::getParam(IBackwardMG), Tuning::getParam(IBackwardEG));
    Score PDoubled = make_score(Tuning::getParam(IDoubledMG), Tuning::getParam(IDoubledEG));
    Score PIsolated = make_score(Tuning::getParam(IIsolatedMG), Tuning::getParam(IIsolatedEG));
    Score PWeakLever = make_score(Tuning::getParam(IWeakLeverMG), Tuning::getParam(IWeakLeverEG));
    Score PWeakUnopposed = make_score(Tuning::getParam(IWeakUnopposedMG), Tuning::getParam(IWeakUnopposedEG));
	
    Bitboard neighbours, stoppers, support, phalanx, opposed;
    Bitboard lever, leverPush, blocked;
    Square s;
    bool backward, passed, doubled;
    Score score = SCORE_ZERO;
    const Square* pl = pos.squares<PAWN>(Us);

    Bitboard ourPawns   = pos.pieces(  Us, PAWN);
    Bitboard theirPawns = pos.pieces(Them, PAWN);

    Bitboard doubleAttackThem = pawn_double_attacks_bb<Them>(theirPawns);

    e->passedPawns[Us] = 0;
    e->kingSquares[Us] = SQ_NONE;
    e->pawnAttacks[Us] = e->pawnAttacksSpan[Us] = pawn_attacks_bb<Us>(ourPawns);

    // Loop through all pawns of the current color and score each pawn
    while ((s = *pl++) != SQ_NONE)
    {
        assert(pos.piece_on(s) == make_piece(Us, PAWN));

        Rank r = relative_rank(Us, s);

        // Flag the pawn
        opposed    = theirPawns & forward_file_bb(Us, s);
        blocked    = theirPawns & (s + Up);
        stoppers   = theirPawns & passed_pawn_span(Us, s);
        lever      = theirPawns & PawnAttacks[Us][s];
        leverPush  = theirPawns & PawnAttacks[Us][s + Up];
        doubled    = ourPawns   & (s - Up);
        neighbours = ourPawns   & adjacent_files_bb(s);
        phalanx    = neighbours & rank_bb(s);
        support    = neighbours & rank_bb(s - Up);

        e->blockedCount += blocked || more_than_one(leverPush);

        // A pawn is backward when it is behind all pawns of the same color on
        // the adjacent files and cannot safely advance.
        backward =  !(neighbours & forward_ranks_bb(Them, s + Up))
                  && (leverPush | blocked);

        // Compute additional span if pawn is not backward nor blocked
        if (!backward && !blocked)
            e->pawnAttacksSpan[Us] |= pawn_attack_span(Us, s);

        // A pawn is passed if one of the three following conditions is true:
        // (a) there is no stoppers except some levers
        // (b) the only stoppers are the leverPush, but we outnumber them
        // (c) there is only one front stopper which can be levered.
        //     (Refined in Evaluation::passed)
        passed =   !(stoppers ^ lever)
                || (   !(stoppers ^ leverPush)
                    && popcount(phalanx) >= popcount(leverPush))
                || (   stoppers == blocked && r >= RANK_5
                    && (shift<Up>(support) & ~(theirPawns | doubleAttackThem)));

        passed &= !(forward_file_bb(Us, s) & ourPawns);

        // Passed pawns will be properly scored later in evaluation when we have
        // full attack info.
        if (passed)
            e->passedPawns[Us] |= s;

        // Score this pawn
        if (support | phalanx)
        {
            double v =  Tuning::getParam(IConnected[r]) * (4 + 2 * bool(phalanx) - 2 * bool(opposed) - bool(blocked)) / 2
                   + 21 * popcount(support);

            score += make_score(v, v * (r - 2) / 4);
	    double grad_mg = (4 + 2 * bool(phalanx) - 2 * bool(opposed) - bool(blocked)) / 2.0;
	    double grad_eg = grad_mg * (r - 2) / 4;
	    double grad = (grad_mg * phase + grad_eg * (PHASE_MIDGAME - phase)) / PHASE_MIDGAME;
   	    Tuning::updateGradient(Us, IConnected[r], grad);
        }

        else if (!neighbours)
	{
            score -=   PIsolated
                     + PWeakUnopposed * !opposed;
   		Tuning::updateGradient(Us, IIsolatedMG, -1.0 * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IIsolatedEG, -1.0 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		Tuning::updateGradient(Us, IWeakUnopposedMG, -1.0 * !opposed * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IWeakUnopposedEG, -1.0 * !opposed * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
	}

        else if (backward)
	{
            score -=   PBackward
                     + PWeakUnopposed * !opposed;

   		Tuning::updateGradient(Us, IBackwardMG, -1.0 * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IBackwardEG, -1.0 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		Tuning::updateGradient(Us, IWeakUnopposedMG, -1.0 * !opposed * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IWeakUnopposedEG, -1.0 * !opposed * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
	}

        if (!support)
	{
		Score PDoubledU = make_score(Tuning::getParam(IDoubledMGU), Tuning::getParam(IDoubledEGU));
            score -=   (opposed ? PDoubled : PDoubledU) * doubled
                     + PWeakLever * more_than_one(lever);
   		//Tuning::updateGradient(Us, IDoubledMG, -1.0 * doubled * phase / PHASE_MIDGAME);
		//Tuning::updateGradient(Us, IDoubledEG, -1.0 * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		Tuning::updateGradient(Us, IWeakLeverMG, -1.0 * more_than_one(lever) * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IWeakLeverEG, -1.0 * more_than_one(lever) * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		
		Tuning::updateGradient(Us, IDoubledMG, -1.0 * bool(opposed) * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledEG, -1.0 * bool(opposed) * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		
		Tuning::updateGradient(Us, IDoubledMGU, -1.0 * !opposed * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledEGU, -1.0 * !opposed * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		
	}
	else{
		Score PDoubledS = make_score(Tuning::getParam(IDoubledMGS), Tuning::getParam(IDoubledEGS));
		score -=   PDoubledS * doubled
                     ;//+ PWeakLever * more_than_one(lever);
   		Tuning::updateGradient(Us, IDoubledMGS, -1.0 * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledEGS, -1.0 * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		
	}
    }

    return score;
  }

} // namespace

namespace Pawns {

/// Pawns::probe() looks up the current position's pawns configuration in
/// the pawns hash table. It returns a pointer to the Entry if the position
/// is found. Otherwise a new Entry is computed and stored there, so we don't
/// have to recompute all when the same pawns configuration occurs again.

Entry* probe(const Position& pos) {

  Key key = pos.pawn_key();
  Entry* e = pos.this_thread()->pawnsTable[key];

  //if (e->key == key)
   //   return e;

  e->key = key;
  e->blockedCount = 0;
  e->scores[WHITE] = evaluate<WHITE>(pos, e);
  e->scores[BLACK] = evaluate<BLACK>(pos, e);
  return e;
}


/// Entry::evaluate_shelter() calculates the shelter bonus and the storm
/// penalty for a king, looking at the king file and the two closest files.

int blockedstorm_mg;
int blockedstorm_eg;

template<Color Us>
Score Entry::evaluate_shelter(const Position& pos, Square ksq) {

  constexpr Color Them = ~Us;

  Bitboard b = pos.pieces(PAWN) & ~forward_ranks_bb(Them, ksq);
  Bitboard ourPawns = b & pos.pieces(Us);
  Bitboard theirPawns = b & pos.pieces(Them);

  blockedstorm_mg = 0;
  blockedstorm_eg = 0;
  Score bonus = make_score(5, 5);
  Score PBlockedStorm = make_score(Tuning::getParam(IBlockedStormMG), Tuning::getParam(IBlockedStormEG));

  File center = Utility::clamp(file_of(ksq), FILE_B, FILE_G);
  for (File f = File(center - 1); f <= File(center + 1); ++f)
  {
      b = ourPawns & file_bb(f);
      int ourRank = b ? relative_rank(Us, frontmost_sq(Them, b)) : 0;

      b = theirPawns & file_bb(f);
      int theirRank = b ? relative_rank(Us, frontmost_sq(Them, b)) : 0;

      File d = File(edge_distance(f));
      bonus += make_score(ShelterStrength[d][ourRank], 0);

      if (ourRank && (ourRank == theirRank - 1))
      {
          bonus -= PBlockedStorm * int(theirRank == RANK_3);
	  blockedstorm_mg -= int(theirRank == RANK_3);
	  blockedstorm_eg -= int(theirRank == RANK_3);
      }
      else
          bonus -= make_score(UnblockedStorm[d][theirRank], 0);
  }

  return bonus;
}


/// Entry::do_king_safety() calculates a bonus for king safety. It is called only
/// when king square changes, which is about 20% of total king_safety() calls.

template<Color Us>
Score Entry::do_king_safety(const Position& pos) {

  Square ksq = pos.square<KING>(Us);
  kingSquares[Us] = ksq;
  castlingRights[Us] = pos.castling_rights(Us);
  auto compare = [](Score a, Score b) { return mg_value(a) < mg_value(b); };

  Score shelter = evaluate_shelter<Us>(pos, ksq);
  int bs_mg = blockedstorm_mg;
  int bs_eg = blockedstorm_mg;

  Phase phase = Material::probe(pos)->game_phase();

  // If we can castle use the bonus after castling if it is bigger

  if (pos.can_castle(Us & KING_SIDE))
  {
      Score sh = evaluate_shelter<Us>(pos, relative_square(Us, SQ_G1));
      if(compare(shelter, sh))
      {
          bs_mg = blockedstorm_mg;
          bs_eg = blockedstorm_mg;
      }
      shelter = std::max(shelter, sh, compare);
  }

  if (pos.can_castle(Us & QUEEN_SIDE))
  {
      Score sh = evaluate_shelter<Us>(pos, relative_square(Us, SQ_C1));
      if(compare(shelter, sh))
      {
          bs_mg = blockedstorm_mg;
          bs_eg = blockedstorm_mg;
      }
      shelter = std::max(shelter, sh, compare);
  }

  // In endgame we like to bring our king near our closest pawn
  Bitboard pawns = pos.pieces(Us, PAWN);
  int minPawnDist = 6;

  if (pawns & PseudoAttacks[KING][ksq])
      minPawnDist = 1;
  else while (pawns)
      minPawnDist = std::min(minPawnDist, distance(ksq, pop_lsb(&pawns)));

  Tuning::updateGradient(Us, IBlockedStormMG, bs_mg * phase / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormEG, bs_eg * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
  return shelter - make_score(0, 16 * minPawnDist);
}

void init() {
	IBackwardMG = Tuning::addParam(mg_value(Backward), USE_FOR_TUNING);
	IBackwardEG = Tuning::addParam(eg_value(Backward), USE_FOR_TUNING);
	IDoubledMG = Tuning::addParam(mg_value(Doubled), true);
	IDoubledEG = Tuning::addParam(eg_value(Doubled), true);
	IIsolatedMG = Tuning::addParam(mg_value(Isolated), USE_FOR_TUNING);
	IIsolatedEG = Tuning::addParam(eg_value(Isolated), USE_FOR_TUNING);
	IWeakLeverMG = Tuning::addParam(mg_value(WeakLever), USE_FOR_TUNING);
	IWeakLeverEG = Tuning::addParam(eg_value(WeakLever), USE_FOR_TUNING);
	IWeakUnopposedMG = Tuning::addParam(mg_value(WeakUnopposed), USE_FOR_TUNING);
	IWeakUnopposedEG = Tuning::addParam(eg_value(WeakUnopposed), USE_FOR_TUNING);
	for(Rank r = RANK_2; r < RANK_8; ++r)
		IConnected[r] = Tuning::addParam(Connected[r], USE_FOR_TUNING);

	IBlockedStormMG = Tuning::addParam(mg_value(BlockedStorm), USE_FOR_TUNING);
	IBlockedStormEG = Tuning::addParam(eg_value(BlockedStorm), USE_FOR_TUNING);
	
	IDoubledMGS = Tuning::addParam(mg_value(DoubledS), false);
	IDoubledEGS = Tuning::addParam(eg_value(DoubledS), false);
	
	IDoubledMGU = Tuning::addParam(mg_value(DoubledU), true);
	IDoubledEGU = Tuning::addParam(eg_value(DoubledU), true);
	
}

// Explicit template instantiation
template Score Entry::do_king_safety<WHITE>(const Position& pos);
template Score Entry::do_king_safety<BLACK>(const Position& pos);

} // namespace Pawns
