/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2014 Marco Costalba, Joona Kiiski, Tord Romstad

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
#include "bitcount.h"
#include "pawns.h"
#include "position.h"
#include "thread.h"
#include "uci.h"

namespace {

  #define V Value
  #define S(mg, eg) make_score(mg, eg)

  // Doubled pawn penalty by file
  Score Doubled[FILE_NB] = {
    S(13, 43), S(20, 48), S(23, 48), S(23, 48),
    S(23, 48), S(23, 48), S(20, 48), S(13, 43) };

  // Isolated pawn penalty by opposed flag and file
  Score Isolated[2][FILE_NB] = {
  { S(37, 45), S(54, 52), S(60, 52), S(60, 52),
    S(60, 52), S(60, 52), S(54, 52), S(37, 45) },
  { S(25, 30), S(36, 35), S(40, 35), S(40, 35),
    S(40, 35), S(40, 35), S(36, 35), S(25, 30) } };

  // Backward pawn penalty by opposed flag and file
  Score Backward[2][FILE_NB] = {
  { S(30, 42), S(43, 46), S(49, 46), S(49, 46),
    S(49, 46), S(49, 46), S(43, 46), S(30, 42) },
  { S(20, 28), S(29, 31), S(33, 31), S(33, 31),
    S(33, 31), S(33, 31), S(29, 31), S(20, 28) } };

  // Connected pawn bonus by opposed, phalanx flags and rank
  Score Connected[2][2][RANK_NB];

  // Levers bonus by rank
  Score Lever[RANK_NB] = {
    S( 0, 0), S( 0, 0), S(0, 0), S(0, 0),
    S(20,20), S(40,40), S(0, 0), S(0, 0) };

  // Unsupported pawn penalty
  Score UnsupportedPawnPenalty = S(20, 10);

  // Weakness of our pawn shelter in front of the king by [distance from edge][rank]
  const Value ShelterWeakness[][RANK_NB] = {
  { V(100), V(13), V(24), V(64), V(89), V( 93), V(104) },
  { V(110), V( 1), V(29), V(75), V(96), V(102), V(107) },
  { V(102), V( 0), V(39), V(74), V(88), V(101), V( 98) },
  { V( 88), V( 4), V(33), V(67), V(92), V( 94), V(107) } };

  // Danger of enemy pawns moving toward our king by [type][distance from edge][rank]
  const Value StormDanger[][4][RANK_NB] = {
  { { V( 0),  V(  63), V( 128), V(43), V(27) },
    { V( 0),  V(  62), V( 131), V(44), V(26) },
    { V( 0),  V(  59), V( 121), V(50), V(28) },
    { V( 0),  V(  62), V( 127), V(54), V(28) } },
  { { V(24),  V(  40), V(  93), V(42), V(22) },
    { V(24),  V(  28), V( 101), V(38), V(20) },
    { V(24),  V(  32), V(  95), V(36), V(23) },
    { V(27),  V(  24), V(  99), V(36), V(24) } },
  { { V( 0),  V(   0), V(  81), V(16), V( 6) },
    { V( 0),  V(   0), V( 165), V(29), V( 9) },
    { V( 0),  V(   0), V( 163), V(23), V(12) },
    { V( 0),  V(   0), V( 161), V(28), V(13) } },
  { { V( 0),  V(-296), V(-299), V(55), V(25) },
    { V( 0),  V(  67), V( 131), V(46), V(21) },
    { V( 0),  V(  65), V( 135), V(50), V(31) },
    { V( 0),  V(  62), V( 128), V(51), V(24) } } };

  // Max bonus for king safety. Corresponds to start position with all the pawns
  // in front of the king and no enemy pawn on the horizon.
  const Value MaxSafetyBonus = V(257);

  #undef S
  #undef V

  template<Color Us>
  Score evaluate(const Position& pos, Pawns::Entry* e) {

    const Color  Them  = (Us == WHITE ? BLACK    : WHITE);
    const Square Up    = (Us == WHITE ? DELTA_N  : DELTA_S);
    const Square Right = (Us == WHITE ? DELTA_NE : DELTA_SW);
    const Square Left  = (Us == WHITE ? DELTA_NW : DELTA_SE);

    Bitboard b, p, doubled, connected;
    Square s;
    bool passed, isolated, opposed, phalanx, backward, unsupported, lever;
    Score score = SCORE_ZERO;
    const Square* pl = pos.list<PAWN>(Us);
    const Bitboard* pawnAttacksBB = StepAttacksBB[make_piece(Us, PAWN)];

    Bitboard ourPawns   = pos.pieces(Us  , PAWN);
    Bitboard theirPawns = pos.pieces(Them, PAWN);

    e->passedPawns[Us] = 0;
    e->kingSquares[Us] = SQ_NONE;
    e->semiopenFiles[Us] = 0xFF;
    e->pawnAttacks[Us] = shift_bb<Right>(ourPawns) | shift_bb<Left>(ourPawns);
    e->pawnsOnSquares[Us][BLACK] = popcount<Max15>(ourPawns & DarkSquares);
    e->pawnsOnSquares[Us][WHITE] = pos.count<PAWN>(Us) - e->pawnsOnSquares[Us][BLACK];

    // Loop through all pawns of the current color and score each pawn
    while ((s = *pl++) != SQ_NONE)
    {
        assert(pos.piece_on(s) == make_piece(Us, PAWN));

        File f = file_of(s);

        // This file cannot be semi-open
        e->semiopenFiles[Us] &= ~(1 << f);

        // Previous rank
        p = rank_bb(s - pawn_push(Us));

        // Flag the pawn as passed, isolated, doubled,
        // unsupported or connected (but not the backward one).
        connected   =   ourPawns   & adjacent_files_bb(f) & (rank_bb(s) | p);
        phalanx     =   connected  & rank_bb(s);
        unsupported = !(ourPawns   & adjacent_files_bb(f) & p);
        isolated    = !(ourPawns   & adjacent_files_bb(f));
        doubled     =   ourPawns   & forward_bb(Us, s);
        opposed     =   theirPawns & forward_bb(Us, s);
        passed      = !(theirPawns & passed_pawn_mask(Us, s));
        lever       =   theirPawns & pawnAttacksBB[s];

        // Test for backward pawn.
        // If the pawn is passed, isolated, or connected it cannot be
        // backward. If there are friendly pawns behind on adjacent files
        // or if it can capture an enemy pawn it cannot be backward either.
        if (   (passed | isolated | connected)
            || (ourPawns & pawn_attack_span(Them, s))
            || (pos.attacks_from<PAWN>(s, Us) & theirPawns))
            backward = false;
        else
        {
            // We now know that there are no friendly pawns beside or behind this
            // pawn on adjacent files. We now check whether the pawn is
            // backward by looking in the forward direction on the adjacent
            // files, and picking the closest pawn there.
            b = pawn_attack_span(Us, s) & (ourPawns | theirPawns);
            b = pawn_attack_span(Us, s) & rank_bb(backmost_sq(Us, b));

            // If we have an enemy pawn in the same or next rank, the pawn is
            // backward because it cannot advance without being captured.
            backward = (b | shift_bb<Up>(b)) & theirPawns;
        }

        assert(opposed | passed | (pawn_attack_span(Us, s) & theirPawns));

        // Passed pawns will be properly scored in evaluation because we need
        // full attack info to evaluate passed pawns. Only the frontmost passed
        // pawn on each file is considered a true passed pawn.
        if (passed && !doubled)
            e->passedPawns[Us] |= s;

        // Score this pawn
        if (isolated)
            score -= Isolated[opposed][f];

        if (unsupported && !isolated)
            score -= UnsupportedPawnPenalty;

        if (doubled)
            score -= Doubled[f] / distance<Rank>(s, frontmost_sq(Us, doubled));

        if (backward)
            score -= Backward[opposed][f];

        if (connected)
            score += Connected[opposed][phalanx][relative_rank(Us, s)];

        if (lever)
            score += Lever[relative_rank(Us, s)];
    }

    b = e->semiopenFiles[Us] ^ 0xFF;
    e->pawnSpan[Us] = b ? int(msb(b) - lsb(b)) : 0;

    return score;
  }

} // namespace

namespace Pawns {

/// Pawns::init() initializes some tables needed by evaluation. Instead of using
/// hard-coded tables, when makes sense, we prefer to calculate them with a formula
/// to reduce independent parameters and to allow easier tuning and better insight.

int Seed[RANK_NB] = { 0, 6, 15, 10, 57, 75, 135, 258 };
int ConnectedPhalanx = 64;
int ConnectedMG = 64;

void init()
{
  //static const int Seed[RANK_NB] = { 0, 6, 15, 10, 57, 75, 135, 258 };

  for (int opposed = 0; opposed <= 1; ++opposed)
      for (int phalanx = 0; phalanx <= 1; ++phalanx)
          for (Rank r = RANK_2; r < RANK_8; ++r)
          {
              int bonus = Seed[r] + (phalanx ? (Seed[r + 1] - Seed[r]) * ConnectedPhalanx / 128 : 0);
              Connected[opposed][phalanx][r] = make_score(bonus * ConnectedMG / 128, bonus >> opposed);
          }
}

void init_spsa()
{
	Seed[1] = Options["ConnectedSeed1"];
	Seed[2] = Options["ConnectedSeed2"];
	Seed[3] = Options["ConnectedSeed3"];
	Seed[4] = Options["ConnectedSeed4"];
	Seed[5] = Options["ConnectedSeed5"];
	Seed[6] = Options["ConnectedSeed6"];
	Seed[7] = Options["ConnectedSeed7"];
	ConnectedPhalanx = Options["ConnectedPhalanx"];
	ConnectedMG = Options["ConnectedMG"];
	
	Doubled[0] = Doubled[7] = make_score(Options["Doubled0MG"], Options["Doubled0EG"]);
	Doubled[1] = Doubled[6] = make_score(Options["Doubled1MG"], Options["Doubled1EG"]);
	Doubled[2] = Doubled[5] = make_score(Options["Doubled2MG"], Options["Doubled2EG"]);
	Doubled[3] = Doubled[4] = make_score(Options["Doubled3MG"], Options["Doubled3EG"]);
	
	Isolated[0][0] = Isolated[0][7] = make_score(Options["Isolated00MG"], Options["Isolated00EG"]);
	Isolated[0][1] = Isolated[0][6] = make_score(Options["Isolated01MG"], Options["Isolated01EG"]);
	Isolated[0][2] = Isolated[0][5] = make_score(Options["Isolated02MG"], Options["Isolated02EG"]);
	Isolated[0][3] = Isolated[0][4] = make_score(Options["Isolated03MG"], Options["Isolated03EG"]);
	Isolated[1][0] = Isolated[1][7] = make_score(Options["Isolated10MG"], Options["Isolated10EG"]);
	Isolated[1][1] = Isolated[1][6] = make_score(Options["Isolated11MG"], Options["Isolated11EG"]);
	Isolated[1][2] = Isolated[1][5] = make_score(Options["Isolated12MG"], Options["Isolated12EG"]);
	Isolated[1][3] = Isolated[1][4] = make_score(Options["Isolated13MG"], Options["Isolated13EG"]);
	
	Backward[0][0] = Backward[0][7] = make_score(Options["Backward00MG"], Options["Backward00EG"]);
	Backward[0][1] = Backward[0][6] = make_score(Options["Backward01MG"], Options["Backward01EG"]);
	Backward[0][2] = Backward[0][5] = make_score(Options["Backward02MG"], Options["Backward02EG"]);
	Backward[0][3] = Backward[0][4] = make_score(Options["Backward03MG"], Options["Backward03EG"]);
	Backward[1][0] = Backward[1][7] = make_score(Options["Backward10MG"], Options["Backward10EG"]);
	Backward[1][1] = Backward[1][6] = make_score(Options["Backward11MG"], Options["Backward11EG"]);
	Backward[1][2] = Backward[1][5] = make_score(Options["Backward12MG"], Options["Backward12EG"]);
	Backward[1][3] = Backward[1][4] = make_score(Options["Backward13MG"], Options["Backward13EG"]);
	
	Lever[1] = make_score(Options["Lever1MG"], Options["Lever1EG"]);
	Lever[2] = make_score(Options["Lever2MG"], Options["Lever2EG"]);
	Lever[3] = make_score(Options["Lever3MG"], Options["Lever3EG"]);
	Lever[4] = make_score(Options["Lever4MG"], Options["Lever4EG"]);
	Lever[5] = make_score(Options["Lever5MG"], Options["Lever5EG"]);
	
	UnsupportedPawnPenalty = make_score(Options["UnsupportedMG"], Options["UnsupportedEG"]);
	
	init();
}

/// Pawns::probe() looks up the current position's pawns configuration in
/// the pawns hash table. It returns a pointer to the Entry if the position
/// is found. Otherwise a new Entry is computed and stored there, so we don't
/// have to recompute all when the same pawns configuration occurs again.

Entry* probe(const Position& pos) {

  Key key = pos.pawn_key();
  Entry* e = pos.this_thread()->pawnsTable[key];

  if (e->key == key)
      return e;

  e->key = key;
  e->score = evaluate<WHITE>(pos, e) - evaluate<BLACK>(pos, e);
  return e;
}


/// Entry::shelter_storm() calculates shelter and storm penalties for the file
/// the king is on, as well as the two adjacent files.

template<Color Us>
Value Entry::shelter_storm(const Position& pos, Square ksq) {

  const Color Them = (Us == WHITE ? BLACK : WHITE);

  enum { NoFriendlyPawn, Unblocked, BlockedByPawn, BlockedByKing };

  Bitboard b = pos.pieces(PAWN) & (in_front_bb(Us, rank_of(ksq)) | rank_bb(ksq));
  Bitboard ourPawns = b & pos.pieces(Us);
  Bitboard theirPawns = b & pos.pieces(Them);
  Value safety = MaxSafetyBonus;
  File center = std::max(FILE_B, std::min(FILE_G, file_of(ksq)));

  for (File f = center - File(1); f <= center + File(1); ++f)
  {
      b = ourPawns & file_bb(f);
      Rank rkUs = b ? relative_rank(Us, backmost_sq(Us, b)) : RANK_1;

      b  = theirPawns & file_bb(f);
      Rank rkThem = b ? relative_rank(Us, frontmost_sq(Them, b)) : RANK_1;

      safety -=  ShelterWeakness[std::min(f, FILE_H - f)][rkUs]
               + StormDanger
                 [f == file_of(ksq) && rkThem == relative_rank(Us, ksq) + 1 ? BlockedByKing  :
                  rkUs   == RANK_1                                          ? NoFriendlyPawn :
                  rkThem == rkUs + 1                                        ? BlockedByPawn  : Unblocked]
                 [std::min(f, FILE_H - f)][rkThem];
  }

  return safety;
}


/// Entry::do_king_safety() calculates a bonus for king safety. It is called only
/// when king square changes, which is about 20% of total king_safety() calls.

template<Color Us>
Score Entry::do_king_safety(const Position& pos, Square ksq) {

  kingSquares[Us] = ksq;
  castlingRights[Us] = pos.can_castle(Us);
  minKingPawnDistance[Us] = 0;

  Bitboard pawns = pos.pieces(Us, PAWN);
  if (pawns)
      while (!(DistanceRingBB[ksq][minKingPawnDistance[Us]++] & pawns)) {}

  if (relative_rank(Us, ksq) > RANK_4)
      return make_score(0, -16 * minKingPawnDistance[Us]);

  Value bonus = shelter_storm<Us>(pos, ksq);

  // If we can castle use the bonus after the castling if it is bigger
  if (pos.can_castle(MakeCastling<Us, KING_SIDE>::right))
      bonus = std::max(bonus, shelter_storm<Us>(pos, relative_square(Us, SQ_G1)));

  if (pos.can_castle(MakeCastling<Us, QUEEN_SIDE>::right))
      bonus = std::max(bonus, shelter_storm<Us>(pos, relative_square(Us, SQ_C1)));

  return make_score(bonus, -16 * minKingPawnDistance[Us]);
}

// Explicit template instantiation
template Score Entry::do_king_safety<WHITE>(const Position& pos, Square ksq);
template Score Entry::do_king_safety<BLACK>(const Position& pos, Square ksq);

} // namespace Pawns
