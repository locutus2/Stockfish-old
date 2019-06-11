/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2019 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#include "types.h"

Value PieceValue[PHASE_NB][PIECE_NB] = {
  { VALUE_ZERO, PawnValueMg, KnightValueMg, BishopValueMg, RookValueMg, QueenValueMg },
  { VALUE_ZERO, PawnValueEg, KnightValueEg, BishopValueEg, RookValueEg, QueenValueEg }
};

namespace PSQT {

#define S(mg, eg) make_score(mg, eg)
#define SX(x) (x/2)
#define SY(x) (x/2)

// Bonus[PieceType][Square / 2] contains Piece-Square scores. For each piece
// type on a given square a (middlegame, endgame) score pair is assigned. Table
// is defined for files A..D and white side: it is symmetric for black side and
// second half of the files.
constexpr Score Bonus[][RANK_NB][int(FILE_NB) / 2] = {
  { },
  { },
  { // Knight
   { S(-169,-105), S(-96,-74), S(-80,-46), S(-79,-18) },
   { S( -79+SX(4), -70+SY(1)), S(-39+SX(6),-56+SY(2)), S(-24+SX(7),-15+SY(2)), S( -9+SX(3),  6+SY(1)) },
   { S( -64+SX(5), -38+SY(2)), S(-20+SX(7),-33+SY(2)), S(  4+SX(5), -5+SY(2)), S( 19+SX(9), 27+SY(3)) },
   { S( -28+SX(15), -36+SY(5)), S(  5+SX(21),  0+SY(7)), S( 41+SX(19), 13+SY(6)), S( 47+SX(17), 34+SY(6)) },
   { S( -29+SX(23), -41+SY(8)), S( 13+SX(15),-20+SY(5)), S( 42+SX(21),  4+SY(7)), S( 52+SX(20), 35+SY(7)) },
   { S( -11+SX(34), -51+SY(11)), S( 28+SX(28),-38+SY(9)), S( 63+SX(30),-17+SY(10)), S( 55+SX(29), 19+SY(10)) },
   { S( -67+SX(14), -64+SY(5)), S(-21+SX(14),-45+SY(5)), S(  6+SX(14),-37+SY(5)), S( 37+SX(14), 16+SY(5)) },
   { S(-200+SX(16), -98+SY(5)), S(-80+SX(17),-89+SY(6)), S(-53+SX(16),-53+SY(5)), S(-32+SX(16),-16+SY(5)) }
  },
  { // Bishop
   { S(-44+SX(4),-63+SY(1)), S( -4+SX(5),-30+SY(2)), S(-11+SX(2),-35+SY(1)), S(-28+SX(4), -8+SY(1)) },
   { S(-18+SX(5),-38+SY(2)), S(  7+SX(2),-13+SY(1)), S( 14+SX(5),-14+SY(2)), S(  3+SX(3),  0+SY(1)) },
   { S( -8+SX(4),-18+SY(1)), S( 24+SX(6),  0+SY(2)), S( -3+SX(5), -7+SY(2)), S( 15+SX(5), 13+SY(2)) },
   { S(  1+SX(11),-26+SY(4)), S(  8+SX(10), -3+SY(3)), S( 26+SX(9),  1+SY(3)), S( 37+SX(11), 16+SY(4)) },
   { S( -7+SX(12),-24+SY(4)), S( 30+SX(9), -6+SY(3)), S( 23+SX(13),-10+SY(4)), S( 28+SX(13), 17+SY(4)) },
   { S(-17+SX(16),-26+SY(5)), S(  4+SX(15),  2+SY(5)), S( -1+SX(14),  1+SY(5)), S(  8+SX(15), 16+SY(5)) },
   { S(-21+SX(7),-34+SY(2)), S(-19+SX(8),-18+SY(3)), S( 10+SX(7), -7+SY(2)), S( -6+SX(7),  9+SY(2)) },
   { S(-48+SX(8),-51+SY(3)), S( -3+SX(7),-40+SY(2)), S(-12+SX(8),-39+SY(3)), S(-25+SX(7),-20+SY(2)) }
  },
  { // Rook
   { S(-24, -2), S(-13,-6), S(-7, -3), S( 2,-2) },
   { S(-18,-10), S(-10,-7), S(-5,  1), S( 9, 0) },
   { S(-21, 10), S( -7,-4), S( 3,  2), S(-1,-2) },
   { S(-13, -5), S( -5, 2), S(-4, -8), S(-6, 8) },
   { S(-24, -8), S(-12, 5), S(-1,  4), S( 6,-9) },
   { S(-24,  3), S( -4,-2), S( 4,-10), S(10, 7) },
   { S( -8,  1), S(  6, 2), S(10, 17), S(12,-8) },
   { S(-22, 12), S(-24,-6), S(-6, 13), S( 4, 7) }
  },
  { // Queen
   { S( 3,-69), S(-5,-57), S(-5,-47), S( 4,-26) },
   { S(-3,-55), S( 5,-31), S( 8,-22), S(12, -4) },
   { S(-3,-39), S( 6,-18), S(13, -9), S( 7,  3) },
   { S( 4,-23), S( 5, -3), S( 9, 13), S( 8, 24) },
   { S( 0,-29), S(14, -6), S(12,  9), S( 5, 21) },
   { S(-4,-38), S(10,-18), S( 6,-12), S( 8,  1) },
   { S(-5,-50), S( 6,-27), S(10,-24), S( 8, -8) },
   { S(-2,-75), S(-2,-52), S( 1,-43), S(-2,-36) }
  },
  { // King
   { S(272,  0), S(325, 41), S(273, 80), S(190, 93) },
   { S(277, 57), S(305, 98), S(241,138), S(183,131) },
   { S(198, 86), S(253,138), S(168,165), S(120,173) },
   { S(169,103), S(191,152), S(136,168), S(108,169) },
   { S(145, 98), S(176,166), S(112,197), S( 69,194) },
   { S(122, 87), S(159,164), S( 85,174), S( 36,189) },
   { S( 87, 40), S(120, 99), S( 64,128), S( 25,141) },
   { S( 64,  5), S( 87, 60), S( 49, 75), S(  0, 75) }
  }
};

constexpr Score PBonus[RANK_NB][FILE_NB] =
  { // Pawn (asymmetric distribution)
   { },
   { S(  0,-10), S( -5,-3), S( 10, 7), S( 13,-1), S( 21,  7), S( 17,  6), S(  6, 1), S( -3,-20) },
   { S(-11, -6), S(-10,-6), S( 15,-1), S( 22,-1), S( 26, -1), S( 28,  2), S(  4,-2), S(-24, -5) },
   { S( -9,  4), S(-18,-5), S(  8,-4), S( 22,-5), S( 33, -6), S( 25,-13), S( -4,-3), S(-16, -7) },
   { S(  6, 18), S( -3, 2), S(-10, 2), S(  1,-9), S( 12,-13), S(  6, -8), S(-12,11), S(  1,  9) },
   { S( -6, 25), S( -8,17), S(  5,19), S( 11,29), S(-14, 29), S(  0,  8), S(-12, 4), S(-14, 12) },
   { S(-10, -1), S(  6,-6), S( -5,18), S(-11,22), S( -2, 22), S(-14, 17), S( 12, 2), S( -1,  9) }
  };

#undef S

Score psq[PIECE_NB][SQUARE_NB];

// init() initializes piece-square tables: the white halves of the tables are
// copied from Bonus[] adding the piece value, then the black halves of the
// tables are initialized by flipping and changing the sign of the white scores.
void init() {

  for (Piece pc = W_PAWN; pc <= W_KING; ++pc)
  {
      PieceValue[MG][~pc] = PieceValue[MG][pc];
      PieceValue[EG][~pc] = PieceValue[EG][pc];

      Score score = make_score(PieceValue[MG][pc], PieceValue[EG][pc]);

      for (Square s = SQ_A1; s <= SQ_H8; ++s)
      {
          File f = std::min(file_of(s), ~file_of(s));
          psq[ pc][ s] = score + (type_of(pc) == PAWN ? PBonus[rank_of(s)][file_of(s)]
                                                      : Bonus[pc][rank_of(s)][f]);
          psq[~pc][~s] = -psq[pc][s];
      }
  }
}

} // namespace PSQT
