/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2018 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#include <algorithm> // For std::min
#include <cassert>
#include <cstring>   // For std::memset

#include "material.h"
#include "thread.h"

using namespace std;

namespace {

  // Polynomial material imbalance parameters

  constexpr int Linear[] = { 1617, 12, -8, 39, 28, -53 };

  constexpr int QuadraticOurs[][PIECE_TYPE_NB] = {
    //            OUR PIECES
    // pair pawn knight bishop rook queen
    {   0                                }, // Bishop pair
    {  25,  285                          }, // Pawn
    {  -7,  222, -58                     }, // Knight      OUR PIECES
    {   0,   86,  39,   -53              }, // Bishop
    { -32,   80, 107,    73,  -224       }, // Rook
    {-188,  107, 325,   247,   -35, -192 }  // Queen
  };

  constexpr int QuadraticTheirs[][PIECE_TYPE_NB] = {
    //           THEIR PIECES
    // pair pawn knight bishop rook queen
    {   0                                }, // Bishop pair
    {  54,    0                          }, // Pawn
    { -26,  -38,   0                     }, // Knight      OUR PIECES
    {  -4,   33,  -8,     0              }, // Bishop
    {  46,   14,  11,   -15,    0        }, // Rook
    {  75,   61, -36,   148,  326,    0  }  // Queen
  };

  constexpr int CubicOurs[][PIECE_TYPE_NB][PIECE_TYPE_NB] = {
    // OUR PIECES:
    {}, // Bishop pair
    {   // Pawn
        //            OUR PIECES
        // pair pawn knight bishop rook queen
        {    0                               }, // Bishop pair
        {  -26, -212                         }  // Pawn        OUR PIECES
    },
    {   // Knight
        //            OUR PIECES
        // pair pawn knight bishop rook queen
        {    0                               }, // Bishop pair
        {  -33,    3                         }, // Pawn
        {   88,   44,  -12                   }  // Knight      OUR PIECES
    },
    {   // Bishop
        //            OUR PIECES
        // pair pawn knight bishop rook queen
        {    0                               }, // Bishop pair
        {   -6,   42                         }, // Pawn
        {  -11,   20,  117                   }, // Knight      OUR PIECES
        {    0,  -43,   82,  -18             }  // Bishop
    },
    {   // Rook
        //            OUR PIECES
        // pair pawn knight bishop rook queen
        {    0                               }, // Bishop pair
        {   17,   29                         }, // Pawn
        {  -73,  -49,  -14                   }, // Knight      OUR PIECES
        {    0,   23,   48,  -14             }, // Bishop
        {   82,   24,  -22,   87, -100       }  // Rook
    },
    {   // Queen
        //            OUR PIECES
        // pair pawn knight bishop rook queen
        {    0                               }, // Bishop pair
        {  -27,  -19                         }, // Pawn
        {  -42,   31,   98                   }, // Knight      OUR PIECES
        {    0,   37,  126,  -36             }, // Bishop
        {    4,   -6,   25,  -76, -172       }, // Rook
        {  -35,   21,   28,  -10,   21,   23 }  // Queen
    }
  };

  constexpr int CubicTheirs[][PIECE_TYPE_NB][PIECE_TYPE_NB] = {
    // OUR PIECES:
    {}, // Bishop pair
    {   // Pawn
        //            THEIR PIECES
        // pair pawn knight bishop rook queen
        {   70,   22,   23,  -20,  -15,   51 }, // Bishop pair
        {  -38,  492,   64,   68,   82,  -37 }  // Pawn        OUR PIECES
    },
    {   // Knight
        //            THEIR PIECES
        // pair pawn knight bishop rook queen
        {  -17,  -39,   33,  -32,   -4,   71 }, // Bishop pair
        {  -32,   37,  -80,  -24,   57, -106 }, // Pawn
        {   23,  -53,   11,   16,  -55,  -75 }  // Knight      OUR PIECES
    },
    {   // Bishop
        //            THEIR PIECES
        // pair pawn knight bishop rook queen
        {   66,   20,  -64,   82,   68,  -57 }, // Bishop pair
        {   10,  -32,  -33,  -34,   19,  -48 }, // Pawn
        {  -83,   21,    0,  -26,   55,  -63 }, // Knight      OUR PIECES
        { -108,   46,  -69,   -3,  -12,  109 }  // Bishop
    },
    {   // Rook
        //            THEIR PIECES
        // pair pawn knight bishop rook queen
        {  -41,  -77,   54,  -51,   -9,   68 }, // Bishop pair
        {   13,   28,   21, -144,    5,  -33 }, // Pawn
        {  -65,  177,   75,  156,   44,  104 }, // Knight      OUR PIECES
        {   12,    2,   26,  -38,   69, -160 }, // Bishop
        {  -67,   53,   80,   70, -116,  -14 }  // Rook
    },
    {   // Queen
        //            THEIR PIECES
        // pair pawn knight bishop rook queen
        {  -21,  -98,   20,   34,  -77,   45 }, // Bishop pair
        {   28,  -96,   36,   88,   21,   35 }, // Pawn
        {   98, -137,  -44,   19,   47,  -61 }, // Knight      OUR PIECES
        {  -86,   29,    2,   52,  -36,   69 }, // Bishop
        {  -55,   15,   14,   -1,  -12,    3 }, // Rook
        {  -87, -118, -105,   -3,   31,   54 }  // Queen
    }
  };

  // Endgame evaluation and scaling functions are accessed directly and not through
  // the function maps because they correspond to more than one material hash key.
  Endgame<KXK>    EvaluateKXK[] = { Endgame<KXK>(WHITE),    Endgame<KXK>(BLACK) };

  Endgame<KBPsK>  ScaleKBPsK[]  = { Endgame<KBPsK>(WHITE),  Endgame<KBPsK>(BLACK) };
  Endgame<KQKRPs> ScaleKQKRPs[] = { Endgame<KQKRPs>(WHITE), Endgame<KQKRPs>(BLACK) };
  Endgame<KPsK>   ScaleKPsK[]   = { Endgame<KPsK>(WHITE),   Endgame<KPsK>(BLACK) };
  Endgame<KPKP>   ScaleKPKP[]   = { Endgame<KPKP>(WHITE),   Endgame<KPKP>(BLACK) };

  // Helper used to detect a given material distribution
  bool is_KXK(const Position& pos, Color us) {
    return  !more_than_one(pos.pieces(~us))
          && pos.non_pawn_material(us) >= RookValueMg;
  }

  bool is_KBPsK(const Position& pos, Color us) {
    return   pos.non_pawn_material(us) == BishopValueMg
          && pos.count<BISHOP>(us) == 1
          && pos.count<PAWN  >(us) >= 1;
  }

  bool is_KQKRPs(const Position& pos, Color us) {
    return  !pos.count<PAWN>(us)
          && pos.non_pawn_material(us) == QueenValueMg
          && pos.count<QUEEN>(us)  == 1
          && pos.count<ROOK>(~us) == 1
          && pos.count<PAWN>(~us) >= 1;
  }

  /// imbalance() calculates the imbalance by comparing the piece count of each
  /// piece type for both colors.
  template<Color Us>
  int imbalance(const int pieceCount[][PIECE_TYPE_NB]) {

    constexpr Color Them = (Us == WHITE ? BLACK : WHITE);

    int bonus = 0;

    // Third-degree polynomial material imbalance, by Tord Romstad and Stefan Geschwentner
    for (int pt1 = NO_PIECE_TYPE; pt1 <= QUEEN; ++pt1)
    {
        if (!pieceCount[Us][pt1])
            continue;

        int v = Linear[pt1];

        for (int pt2 = NO_PIECE_TYPE; pt2 <= pt1; ++pt2)
        {
            int w = 0;
            for (int pt3 = NO_PIECE_TYPE; pt3 <= pt2; ++pt3)
                w += CubicOurs[pt1][pt2][pt3] * pieceCount[Us][pt3];

            for (int pt3 = NO_PIECE_TYPE; pt3 <= QUEEN; ++pt3)
                w += CubicTheirs[pt1][pt2][pt3] * pieceCount[Them][pt3];

            v +=  QuadraticOurs[pt1][pt2] * pieceCount[Us][pt2]
                + QuadraticTheirs[pt1][pt2] * pieceCount[Them][pt2]
                + w * pieceCount[Us][pt2];
        }

        bonus += pieceCount[Us][pt1] * v;
    }

    return bonus;
  }

} // namespace

namespace Material {

/// Material::probe() looks up the current position's material configuration in
/// the material hash table. It returns a pointer to the Entry if the position
/// is found. Otherwise a new Entry is computed and stored there, so we don't
/// have to recompute all when the same material configuration occurs again.

Entry* probe(const Position& pos) {

  Key key = pos.material_key();
  Entry* e = pos.this_thread()->materialTable[key];

  if (e->key == key)
      return e;

  std::memset(e, 0, sizeof(Entry));
  e->key = key;
  e->factor[WHITE] = e->factor[BLACK] = (uint8_t)SCALE_FACTOR_NORMAL;

  Value npm_w = pos.non_pawn_material(WHITE);
  Value npm_b = pos.non_pawn_material(BLACK);
  Value npm = std::max(EndgameLimit, std::min(npm_w + npm_b, MidgameLimit));

  // Map total non-pawn material into [PHASE_ENDGAME, PHASE_MIDGAME]
  e->gamePhase = Phase(((npm - EndgameLimit) * PHASE_MIDGAME) / (MidgameLimit - EndgameLimit));

  // Let's look if we have a specialized evaluation function for this particular
  // material configuration. Firstly we look for a fixed configuration one, then
  // for a generic one if the previous search failed.
  if ((e->evaluationFunction = pos.this_thread()->endgames.probe<Value>(key)) != nullptr)
      return e;

  for (Color c = WHITE; c <= BLACK; ++c)
      if (is_KXK(pos, c))
      {
          e->evaluationFunction = &EvaluateKXK[c];
          return e;
      }

  // OK, we didn't find any special evaluation function for the current material
  // configuration. Is there a suitable specialized scaling function?
  EndgameBase<ScaleFactor>* sf;

  if ((sf = pos.this_thread()->endgames.probe<ScaleFactor>(key)) != nullptr)
  {
      e->scalingFunction[sf->strongSide] = sf; // Only strong color assigned
      return e;
  }

  // We didn't find any specialized scaling function, so fall back on generic
  // ones that refer to more than one material distribution. Note that in this
  // case we don't return after setting the function.
  for (Color c = WHITE; c <= BLACK; ++c)
  {
    if (is_KBPsK(pos, c))
        e->scalingFunction[c] = &ScaleKBPsK[c];

    else if (is_KQKRPs(pos, c))
        e->scalingFunction[c] = &ScaleKQKRPs[c];
  }

  if (npm_w + npm_b == VALUE_ZERO && pos.pieces(PAWN)) // Only pawns on the board
  {
      if (!pos.count<PAWN>(BLACK))
      {
          assert(pos.count<PAWN>(WHITE) >= 2);

          e->scalingFunction[WHITE] = &ScaleKPsK[WHITE];
      }
      else if (!pos.count<PAWN>(WHITE))
      {
          assert(pos.count<PAWN>(BLACK) >= 2);

          e->scalingFunction[BLACK] = &ScaleKPsK[BLACK];
      }
      else if (pos.count<PAWN>(WHITE) == 1 && pos.count<PAWN>(BLACK) == 1)
      {
          // This is a special case because we set scaling functions
          // for both colors instead of only one.
          e->scalingFunction[WHITE] = &ScaleKPKP[WHITE];
          e->scalingFunction[BLACK] = &ScaleKPKP[BLACK];
      }
  }

  // Zero or just one pawn makes it difficult to win, even with a small material
  // advantage. This catches some trivial draws like KK, KBK and KNK and gives a
  // drawish scale factor for cases such as KRKBP and KmmKm (except for KBBKN).
  if (!pos.count<PAWN>(WHITE) && npm_w - npm_b <= BishopValueMg)
      e->factor[WHITE] = uint8_t(npm_w <  RookValueMg   ? SCALE_FACTOR_DRAW :
                                 npm_b <= BishopValueMg ? 4 : 14);

  if (!pos.count<PAWN>(BLACK) && npm_b - npm_w <= BishopValueMg)
      e->factor[BLACK] = uint8_t(npm_b <  RookValueMg   ? SCALE_FACTOR_DRAW :
                                 npm_w <= BishopValueMg ? 4 : 14);

  if (pos.count<PAWN>(WHITE) == 1 && npm_w - npm_b <= BishopValueMg)
      e->factor[WHITE] = (uint8_t) SCALE_FACTOR_ONEPAWN;

  if (pos.count<PAWN>(BLACK) == 1 && npm_b - npm_w <= BishopValueMg)
      e->factor[BLACK] = (uint8_t) SCALE_FACTOR_ONEPAWN;

  // Evaluate the material imbalance. We use PIECE_TYPE_NONE as a place holder
  // for the bishop pair "extended piece", which allows us to be more flexible
  // in defining bishop pair bonuses.
  const int pieceCount[COLOR_NB][PIECE_TYPE_NB] = {
  { pos.count<BISHOP>(WHITE) > 1, pos.count<PAWN>(WHITE), pos.count<KNIGHT>(WHITE),
    pos.count<BISHOP>(WHITE)    , pos.count<ROOK>(WHITE), pos.count<QUEEN >(WHITE) },
  { pos.count<BISHOP>(BLACK) > 1, pos.count<PAWN>(BLACK), pos.count<KNIGHT>(BLACK),
    pos.count<BISHOP>(BLACK)    , pos.count<ROOK>(BLACK), pos.count<QUEEN >(BLACK) } };

  e->value = int16_t((imbalance<WHITE>(pieceCount) - imbalance<BLACK>(pieceCount)) / 16);
  return e;
}

} // namespace Material
