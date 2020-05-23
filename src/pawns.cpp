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

  int IDoubledIsolated[2];
  int IDoubledIsolatedOpposed[2];
  
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
  int ISupported;

  int IBlockedStormMG;
  int IBlockedStormEG;

  int IBlockedStormPolyMG[2];
  int IBlockedStormPolyEG[2];
  
  int IBlockedStormFMG[4];
  int IBlockedStormFEG[4];

  int IDoubledMGS;
  int IDoubledEGS;
  
  int IDoubledMGU;
  int IDoubledEGU;
  
  int IDoubledMGP;
  int IDoubledEGP;
  
  int IDoubledDist[6][2];
  
  int IPawnChainMG[8];
  int IPawnChainEG[8];
  
  int IDoubledFMG[4];
  int IDoubledFEG[4];

  int IDoubledRMG[RANK_NB];
  int IDoubledREG[RANK_NB];
 
  int IDoubledLinMG[3]; 
  int IDoubledLinEG[3]; 

  int IIsolatedRMG[RANK_NB];
  int IIsolatedREG[RANK_NB];
  
  int IIsolatedLinMG[2];
  int IIsolatedLinEG[2];

  int IIsolatedGDMG[RANK_NB];
  int IIsolatedGDEG[RANK_NB];

  int IConnectedPoly[4];
  int IConnectedFactor[4];
  int IConnectedC[4];
  
  int IConnectedBR[RANK_NB];
  int IConnectedBRPoly[2];

  int IConnectedPHPoly[2];

  #define V Value
  #define S(mg, eg) make_score(mg, eg)
  
  constexpr Score DoubledS       = S(0, 0);
  constexpr Score DoubledU       = S(11, 56);
  constexpr Score DoubledP       = S(11, 56);
  constexpr Score PawnChain      = S( 0,  0);

  // Pawn penalties
  constexpr Score Backward      = S( 9, 24);
  //constexpr Score Backward      = S( 0, 0);
  constexpr Score BlockedStorm  = S(82, 82);
  constexpr Score Doubled       = S(11, 56);
  //constexpr Score Doubled       = S(0, 0);
  constexpr Score Isolated      = S( 5, 15);
  //constexpr Score Isolated      = S( 0, 0);
  constexpr Score WeakLever     = S( 0, 56);
  //constexpr Score WeakLever     = S( 0, 0);
  constexpr Score WeakUnopposed = S(13, 27);
  //constexpr Score WeakUnopposed = S(0, 0);

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
	constexpr Direction LeftUp   = Up + (Us == WHITE ? WEST : EAST);
    constexpr Direction RightUp  = Up + (Us == WHITE ? EAST : WEST);

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
	
	Bitboard pawnChains = (  shift<LeftUp >(shift<LeftUp >(ourPawns) & ourPawns)
                           | shift<RightUp>(shift<RightUp>(ourPawns) & ourPawns))
                         & ourPawns;
    
	

    e->passedPawns[Us] = 0;
    e->kingSquares[Us] = SQ_NONE;
    e->pawnAttacks[Us] = e->pawnAttacksSpan[Us] = pawn_attacks_bb<Us>(ourPawns);

    // Loop through all pawns of the current color and score each pawn
    while ((s = *pl++) != SQ_NONE)
    {
        assert(pos.piece_on(s) == make_piece(Us, PAWN));

        Rank r = relative_rank(Us, s);
		
		if (pawnChains & s)
		{
			File f = File(edge_distance(file_of(s)));
			Score PPawnChain = make_score(Tuning::getParam(IPawnChainMG[f]), Tuning::getParam(IPawnChainEG[f]));
			score += PPawnChain;
			Tuning::updateGradient(Us, IPawnChainMG[f], 1.0 * phase / PHASE_MIDGAME);
		    Tuning::updateGradient(Us, IPawnChainEG[f], 1.0 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		
		}

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
            double v =  Tuning::getParam(IConnected[r]) * (4 + 2 * bool(phalanx) - 2 * bool(opposed) - bool(blocked)) / 2;
	    double P = Tuning::getParam(IConnectedPoly[0])
	    		+ r * Tuning::getParam(IConnectedPoly[1])
	    		+ r * r * Tuning::getParam(IConnectedPoly[2])
	    		+ r * r * r * Tuning::getParam(IConnectedPoly[3]);
			//double v =  P * (4 + 2 * bool(phalanx) - 2 * bool(opposed) - bool(blocked)) / 64
			//double v =  Tuning::getParam(IConnected[r]) * (4 + 2 * bool(phalanx) - 2 * bool(opposed) + bool(blocked) * Tuning::getParam(IConnectedBR[r]) / 64) / 2
			//double v =  Tuning::getParam(IConnected[r]) * (4 
				//	+ bool(phalanx) * (IConnectedPHPoly[0] + IConnectedPHPoly[1] * r) 
					//- 2 * bool(opposed) + bool(blocked) * (Tuning::getParam(IConnectedBRPoly[0]) + Tuning::getParam(IConnectedBRPoly[1]) * r)) / 2
            //double v =  Connected[r] * (Tuning::getParam(IConnectedFactor[0]) 
			//               + Tuning::getParam(IConnectedFactor[1]) * bool(phalanx) 
			//	       + Tuning::getParam(IConnectedFactor[2]) * bool(opposed) 
			//	       + Tuning::getParam(IConnectedFactor[3]) *  bool(blocked)) / (2 * 64)
                  v += Tuning::getParam(ISupported) * popcount(support);

            score += make_score(v * (IConnectedC[0] + IConnectedC[1] * r) / 64, (IConnectedC[2] + IConnectedC[3] * r) / 64);
			//score += make_score(v, v * (r - 2) / 4);
	    double grad_mg = (4 + 2 * bool(phalanx) - 2 * bool(opposed) - bool(blocked)) / 2.0;
	    double grad_eg = grad_mg * (r - 2) / 4;
	    double grad = (grad_mg * phase + grad_eg * (PHASE_MIDGAME - phase)) / PHASE_MIDGAME;
   	    Tuning::updateGradient(Us, IConnected[r], grad);
		double grad_s = popcount(support) * (phase + (r-2)/4.0 * (PHASE_MIDGAME - phase)) / PHASE_MIDGAME;
		Tuning::updateGradient(Us, ISupported, grad_s);
   	    //Tuning::updateGradient(Us, IConnectedPoly[0], grad / 32);
   	    //Tuning::updateGradient(Us, IConnectedPoly[1], r * grad / 32);
   	    //Tuning::updateGradient(Us, IConnectedPoly[2], r * r * grad / 32);
   	    //Tuning::updateGradient(Us, IConnectedPoly[3], r * r * r * grad / 32);
   	    Tuning::updateGradient(Us, IConnectedPoly[0], grad / 32);
   	    Tuning::updateGradient(Us, IConnectedPoly[1], r * grad / 32 * 2);
   	    Tuning::updateGradient(Us, IConnectedPoly[2], r * r * grad / 32 * 4);
   	    Tuning::updateGradient(Us, IConnectedPoly[3], r * r * r * grad / 32 * 8);
		
		Tuning::updateGradient(Us, IConnectedC[0], v / 64.0 * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IConnectedC[1], v * r / 64.0 * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IConnectedC[2], v / 64.0 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IConnectedC[3], v * r / 64.0 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);

	    double rgrad_mg = Connected[r] / (2.0 * 64);
	    double rgrad_eg = grad_mg * (r - 2) / 4;
	    double rgrad = (grad_mg * phase + grad_eg * (PHASE_MIDGAME - phase)) / PHASE_MIDGAME;

   	    Tuning::updateGradient(Us, IConnectedFactor[0], rgrad);
   	    Tuning::updateGradient(Us, IConnectedFactor[1], rgrad * bool(phalanx));
   	    Tuning::updateGradient(Us, IConnectedFactor[2], rgrad * bool(opposed));
   	    Tuning::updateGradient(Us, IConnectedFactor[3], rgrad * bool(blocked));
		
		if (blocked)
		{
			grad_mg = Tuning::getParam(IConnected[r]) / 2.0;
			grad_eg = grad_mg * (r - 2) / 4;
			grad = (grad_mg * phase + grad_eg * (PHASE_MIDGAME - phase)) / PHASE_MIDGAME;
			Tuning::updateGradient(Us, IConnectedBR[r], grad);

			grad_mg = Tuning::getParam(IConnected[r]) / 2.0;
			grad_eg = grad_mg * (r - 2) / 4;
			grad = (grad_mg * phase + grad_eg * (PHASE_MIDGAME - phase)) / PHASE_MIDGAME;
			Tuning::updateGradient(Us, IConnectedBRPoly[0], grad);
			Tuning::updateGradient(Us, IConnectedBRPoly[1], grad * r);
		}

		if (phalanx)
		{
			grad_mg = Tuning::getParam(IConnected[r]) / 2.0;
			grad_eg = grad_mg * (r - 2) / 4;
			grad = (grad_mg * phase + grad_eg * (PHASE_MIDGAME - phase)) / PHASE_MIDGAME;
			Tuning::updateGradient(Us, IConnectedPHPoly[0], grad);
			Tuning::updateGradient(Us, IConnectedPHPoly[1], grad * r);
		}
        }

        else if (!neighbours)
	{
	    Score PIsolatedR = make_score(Tuning::getParam(IIsolatedRMG[r]), Tuning::getParam(IIsolatedREG[r]));
	    Score PIsolatedLin[2] =  { make_score(Tuning::getParam(IIsolatedLinMG[0]), Tuning::getParam(IIsolatedLinEG[0]))
	                             , make_score(Tuning::getParam(IIsolatedLinMG[1]), Tuning::getParam(IIsolatedLinEG[1])) };
	    Score PIsolatedGD[3] =  { make_score(Tuning::getParam(IIsolatedGDMG[r-1]), Tuning::getParam(IIsolatedGDEG[r-1]))
	                             , make_score(Tuning::getParam(IIsolatedGDMG[r]), Tuning::getParam(IIsolatedGDEG[r]))
	                             , make_score(Tuning::getParam(IIsolatedGDMG[r+1]), Tuning::getParam(IIsolatedGDEG[r+1])) };
            score -=   PIsolated
            //score -=   PIsolatedR
            //score -=   (PIsolatedLin[0] + PIsolatedLin[1] * r)
            //score -=   (PIsolatedGD[0] + PIsolatedGD[1] + PIsolatedGD[2]) / 3
                     + PWeakUnopposed * !opposed;
   		Tuning::updateGradient(Us, IIsolatedMG, -1.0 * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IIsolatedEG, -1.0 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		Tuning::updateGradient(Us, IWeakUnopposedMG, -1.0 * !opposed * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IWeakUnopposedEG, -1.0 * !opposed * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		Tuning::updateGradient(Us, IIsolatedRMG[r], -1.0 * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IIsolatedREG[r], -1.0 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		Tuning::updateGradient(Us, IIsolatedLinMG[0], -1.0 * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IIsolatedLinEG[0], -1.0 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		Tuning::updateGradient(Us, IIsolatedLinMG[1], -1.0 * r * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IIsolatedLinEG[1], -1.0 * r * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);

   		Tuning::updateGradient(Us, IIsolatedGDMG[r-1], -1.0 / 3 * phase / PHASE_MIDGAME);
   		Tuning::updateGradient(Us, IIsolatedGDMG[r], -1.0 / 3 * phase / PHASE_MIDGAME);
   		Tuning::updateGradient(Us, IIsolatedGDMG[r+1], -1.0 / 3 * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IIsolatedGDEG[r-1], -1.0 / 3 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IIsolatedGDEG[r], -1.0 / 3 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IIsolatedGDEG[r+1], -1.0 / 3 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
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
		Score PDoubledP = make_score(Tuning::getParam(IDoubledMGP), Tuning::getParam(IDoubledEGP));
		File f = File(edge_distance(file_of(s)));
		Score PDoubledF = make_score(Tuning::getParam(IDoubledFMG[f]), Tuning::getParam(IDoubledFEG[f]));
		Score PDoubledR = make_score(Tuning::getParam(IDoubledRMG[r]), Tuning::getParam(IDoubledREG[r]));
		Score PDoubledLin[3] = { make_score(Tuning::getParam(IDoubledLinMG[0]), Tuning::getParam(IDoubledLinEG[0]))
		                        ,make_score(Tuning::getParam(IDoubledLinMG[1]), Tuning::getParam(IDoubledLinEG[1]))
		                        ,make_score(Tuning::getParam(IDoubledLinMG[2]), Tuning::getParam(IDoubledLinEG[2]))};
			score -=   PDoubled * doubled
		//score -=   (opposed ? PDoubled : PDoubledU) * doubled
		//score -=   (opposed ? PDoubled : PDoubledU) * doubled
            //score -=   (phalanx ? PDoubledP : PDoubled) * doubled
			//score -=   PDoubledF * doubled
			//score -=   PDoubledR * doubled
			//score -=   (PDoubledLin[0] + PDoubledLin[1] * r) * doubled
			//score -=   (PDoubledLin[0] + PDoubledLin[1] * r + PDoubledLin[2] * r * r) * doubled
                     + PWeakLever * more_than_one(lever);
		
   		Tuning::updateGradient(Us, IDoubledMG, -1.0 * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledEG, -1.0 * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		Tuning::updateGradient(Us, IWeakLeverMG, -1.0 * more_than_one(lever) * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IWeakLeverEG, -1.0 * more_than_one(lever) * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		
		//Tuning::updateGradient(Us, IDoubledMG, -1.0 * !(phalanx) * doubled * phase / PHASE_MIDGAME);
		//Tuning::updateGradient(Us, IDoubledEG, -1.0 * !(phalanx) * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		
		Tuning::updateGradient(Us, IDoubledMGP, -1.0 * bool(phalanx) * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledEGP, -1.0 * bool(phalanx) * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		
		
		//Tuning::updateGradient(Us, IDoubledMG, -1.0 * bool(opposed) * doubled * phase / PHASE_MIDGAME);
		//Tuning::updateGradient(Us, IDoubledEG, -1.0 * bool(opposed) * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		
   		
		Tuning::updateGradient(Us, IDoubledMGU, -1.0 * !opposed * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledEGU, -1.0 * !opposed * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		
		Tuning::updateGradient(Us, IDoubledFMG[f], -1.0 * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledFEG[f], -1.0 * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);

		Tuning::updateGradient(Us, IDoubledRMG[r], -1.0 * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledREG[r], -1.0 * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		
		Tuning::updateGradient(Us, IDoubledLinMG[0], -1.0 * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledLinMG[1], -1.0 * r * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledLinMG[2], -1.0 * r * r * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledLinEG[0], -1.0 * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledLinEG[1], -1.0 * r * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledLinEG[2], -1.0 * r * r * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		
	
	}
	else{
		Score PDoubledS = make_score(Tuning::getParam(IDoubledMGS), Tuning::getParam(IDoubledEGS));
		score -=   PDoubledS * doubled
                     ;//+ PWeakLever * more_than_one(lever);
   		Tuning::updateGradient(Us, IDoubledMGS, -1.0 * doubled * phase / PHASE_MIDGAME);
		Tuning::updateGradient(Us, IDoubledEGS, -1.0 * doubled * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
   		
	}
	
		//if(!neighbours && (ourPawns & forward_file_bb(Them, s)) && popcount(opposed) == 1 && !(theirPawns & adjacent_files_bb(s)))	
		if(!neighbours && (ourPawns & forward_file_bb(Them, s)) && !lever && popcount(stoppers) == 1)
		{			
			if(!opposed)
					 score -= make_score(Tuning::getParam(IDoubledIsolated[0]), Tuning::getParam(IDoubledIsolated[1]));
			else
					 score -= make_score(Tuning::getParam(IDoubledIsolatedOpposed[0]), Tuning::getParam(IDoubledIsolatedOpposed[1])) * bool(opposed);
					 if(opposed)
					 {
					 Tuning::updateGradient(Us, IDoubledIsolatedOpposed[0], -1.0 * phase / PHASE_MIDGAME);
					 Tuning::updateGradient(Us, IDoubledIsolatedOpposed[1], -1.0 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
					 }
					 else
					 {
					 Tuning::updateGradient(Us, IDoubledIsolated[0], -1.0 * phase / PHASE_MIDGAME);
					 Tuning::updateGradient(Us, IDoubledIsolated[1], -1.0 * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
					 }

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

int blockedstorm_mg[2];
int blockedstorm_eg[2];

int blockedstormf_mg[4];
int blockedstormf_eg[4];

template<Color Us>
Score Entry::evaluate_shelter(const Position& pos, Square ksq) {

  constexpr Color Them = ~Us;

  Bitboard b = pos.pieces(PAWN) & ~forward_ranks_bb(Them, ksq);
  Bitboard ourPawns = b & pos.pieces(Us);
  Bitboard theirPawns = b & pos.pieces(Them);

  blockedstormf_mg[0] = 0;
  blockedstormf_mg[1] = 0;
  blockedstormf_mg[2] = 0;
  blockedstormf_mg[3] = 0;
  blockedstormf_eg[0] = 0;
  blockedstormf_eg[1] = 0;
  blockedstormf_eg[2] = 0;
  blockedstormf_eg[3] = 0;

  blockedstorm_mg[0] = 0;
  blockedstorm_mg[1] = 0;
  blockedstorm_eg[0] = 0;
  blockedstorm_eg[1] = 0;
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
                Score PBlockedStormF = 
		  	make_score(Tuning::getParam(IBlockedStormFMG[d]), Tuning::getParam(IBlockedStormFEG[d]));
          bonus -= PBlockedStormF * int(theirRank == RANK_3);
	  blockedstormf_mg[d] -= int(theirRank == RANK_3);
	  blockedstormf_eg[d] -= int(theirRank == RANK_3);
          //bonus -= PBlockedStorm * int(theirRank == RANK_3);
	 // blockedstorm_mg -= int(theirRank == RANK_3);
	  //blockedstorm_eg -= int(theirRank == RANK_3);
	  //
	  //
	  if (RANK_3 <= theirRank && theirRank <= RANK_4)
	  {
                Score PBlockedStormPoly[2] = {
		  	make_score(Tuning::getParam(IBlockedStormPolyMG[0]), Tuning::getParam(IBlockedStormPolyEG[0])),
		  	make_score(Tuning::getParam(IBlockedStormPolyMG[1]), Tuning::getParam(IBlockedStormPolyEG[1]))
	                 };
          	bonus -= PBlockedStormPoly[0] + PBlockedStormPoly[1] * int(theirRank - RANK_3);
	  	blockedstorm_mg[0] -= 1;
	  	blockedstorm_mg[1] -= int(theirRank - RANK_3);
	  	blockedstorm_eg[0] -= 1;
	  	blockedstorm_eg[1] -= int(theirRank - RANK_3);
	  }
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
  int bs_mg[2] = { blockedstorm_mg[0], blockedstorm_mg[1] };
  int bs_eg[2] = { blockedstorm_eg[0], blockedstorm_eg[1] };

  int bsf_mg[4] = { blockedstormf_mg[0], blockedstormf_mg[1] ,
  			blockedstormf_mg[2], blockedstormf_mg[3] };
  int bsf_eg[4] = { blockedstormf_eg[0], blockedstormf_eg[1] ,
  			blockedstormf_eg[2], blockedstormf_eg[3] };
  Phase phase = Material::probe(pos)->game_phase();

  // If we can castle use the bonus after castling if it is bigger

  if (pos.can_castle(Us & KING_SIDE))
  {
      Score sh = evaluate_shelter<Us>(pos, relative_square(Us, SQ_G1));
      if(compare(shelter, sh))
      {
          bs_mg[0] = blockedstorm_mg[0];
          bs_mg[1] = blockedstorm_mg[1];
          bs_eg[0] = blockedstorm_eg[0];
          bs_eg[1] = blockedstorm_eg[1];

          bsf_mg[0] = blockedstormf_mg[0];
          bsf_mg[1] = blockedstormf_mg[1];
          bsf_mg[2] = blockedstormf_mg[2];
          bsf_mg[3] = blockedstormf_mg[3];
          bsf_eg[0] = blockedstormf_eg[0];
          bsf_eg[1] = blockedstormf_eg[1];
          bsf_eg[2] = blockedstormf_eg[2];
          bsf_eg[3] = blockedstormf_eg[3];
      }
      shelter = std::max(shelter, sh, compare);
  }

  if (pos.can_castle(Us & QUEEN_SIDE))
  {
      Score sh = evaluate_shelter<Us>(pos, relative_square(Us, SQ_C1));
      if(compare(shelter, sh))
      {
          bs_mg[0] = blockedstorm_mg[0];
          bs_mg[1] = blockedstorm_mg[1];
          bs_eg[0] = blockedstorm_eg[0];
          bs_eg[1] = blockedstorm_eg[1];

          bsf_mg[0] = blockedstormf_mg[0];
          bsf_mg[1] = blockedstormf_mg[1];
          bsf_mg[2] = blockedstormf_mg[2];
          bsf_mg[3] = blockedstormf_mg[3];
          bsf_eg[0] = blockedstormf_eg[0];
          bsf_eg[1] = blockedstormf_eg[1];
          bsf_eg[2] = blockedstormf_eg[2];
          bsf_eg[3] = blockedstormf_eg[3];
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

  Tuning::updateGradient(Us, IBlockedStormMG, bs_mg[0] * phase / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormEG, bs_eg[0] * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);

  Tuning::updateGradient(Us, IBlockedStormPolyMG[0], bs_mg[0] * phase / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormPolyEG[0], bs_eg[0] * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormPolyMG[1], bs_mg[1] * phase / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormPolyEG[1], bs_eg[1] * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);

  Tuning::updateGradient(Us, IBlockedStormFMG[0], bsf_mg[0] * phase / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormFMG[1], bsf_mg[1] * phase / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormFMG[2], bsf_mg[2] * phase / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormFMG[3], bsf_mg[3] * phase / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormFEG[0], bsf_eg[0] * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormFEG[1], bsf_eg[1] * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormFEG[2], bsf_eg[2] * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
  Tuning::updateGradient(Us, IBlockedStormFEG[3], bsf_eg[3] * (PHASE_MIDGAME - phase) / PHASE_MIDGAME);
  return shelter - make_score(0, 16 * minPawnDist);
}

void init() {
IDoubledIsolated[0] = Tuning::addParam(0, false);
IDoubledIsolated[1] = Tuning::addParam(0, false);
IDoubledIsolatedOpposed[0] = Tuning::addParam(15, false);
IDoubledIsolatedOpposed[1] = Tuning::addParam(57, false);
	IBackwardMG = Tuning::addParam(mg_value(Backward), false);
	IBackwardEG = Tuning::addParam(eg_value(Backward), false);
	IDoubledMG = Tuning::addParam(mg_value(Doubled), false);
	IDoubledEG = Tuning::addParam(eg_value(Doubled), false);
	IIsolatedMG = Tuning::addParam(mg_value(Isolated), false);
	IIsolatedEG = Tuning::addParam(eg_value(Isolated), false);
	IWeakLeverMG = Tuning::addParam(mg_value(WeakLever), false);
	IWeakLeverEG = Tuning::addParam(eg_value(WeakLever), false);
	IWeakUnopposedMG = Tuning::addParam(mg_value(WeakUnopposed), false);
	IWeakUnopposedEG = Tuning::addParam(eg_value(WeakUnopposed), false);
	ISupported = Tuning::addParam(21, false);
	for(Rank r = RANK_2; r < RANK_8; ++r)
		IConnected[r] = Tuning::addParam(Connected[r], false);
	
	for(int d = 1; d <= 5; ++d)
	{
		IDoubledDist[d][0] = Tuning::addParam(d == 1 ? mg_value(Doubled): 0, true);
		IDoubledDist[d][1] = Tuning::addParam(d == 1 ? eg_value(Doubled): 0, true);
	}

	IBlockedStormMG = Tuning::addParam(mg_value(BlockedStorm), USE_FOR_TUNING);
	IBlockedStormEG = Tuning::addParam(eg_value(BlockedStorm), USE_FOR_TUNING);

	IBlockedStormFMG[0] = Tuning::addParam(mg_value(BlockedStorm), false);
	IBlockedStormFEG[0] = Tuning::addParam(mg_value(BlockedStorm), false);
	IBlockedStormFMG[1] = Tuning::addParam(mg_value(BlockedStorm), false);
	IBlockedStormFEG[1] = Tuning::addParam(mg_value(BlockedStorm), false);
	IBlockedStormFMG[2] = Tuning::addParam(mg_value(BlockedStorm), false);
	IBlockedStormFEG[2] = Tuning::addParam(mg_value(BlockedStorm), false);
	IBlockedStormFMG[3] = Tuning::addParam(mg_value(BlockedStorm), false);
	IBlockedStormFEG[3] = Tuning::addParam(mg_value(BlockedStorm), false);

	IBlockedStormPolyMG[0] = Tuning::addParam(mg_value(BlockedStorm), false);
	IBlockedStormPolyMG[1] = Tuning::addParam(-mg_value(BlockedStorm), false);
	//IBlockedStormPolyMG[1] = Tuning::addParam(-mg_value(BlockedStorm) / 2, true);
	IBlockedStormPolyEG[0] = Tuning::addParam(eg_value(BlockedStorm), false);
	IBlockedStormPolyEG[1] = Tuning::addParam(-eg_value(BlockedStorm), false);
	//IBlockedStormPolyEG[1] = Tuning::addParam(-eg_value(BlockedStorm) / 2, true);
	
	IDoubledMGS = Tuning::addParam(mg_value(DoubledS), false);
	IDoubledEGS = Tuning::addParam(eg_value(DoubledS), false);
	
	IDoubledMGU = Tuning::addParam(mg_value(DoubledU), false);
	IDoubledEGU = Tuning::addParam(eg_value(DoubledU), false);
	
	IDoubledMGP = Tuning::addParam(mg_value(DoubledP), false);
	IDoubledEGP = Tuning::addParam(eg_value(DoubledP), false);
	
	//IPawnChainMG = Tuning::addParam(mg_value(PawnChain), true);
	//IPawnChainEG = Tuning::addParam(eg_value(PawnChain), true);

	for(File f = FILE_A; f < FILE_E; ++f)
	//for(Rank r = RANK_4; r < RANK_8; ++r)
	{
		IDoubledFMG[f] = Tuning::addParam(mg_value(Doubled), false);
	IDoubledFEG[f] = Tuning::addParam(eg_value(Doubled), false);
	
		IPawnChainMG[f] = Tuning::addParam(mg_value(PawnChain), false);
	IPawnChainEG[f] = Tuning::addParam(eg_value(PawnChain), false);

	}

	for(Rank r = RANK_3; r < RANK_8; ++r)
	{
		IDoubledRMG[r] = Tuning::addParam(mg_value(Doubled), false);
	IDoubledREG[r] = Tuning::addParam(eg_value(Doubled), false);
	

	}

	IDoubledLinMG[0] = Tuning::addParam(mg_value(Doubled), false);
	IDoubledLinMG[1] = Tuning::addParam(0, false);
	IDoubledLinMG[2] = Tuning::addParam(0, false);
	IDoubledLinEG[0] = Tuning::addParam(eg_value(Doubled), false);
	IDoubledLinEG[1] = Tuning::addParam(0, false);
	IDoubledLinEG[2] = Tuning::addParam(0, false);

	for(Rank r = RANK_2; r < RANK_8; ++r)
	{
		IIsolatedRMG[r] = Tuning::addParam(mg_value(Isolated), false);
		IIsolatedREG[r] = Tuning::addParam(eg_value(Isolated), false);
        }

	IIsolatedLinMG[0] = Tuning::addParam(mg_value(Isolated), false);
	IIsolatedLinMG[1] = Tuning::addParam(0, false);
	IIsolatedLinEG[0] = Tuning::addParam(eg_value(Isolated), false);
	IIsolatedLinEG[1] = Tuning::addParam(0, false);

	for(int r = (int)RANK_1; r <= (int)RANK_8; ++r)
	{
		IIsolatedGDMG[r] = Tuning::addParam(mg_value(Isolated), false);
		IIsolatedGDEG[r] = Tuning::addParam(eg_value(Isolated), false);
        }

	IConnectedPoly[0] = Tuning::addParam(281, false);
	IConnectedPoly[1] = Tuning::addParam(-52, false);
	IConnectedPoly[2] = Tuning::addParam(-6, false);
	IConnectedPoly[3] = Tuning::addParam(14, false);

	IConnectedFactor[0] = Tuning::addParam(4 * 64, false);
	IConnectedFactor[1] = Tuning::addParam(2 * 64, false);
	IConnectedFactor[2] = Tuning::addParam(-2 * 64, false);
	IConnectedFactor[3] = Tuning::addParam(-1 * 64, false);
	
	IConnectedC[0] = Tuning::addParam(64, false);
	IConnectedC[1] = Tuning::addParam(0, false);
	IConnectedC[2] = Tuning::addParam(-32, false);
	IConnectedC[3] = Tuning::addParam(16, false);
	
	IConnectedBR[1] = Tuning::addParam(-64, false);
	IConnectedBR[2] = Tuning::addParam(-64, false);
	IConnectedBR[3] = Tuning::addParam(-64, false);
	IConnectedBR[4] = Tuning::addParam(-64, false);
	IConnectedBR[5] = Tuning::addParam(-64, false);

	IConnectedBRPoly[0] = Tuning::addParam(-1, false);
	IConnectedBRPoly[1] = Tuning::addParam(0, false);

	IConnectedPHPoly[0] = Tuning::addParam(2, false);
	IConnectedPHPoly[1] = Tuning::addParam(0, false);
}

// Explicit template instantiation
template Score Entry::do_king_safety<WHITE>(const Position& pos);
template Score Entry::do_king_safety<BLACK>(const Position& pos);

} // namespace Pawns
