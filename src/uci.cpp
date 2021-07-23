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
		c19 bool(int(depth) & 8),
		c20 bool(moveCount & 1),
		    bool(moveCount & 2),
		    bool(moveCount & 4),
		c23 bool(moveCount & 8),
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
               c64  bool(thisThread->mainHistory[us][from_to(move)] & (1 << 14)),

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
     /*
      CC = true;
N: 80
Score: cramer
Maximize: true
Sample size: 29861941
BASE val=0 support=1
=>c0 cramer=0.1349 support=0.345656
c1 cramer=-0.0297853 support=0.910955
c2 cramer=-0.000777146 support=0.896151
c3 cramer=0.0592456 support=0.0739846
c4 cramer=0.0125895 support=0.0345092
c5 cramer=0.0651792 support=0.418966
c6 cramer=-0.0275422 support=0.959912
c7 cramer=-0.0200058 support=0.927643
c8 cramer=0.0035105 support=0.000161075
c9 cramer=-0.0126512 support=0.993593
c10 cramer=-0.0364685 support=0.923865
c11 cramer=-0.0187989 support=0.976593
c12 cramer=-0.0378052 support=0.877085
c13 cramer=0.123523 support=0.021057
c14 cramer=0.0871705 support=0.0177809
c15 cramer=0.127706 support=0.0311169
c16 cramer=0.0270161 support=0.560741
c17 cramer=0.0267242 support=0.494045
c18 cramer=-0.0134257 support=0.443111
c19 cramer=-0.0885859 support=0.751706
c20 cramer=-0.0535825 support=0.550205
c21 cramer=0.0820123 support=0.5848
c22 cramer=-0.0848866 support=0.541283
c23 cramer=-0.102352 support=0.690832
c24 cramer=-0.0835335 support=0.837624
c25 cramer=-0.0330835 support=0.97487
c26 cramer=-5.90038e-05 support=1
c27 cramer=0.00863242 support=0.423895
c28 cramer=-0.0145624 support=0.539206
c29 cramer=-0.00117061 support=0.433596
c30 cramer=-0.00628826 support=0.563202
c31 cramer=-0.00272797 support=0.505909
c32 cramer=0.00860009 support=0.538623
c33 cramer=-0.00856252 support=0.458167
c34 cramer=-0.0195648 support=0.459381
c35 cramer=-0.00756345 support=0.983368
c36 cramer=-0.0773471 support=0.262626
c37 cramer=-0.0233117 support=0.730116
c38 cramer=-0.0170464 support=0.969258
c39 cramer=0.114718 support=0.258023
c40 cramer=0.0942794 support=0.313382
c41 cramer=-0.0241112 support=0.902906
c42 cramer=-0.00748018 support=0.994302
c43 cramer=-0.0206961 support=0.325465
c44 cramer=0.0436073 support=0.306078
c45 cramer=0.0409404 support=0.181184
c46 cramer=0.0221508 support=0.123395
c47 cramer=0.0195482 support=0.0491684
c48 cramer=0.0164873 support=0.00399251
c49 cramer=-0.0770602 support=0.722301
c50 cramer=-0.00601184 support=0.484394
c51 cramer=-0.00632135 support=0.485034
c52 cramer=0.00607724 support=0.484261
c53 cramer=-0.00692029 support=0.483997
c54 cramer=0.00551223 support=0.485631
c55 cramer=0.0076183 support=0.481873
c56 cramer=-0.00868062 support=0.47863
c57 cramer=-0.00743602 support=0.481924
c58 cramer=-0.00501985 support=0.487433
c59 cramer=-0.00170903 support=0.4958
c60 cramer=-0.00434421 support=0.49093
c61 cramer=-0.0173127 support=0.455898
c62 cramer=0.00282813 support=0.480316
c63 cramer=0.0486801 support=0.392725
     c64 cramer=-0.123868 support=0.29628
     c65 cramer=0.00415039 support=0.0564449
     c66 cramer=0.00273059 support=0.0557379
     c67 cramer=0.00470614 support=0.0582942
     c68 cramer=0.00242411 support=0.056062
     c69 cramer=0.00202538 support=0.0560337
     c70 cramer=0.0040574 support=0.0565281
     c71 cramer=0.00398809 support=0.0567344
     c72 cramer=0.0054971 support=0.0573399
     c73 cramer=0.00656795 support=0.0573815
     c74 cramer=0.00128461 support=0.0558256
     c75 cramer=0.00225757 support=0.0564057
     c76 cramer=0.00490294 support=0.0560903
     c77 cramer=0.000602341 support=0.0571093
     c78 cramer=0.00546808 support=0.0506723
     c79 cramer=-0.0195482 support=0.954992

CC = !cutNode;
N: 80
	   Score: cramer
	   Maximize: true
	   Sample size: 19539986
	   BASE val=0 support=1
	   c0 cramer=0 support=0
	   =>c1 cramer=0.00144437 support=0.136082
	   c2 cramer=-0.000830235 support=0.897402
	   =>c3 cramer=0.0397234 support=0.0732038
	   c4 cramer=0.0116764 support=0.0355268
	   c5 cramer=0.031781 support=0.348434
	   c6 cramer=-0.0122915 support=0.938735
	   c7 cramer=-0.0202612 support=0.935634
	   c8 cramer=0.00256109 support=0.000133368
	   c9 cramer=-0.00489589 support=0.998355
	   c10 cramer=-0.0225764 support=0.925425
	   c11 cramer=-0.00801937 support=0.98521
	   c12 cramer=-0.0226642 support=0.882655
	   =>c13 cramer=0.0469346 support=0.0146144
	   c14 cramer=0.0443839 support=0.0125892
	   =>c15 cramer=0.079246 support=0.0243577
	   c16 cramer=0.0204067 support=0.564146
	   c17 cramer=0.0167002 support=0.48961
	   c18 cramer=-9.11594e-05 support=0.462165
	   c19 cramer=-0.0726308 support=0.698184
	   c20 cramer=-0.0325225 support=0.544738
	   c21 cramer=0.0505254 support=0.576498
	   c22 cramer=-0.0503487 support=0.533777
	   c23 cramer=-0.0715054 support=0.678692
	   c24 cramer=-0.0630285 support=0.824036
	   c25 cramer=-0.0252584 support=0.972683
	   c26 cramer=0 support=0
	   c27 cramer=0.00539903 support=0.419121
	   c28 cramer=-0.0111547 support=0.535557
	   c29 cramer=0.000248958 support=0.571875
	   c30 cramer=-0.00908748 support=0.556928
	   c31 cramer=-0.00427801 support=0.503971
	   c32 cramer=0.00704354 support=0.536898
	   c33 cramer=-0.00754474 support=0.457941
	   c34 cramer=-0.0127771 support=0.464665
	   c35 cramer=-0.00581489 support=0.980601
	   c36 cramer=-0.0287877 support=0.168448
	   c37 cramer=-0.0158573 support=0.758025
	   c38 cramer=0.000677561 support=0.0469814
	   c39 cramer=0.0404509 support=0.155916
	   c40 cramer=0.0472287 support=0.254129
	   c41 cramer=-0.019568 support=0.889126
	   c42 cramer=-0.00540703 support=0.995126
	   c43 cramer=0.0207304 support=0.736915
	   c44 cramer=-0.00183437 support=0.794633
	   c45 cramer=0.000501309 support=0.152925
	   c46 cramer=-0.0102153 support=0.87794
	   c47 cramer=-0.00898973 support=0.950626
	   c48 cramer=0.00125127 support=0.00209524
	   c49 cramer=-0.0593207 support=0.664108
	   c50 cramer=-0.00492556 support=0.483068
	   c51 cramer=-0.00495108 support=0.483721
	   c52 cramer=0.00479439 support=0.482854
	   c53 cramer=-0.00569063 support=0.482538
	   c54 cramer=0.00444129 support=0.484434
	   c55 cramer=0.00583407 support=0.480261
	   c56 cramer=-0.00686509 support=0.476522
	   c57 cramer=-0.00580805 support=0.480483
	   c58 cramer=-0.0044255 support=0.486485
	   c59 cramer=-0.00163333 support=0.495481
	   c60 cramer=-0.0024629 support=0.490194
	   c61 cramer=-0.0137031 support=0.451968
	   c62 cramer=0.00454067 support=0.478998
	   c63 cramer=0.0371361 support=0.384436
	   c64 cramer=-0.0776525 support=0.274561
	   c65 cramer=0.0018951 support=0.0554803
	   c66 cramer=0.00117405 support=0.054913
	   c67 cramer=0.00239949 support=0.0572898
	   c68 cramer=0.00102901 support=0.0552539
	   c69 cramer=0.000766035 support=0.0552345
	   c70 cramer=0.00199072 support=0.0555518
	   c71 cramer=0.00236103 support=0.0559249
	   c72 cramer=0.00328439 support=0.0563701
	   c73 cramer=0.00394302 support=0.0563648
	   c74 cramer=7.33085e-06 support=0.0550696
	   c75 cramer=0.00123306 support=0.055602
	   c76 cramer=0.00329865 support=0.0550607
	   c77 cramer=0.000979485 support=0.0563494
	   c78 cramer=0.0035812 support=0.0489557
	   c79 cramer=-0.0127328 support=0.956128

CC = !cutNode && move != countermove;
N: 80
	   Score: cramer
	   Maximize: true
	   Sample size: 19064037
	   BASE val=0 support=1
	   c0 cramer=0 support=0
	   =>c1 cramer=0.00146738 support=0.136381
	   =>c2 cramer=0.00351518 support=0.105038
	   =>c3 cramer=0.0402159 support=0.0729425
	   c4 cramer=0.0112241 support=0.0338686
	   c5 cramer=0.0320735 support=0.349993
	   c6 cramer=-0.0126188 support=0.93872
	   c7 cramer=-0.0205218 support=0.936054
	   c8 cramer=0.00245979 support=0.000131347
	   c9 cramer=-0.00464503 support=0.99834
	   c10 cramer=-0.0200256 support=0.923634
	   c11 cramer=-0.00754381 support=0.985108
	   c12 cramer=-0.0221 support=0.882636
	   c13 cramer=0.0395051 support=0.0131318
	   c14 cramer=0.0392533 support=0.0113184
	   c15 cramer=0 support=0
	   c16 cramer=0.0203869 support=0.563521
	   c17 cramer=0.0164307 support=0.489384
	   c18 cramer=-0.000660166 support=0.462124
	   =>c19 cramer=-0.0711239 support=0.696352
	   c20 cramer=-0.0278122 support=0.540429
	   c21 cramer=0.0432345 support=0.568294
	   c22 cramer=-0.0420644 support=0.524591
	   c23 cramer=-0.0664826 support=0.670711
	   c24 cramer=-0.0603077 support=0.819675
	   c25 cramer=-0.0243486 support=0.972013
	   c26 cramer=0 support=0
	   c27 cramer=0.00424169 support=0.418027
	   c28 cramer=-0.011057 support=0.535888
	   c29 cramer=0.0024814 support=0.574047
	   c30 cramer=-0.00757933 support=0.556253
	   c31 cramer=-0.00415742 support=0.503862
	   c32 cramer=0.0072339 support=0.537205
	   c33 cramer=-0.00864788 support=0.458642
	   c34 cramer=-0.015695 support=0.467283
	   c35 cramer=-0.005693 support=0.980599
	   c36 cramer=-0.0285977 support=0.168289
	   c37 cramer=-0.0156855 support=0.757958
	   c38 cramer=0.00112784 support=0.0471248
	   c39 cramer=0.0407943 support=0.154616
	   c40 cramer=0.048077 support=0.25296
	   c41 cramer=-0.0183692 support=0.88806
	   c42 cramer=-0.00476075 support=0.995004
	   c43 cramer=0.0207692 support=0.736704
	   c44 cramer=-0.00317985 support=0.795672
	   c45 cramer=-0.000520451 support=0.847661
	   c46 cramer=-0.0106201 support=0.87807
	   c47 cramer=-0.0089976 support=0.950562
	   c48 cramer=0.0010604 support=0.00209043
	   c49 cramer=-0.057465 support=0.663121
	   c50 cramer=-0.00489184 support=0.482681
	   c51 cramer=-0.00486713 support=0.483325
	   c52 cramer=0.00457493 support=0.482405
	   c53 cramer=-0.00551062 support=0.482096
	   c54 cramer=0.00422944 support=0.484041
	   c55 cramer=0.00556622 support=0.479756
	   c56 cramer=-0.00671587 support=0.475936
	   c57 cramer=-0.00576568 support=0.479961
	   c58 cramer=-0.00438198 support=0.486177
	   c59 cramer=-0.00178282 support=0.495439
	   c60 cramer=-0.00223653 support=0.489852
	   c61 cramer=-0.0132831 support=0.450801
	   c62 cramer=0.00525411 support=0.478993
	   c63 cramer=0.034844 support=0.380882
	   c64 cramer=-0.0700037 support=0.266136
	   c65 cramer=0.00433215 support=0.0564354
	   c66 cramer=0.00373759 support=0.0559385
	   c67 cramer=0.00495769 support=0.0582855
	   c68 cramer=0.00361556 support=0.0562917
	   c69 cramer=0.00337427 support=0.0562705
	   c70 cramer=0.00443222 support=0.0565306
	   c71 cramer=0.00496759 support=0.056932
	   c72 cramer=0.00584163 support=0.0573489
	   c73 cramer=0.00654609 support=0.0573027
	   c74 cramer=0.00247701 support=0.0560895
	   c75 cramer=0.0037676 support=0.056647
	   c76 cramer=0.00578334 support=0.056035
	   c77 cramer=0.00361275 support=0.0574105
	   c78 cramer=0.00595582 support=0.0497932
	   c79 cramer=-0.0107251 support=0.955106

CC = !cutNode && move != countermove && (int(depth) & 8);
N: 80
	   Score: cramer
	   Maximize: true
	   Sample size: 5788751
	   BASE val=0 support=1
	   c0 cramer=0 support=0
	   =>c1 cramer=0.00801062 support=0.220818
	   c2 cramer=0.0015362 support=0.0463914
	   =>c3 cramer=0.0158456 support=0.0517986
	   =>c4 cramer=0.0252661 support=0.0253799
	   c5 cramer=0.0218959 support=0.36401
	   c6 cramer=-0.00535813 support=0.887874
	   c7 cramer=-0.015197 support=0.890269
	   c8 cramer=0.00156009 support=6.30533e-05
	   c9 cramer=-0.000942508 support=0.996955
	   c10 cramer=-0.00677188 support=0.987898
	   c11 cramer=0.0108192 support=0.0386572
	   c12 cramer=-0.00848401 support=0.737339
	   =>c13 cramer=0.0586285 support=0.0172718
	   c14 cramer=0.0455928 support=0.0144689
	   c15 cramer=0 support=0
	   c16 cramer=-0.0116741 support=0.581195
	   c17 cramer=-0.027689 support=0.659484
	   c18 cramer=-0.0185544 support=0.926887
	   c19 cramer=0 support=1
	   c20 cramer=-0.0136392 support=0.518592
	   c21 cramer=0.023397 support=0.532849
	   c22 cramer=-0.0207653 support=0.512136
	   c23 cramer=-0.0420027 support=0.572625
	   c24 cramer=-0.0456561 support=0.698651
	   c25 cramer=-0.0197236 support=0.932793
	   c26 cramer=0 support=0
	   c27 cramer=0.0019763 support=0.429786
	   c28 cramer=-0.00242387 support=0.568257
	   c29 cramer=0.000617556 support=0.574963
	   c30 cramer=-0.00551299 support=0.572651
	   c31 cramer=-0.00393818 support=0.496073
	   c32 cramer=0.00111326 support=0.536085
	   c33 cramer=0.000498076 support=0.555727
	   c34 cramer=-0.00998375 support=0.413849
	   c35 cramer=0.0134954 support=0.0414177
	   c36 cramer=-0.0203974 support=0.0847685
	   c37 cramer=0.00942275 support=0.427616
	   c38 cramer=0.00238728 support=0.0641916
	   c39 cramer=0.0201822 support=0.156261
	   c40 cramer=0.0364336 support=0.303213
	   c41 cramer=-0.0120943 support=0.864529
	   c42 cramer=-0.00163489 support=0.998402
	   c43 cramer=0.00928906 support=0.598614
	   c44 cramer=0.000267781 support=0.351605
	   c45 cramer=0.00542622 support=0.310234
	   c46 cramer=0.000607941 support=0.285547
	   c47 cramer=-0.00215938 support=0.864307
	   c48 cramer=0.00272231 support=0.00428313
	   c49 cramer=-0.0377041 support=0.676824
	   c50 cramer=-0.00510545 support=0.473579
	   c51 cramer=-0.00484698 support=0.473835
	   c52 cramer=0.00436039 support=0.474014
	   c53 cramer=-0.0041223 support=0.473603
	   c54 cramer=0.00444063 support=0.475369
	   c55 cramer=0.00463132 support=0.46985
	   c56 cramer=-0.00642165 support=0.463713
	   c57 cramer=-0.00514239 support=0.468734
	   c58 cramer=-0.0042678 support=0.477204
	   c59 cramer=-0.0019177 support=0.489529
	   c60 cramer=-0.000503856 support=0.491305
	   c61 cramer=-0.0119165 support=0.426673
	   c62 cramer=0.00656484 support=0.458689
	   c63 cramer=0.0285554 support=0.338323
	   c64 cramer=-0.0573814 support=0.230104
	   c65 cramer=0.0031305 support=0.0266336
	   c66 cramer=0.00198393 support=0.0263872
	   c67 cramer=0.00253049 support=0.0280639
	   c68 cramer=0.00189151 support=0.0264798
	   c69 cramer=0.00212901 support=0.0265569
	   c70 cramer=0.00267436 support=0.0268478
	   c71 cramer=0.00278611 support=0.0269934
	   c72 cramer=0.00332718 support=0.027367
	   c73 cramer=0.0032588 support=0.0275092
	   c74 cramer=0.00169295 support=0.0263392
	   c75 cramer=0.00152752 support=0.0263839
	   c76 cramer=0.00287771 support=0.0266541
	   c77 cramer=0.00164278 support=0.0270278
	   c78 cramer=0.00214058 support=0.0256857
	   c79 cramer=-0.00574551 support=0.977339

CC = !cutNode && move != countermove && (int(depth) & 8) && move != ss->killers[0];
N: 80
	   Score: cramer
	   Maximize: true
	   Sample size: 5688769
	   BASE val=0 support=1
	   c0 cramer=0 support=0
	   =>c1 cramer=0.0032714 support=0.219667
	   =>c2 cramer=0.00339294 support=0.0471708
	   =>c3 cramer=0.0160695 support=0.0516259
	   =>c4 cramer=0.0252682 support=0.0250251
	   c5 cramer=0.0212699 support=0.364624
	   c6 cramer=-0.00668874 support=0.888533
	   c7 cramer=-0.0153458 support=0.891169
	   c8 cramer=0.00147943 support=6.34584e-05
	   c9 cramer=-0.00068717 support=0.996966
	   c10 cramer=-0.00621925 support=0.98769
	   c11 cramer=0.0110111 support=0.0384932
	   c12 cramer=-0.00816606 support=0.738608
	   c13 cramer=0 support=0
	   =>c14 cramer=0.0489795 support=0.0147232
	   c15 cramer=0 support=0
	   c16 cramer=-0.0114253 support=0.580778
	   c17 cramer=-0.0273366 support=0.65859
	   c18 cramer=-0.0181587 support=0.926574
	   c19 cramer=0 support=1
	   c20 cramer=-0.0091088 support=0.51366
	   c21 cramer=0.0180125 support=0.525666
	   c22 cramer=-0.0149978 support=0.504657
	   c23 cramer=-0.0374391 support=0.565154
	   c24 cramer=-0.042841 support=0.69337
	   c25 cramer=-0.0185711 support=0.931622
	   c26 cramer=0 support=0
	   c27 cramer=0.00217389 support=0.430024
	   c28 cramer=-0.00296178 support=0.569442
	   c29 cramer=0.000901768 support=0.575546
	   c30 cramer=-0.00443876 support=0.572619
	   c31 cramer=-0.00351748 support=0.495863
	   c32 cramer=0.000814513 support=0.536088
	   c33 cramer=-9.19245e-09 support=0.444742
	   c34 cramer=-0.00973648 support=0.413464
	   c35 cramer=0.0138866 support=0.0412929
	   c36 cramer=-0.0145521 support=0.083789
	   c37 cramer=0.0039335 support=0.423847
	   c38 cramer=-0.0002555 support=0.936137
	   c39 cramer=0.0202967 support=0.155664
	   c40 cramer=0.0362684 support=0.302301
	   c41 cramer=-0.0111109 support=0.863548
	   c42 cramer=-0.00139331 support=0.998374
	   c43 cramer=0.00894335 support=0.598695
	   c44 cramer=3.6847e-05 support=0.35045
	   c45 cramer=0.00515083 support=0.308752
	   c46 cramer=0.000403013 support=0.284459
	   c47 cramer=-0.00272857 support=0.865064
	   c48 cramer=0.00255274 support=0.00424644
	   c49 cramer=-0.0351125 support=0.675808
	   c50 cramer=-0.00505776 support=0.473136
	   c51 cramer=-0.0048424 support=0.473332
	   c52 cramer=0.00377261 support=0.473522
	   c53 cramer=-0.00402607 support=0.473139
	   c54 cramer=0.00432418 support=0.474952
	   c55 cramer=0.00453748 support=0.46931
	   c56 cramer=-0.00614039 support=0.463112
	   c57 cramer=-0.00492074 support=0.468154
	   c58 cramer=-0.00397736 support=0.476789
	   c59 cramer=-0.00204722 support=0.489426
	   c60 cramer=-0.000601416 support=0.490984
	   c61 cramer=-0.0114463 support=0.425392
	   c62 cramer=0.00680934 support=0.45829
	   c63 cramer=0.0265811 support=0.334686
	   =>c64 cramer=-0.0520052 support=0.222971
	   c65 cramer=0.00416802 support=0.0269129
	   c66 cramer=0.00310637 support=0.0267002
	   c67 cramer=0.00364139 support=0.0283499
	   c68 cramer=0.00298612 support=0.0267968
	   c69 cramer=0.00330103 support=0.0268729
	   c70 cramer=0.00368894 support=0.0271384
	   c71 cramer=0.00399944 support=0.0272931
	   c72 cramer=0.00430425 support=0.0276429
	   c73 cramer=0.004301 support=0.0277614
	   c74 cramer=0.00275043 support=0.0266494
	   c75 cramer=0.00255298 support=0.0266951
	   c76 cramer=0.00394443 support=0.0269234
	   c77 cramer=0.00279762 support=0.0273602
	   c78 cramer=0.00310917 support=0.0259357
	   c79 cramer=-0.00485603 support=0.976962

CC = !cutNode && move != countermove && (int(depth) & 8) && move != ss->killers[0]
    &&  (thisThread->mainHistory[us][from_to(move)] & (1 << 14));
      */

    constexpr bool MAXIMIZE = true;
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

    const std::string measure = (USE_CRAMER_AND_HIT ? "cramer+hit" : USE_CRAMER ? "cramer" : "hit");
    std::cerr << "N: " << F_N << std::endl;
    std::cerr << "Score: " << measure << std::endl;
    std::cerr << "Maximize: " << (MAXIMIZE ? "true" : "false" ) << std::endl;

    func.init();

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
                cerr << "=>c" << c << " " << measure << "=" << val << " support=" << support << std::endl;
	    }
	    else
            {
                cerr << "c" << c << " " << measure << "=" << val << " support=" << support << std::endl;
	    }

	/*
	if(USE_CRAMER)
	{
	    if(MAXIMIZE && (val > bestVal || (val == bestVal && support > bestSupport)))
	    {
                bestVal = val;
                bestSupport = support;
                cerr << "=>c" << c << " " << measure << "=" << val << " support=" << support << std::endl;
	    }
	    else if(!MAXIMIZE && (val < bestVal || (val == bestVal && support > bestSupport)))
	    {
                bestVal = val;
                bestSupport = support;
                cerr << "=>c" << c << " " << measure << "=" << val << " support=" << support << std::endl;
	    }
	    else
            {
                cerr << "c" << c << " " << measure << "=" << val << " support=" << support << std::endl;
	    }
	}
	else
	{
	    if(std::abs(val) > std::abs(bestVal) || (std::abs(val) == std::abs(bestVal) && support > bestSupport))
	    {
                bestVal = val;
                bestSupport = support;
                cerr << "=>c" << c << " " << measure << "=" << val << " support=" << support << std::endl;
	    }
	    else
            {
                cerr << "c" << c << " " << measure << "=" << val << " support=" << support << std::endl;
	    }
	}
	*/
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
