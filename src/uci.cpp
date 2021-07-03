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
    num = count_if(list.begin(), list.end(), [](string s) { return s.find("go ") == 0 || s.find("eval") == 0; });

    TimePoint elapsed = now();

    /*
     * {PvNode, cutNode, captureOrPromotion, givesCheck, ss->inCheck, improving, likelyFailLow, ttCapture}
     * it=0 cramer=0.0223331 => !c1*c2*c4*!c6*!c7 c2*!c4*c5*c6*!c7 c0*c4*c6*!c7 c0*c1*!c5*c6 !c4*!c5*c7 c0*c2*!c3*!c4*!c6*!c7 !c5*!c6*!c7 !c0*c2*c3*!c5*c7 !c1*c2*c3*c4 c4*!c5*c7 c4*c5*!c6*!c7 c0*!c3*c5*!c6*!c7 c1*!c4*c6*c7 !c0*!c1*!c2*!c3*c4*c7 !c0*!c1*c4 c1*c3 c2*c4*c5*c6*c7 !c0*c1*c2*!c3*c6 !c3*c4*c5*!c6 !c0*c3 c0*c1*!c4*c5*c6 !c1*c2*!c3*!c5 c0*c1 c1*c5*!c6*!c7 !c3*c4*c7 !c1*!c2*!c4*!c5*c7 c1*c3*!c6*c7 c0*!c1*c3*!c5*c7 !c0*!c2*c3*!c7 !c0*!c2*!c3*c5 !c0*!c1 c1*!c3*c7
     * it=16 cramer=0.0832873 => c1*c5 c0*c1*c4*!c5*c6 c0*c3*!c4*c5*c6 c5*!c7 c0*c2*c3*!c4*c5*c7 c0*!c1*c4*!c7 c2*c3*c4*!c6 c2*!c5 c2*c6*!c7 c0*c2*c5*!c6 c0*c2*!c3*!c5*c6*c7 !c1*c2*!c3*c4*!c5*!c6*c7 c0*c2*!c6*c7 !c0*c2*!c3*c5*c7 c1*!c2*!c4*c5 c0*!c2*c6 c0*!c2*c3 c2*!c3*!c5 !c1*c2*!c5 c1 !c1*!c3*c4*!c5*!c6 c0*!c3*c4*!c5*c6*c7 !c1*c4*!c5 !c0*c2*c5*c6*c7 c1*!c7 c2*!c4*c5 !c0*c5 c2 c0*!c1*!c2*c5*!c6*c7 c0*c2*c3*!c6*!c7 !c2*!c3*c4*!c7 c1*!c2*!c4*c5*!c6
     * it=77 cramer=0.085573 => !c1*!c2*!c7 !c0*!c1*!c2*!c4*!c5*c7 c0*c1*c4*c5*c6*!c7 c0*!c1*!c4*c6*c7 !c2*!c3*c5*c6*!c7 c4*!c6 !c0*!c2*!c3*c4 !c0*!c1*c4*!c5*!c6*c7 c0*!c1*c2*!c5*c7 c2*!c3 c1*!c3*!c4*c5*c7 !c0*!c3*!c5*!c6*!c7 c2*!c6 !c0*!c2*!c4*!c5*!c7 c4*c5*!c6*c7 c1*c4*c6*!c7 !c2*c3*!c4*!c5*!c6*c7 c1*c2*c3*!c4*c5 c0*!c2 !c2*c4*c5*c7 c0*!c1*!c4*!c5*!c6 c0*!c3*!c5*c7 !c0*c2*c4 !c0*c2*c3*c5 c0*c5*!c6 !c0*!c1*c4*!c6*!c7 c2*c3*c4 !c2*!c3*!c4 c0*!c1*!c3*c6 c0*c1*c3*c5*!c6 !c1*c2*c3*!c7 !c0*!c4*!c5*c7
     * it=156 cramer=0.0950918 => !c1*!c3*!c7 !c1*!c2*!c4*!c5*!c7 c1*c3*c4*!c5 !c0*!c1*!c5 c1*!c2*c3*!c4*c5*c6 !c1*c5 !c0*!c1*c3*c6*!c7 c1*!c4*!c5 c3*!c4 c0*c5*!c7 c0*!c3*!c7 !c1*c5*c6 c0*c1*!c2*c6 !c0*!c1*!c4*!c5*!c6*c7 c4*!c6*c7 !c0*!c2*c6*!c7 c1*c3*c5*!c7 !c0*!c1*!c3*!c4*c7 c1*c3*c4*!c6*c7 !c2*c3*c4*!c5*c6*!c7 c3*!c5*!c7 !c2*c6 !c0*c1*c2*c4*!c7 c1*c3*!c4*c5*!c6 c0*!c2*!c5*!c7 c1*c3*!c4 !c1*c2*c3*c5 c0*!c1*!c3*c4*!c7 c3*!c6*c7 !c0*!c2*c3*c4*!c5*!c6*c7 !c3*!c5 !c0*c1*c4*!c5*c6*c7
     * it=207 cramer=0.105246 => !c1*c3*!c4*!c5 c1*!c2*c3*!c5*!c6*!c7 c0*c1*!c4*c5*c7 !c0*!c2*c3*c5 !c1*!c3*c7 !c1*!c2*c5*c6*c7 c0*c2*c3*!c6 c1*c5*c7 c2*c3*c4*!c6*!c7 c2*c3*!c5 c0*!c1*!c4*!c5*!c7 c0*c5*!c6*!c7 c0*c1*!c3*c5*c6*!c7 !c1*!c4*c7 c1*c5*!c6 !c1*c3*c4*!c5*!c6 !c2*c6*!c7 !c3*c4*c5*!c7 !c4*c6 c4*!c5*c6 c1*!c3*!c7 c1*!c3*c4*!c5*c7 c0*c2*!c7 c0*!c2*!c3*!c7 !c1*c3*c4*c5*!c6 !c0*c2*c3*c5*!c7 c1*c3*!c4*!c5*!c6*c7 c0*!c1*!c3*!c4*c6 !c1*!c4*c5*c6*c7 !c0*c2*c6*!c7 c0*c1*c2*c5 c0*!c4*c5*c6*c7
     * it=278 cramer=0.117186 => c0*!c1*c2*!c4*!c5*c7 !c0*c2 c0*!c2*c3*c4*c6*!c7 !c2*!c4*c5*c7 !c0*c1*!c2*c6*c7 !c1 c4*c6*!c7 !c0*!c1*!c3 c0*!c2*c4 !c1*!c3*c6*c7 !c2*c5*!c6*c7 c0*!c2*c3*!c4*c5*!c6*!c7 c0*!c1*c2*c3*c4 !c2*c3*c4*c5*c7 !c4*!c5*!c6 c1*c2*c5 c0*!c2*c3*!c7 c0*c1*!c4*c5 c1*c2*c7 c0*!c1*!c4 c0*!c6*!c7 !c1*!c3*c4*!c5*!c6 !c0*!c4*!c5*!c6 c1*c2 !c0*!c1*!c2*c4*c7 c3*c4*c5*c6 c0*!c3*!c7 c0*!c1*!c2*c5*c7 c1*!c4*c6*!c7 c1*!c5*c6 c0*!c1*c3*!c5*!c6 !c0*c3*c5*c6
     *
     * it=17972 cramer=0.19789 => c0*!c2*c3 c2*!c4*!c5*c6 !c0*!c1*!c6 c4*!c5*c6*c7 !c0*c3*c4*!c5 !c0*c1*!c2*c3*!c5*c6*c7 !c0*!c3*!c6 !c0*!c2*c3*!c4*!c5*c6 c2*c3*!c6 !c0*c3*c4*!c7 !c0*!c1*!c2*!c3*c7 c1*c2*!c3*!c6*!c7 !c0*!c1*!c2*!c3*c4*!c5*c7 !c0*c3*c4*!c5*c6 !c0*c1*c3*c4*c5 !c0*!c1*c2*!c4*c5*!c6*!c7 c1*c3*c4*c5*c6*c7 !c1*!c3*!c5 c1*!c2*c4*c5*!c6 c1*c3*c5*c6*c7 !c2*c6 !c0*!c1*!c2*c4*!c5*!c7 c1*!c2*!c3*c4*c6 c0*!c2*!c6*!c7 !c1*!c2*c4*!c5*c6 !c1*c2*c4*!c5*c7 !c0*!c1*!c2*c4*c5*c6 !c1*c7 c0*!c3*!c4*!c6 !c1*c2*c6 c0*!c1*c3*c6*c7 c0*c2*!c4*!c6*c7
     *
     * it=3156 cramer=0.130392 => c0*c4*!c5*c7 !c0*c3*!c5*!c7 c0*c2*!c3*!c5 c0*!c1*!c4*!c5*c6 c0*!c1*!c3*!c4*!c6*c7 c3*c4*!c5 !c0*!c1*c2*!c3*!c4*!c5*!c6*!c7 c3*!c7 !c2*c3*c4 c1*!c3*c6*!c7 !c1*c2*c5 !c1*!c2*c4*c5 c1*c2*c3*!c4*!c5*c6 c1*c3*!c4*!c7 c1*c3*c6 !c0*c3*c4 c1*!c2*!c3*c7 c0*c1*!c2*!c6*!c7 !c0*c1*c2*c3*c5*!c6*!c7 !c2*!c4*!c5*!c7 !c1*!c4*c5*c6*c7 !c0*!c1*!c2*c7 c4*c5*c6*!c7 !c0*c1*c3*!c6*!c7 !c1*c3*!c5*!c6*c7 c0*c2*c3*!c5*!c6 !c1*c2*c6*!c7 c0*!c1*c2*!c3*!c4*!c7 !c2*c3*!c6*!c7 !c0*c1*c2*!c3*c6 !c1*!c2*!c5*!c6 !c1*!c4
     *
     * it=131 cramer=0.111711 msteps=2 => c1*c6*c7 !c0*c2*c4*!c7 c2*c3*!c4*c5*!c6 !c2*c4*c5*c6 c0*!c1*c2 !c2*c3*!c4*!c6*!c7 !c0*c1*!c2*!c4*!c5 !c0*!c1*!c4*c5*!c7 c3*c5 c1*c2*c3*c7 c3*c5*c6 !c0*c1*c3*!c4*!c7 c2*c5*c6 c0*!c3*!c4*!c5*!c7 c0*!c1*c2 c0*!c2*c3*!c6*c7 !c0*c1*!c3*c4*!c6 c0*!c1*c2*!c3*c4*c6*c7 !c0*!c1*!c3*c4*!c6*!c7 c1*!c6 c0*c3*c6*!c7 !c0*!c1*c2*!c3*!c5*!c7 !c0*c3*c4*c5 c3*!c4*c5*c6 c1*c4*!c5*!c6*!c7 !c0*c1*c2*c3*!c4*!c5*!c6*!c7 c0*!c2*c4*!c7 !c2*c3*c5*c6 !c1*c2*!c3*c4*c7 !c1*c2*c6*c7 c1*!c2*c3*c4*c7 c0*!c1*!c2*c3*c5*c6
     *
     * it=0 cramer=0 msteps=1 => c1*!c4*c6*!c7
     * it=6 cramer=0.0198077 msteps=1 => !c4*c6*!c7
     * it=8 cramer=0.0205214 msteps=1 => !c4*!c6*!c7
     * it=9 cramer=0.106422 msteps=1 => !c4*c5*!c6*!c7
     * it=43 cramer=0.12846 msteps=11 => !c0*c1*!c4*c5*!c6*!c7
     * it=67 cramer=0.128563 msteps=6 => c1*!c7
     *
     * it=0 cramer=0.00819133 msteps=1 => !c0*c5*c7 c0*!c1*c2*!c3*!c5 !c0*c4*!c6*!c7
     * it=3 cramer=0.110463 msteps=1 => !c0*c5 c0*!c1*c2*!c3*!c5 !c0*c4*!c6*!c7
     * it=27 cramer=0.11831 msteps=6 => !c1*!c5 c0*!c1*c2*!c3*!c5 c0*c4*!c6
     * it=29 cramer=0.120865 msteps=1 => !c1*!c5 c0*!c1*c2*!c3*!c5 c0*!c6
     * it=30 cramer=0.123473 msteps=1 => !c1*!c5 c0*!c1*c2*!c3*!c5 c0*c6
     * it=56 cramer=0.12355 msteps=7 => !c0*!c1*!c5 c0*!c1*!c3*!c4*!c5 c0*!c2*c6
     * it=64 cramer=0.123592 msteps=1 => !c0*!c1*!c5 c0*!c1*!c4*!c5 c0*!c2*c6
     * it=96 cramer=0.127924 msteps=10 => !c0*!c1*!c6 c0*!c4*!c5 c0*!c2
     * it=102 cramer=0.127926 msteps=1 => !c0*!c1*!c6 c0*!c4*!c5 c0
     * it=118 cramer=0.128083 msteps=3 => !c1*!c6 c0*!c4*!c5 c0*c5
     * it=135 cramer=0.128154 msteps=3 => !c0*!c1*!c6 c0*!c4*!c5 c0*c5
     * it=150 cramer=0.133232 msteps=2 => !c0*!c1*!c6 !c4*!c5 c0*!c4*c5
     * it=153 cramer=0.135885 msteps=1 => !c0*!c1*!c6 !c3*!c4*!c5 c0*!c4*c5
     *
     *it=0 cramer=0.0839565 msteps=1 => !c0*!c1*!c2 c4
     it=2 cramer=0.0850625 msteps=1 => !c0*!c1*!c2 !c2*c4
     it=9 cramer=0.100589 msteps=1 => !c0*!c1 !c2*c4
     it=10 cramer=0.106029 msteps=1 => !c0*!c1 !c2*c4*c7
     it=12 cramer=0.11576 msteps=1 => !c0*!c1 !c2*!c4*c7
     it=19 cramer=0.131894 msteps=1 => !c1 !c2*!c4*c7
     * */
    /*
     * {PvNode, cutNode, captureOrPromotion, givesCheck,
     *                     ss->inCheck, improving, likelyFailLow, ttCapture,
     *                                         type_of(movedPiece) == PAWN,
     *                                                             type_of(movedPiece) == KNIGHT,
     *                                                                                 type_of(movedPiece) == BISHOP,
     *                                                                                                     type_of(movedPiece) == ROOK,
     *                                                                                                                         type_of(movedPiece) == QUEEN,
     *                                                                                                                                             type_of(movedPiece) == KING}
     * */
    /* SA
     * !it=37 cramer=0.0126155 T=8.26565 msteps=1 => c1*!c2*!c3*c4*c7*!c11*!c12*c13
     * !it=70 cramer=-0.0289673 T=7.00549 msteps=1 => c0*!c1*!c3*!c4*!c10*!c11*!c13
     *
     * HC
     * it=697 cramer=0.218433 msteps=1 => c1*c3*!c10
     * */
    /*
     * {
     *                     cutNode, PvNode || cutNode,  // PvNode = 00, cutNode = 01, allNode = 10
     *                                         captureOrPromotion, givesCheck,
     *                                                             ss->inCheck, improving, likelyFailLow, ttCapture,
     *                                                                                 bool(type_of(movedPiece) & 1),
     *                                                                                                     bool(type_of(movedPiece) & 2),
     *                                                                                                                         bool(type_of(movedPiece) & 4)
     *                                                                                                                                             }
     * SA
     * !it=7 cramer=-0.0566331 T=9.60693 msteps=1 => !c1*!c3*c5*!c9
     * !it=150 cramer=0.144986 T=0.0676799 msteps=1 => c3*c5*!c7*!c9*c10
     * !it=174 cramer=0.165945 T=0.0600086 msteps=1 => c1*!c2*c3*c5*!c6*!c7*!c9*c10
     * !it=188 cramer=0.206156 T=0.056223 msteps=1 => c0*c1*!c2*c3*!c6*!c9*c10
     * !it=520 cramer=0.21261 T=0.0105925 msteps=1 => c0*c3*!c9*c10
     * !it=536 cramer=0.212756 T=0.00977617 msteps=1 => c0*c3*c10
     *
     * !it=246 cramer=0.139198 T=0.0418288 msteps=1 => c0*!c2*c3*!c7*c8*c10
     * !it=282 cramer=-0.159476 T=0.0349226 msteps=1 => !c0*!c3
     * !it=789 cramer=0.212756 T=0.00275048 msteps=1 => c0*c3*c10
     *
     *
     * !it=69 cramer=0.133803 T=0.101576 msteps=1 => c1*c3*!c7*c8*!c9
     * !it=135 cramer=0.147517 T=0.0729648 msteps=1 => c0*c1*c3*!c7*!c8
     * !it=176 cramer=0.190269 T=0.05941 msteps=1 => c0*c3*c5*!c6
     * !it=285 cramer=0.208086 T=0.0344014 msteps=1 => c0*c3*!c4*c10
     * !it=311 cramer=0.21033 T=0.0301978 msteps=1 => c0*c1*c3*!c9
     *
     * */
    /* SA11_1 less neutral
     * !it=87 cramer=0.0902212 T=0.0928123 msteps=1 => c1*!c2*!c3*c5*!c6*!c7*!c8
     * !it=105 cramer=0.118283 T=0.0848049 msteps=1 => c0*!c2*!c7
     * !it=245 cramer=0.128563 T=0.042039 msteps=1 => c0*!c6*!c7
     */
    /*
     *  cutNode, PvNode || cutNode,  // PvNode = 00, cutNode = 01, allNode = 10
     *                      captureOrPromotion, givesCheck,
     *                                          ss->inCheck, improving, likelyFailLow, ttCapture,
     *                                                              bool(type_of(movedPiece) & 1),
     *                                                                                  bool(type_of(movedPiece) & 2),
     *                                                                                                      bool(type_of(movedPiece) & 4),
     *bool(int(us == WHITE ? to_sq(move) : flip_rank(to_sq(move))) & 1),
     *bool(int(us == WHITE ? to_sq(move) : flip_rank(to_sq(move))) & 2),
     *bool(int(us == WHITE ? to_sq(move) : flip_rank(to_sq(move))) & 4),
     *bool(int(us == WHITE ? to_sq(move) : flip_rank(to_sq(move))) & 8),
     *bool(int(us == WHITE ? to_sq(move) : flip_rank(to_sq(move))) & 16),
     *bool(int(us == WHITE ? to_sq(move) : flip_rank(to_sq(move))) & 32),
     * SA17_1 (+ all squares)
     *
     * !it=13 cramer=-0.0154655 T=0.134492 msteps=1 => !c5*!c10*!c11*!c12*c15
     * !it=131 cramer=0.0256613 T=0.0744425 msteps=1 => c0*!c2*!c4*c8*!c9*!c12*!c13*c14*!c15*c16
     * !it=159 cramer=0.0695837 T=0.0646945 msteps=1 => c0*!c2*!c3*c5*!c6*!c8*!c15
     * !it=368 cramer=-0.105991 T=0.022693 msteps=1 => !c0*!c1
     * !it=377 cramer=-0.127926 T=0.021692 msteps=1 => !c0
     *
     * !it=27 cramer=-0.0401729 T=0.125378 msteps=1 => !c0*!c2*!c4*!c6*!c8*!c9*!c16
     * !it=141 cramer=0.0639901 T=0.0708031 msteps=1 => !c2*c5*!c7*c10*!c11*!c12*c13*!c14
     * !it=144 cramer=0.0642419 T=0.0697463 msteps=1 => !c2*c5*!c7*c10*!c11*c13*!c14
     * !it=285 cramer=0.0681137 T=0.0344014 msteps=1 => c0*c1*!c7*c10*c11*!c15
BEST it=1300 cramer=0.128563 T=0.000212326 msteps=1 => c0*!c7
     *
     * !it=13 cramer=0.0397511 T=0.134492 msteps=1 => c0*c1*c8*!c10*!c15*c16
     * !it=101 cramer=0.0621644 T=0.0865224 msteps=1 => c1*!c3*c5*!c7*c9*!c12
     * !it=236 cramer=-0.127926 T=0.0439789 msteps=1 => !c0
     * !it=361 cramer=0.073454 T=0.0235034 msteps=1 => c0*!c2*c5*!c8*!c12
     *BEST it=1400 cramer=0.128563 T=0.000128621 msteps=1 => c0*c1*!c7

     HIT
     BEST it=9400 score=0.0392538 T=4.94261e-22 msteps=1 => !c0*!c1*c3*!c6*c8*c13

     BEST it=9600 score=0.099455 T=1.81373e-22 msteps=1 => !c0*c1*!c2*c4*c6*c8*c9*c15
     * */
    /*
     *  cutNode, PvNode || cutNode,  // PvNode = 00, cutNode = 01, allNode = 10
     *  captureOrPromotion, givesCheck,
     *  ss->inCheck, improving, likelyFailLow, ttCapture,
     *  bool(type_of(movedPiece) & 1),
     *  bool(type_of(movedPiece) & 2),
     *  bool(type_of(movedPiece) & 4),
     *bool(pos.count<ALL_PIECES>() & 1),
     *bool(pos.count<ALL_PIECES>() & 2),
     *bool(pos.count<ALL_PIECES>() & 4),
     *bool(pos.count<ALL_PIECES>() & 8),
     *bool(pos.count<ALL_PIECES>() & 16),
     * SA16_1 (+ material)
     * BEST it=8700 score=0.19103 T=1.6512e-20 msteps=1 => !c3*!c4*c5*!c6*!c8*c9*c10*!c12*c13*!c14*!c15
BEST it=8700 score=0.128563 T=1.6512e-20 msteps=1 => c0*!c7
     BEST it=8800 score=0.19103 T=1.00025e-20 msteps=1 => !c3*!c4*c5*!c6*c9*c10*!c12*c13*!c14*!c15
     */
    /*
     * *  cutNode, PvNode || cutNode,  // PvNode = 00, cutNode = 01, allNode = 10
     *      *  captureOrPromotion, givesCheck,
     *           *  ss->inCheck, improving, likelyFailLow, ttCapture,
     *                *  bool(type_of(movedPiece) & 1),
     *                     *  bool(type_of(movedPiece) & 2),
     *                          *  bool(type_of(movedPiece) & 4),
     *                               *bool(pos.count<ALL_PIECES>() & 1),
     *                                    *bool(pos.count<ALL_PIECES>() & 2),
     *                                         *bool(pos.count<ALL_PIECES>() & 4),
     *                                              *bool(pos.count<ALL_PIECES>() & 8),
     *                                                   *bool(pos.count<ALL_PIECES>() & 16),
     *
     * bench 16 1 13 pos1000.fen
     * SA16_1 (+material)
     * BEST it=300 score=-0.132012 T=0.0319096 msteps=1 => !c0*!c4
     * */
    /*
     c0-c1 *  cutNode, PvNode || cutNode,  // PvNode = 00, cutNode = 01, allNode = 10
     c2-c3 *  captureOrPromotion, givesCheck,
     c4-c7 *ss->inCheck, improving, likelyFailLow, ttCapture,
     c8 *more_than_one(pos.checkers()),
     c9 *doubleExtension,
     c10 *bool(extension),
     c11 *singularQuietLMR,
     c12 *ss->ttPv && !PvNode,
     c13 *move == ss->killers[0],
     c14* move == ss->killers[1],
     c15 *move == countermove
     * SA16_1_mix
     * !it=559 score=-0.0938326 T=0.00871164 msteps=1 => !c0*!c5*!c7*!c11*!c13*!c14
     *
     * BEST it=500 score=0.147967 T=0.0117095 msteps=1 => c0*c1*!c11*c13
     * C = cutNode && move == countermove && !singularQuietLMR
     *
     * !it=574 score=-0.171404 T=0.00808064 msteps=1 => !c9*!c13*!c14*!c15
     * C = doubleExtension || move == ss->killers[0] || move == ss->killers[1] || move == countermove;
     * */
    /* SA27_1
c0   cutNode, 
c1   PvNode || cutNode,  // PvNode = 00, cutNode = 01, allNode = 10
c2   captureOrPromotion, 
c3   givesCheck,
c4   ss->inCheck, 
c5   improving, 
c6   likelyFailLow, 
c7   ttCapture,
c8   more_than_one(pos.checkers()),
c9   doubleExtension,
c19  bool(extension),
c11  singularQuietLMR,
c12  ss->ttPv && !PvNode,
c13  move == ss->killers[0],
c14  move == ss->killers[1],
c15  move == countermove,
c16  bool(int(depth) & 1),
c17  bool(int(depth) & 2),
c18  bool(int(depth) & 4),
c19  bool(int(depth) & 8),
c20  bool(moveCount & 1),
c21  bool(moveCount & 2),
c22  bool(moveCount & 4),
c23  bool(moveCount & 8),
c24  bool(moveCount & 16),
c25  bool(moveCount & 32),
c26  bool(moveCount & 64)
     * CRAMER:
     *
     * !it=26 cramer=0.00115979 T=0.126008 msteps=1 => c1*!c6*c8*c10*c13*!c20*c21*!c24*!c25*!c26
     *
     * !it=16 cramer=-0.000407983 T=0.132485 msteps=1 => c0*!c2*!c3*c5*!c9*!c10*!c12*!c13*!c15*c16*c20*!c21*c24*c25
     *
     * HIT:
     *
     * !it=19 fh=0.113071 T=0.130508 msteps=1 => c0*!c3*!c6*!c8*!c11*c14*!c16*!c20*!c21*!c26
       C = cutNode && !givesCheck && !likelyFaillow && !more_than_one(pos.checkers())
          && !singularQuietLMR && move == ss->killers[1] &&  !(int(depth) & 1)
          && !(moveCount & 1) && !(moveCount & 2) && !(moveCount & 64)
     *
     * !it=36 fh=0.0847458 T=0.119847 msteps=1 => !c3*c4*c12*!c13*c15*!c16*!c18*c19*!c20*!c25
     * */
    FUNC best, tmp;
    double bestVal = 0;
    double curVal = 0;
    int fails = 0;
    int steps = 0;
    constexpr bool WEIGHT_WITH_FREQ = false;
    constexpr bool USE_CRAMER = false;
    constexpr bool USE_CRAMER_AND_HIT = false;
    constexpr double LAMBDA = 0.995;
    constexpr double P0 = 0.5;
    constexpr double LOSS_P0 = 0.1;
    //double T0 = 10;
    double T0 = -LOSS_P0/std::log(P0);
    double T = T0;
    double support = 0;
    double bestSupport = 0;

    const std::string measure = (USE_CRAMER_AND_HIT ? "cramer+hit" : USE_CRAMER ? "cramer" : "fh");

    func.randomInit();

    for(int it = 0;true;++it)
    {
	dbg_reset();
        if(SIMULATED_ANNEALING)
        {
	    T *= LAMBDA;
	    tmp = func;
	    //steps = std::min(100.0, fails * fails / 100.0 + 1.0);
	    steps = 1;
            func.mutate(steps);
        }
	else if(HILL_CLIMBING)
        {
	    tmp = func;
	    //steps = fails / 20 + 1;
	    //steps = 100.0 * (1.0 / (1 + std::exp(-0.01 * fails)) - 0.5) * 2 + 1;
	    steps = std::min(100.0, fails * fails / 100.0 + 1.0);
            func.mutate(steps);
        }
        else
        {
            func.randomInit();
        }

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
            else if (token == "ucinewgame") { Search::clear(); elapsed = now(); } // Search::clear() may take some while
        }

	double val = 0;
	double w = get_hit(10);
	auto wFunc = [](double x) { return x; };

        if(USE_CRAMER_AND_HIT)
        {
            double valc = get_cramer();
            double v1 = get_hit(1);
            double v0 = get_hit(0);
	    if (WEIGHT_WITH_FREQ)
	    {
		valc *= wFunc(val >= 0 ? w : 1-w);
                v1 *= wFunc(w);
                v0 *= wFunc(1-w);
	    }
            double valh = v1 >= v0 ? v1 : -v0;

	    val = (valc + valh) / 2;
        }	 
	else if(USE_CRAMER)
        {
            val = get_cramer();
	    if (WEIGHT_WITH_FREQ)
		    val *= wFunc(val >= 0 ? w : 1-w);
        }	 
	else // HIT
	{
            double v1 = get_hit(1);
            double v0 = get_hit(0);
	    if (WEIGHT_WITH_FREQ)
	    {
                v1 *= wFunc(w);
                v0 *= wFunc(1-w);
	    }
            val = v1 >= v0 ? v1 : -v0;
	}

	support = (val >= 0 ? get_hit(10) : 1 - get_hit(10) );

		/*
	double val = (USE_CRAMER_AND_HIT       ? (get_cramer() + (get_hit(1) >= get_hit(0) ? get_hit(1)   : -get_hit(0))) / 2 :
	              USE_CRAMER               ? get_cramer() : 
	              get_hit(1) >= get_hit(0) ? get_hit(1)   : -get_hit(0));

	if (WEIGHT_WITH_FREQ)
	{
		val *= (val < 0 ? 1 - get_hit(10) : get_hit(10));
	}
	*/

	if(it == 0 || std::abs(val) > std::abs(curVal))
	{
		curVal = val;
	        if(!SIMULATED_ANNEALING || it == 0 || std::abs(curVal) > std::abs(bestVal))
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
		double p = std::exp(-(std::abs(curVal) - std::abs(val))/T);
		double r = std::rand()/double(RAND_MAX);
		if(r <= p) // accept
		{
		    curVal = val;
		    cerr << "-it=" << it << " " << measure << "=" << curVal << " support=" << support << " T=" << T << " p=" << p << " msteps=" << steps << " => ";
                    func.print(cerr);
		}
		else //reject
		{
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


    //elapsed = now() - elapsed + 1; // Ensure positivity to avoid a 'divide by zero'

    //dbg_print(); // Just before exiting
    //dbg_printc(); // Just before exiting

    //cerr << "\n==========================="
    //     << "\nTotal time (ms) : " << elapsed
    //     << "\nNodes searched  : " << nodes
    //     << "\nNodes/second    : " << 1000 * nodes / elapsed << endl;

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
