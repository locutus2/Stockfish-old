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
#include <cstdlib>
#include <sstream>

#include "misc.h"
#include "thread.h"
#include "tt.h"
#include "uci.h"
#include "syzygy/tbprobe.h"
#include "pawns.h"
#include "evaluate.h"

using std::string;

UCI::OptionsMap Options; // Global object

namespace UCI {

/// 'On change' actions, triggered by an option's value change
void on_spsa(const Option&) { Pawns::init_spsa(); Eval::init_spsa(); }
void on_clear_hash(const Option&) { TT.clear(); }
void on_hash_size(const Option& o) { TT.resize(o); }
void on_logger(const Option& o) { start_logger(o); }
void on_threads(const Option&) { Threads.read_uci_options(); }
void on_tb_path(const Option& o) { Tablebases::init(o); }


/// Our case insensitive less() function as required by UCI protocol
bool ci_less(char c1, char c2) { return tolower(c1) < tolower(c2); }

bool CaseInsensitiveLess::operator() (const string& s1, const string& s2) const {
  return std::lexicographical_compare(s1.begin(), s1.end(), s2.begin(), s2.end(), ci_less);
}


/// init() initializes the UCI options to their hard-coded default values

void init(OptionsMap& o) {

  o["Write Debug Log"]       << Option(false, on_logger);
  o["Contempt"]              << Option(0, -100, 100);
  o["Min Split Depth"]       << Option(0, 0, 12, on_threads);
  o["Threads"]               << Option(1, 1, MAX_THREADS, on_threads);
  o["Hash"]                  << Option(16, 1, 1024 * 1024, on_hash_size);
  o["Clear Hash"]            << Option(on_clear_hash);
  o["Ponder"]                << Option(true);
  o["MultiPV"]               << Option(1, 1, 500);
  o["Skill Level"]           << Option(20, 0, 20);
  o["Move Overhead"]         << Option(30, 0, 5000);
  o["Minimum Thinking Time"] << Option(20, 0, 5000);
  o["Slow Mover"]            << Option(80, 10, 1000);
  o["UCI_Chess960"]          << Option(false);
  o["SyzygyPath"]            << Option("<empty>", on_tb_path);
  o["SyzygyProbeDepth"]      << Option(1, 1, 100);
  o["Syzygy50MoveRule"]      << Option(true);
  o["SyzygyProbeLimit"]      << Option(6, 0, 6);
  
  o["PawnWeightMG"]		<< Option(0, 0, 600, on_spsa);
  o["PawnWeightEG"]		<< Option(0, 0, 600, on_spsa);
  
  o["ConnectedSeed1"]		<< Option(0, 0, 600, on_spsa);
  o["ConnectedSeed2"]		<< Option(0, 0, 600, on_spsa);
  o["ConnectedSeed3"]		<< Option(0, 0, 600, on_spsa);
  o["ConnectedSeed4"]		<< Option(0, 0, 600, on_spsa);
  o["ConnectedSeed5"]		<< Option(0, 0, 600, on_spsa);
  o["ConnectedSeed6"]		<< Option(0, 0, 600, on_spsa);
  o["ConnectedSeed7"]		<< Option(0, 0, 600, on_spsa);
  o["ConnectedPhalanx"]		<< Option(0, 0, 256, on_spsa);
  o["ConnectedMG"]		    << Option(0, 0, 256, on_spsa);
  
  o["Doubled0MG"]		<< Option(0, 0, 256, on_spsa);
  o["Doubled0EG"]		<< Option(0, 0, 256, on_spsa);
  o["Doubled1MG"]		<< Option(0, 0, 256, on_spsa);
  o["Doubled1EG"]		<< Option(0, 0, 256, on_spsa);
  o["Doubled2MG"]		<< Option(0, 0, 256, on_spsa);
  o["Doubled2EG"]		<< Option(0, 0, 256, on_spsa);
  o["Doubled3MG"]		<< Option(0, 0, 256, on_spsa);
  o["Doubled3EG"]		<< Option(0, 0, 256, on_spsa);
  
  o["Isolated00MG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated00EG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated01MG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated01EG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated02MG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated02EG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated03MG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated03EG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated10MG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated10EG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated11MG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated11EG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated12MG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated12EG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated13MG"]		<< Option(0, 0, 256, on_spsa);
  o["Isolated13EG"]		<< Option(0, 0, 256, on_spsa);
  
  o["Backward00MG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward00EG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward01MG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward01EG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward02MG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward02EG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward03MG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward03EG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward10MG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward10EG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward11MG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward11EG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward12MG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward12EG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward13MG"]		<< Option(0, 0, 256, on_spsa);
  o["Backward13EG"]		<< Option(0, 0, 256, on_spsa);
  
  o["Lever1MG"]		<< Option(0, 0, 256, on_spsa);
  o["Lever1EG"]		<< Option(0, 0, 256, on_spsa);
  o["Lever2MG"]		<< Option(0, 0, 256, on_spsa);
  o["Lever2EG"]		<< Option(0, 0, 256, on_spsa);
  o["Lever3MG"]		<< Option(0, 0, 256, on_spsa);
  o["Lever3EG"]		<< Option(0, 0, 256, on_spsa);
  o["Lever4MG"]		<< Option(0, 0, 256, on_spsa);
  o["Lever4EG"]		<< Option(0, 0, 256, on_spsa);
  o["Lever5MG"]		<< Option(0, 0, 256, on_spsa);
  o["Lever5EG"]		<< Option(0, 0, 256, on_spsa);
	
  o["UnsupportedMG"]		<< Option(0, 0, 256, on_spsa);
  o["UnsupportedEG"]		<< Option(0, 0, 256, on_spsa);
	
}


/// operator<<() is used to print all the options default values in chronological
/// insertion order (the idx field) and in the format defined by the UCI protocol.

std::ostream& operator<<(std::ostream& os, const OptionsMap& om) {

  for (size_t idx = 0; idx < om.size(); ++idx)
      for (OptionsMap::const_iterator it = om.begin(); it != om.end(); ++it)
          if (it->second.idx == idx)
          {
              const Option& o = it->second;
              os << "\noption name " << it->first << " type " << o.type;

              if (o.type != "button")
                  os << " default " << o.defaultValue;

              if (o.type == "spin")
                  os << " min " << o.min << " max " << o.max;

              break;
          }
  return os;
}


/// Option class constructors and conversion operators

Option::Option(const char* v, OnChange f) : type("string"), min(0), max(0), on_change(f)
{ defaultValue = currentValue = v; }

Option::Option(bool v, OnChange f) : type("check"), min(0), max(0), on_change(f)
{ defaultValue = currentValue = (v ? "true" : "false"); }

Option::Option(OnChange f) : type("button"), min(0), max(0), on_change(f)
{}

Option::Option(int v, int minv, int maxv, OnChange f) : type("spin"), min(minv), max(maxv), on_change(f)
{ std::ostringstream ss; ss << v; defaultValue = currentValue = ss.str(); }


Option::operator int() const {
  assert(type == "check" || type == "spin");
  return (type == "spin" ? atoi(currentValue.c_str()) : currentValue == "true");
}

Option::operator std::string() const {
  assert(type == "string");
  return currentValue;
}


/// operator<<() inits options and assigns idx in the correct printing order

void Option::operator<<(const Option& o) {

  static size_t insert_order = 0;

  *this = o;
  idx = insert_order++;
}


/// operator=() updates currentValue and triggers on_change() action. It's up to
/// the GUI to check for option's limits, but we could receive the new value from
/// the user by console window, so let's check the bounds anyway.

Option& Option::operator=(const string& v) {

  assert(!type.empty());

  if (   (type != "button" && v.empty())
      || (type == "check" && v != "true" && v != "false")
      || (type == "spin" && (atoi(v.c_str()) < min || atoi(v.c_str()) > max)))
      return *this;

  if (type != "button")
      currentValue = v;

  if (on_change)
      on_change(*this);

  return *this;
}

} // namespace UCI
