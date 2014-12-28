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
#include "evaluate.h"

using std::string;

UCI::OptionsMap Options; // Global object

namespace UCI {

/// 'On change' actions, triggered by an option's value change
void on_clear_hash(const Option&) { TT.clear(); }
void on_spsa(const Option&) { Eval::init_spsa(); Pawns::init_spsa(); }
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
  
  o["maxSafety"]       << Option(0, 0, 600, on_spsa);
  
  o["sw00"]       << Option(0, 0, 300, on_spsa);
  o["sw01"]       << Option(0, 0, 300, on_spsa);
  o["sw02"]       << Option(0, 0, 300, on_spsa);
  o["sw03"]       << Option(0, 0, 300, on_spsa);
  o["sw04"]       << Option(0, 0, 300, on_spsa);
  o["sw05"]       << Option(0, 0, 300, on_spsa);
  o["sw06"]       << Option(0, 0, 300, on_spsa);
  
  o["sw10"]       << Option(0, 0, 300, on_spsa);
  o["sw11"]       << Option(0, 0, 300, on_spsa);
  o["sw12"]       << Option(0, 0, 300, on_spsa);
  o["sw13"]       << Option(0, 0, 300, on_spsa);
  o["sw14"]       << Option(0, 0, 300, on_spsa);
  o["sw15"]       << Option(0, 0, 300, on_spsa);
  o["sw16"]       << Option(0, 0, 300, on_spsa);
  
  o["sw20"]       << Option(0, 0, 300, on_spsa);
  o["sw21"]       << Option(0, 0, 300, on_spsa);
  o["sw22"]       << Option(0, 0, 300, on_spsa);
  o["sw23"]       << Option(0, 0, 300, on_spsa);
  o["sw24"]       << Option(0, 0, 300, on_spsa);
  o["sw25"]       << Option(0, 0, 300, on_spsa);
  o["sw26"]       << Option(0, 0, 300, on_spsa);
  
  o["sw30"]       << Option(0, 0, 300, on_spsa);
  o["sw31"]       << Option(0, 0, 300, on_spsa);
  o["sw32"]       << Option(0, 0, 300, on_spsa);
  o["sw33"]       << Option(0, 0, 300, on_spsa);
  o["sw34"]       << Option(0, 0, 300, on_spsa);
  o["sw35"]       << Option(0, 0, 300, on_spsa);
  o["sw36"]       << Option(0, 0, 300, on_spsa);
 
  o["sd001"]       << Option(0, 0, 300, on_spsa);
  o["sd002"]       << Option(0, 0, 300, on_spsa);
  o["sd003"]       << Option(0, 0, 300, on_spsa);
  o["sd004"]       << Option(0, 0, 300, on_spsa);
  o["sd011"]       << Option(0, 0, 300, on_spsa);
  o["sd012"]       << Option(0, 0, 300, on_spsa);
  o["sd013"]       << Option(0, 0, 300, on_spsa);
  o["sd014"]       << Option(0, 0, 300, on_spsa);
  o["sd021"]       << Option(0, 0, 300, on_spsa);
  o["sd022"]       << Option(0, 0, 300, on_spsa);
  o["sd023"]       << Option(0, 0, 300, on_spsa);
  o["sd024"]       << Option(0, 0, 300, on_spsa);
  o["sd031"]       << Option(0, 0, 300, on_spsa);
  o["sd032"]       << Option(0, 0, 300, on_spsa);
  o["sd033"]       << Option(0, 0, 300, on_spsa);
  o["sd034"]       << Option(0, 0, 300, on_spsa);
  
  o["sd100"]       << Option(0, 0, 300, on_spsa);
  o["sd101"]       << Option(0, 0, 300, on_spsa);
  o["sd102"]       << Option(0, 0, 300, on_spsa);
  o["sd103"]       << Option(0, 0, 300, on_spsa);
  o["sd104"]       << Option(0, 0, 300, on_spsa);
  o["sd110"]       << Option(0, 0, 300, on_spsa);
  o["sd111"]       << Option(0, 0, 300, on_spsa);
  o["sd112"]       << Option(0, 0, 300, on_spsa);
  o["sd113"]       << Option(0, 0, 300, on_spsa);
  o["sd114"]       << Option(0, 0, 300, on_spsa);
  o["sd120"]       << Option(0, 0, 300, on_spsa);
  o["sd121"]       << Option(0, 0, 300, on_spsa);
  o["sd122"]       << Option(0, 0, 300, on_spsa);
  o["sd123"]       << Option(0, 0, 300, on_spsa);
  o["sd124"]       << Option(0, 0, 300, on_spsa);
  o["sd130"]       << Option(0, 0, 300, on_spsa);
  o["sd131"]       << Option(0, 0, 300, on_spsa);
  o["sd132"]       << Option(0, 0, 300, on_spsa);
  o["sd133"]       << Option(0, 0, 300, on_spsa);
  o["sd134"]       << Option(0, 0, 300, on_spsa);
  
  o["sd202"]       << Option(0, 0, 300, on_spsa);
  o["sd203"]       << Option(0, 0, 300, on_spsa);
  o["sd204"]       << Option(0, 0, 300, on_spsa);
  o["sd212"]       << Option(0, 0, 300, on_spsa);
  o["sd213"]       << Option(0, 0, 300, on_spsa);
  o["sd214"]       << Option(0, 0, 300, on_spsa);
  o["sd222"]       << Option(0, 0, 300, on_spsa);
  o["sd223"]       << Option(0, 0, 300, on_spsa);
  o["sd224"]       << Option(0, 0, 300, on_spsa);
  o["sd232"]       << Option(0, 0, 300, on_spsa);
  o["sd233"]       << Option(0, 0, 300, on_spsa);
  o["sd234"]       << Option(0, 0, 300, on_spsa);
  
  o["sd301"]       << Option(0, -600, 0, on_spsa);
  o["sd302"]       << Option(0, -600, 0, on_spsa);
  o["sd303"]       << Option(0, 0, 300, on_spsa);
  o["sd304"]       << Option(0, 0, 300, on_spsa);
  o["sd311"]       << Option(0, 0, 300, on_spsa);
  o["sd312"]       << Option(0, 0, 300, on_spsa);
  o["sd313"]       << Option(0, 0, 300, on_spsa);
  o["sd314"]       << Option(0, 0, 300, on_spsa);
  o["sd321"]       << Option(0, 0, 300, on_spsa);
  o["sd322"]       << Option(0, 0, 300, on_spsa);
  o["sd323"]       << Option(0, 0, 300, on_spsa);
  o["sd324"]       << Option(0, 0, 300, on_spsa);
  o["sd331"]       << Option(0, 0, 300, on_spsa);
  o["sd332"]       << Option(0, 0, 300, on_spsa);
  o["sd333"]       << Option(0, 0, 300, on_spsa);
  o["sd334"]       << Option(0, 0, 300, on_spsa);
  
  o["KingSafetyWeight"] << Option(0, 0, 600, on_spsa);
	
  o["KingAttackWeights2"] << Option(0, 0, 20, on_spsa);
  o["KingAttackWeights3"] << Option(0, 0, 20, on_spsa);
  o["KingAttackWeights4"] << Option(0, 0, 20, on_spsa);
  o["KingAttackWeights5"] << Option(0, 0, 20, on_spsa);

	o["QueenContactCheck"] << Option(0, 0, 50, on_spsa);
    o["RookContactCheck"] << Option(0, 0, 50, on_spsa);
    o["QueenCheck"] << Option(0, 0, 50, on_spsa);
    o["RookCheck"] << Option(0, 0, 50, on_spsa);
    o["BishopCheck"] << Option(0, 0, 50, on_spsa);
    o["KnightCheck"] << Option(0, 0, 50, on_spsa);
	
	o["AUmax"] << Option(0, 0, 50, on_spsa);
    o["AUattackFactor"] << Option(0, 0, 256, on_spsa);
    o["AUzone"] << Option(0, 0, 20, on_spsa);
    o["AUundefended"] << Option(0, 0, 20, on_spsa);
    o["AUpinned"] << Option(0, 0, 20, on_spsa);
    o["AUscoreFactor"] << Option(0, 0, 256, on_spsa);
    o["AUnoQueen"] << Option(0, 0, 50, on_spsa);
	
	o["KDMaxSlope"] << Option(0, 0, 2000, on_spsa);
    o["KDPeak"] << Option(0, 0, 2000, on_spsa);
	o["KDFactor"] << Option(0, 0, 2000, on_spsa);
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
