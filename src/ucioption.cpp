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
#include "evaluate.h"

using std::string;

UCI::OptionsMap Options; // Global object

namespace UCI {

/// 'On change' actions, triggered by an option's value change
void on_logger(const Option& o) { start_logger(o); }
void on_spsa(const Option&) { Eval::init_spsa(); }
void on_threads(const Option&) { Threads.read_uci_options(); }
void on_hash_size(const Option& o) { TT.resize(o); }
void on_clear_hash(const Option&) { TT.clear(); }


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
  
  o["ppm"]       << Option(0, 0, 600, on_spsa);
  o["ppe"]       << Option(0, 0, 600, on_spsa);
  o["npm"]       << Option(0, 0, 600, on_spsa);
  o["npe"]       << Option(0, 0, 600, on_spsa);
  o["bpm"]       << Option(0, 0, 600, on_spsa);
  o["bpe"]       << Option(0, 0, 600, on_spsa);
  o["rpm"]       << Option(0, 0, 600, on_spsa);
  o["rpe"]       << Option(0, 0, 600, on_spsa);
  o["qpm"]       << Option(0, 0, 600, on_spsa);
  o["qpe"]       << Option(0, 0, 600, on_spsa);
  o["kpm"]       << Option(0, 0, 600, on_spsa);
  o["kpe"]       << Option(0, 0, 600, on_spsa);
  o["nnm"]       << Option(0, 0, 600, on_spsa);
  o["nne"]       << Option(0, 0, 600, on_spsa);
  o["bnm"]       << Option(0, 0, 600, on_spsa);
  o["bne"]       << Option(0, 0, 600, on_spsa);
  o["rnm"]       << Option(0, 0, 600, on_spsa);
  o["rne"]       << Option(0, 0, 600, on_spsa);
  o["qnm"]       << Option(0, 0, 600, on_spsa);
  o["qne"]       << Option(0, 0, 600, on_spsa);
  o["knm"]       << Option(0, 0, 600, on_spsa);
  o["kne"]       << Option(0, 0, 600, on_spsa);
  o["bbm"]       << Option(0, 0, 600, on_spsa);
  o["bbe"]       << Option(0, 0, 600, on_spsa);
  o["rbm"]       << Option(0, 0, 600, on_spsa);
  o["rbe"]       << Option(0, 0, 600, on_spsa);
  o["qbm"]       << Option(0, 0, 600, on_spsa);
  o["qbe"]       << Option(0, 0, 600, on_spsa);
  o["kbm"]       << Option(0, 0, 600, on_spsa);
  o["kbe"]       << Option(0, 0, 600, on_spsa);
  o["rrm"]       << Option(0, 0, 600, on_spsa);
  o["rre"]       << Option(0, 0, 600, on_spsa);
  o["qrm"]       << Option(0, 0, 600, on_spsa);
  o["qre"]       << Option(0, 0, 600, on_spsa);
  o["krm"]       << Option(0, 0, 600, on_spsa);
  o["kre"]       << Option(0, 0, 600, on_spsa);
  o["qqm"]       << Option(0, 0, 600, on_spsa);
  o["qqe"]       << Option(0, 0, 600, on_spsa);
  o["kqm"]       << Option(0, 0, 600, on_spsa);
  o["kqe"]       << Option(0, 0, 600, on_spsa);
  
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
