/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2017 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad

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

#ifdef _WIN32
#if _WIN32_WINNT < 0x0601
#undef  _WIN32_WINNT
#define _WIN32_WINNT 0x0601 // Force to include needed API prototypes
#endif
#include <windows.h>
// The needed Windows API for processor groups could be missed from old Windows
// versions, so instead of calling them directly (forcing the linker to resolve
// the calls at compile time), try to load them at runtime. To do this we need
// first to define the corresponding function pointers.
extern "C" {
typedef bool(*fun1_t)(LOGICAL_PROCESSOR_RELATIONSHIP,
                      PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX, PDWORD);
typedef bool(*fun2_t)(USHORT, PGROUP_AFFINITY);
typedef bool(*fun3_t)(HANDLE, CONST GROUP_AFFINITY*, PGROUP_AFFINITY);
}
#endif

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>
#include <cmath>

#include "misc.h"
#include "thread.h"

using namespace std;

namespace {

const int DBG_N = 256;

/// Version number. If Version is left empty, then compile date in the format
/// DD-MM-YY and show in engine_info.
const string Version = "";

/// Our fancy logging facility. The trick here is to replace cin.rdbuf() and
/// cout.rdbuf() with two Tie objects that tie cin and cout to a file stream. We
/// can toggle the logging of std::cout and std:cin at runtime whilst preserving
/// usual I/O functionality, all without changing a single line of code!
/// Idea from http://groups.google.com/group/comp.lang.c++/msg/1d941c0f26ea0d81

struct Tie: public streambuf { // MSVC requires split streambuf for cin and cout

  Tie(streambuf* b, streambuf* l) : buf(b), logBuf(l) {}

  int sync() { return logBuf->pubsync(), buf->pubsync(); }
  int overflow(int c) { return log(buf->sputc((char)c), "<< "); }
  int underflow() { return buf->sgetc(); }
  int uflow() { return log(buf->sbumpc(), ">> "); }

  streambuf *buf, *logBuf;

  int log(int c, const char* prefix) {

    static int last = '\n'; // Single log file

    if (last == '\n')
        logBuf->sputn(prefix, 3);

    return last = logBuf->sputc((char)c);
  }
};

class Logger {

  Logger() : in(cin.rdbuf(), file.rdbuf()), out(cout.rdbuf(), file.rdbuf()) {}
 ~Logger() { start(""); }

  ofstream file;
  Tie in, out;

public:
  static void start(const std::string& fname) {

    static Logger l;

    if (!fname.empty() && !l.file.is_open())
    {
        l.file.open(fname, ifstream::out);
        cin.rdbuf(&l.in);
        cout.rdbuf(&l.out);
    }
    else if (fname.empty() && l.file.is_open())
    {
        cout.rdbuf(l.out.buf);
        cin.rdbuf(l.in.buf);
        l.file.close();
    }
  }
};

} // namespace

/// engine_info() returns the full name of the current Stockfish version. This
/// will be either "Stockfish <Tag> DD-MM-YY" (where DD-MM-YY is the date when
/// the program was compiled) or "Stockfish <Version>", depending on whether
/// Version is empty.

const string engine_info(bool to_uci) {

  const string months("Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec");
  string month, day, year;
  stringstream ss, date(__DATE__); // From compiler, format is "Sep 21 2008"

  ss << "Stockfish " << Version << setfill('0');

  if (Version.empty())
  {
      date >> month >> day >> year;
      ss << setw(2) << day << setw(2) << (1 + months.find(month) / 4) << year.substr(2);
  }

  ss << (Is64Bit ? " 64" : "")
     << (HasPext ? " BMI2" : (HasPopCnt ? " POPCNT" : ""))
     << (to_uci  ? "\nid author ": " by ")
     << "T. Romstad, M. Costalba, J. Kiiski, G. Linscott";

  return ss.str();
}


/// Debug functions used mainly to collect run-time statistics
static int64_t hits[DBG_N][2], means[DBG_N][2], stds[DBG_N][3], covs[DBG_N][6], corrs[DBG_N][6], cramer[DBG_N][5];

void dbg_hit_on(bool b, int n) { ++hits[n][0]; if (b) ++hits[n][1]; }
void dbg_hit_on(bool c, bool b, int n) { if (c) dbg_hit_on(b, n); }
void dbg_mean_of(int v, int n) { ++means[n][0]; means[n][1] += v; }
void dbg_std_of(int v, int n) { ++stds[n][0]; stds[n][1] += v; stds[n][2] += v * v; }
void dbg_cov_of(int v, int w, int n) { ++covs[n][0]; covs[n][1] += v; covs[n][2] += w; covs[n][3] += v*v; covs[n][4] += w*w; covs[n][5] += v*w;}
void dbg_corr_of(int v, int w, int n) { ++corrs[n][0]; corrs[n][1] += v; corrs[n][2] += w; corrs[n][3] += v*v; corrs[n][4] += w*w; corrs[n][5] += v*w;}
void dbg_cramer_of(bool v, bool w, int n) { ++cramer[n][0]; ++cramer[n][2*v+w+1];};

void dbg_print() {

  for(int n = 0; n < DBG_N; ++n)
    if (hits[n][0])
        cerr << "[" << n << "] Total " << hits[n][0] << " Hits " << hits[n][1]
             << " hit rate (%) " << 100. * hits[n][1] / hits[n][0] << endl;

  for(int n = 0; n < DBG_N; ++n)
    if (means[n][0])
        cerr << "[" << n << "] Total " << means[n][0] << " Mean "
             << (double)means[n][1] / means[n][0] << endl;

  for(int n = 0; n < DBG_N; ++n)
    if (stds[n][0])
        cerr << "[" << n << "] Total " << stds[n][0] << " Std "
             << std::sqrt((double)stds[n][2] / stds[n][0] - std::pow((double)stds[n][1] / stds[n][0], 2)) << endl;

  for(int n = 0; n < DBG_N; ++n)
    if (corrs[n][0])
    {
        double x = corrs[n][1] / (double)corrs[n][0];
        double y = corrs[n][2] / (double)corrs[n][0];
        double x2 = corrs[n][3] - corrs[n][0] * x * x;
        double y2 = corrs[n][4] - corrs[n][0] * y * y;
        double xy = corrs[n][5] - corrs[n][0] * x * y;
        double w = (y2 - xy) / (x2 + y2 - 2 * xy);
        cerr << "[" << n << "] Total " << corrs[n][0] << " Correlation(x,y) = "
             << xy / std::sqrt(x2 * y2)
             << " y = " << xy / x2
             << " * x + " << y - x * xy / x2
             << " x = " << xy / y2
             << " * y + " << x - y * xy / y2
             << " var_min with w(x) = " << w << endl;
    }

  for(int n = 0; n < DBG_N; ++n)
    if (covs[n][0])
    {
        double x = covs[n][1] / (double)covs[n][0];
        double y = covs[n][2] / (double)covs[n][0];
        double xy = covs[n][5] / (double)covs[n][0] - x * y;
        cerr << "[" << n << "] Total " << covs[n][0] << " Cov(x,y) = "
             << xy << endl;
    }
    
  for(int n = 0; n < DBG_N; ++n)
    if (cramer[n][0])
    {
        double a = cramer[n][1];
        double b = cramer[n][2];
        double c = cramer[n][3];
        double d = cramer[n][4];
        double cr = (a*d-b*c)/std::sqrt((a+b)*(c+d)*(a+c)*(b+d));
        double ce = 100.*(cramer[n][2]+cramer[n][3])/(double)cramer[n][0];
        cerr << "[" << n << "] Total " << cramer[n][0] << " CramersV(x,y) = "
             //<< cramer[n][1] << " "
             //<< cramer[n][2] << " "
             //<< cramer[n][3] << " "
             //<< cramer[n][4] << " "
             << cr
             << " error% =" << ce << endl;
    }
}


/// Used to serialize access to std::cout to avoid multiple threads writing at
/// the same time.

std::ostream& operator<<(std::ostream& os, SyncCout sc) {

  static Mutex m;

  if (sc == IO_LOCK)
      m.lock();

  if (sc == IO_UNLOCK)
      m.unlock();

  return os;
}


/// Trampoline helper to avoid moving Logger to misc.h
void start_logger(const std::string& fname) { Logger::start(fname); }


/// prefetch() preloads the given address in L1/L2 cache. This is a non-blocking
/// function that doesn't stall the CPU waiting for data to be loaded from memory,
/// which can be quite slow.
#ifdef NO_PREFETCH

void prefetch(void*) {}

#else

void prefetch(void* addr) {

#  if defined(__INTEL_COMPILER)
   // This hack prevents prefetches from being optimized away by
   // Intel compiler. Both MSVC and gcc seem not be affected by this.
   __asm__ ("");
#  endif

#  if defined(__INTEL_COMPILER) || defined(_MSC_VER)
  _mm_prefetch((char*)addr, _MM_HINT_T0);
#  else
  __builtin_prefetch(addr);
#  endif
}

#endif

void prefetch2(void* addr) {

    prefetch(addr);
    prefetch((uint8_t*)addr + 64);
}

namespace WinProcGroup {

#ifndef _WIN32

void bindThisThread(size_t) {}

#else

/// get_group() retrieves logical processor information using Windows specific
/// API and returns the best group id for the thread with index idx. Original
/// code from Texel by Peter Österlund.

int get_group(size_t idx) {

  int threads = 0;
  int nodes = 0;
  int cores = 0;
  DWORD returnLength = 0;
  DWORD byteOffset = 0;

  // Early exit if the needed API is not available at runtime
  HMODULE k32 = GetModuleHandle("Kernel32.dll");
  auto fun1 = (fun1_t)GetProcAddress(k32, "GetLogicalProcessorInformationEx");
  if (!fun1)
      return -1;

  // First call to get returnLength. We expect it to fail due to null buffer
  if (fun1(RelationAll, nullptr, &returnLength))
      return -1;

  // Once we know returnLength, allocate the buffer
  SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX *buffer, *ptr;
  ptr = buffer = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*)malloc(returnLength);

  // Second call, now we expect to succeed
  if (!fun1(RelationAll, buffer, &returnLength))
  {
      free(buffer);
      return -1;
  }

  while (ptr->Size > 0 && byteOffset + ptr->Size <= returnLength)
  {
      if (ptr->Relationship == RelationNumaNode)
          nodes++;

      else if (ptr->Relationship == RelationProcessorCore)
      {
          cores++;
          threads += (ptr->Processor.Flags == LTP_PC_SMT) ? 2 : 1;
      }

      byteOffset += ptr->Size;
      ptr = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*)(((char*)ptr) + ptr->Size);
  }

  free(buffer);

  std::vector<int> groups;

  // Run as many threads as possible on the same node until core limit is
  // reached, then move on filling the next node.
  for (int n = 0; n < nodes; n++)
      for (int i = 0; i < cores / nodes; i++)
          groups.push_back(n);

  // In case a core has more than one logical processor (we assume 2) and we
  // have still threads to allocate, then spread them evenly across available
  // nodes.
  for (int t = 0; t < threads - cores; t++)
      groups.push_back(t % nodes);

  // If we still have more threads than the total number of logical processors
  // then return -1 and let the OS to decide what to do.
  return idx < groups.size() ? groups[idx] : -1;
}


/// bindThisThread() set the group affinity of the current thread

void bindThisThread(size_t idx) {

  // If OS already scheduled us on a different group than 0 then don't overwrite
  // the choice, eventually we are one of many one-threaded processes running on
  // some Windows NUMA hardware, for instance in fishtest. To make it simple,
  // just check if running threads are below a threshold, in this case all this
  // NUMA machinery is not needed.
  if (Threads.size() < 8)
      return;

  // Use only local variables to be thread-safe
  int group = get_group(idx);

  if (group == -1)
      return;

  // Early exit if the needed API are not available at runtime
  HMODULE k32 = GetModuleHandle("Kernel32.dll");
  auto fun2 = (fun2_t)GetProcAddress(k32, "GetNumaNodeProcessorMaskEx");
  auto fun3 = (fun3_t)GetProcAddress(k32, "SetThreadGroupAffinity");

  if (!fun2 || !fun3)
      return;

  GROUP_AFFINITY affinity;
  if (fun2(group, &affinity))
      fun3(GetCurrentThread(), &affinity, nullptr);
}

#endif

} // namespace WinProcGroup
