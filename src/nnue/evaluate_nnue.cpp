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

// Code for calculating NNUE evaluation function

#include <iostream>
#include <set>

#include "../evaluate.h"
#include "../position.h"
#include "../misc.h"
#include "../uci.h"
#include "../types.h"

#include "evaluate_nnue.h"

namespace Stockfish::Eval::NNUE {

  // Input feature converter
  LargePagePtr<FeatureTransformer> featureTransformer;

  // Evaluation function
  AlignedPtr<Network> network[LayerStacks];

  // Evaluation function file name
  std::string fileName;
  std::string netDescription;

  namespace Detail {

  // Initialize the evaluation function parameters
  template <typename T>
  void initialize(AlignedPtr<T>& pointer) {

    pointer.reset(reinterpret_cast<T*>(std_aligned_alloc(alignof(T), sizeof(T))));
    std::memset(pointer.get(), 0, sizeof(T));
  }

  template <typename T>
  void initialize(LargePagePtr<T>& pointer) {

    static_assert(alignof(T) <= 4096, "aligned_large_pages_alloc() may fail for such a big alignment requirement of T");
    pointer.reset(reinterpret_cast<T*>(aligned_large_pages_alloc(sizeof(T))));
    std::memset(pointer.get(), 0, sizeof(T));
  }

  // Read evaluation function parameters
  template <typename T>
  bool read_parameters(std::istream& stream, T& reference) {

    std::uint32_t header;
    header = read_little_endian<std::uint32_t>(stream);
    if (!stream || header != T::get_hash_value()) return false;
    return reference.read_parameters(stream);
  }

  // Write evaluation function parameters
  template <typename T>
  bool write_parameters(std::ostream& stream, const T& reference) {

    write_little_endian<std::uint32_t>(stream, T::get_hash_value());
    return reference.write_parameters(stream);
  }

  }  // namespace Detail

  // Initialize the evaluation function parameters
  void initialize() {

    Detail::initialize(featureTransformer);
    for (std::size_t i = 0; i < LayerStacks; ++i)
      Detail::initialize(network[i]);
  }

  // Read network header
  bool read_header(std::istream& stream, std::uint32_t* hashValue, std::string* desc)
  {
    std::uint32_t version, size;

    version     = read_little_endian<std::uint32_t>(stream);
    *hashValue  = read_little_endian<std::uint32_t>(stream);
    size        = read_little_endian<std::uint32_t>(stream);
    if (!stream || version != Version) return false;
    desc->resize(size);
    stream.read(&(*desc)[0], size);
    return !stream.fail();
  }

  // Write network header
  bool write_header(std::ostream& stream, std::uint32_t hashValue, const std::string& desc)
  {
    write_little_endian<std::uint32_t>(stream, Version);
    write_little_endian<std::uint32_t>(stream, hashValue);
    write_little_endian<std::uint32_t>(stream, desc.size());
    stream.write(&desc[0], desc.size());
    return !stream.fail();
  }

  // Read network parameters
  bool read_parameters(std::istream& stream) {

    std::uint32_t hashValue;
    if (!read_header(stream, &hashValue, &netDescription)) return false;
    if (hashValue != HashValue) return false;
    if (!Detail::read_parameters(stream, *featureTransformer)) return false;
    for (std::size_t i = 0; i < LayerStacks; ++i)
      if (!Detail::read_parameters(stream, *(network[i]))) return false;
    return stream && stream.peek() == std::ios::traits_type::eof();
  }

  // Write network parameters
  bool write_parameters(std::ostream& stream) {

    if (!write_header(stream, HashValue, netDescription)) return false;
    if (!Detail::write_parameters(stream, *featureTransformer)) return false;
    for (std::size_t i = 0; i < LayerStacks; ++i)
      if (!Detail::write_parameters(stream, *(network[i]))) return false;
    return (bool)stream;
  }

  // Evaluation function. Perform differential calculation.
  Value evaluate(const Position& pos) {

    // We manually align the arrays on the stack because with gcc < 9.3
    // overaligning stack variables with alignas() doesn't work correctly.

    constexpr uint64_t alignment = CacheLineSize;

#if defined(ALIGNAS_ON_STACK_VARIABLES_BROKEN)
    TransformedFeatureType transformedFeaturesUnaligned[
      FeatureTransformer::BufferSize + alignment / sizeof(TransformedFeatureType)];
    char bufferUnaligned[Network::BufferSize + alignment];

    auto* transformedFeatures = align_ptr_up<alignment>(&transformedFeaturesUnaligned[0]);
    auto* buffer = align_ptr_up<alignment>(&bufferUnaligned[0]);
#else
    alignas(alignment)
      TransformedFeatureType transformedFeatures[FeatureTransformer::BufferSize];
    alignas(alignment) char buffer[Network::BufferSize];
#endif

    ASSERT_ALIGNED(transformedFeatures, alignment);
    ASSERT_ALIGNED(buffer, alignment);

    const std::size_t bucket = (pos.count<ALL_PIECES>() - 1) / 4;

    const auto [psqt, lazy] = featureTransformer->transform(pos, transformedFeatures, bucket);
    dbg_hit_on(true, lazy, bucket);
    if (lazy) {
      dbg_mean_of(int(psqt), 1000+bucket);
      return static_cast<Value>(psqt / OutputScale);
    } else {
      const auto output = network[bucket]->propagate(transformedFeatures, buffer);
      dbg_mean_of(output[0]-psqt, bucket);
      dbg_mean_of(int(output[0])-int(psqt), 100+bucket);
      dbg_mean_of(int(output[0]), 200+bucket);
      dbg_mean_of(int(psqt), 300+bucket);
      dbg_mean_of(int(output[0])+int(psqt), 400+bucket);
      dbg_corr_of(bucket, output[0]-psqt, 0);
      dbg_corr_of(bucket, int(output[0])-int(psqt), 100);
      return static_cast<Value>((output[0] + psqt) / OutputScale);
    }
    /*
     * [0] Total 145227 Hits 9 hit rate (%) 0.00619719
     * [1] Total 1933877 Hits 21 hit rate (%) 0.0010859
     * [2] Total 1899974 Hits 2 hit rate (%) 0.000105265
     * [3] Total 1412299 Hits 5 hit rate (%) 0.000354033
     * [4] Total 1750074 Hits 10 hit rate (%) 0.000571404
     * [5] Total 2245138 Hits 47 hit rate (%) 0.00209341
     * [6] Total 2023430 Hits 4 hit rate (%) 0.000197684
     * [7] Total 390815 Hits 0 hit rate (%) 0
     * [0] Total 145218 Mean 403.87
     * [1] Total 1933856 Mean 915.296
     * [2] Total 1899972 Mean 1123.83
     * [3] Total 1412294 Mean 1555.72
     * [4] Total 1750064 Mean 1667.15
     * [5] Total 2245091 Mean 1523.31
     * [6] Total 2023426 Mean 1652.65
     * [7] Total 390815 Mean 2006.28
     * [100] Total 145218 Mean 403.87
     * [101] Total 1933856 Mean 915.296
     * [102] Total 1899972 Mean 1123.83
     * [103] Total 1412294 Mean 1555.72
     * [104] Total 1750064 Mean 1667.15
     * [105] Total 2245091 Mean 1523.31
     * [106] Total 2023426 Mean 1652.65
     * [107] Total 390815 Mean 2006.28
     * [200] Total 145218 Mean 416.422
     * [201] Total 1933856 Mean 887.266
     * [202] Total 1899972 Mean 1018.05
     * [203] Total 1412294 Mean 1244.77
     * [204] Total 1750064 Mean 1192.17
     * [205] Total 2245091 Mean 935.174
     * [206] Total 2023426 Mean 1043.99
     * [207] Total 390815 Mean 1237.35
     * [300] Total 145218 Mean 12.5513
     * [301] Total 1933856 Mean -28.0307
     * [302] Total 1899972 Mean -105.776
     * [303] Total 1412294 Mean -310.944
     * [304] Total 1750064 Mean -474.973
     * [305] Total 2245091 Mean -588.134
     * [306] Total 2023426 Mean -608.658
     * [307] Total 390815 Mean -768.929
     * [400] Total 145218 Mean 428.973
     * [401] Total 1933856 Mean 859.235
     * [402] Total 1899972 Mean 912.275
     * [403] Total 1412294 Mean 933.83
     * [404] Total 1750064 Mean 717.199
     * [405] Total 2245091 Mean 347.041
     * [406] Total 2023426 Mean 435.332
     * [407] Total 390815 Mean 468.421
     * [1000] Total 9 Mean -51038.7
     * [1001] Total 21 Mean -35587.4
     * [1002] Total 2 Mean -22654.5
     * [1003] Total 5 Mean -23436.8
     * [1004] Total 10 Mean -14271.7
     * [1005] Total 47 Mean -16390.5
     * [1006] Total 4 Mean 22985.8
     * [0] Total 11800736 Correlation(x,y) = 0.0397905 y = 152.3 * x + 853.063 x = 1.03958e-05 * y + 3.63535 var_min with w(x) = 1.00001
     * [100] Total 11800736 Correlation(x,y) = 0.0397905 y = 152.3 * x + 853.063 x = 1.03958e-05 * y + 3.63535 var_min with w(x) = 1.00001
     *
     * [0] Total 145218 Mean 403.87
     * [1] Total 1933856 Mean 915.296
     * [2] Total 1899972 Mean 1123.83
     * [3] Total 1412294 Mean 1555.72
     * [4] Total 1750064 Mean 1667.15
     * [5] Total 2245091 Mean 1523.31
     * [6] Total 2023426 Mean 1652.65
     * [7] Total 390815 Mean 2006.28
     * [100] Total 145218 Mean 403.87
     * [101] Total 1933856 Mean 915.296
     * [102] Total 1899972 Mean 1123.83
     * [103] Total 1412294 Mean 1555.72
     * [104] Total 1750064 Mean 1667.15
     * [105] Total 2245091 Mean 1523.31
     * [106] Total 2023426 Mean 1652.65
     * [107] Total 390815 Mean 2006.28
     * [200] Total 145218 Mean 416.422
     * [201] Total 1933856 Mean 887.266
     * [202] Total 1899972 Mean 1018.05
     * [203] Total 1412294 Mean 1244.77
     * [204] Total 1750064 Mean 1192.17
     * [205] Total 2245091 Mean 935.174
     * [206] Total 2023426 Mean 1043.99
     * [207] Total 390815 Mean 1237.35
     * [300] Total 145218 Mean 12.5513
     * [301] Total 1933856 Mean -28.0307
     * [302] Total 1899972 Mean -105.776
     * [303] Total 1412294 Mean -310.944
     * [304] Total 1750064 Mean -474.973
     * [305] Total 2245091 Mean -588.134
     * [306] Total 2023426 Mean -608.658
     * [307] Total 390815 Mean -768.929
     * [400] Total 145218 Mean 428.973
     * [401] Total 1933856 Mean 859.235
     * [402] Total 1899972 Mean 912.275
     * [403] Total 1412294 Mean 933.83
     * [404] Total 1750064 Mean 717.199
     * [405] Total 2245091 Mean 347.041
     * [406] Total 2023426 Mean 435.332
     * [407] Total 390815 Mean 468.421
     * [0] Total 11800736 Correlation(x,y) = 0.0397905 y = 152.3 * x + 853.063 x = 1.03958e-05 * y + 3.63535 var_min with w(x) = 1.00001
     * [100] Total 11800736 Correlation(x,y) = 0.0397905 y = 152.3 * x + 853.063 x = 1.03958e-05 * y + 3.63535 var_min with w(x) = 1.00001
     * */
  }

  // Load eval, from a file stream or a memory stream
  bool load_eval(std::string name, std::istream& stream) {

    initialize();
    fileName = name;
    return read_parameters(stream);
  }

  // Save eval, to a file stream or a memory stream
  bool save_eval(std::ostream& stream) {

    if (fileName.empty())
      return false;

    return write_parameters(stream);
  }

} // namespace Stockfish::Eval::NNUE
