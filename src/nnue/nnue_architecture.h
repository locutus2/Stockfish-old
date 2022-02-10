/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2022 The Stockfish developers (see AUTHORS file)

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

// Input features and network structure used in NNUE evaluation function

#ifndef NNUE_ARCHITECTURE_H_INCLUDED
#define NNUE_ARCHITECTURE_H_INCLUDED

#include "nnue_common.h"

#include "features/half_ka_v2_hm.h"

#include "layers/affine_transform.h"
#include "layers/clipped_relu.h"

#include "../misc.h"

namespace Stockfish::Eval::NNUE {

// Input features used in evaluation function
using FeatureSet = Features::HalfKAv2_hm;

// Number of input feature dimensions after conversion
constexpr IndexType TransformedFeatureDimensions = 1024;
constexpr IndexType PSQTBuckets = 8;
constexpr IndexType LayerStacks = 8;

struct Network
{
  static constexpr int FC_0_OUTPUTS = 15;
  static constexpr int FC_1_OUTPUTS = 32;

  Layers::AffineTransform<TransformedFeatureDimensions, FC_0_OUTPUTS + 1> fc_0;
  Layers::ClippedReLU<FC_0_OUTPUTS> ac_0;
  Layers::AffineTransform<FC_0_OUTPUTS, FC_1_OUTPUTS> fc_1;
  Layers::ClippedReLU<FC_1_OUTPUTS> ac_1;
  Layers::AffineTransform<FC_1_OUTPUTS, 1> fc_2;

  // Hash value embedded in the evaluation file
  static constexpr std::uint32_t get_hash_value() {
    // input slice hash
    std::uint32_t hash_value = 0xEC42E90Du;
    hash_value ^= TransformedFeatureDimensions * 2;

    hash_value = decltype(fc_0)::get_hash_value(hash_value);
    hash_value = decltype(ac_0)::get_hash_value(hash_value);
    hash_value = decltype(fc_1)::get_hash_value(hash_value);
    hash_value = decltype(ac_1)::get_hash_value(hash_value);
    hash_value = decltype(fc_2)::get_hash_value(hash_value);

    return hash_value;
  }

  // Read network parameters
  bool read_parameters(std::istream& stream) {
    if (!fc_0.read_parameters(stream)) return false;
    if (!ac_0.read_parameters(stream)) return false;
    if (!fc_1.read_parameters(stream)) return false;
    if (!ac_1.read_parameters(stream)) return false;
    if (!fc_2.read_parameters(stream)) return false;
    return true;
  }

  // Read network parameters
  bool write_parameters(std::ostream& stream) const {
    if (!fc_0.write_parameters(stream)) return false;
    if (!ac_0.write_parameters(stream)) return false;
    if (!fc_1.write_parameters(stream)) return false;
    if (!ac_1.write_parameters(stream)) return false;
    if (!fc_2.write_parameters(stream)) return false;
    return true;
  }

  std::int32_t propagate(const TransformedFeatureType* transformed_features)
  {
    constexpr uint64_t alignment = CacheLineSize;

    struct Buffer
    {
      alignas(CacheLineSize) decltype(fc_0)::OutputBuffer fc_0_out;
      alignas(CacheLineSize) decltype(ac_0)::OutputBuffer ac_0_out;
      alignas(CacheLineSize) decltype(fc_1)::OutputBuffer fc_1_out;
      alignas(CacheLineSize) decltype(ac_1)::OutputBuffer ac_1_out;
      alignas(CacheLineSize) decltype(fc_2)::OutputBuffer fc_2_out;
    };

#if defined(ALIGNAS_ON_STACK_VARIABLES_BROKEN)
    char buffer_raw[sizeof(Buffer) + alignment];
    char* buffer_raw_aligned = align_ptr_up<alignment>(&buffer_raw[0]);
    Buffer& buffer = *(new (buffer_raw_aligned) Buffer);
#else
    alignas(alignment) Buffer buffer;
#endif

    fc_0.propagate(transformed_features, buffer.fc_0_out);
    ac_0.propagate(buffer.fc_0_out, buffer.ac_0_out);
    fc_1.propagate(buffer.ac_0_out, buffer.fc_1_out);
    ac_1.propagate(buffer.fc_1_out, buffer.ac_1_out);
    fc_2.propagate(buffer.ac_1_out, buffer.fc_2_out);

    // buffer.fc_0_out[FC_0_OUTPUTS] is such that 1.0 is equal to 127*(1<<WeightScaleBits) in quantized form
    // but we want 1.0 to be equal to 600*OutputScale
    std::int32_t fwd_out = int(buffer.fc_0_out[FC_0_OUTPUTS]) * (600*OutputScale) / (127*(1<<WeightScaleBits));
    std::int32_t output_value = buffer.fc_2_out[0] + fwd_out;

#if defined(ALIGNAS_ON_STACK_VARIABLES_BROKEN)
    buffer.~Buffer();
#endif

    return output_value;
  }
};

}  // namespace Stockfish::Eval::NNUE

#endif // #ifndef NNUE_ARCHITECTURE_H_INCLUDED
