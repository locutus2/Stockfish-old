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

// NNUE evaluation function layer ReuseLayer definition

#ifndef NNUE_LAYERS_REUSE_LAYER_H_INCLUDED
#define NNUE_LAYERS_REUSE_LAYER_H_INCLUDED

#include "../nnue_common.h"

namespace Eval::NNUE::Layers {

// Input layer
template <typename PreviousLayer>
class ReuseLayer {
 public:

  // Output type
  using OutputType = typename PreviousLayer::OutputType;

  // Output dimensionality
  static constexpr IndexType kOutputDimensions = PreviousLayer::kOutputDimensions;

  // Size of forward propagation buffer used from the reused layer to this layer
  static constexpr std::size_t kBufferSize = PreviousLayer::kBufferSize;

  // Hash value embedded in the evaluation file
  static constexpr std::uint32_t GetHashValue() {
    std::uint32_t hash_value = 0xB9C139A8u;
    hash_value += kOutputDimensions;
    hash_value ^= PreviousLayer::GetHashValue() >> 1;
    hash_value ^= PreviousLayer::GetHashValue() << 31;
    return hash_value;
  }

  // Read network parameters
  bool ReadParameters(std::istream& /*stream*/) {
    return true;
  }

  // Forward propagation
  const OutputType* Propagate(
      const TransformedFeatureType* transformed_features, char* buffer) const {

    const auto output = previous_layer_->Propagate(transformed_features, buffer);
    return output;
  }

  void setLayer(PreviousLayer* layer) {
    previous_layer_ = layer;
  }

 private:
  PreviousLayer* previous_layer_;
};

}  // namespace Layers

#endif // #ifndef NNUE_LAYERS_REUSE_LAYER_H_INCLUDED
