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
#include "../thread.h"
#include "../types.h"

#include "evaluate_nnue.h"



namespace Eval::NNUE {

#include "policy_weights.h"
//  int w[64][32], b[64];
  

  // Input feature converter
  LargePagePtr<FeatureTransformer> feature_transformer;

  // Evaluation function
  AlignedPtr<Network> network;

  // Evaluation function file name
  std::string fileName;

  namespace Detail {

  // Initialize the evaluation function parameters
  template <typename T>
  void Initialize(AlignedPtr<T>& pointer) {

    pointer.reset(reinterpret_cast<T*>(std_aligned_alloc(alignof(T), sizeof(T))));
    std::memset(pointer.get(), 0, sizeof(T));
  }

  template <typename T>
  void Initialize(LargePagePtr<T>& pointer) {

    static_assert(alignof(T) <= 4096, "aligned_large_pages_alloc() may fail for such a big alignment requirement of T");
    pointer.reset(reinterpret_cast<T*>(aligned_large_pages_alloc(sizeof(T))));
    std::memset(pointer.get(), 0, sizeof(T));
  }

  // Read evaluation function parameters
  template <typename T>
  bool ReadParameters(std::istream& stream, T& reference) {

    std::uint32_t header;
    header = read_little_endian<std::uint32_t>(stream);
    if (!stream || header != T::GetHashValue()) return false;
    return reference.ReadParameters(stream);
  }

  }  // namespace Detail

  // Initialize the evaluation function parameters
  void Initialize() {

    Detail::Initialize(feature_transformer);
    Detail::Initialize(network);
  }

  // Read network header
  bool ReadHeader(std::istream& stream, std::uint32_t* hash_value, std::string* architecture)
  {
    std::uint32_t version, size;

    version     = read_little_endian<std::uint32_t>(stream);
    *hash_value = read_little_endian<std::uint32_t>(stream);
    size        = read_little_endian<std::uint32_t>(stream);
    if (!stream || version != kVersion) return false;
    architecture->resize(size);
    stream.read(&(*architecture)[0], size);
    return !stream.fail();
  }

  // Read network parameters
  bool ReadParameters(std::istream& stream) {

    std::uint32_t hash_value;
    std::string architecture;
    if (!ReadHeader(stream, &hash_value, &architecture)) return false;
    if (hash_value != kHashValue) return false;
    if (!Detail::ReadParameters(stream, *feature_transformer)) return false;
    if (!Detail::ReadParameters(stream, *network)) return false;
    return stream && stream.peek() == std::ios::traits_type::eof();
  }

  // Evaluation function. Perform differential calculation.
  Value evaluate(const Position& pos) {

    // We manually align the arrays on the stack because with gcc < 9.3
    // overaligning stack variables with alignas() doesn't work correctly.

    constexpr uint64_t alignment = kCacheLineSize;

#if defined(ALIGNAS_ON_STACK_VARIABLES_BROKEN)
    TransformedFeatureType transformed_features_unaligned[
      FeatureTransformer::kBufferSize + alignment / sizeof(TransformedFeatureType)];
    char buffer_unaligned[Network::kBufferSize + alignment];

    auto* transformed_features = align_ptr_up<alignment>(&transformed_features_unaligned[0]);
    auto* buffer = align_ptr_up<alignment>(&buffer_unaligned[0]);
#else
    alignas(alignment)
      TransformedFeatureType transformed_features[FeatureTransformer::kBufferSize];
    alignas(alignment) char buffer[Network::kBufferSize];
#endif

    ASSERT_ALIGNED(transformed_features, alignment);
    ASSERT_ALIGNED(buffer, alignment);

    feature_transformer->Transform(pos, transformed_features);
    const auto output = network->Propagate(transformed_features, buffer);
        
    if(!pos.this_thread()->policy_output)
        pos.this_thread()->policy_output = new int[64];
    
    for (int i = 0; i < 64; ++i)
    {
        dbg_mean_of(std::abs(output[i+1]));
        pos.this_thread()->policy_output[i] = static_cast<int>(output[i+1] / POLICY_SCALE);
    }

    return static_cast<Value>(output[0] / FV_SCALE);
  }
  
  void updatePolicyWeights()
  {
      if (!network) return;

      for(int i = 0; i < 64; ++i)
          network->biases_[i + 1] = (int32_t)b[i];

int sum = 0, count = 0;
   for(int i = 0; i < 65; ++i)
      {
          int offset = i * 32;
          for(int j = 0; j < 32; ++j)
          {
             
            
              //if(i==0) std::cerr << "j=" << j << " " << w[i][j] << " '" << (int)(network->weights_[offset + j]) << "'" << std::endl;
              sum += network->weights_[offset + j];
              ++count;
              
          }
      }
      
      std::cerr << "sum1=" << sum << " count=" << count << std::endl;
      
      sum = count = 0;
      
      std::set<int> fixed_weights;
      for(int i = 0; i < 32; ++i)
      {
         int index = network->getWeightIndex(i);   
         //std::cerr << "fixed weight " << index << std::endl;
         // std::cerr << "=> weight " << (int)network->weights_[i] << " vs " << (int)network->weights_[index] << std::endl;
         fixed_weights.insert(index); 
      }
          
      
      for(int i = 0; i < 64; ++i)
      {
          int offset = 32 + i * 32;
          for(int j = 0; j < 32; ++j)
          {
            int index = network->getWeightIndex(offset + j);
            if(fixed_weights.find(offset + j) == fixed_weights.end())
                network->weights_[offset + j] = (int)w[i][j];
            else    
               std::cerr << "skip weight[" << offset + j << "] " << index << std::endl;
          }
      }
      
      for(int i = 0; i < 65; ++i)
      {
          int offset = i * 32;
          for(int j = 0; j < 32; ++j)
          {
             
            
              //if(i==0) std::cerr << "j=" << j << " " << w[i][j] << " '" << (int)(network->weights_[offset + j]) << "'" << std::endl;
              sum += network->weights_[offset + j];
                ++count;
          }
      }
      
      std::cerr << "sum2=" << sum << " count=" << count << std::endl;
  }

  // Load eval, from a file stream or a memory stream
  bool load_eval(std::string name, std::istream& stream) {

    Initialize();
    fileName = name;
    if(ReadParameters(stream))
    {
        updatePolicyWeights();
        return true;
    }
    else
        return false;
  }

  int evaluate_move(const Position& pos, Move move)
  {
      //return 0;
      Square to = Eval::NNUE::Features::orient(pos.side_to_move(), to_sq(move));
      return pos.this_thread()->policy_output[to];
  }
  
  void init_policy(const Position& pos)
  {
      evaluate(pos);
  }

  TUNE(SetRange(-128, 127), w, updatePolicyWeights);
  TUNE(SetRange(-8192, 8192), b, updatePolicyWeights);
  UPDATE_ON_LAST();

} // namespace Eval::NNUE
