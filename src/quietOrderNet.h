#ifndef QUIET_ORDER_NET_H_INCLUDED
#define QUIET_ORDER_NET_H_INCLUDED

#include "position.h"

namespace Net {

constexpr int INPUT_SIZE = 800;

typedef float Input[INPUT_SIZE];

int calculateQuietOrderValue(Input input, Piece p, Square s);
void calculateQuietOrderNetInput(const Position& pos, Input input);

}

#endif
