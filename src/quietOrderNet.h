#ifndef QUIET_ORDER_NET_H_INCLUDED
#define QUIET_ORDER_NET_H_INCLUDED

#include "position.h"

namespace Net {

constexpr int INPUT_SIZE = 800;

int calculateQuietOrderValue(float input[], Piece p, Square s);
void calculateQuietOrderNetInput(const Position& pos, float input[]);

}

#endif
