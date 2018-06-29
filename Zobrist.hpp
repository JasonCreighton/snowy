// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Common.hpp"

namespace Zobrist {
    typedef std::uint64_t hash_t;

    // Populate tables
    void Init();

    extern hash_t SideToMove[2];
    extern hash_t Piece[2][6][64];
    extern hash_t CastlingRights[16];
    extern hash_t EnPassantFile[8];
}
