// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "Zobrist.hpp"

#include <random>

namespace Zobrist {
    hash_t WhiteToMove;
    hash_t Piece[2][6][64];
    hash_t CastlingRights[16];
    hash_t EnPassantFile[8];

    void Init() {
        std::mt19937_64 prng(123456789); // Fixed seed for repeatability
        std::uniform_int_distribution<hash_t> dist;

        WhiteToMove = dist(prng);

        for(int color = 0; color < 2; ++color) { // NOLINT
            for(int piece = 0; piece < 6; ++piece) { // NOLINT
                for(int square = 0; square < 64; ++square) { // NOLINT
                    Piece[color][piece][square] = dist(prng);
                }
            }
        }

        for(int i = 0; i < 16; ++i) { // NOLINT
            CastlingRights[i] = dist(prng);
        }

        for(int i = 0; i < 8; ++i) { // NOLINT
            EnPassantFile[i] = dist(prng);
        }
    }
}