// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Common.hpp"
#include "Zobrist.hpp"
#include "Board.hpp"

class TranspositionTable {
public:
    enum class ScoreBound : std::uint8_t { EXACT, UPPER_BOUND, LOWER_BOUND };

    explicit TranspositionTable(int bytesLog2);
    void Resize(int bytesLog2);
    void Insert(Zobrist::hash_t hash, int score, ScoreBound bound, int depth, int ply, const Board::Move* move);

    static const int PROBE_FOUND_SCORE = 0x1;
    static const int PROBE_FOUND_MOVE = 0x2;
    int Probe(Zobrist::hash_t hash, int alpha, int beta, int depth, int ply, int& out_score, Board::Move& out_move);

    void AdvanceTime();

private:
    static const int ENTRIES_PER_BUCKET = 4;

    static int ScoreRelativeToNode(int scoreRelativeToRoot, int ply);
    static int ScoreRelativeToRoot(int scoreRelativeToNode, int ply);

    struct Payload {
        std::int16_t Score;
        std::int8_t Depth;
        std::uint8_t Timestamp;
        Board::square_t SrcSquare;
        Board::square_t DestSquare;
        Board::piece_t Promotion;
        // bitfield stuff, otherwise we don't have room
        // Note: gcc always warns on using an enum as a bitfield:
        // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=61414
        ScoreBound Bound : 2;
        bool HasMove : 1;
    };

    static_assert(sizeof(Payload) == 8, "Hash table payload is expected to be 8 bytes");

    struct Entry {
        Zobrist::hash_t Key;
        Payload Value;
    };

    static_assert(sizeof(Entry) == 16, "Hash table entry is expected to be 16 bytes");

    struct Bucket {
        Entry entries[ENTRIES_PER_BUCKET];
    };

    static_assert(sizeof(Bucket) == (sizeof(Entry) * ENTRIES_PER_BUCKET), "Bucket should introduce no additional padding");

    bool Lookup(Zobrist::hash_t hash, Payload &out_payload);
    Entry& FindEntryToReplace(Zobrist::hash_t hash);

    std::uint8_t m_Now;
    Zobrist::hash_t m_HashMask;
    std::vector<Bucket> m_Table;
};