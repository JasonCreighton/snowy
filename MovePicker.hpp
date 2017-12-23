// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Board.hpp"

#include <vector>

class MovePicker {
public:
    MovePicker(Board& board, std::vector<Board::Move>& moveList, Board::Move* killersArray, int generateFlags);
    void SetHashMove(Board::Move move);
    bool Next(Board::Move& out_move);

private:
    enum class Phase { HASH_MOVE, GENERATE_MOVES, READ_MOVES, READ_MOVES_WITHOUT_SORT };

    Board& m_Board;
    std::vector<Board::Move>& m_MoveList;
    Board::Move* m_KillersArray; // XXX This is ugly, need a better way to share the "killers"
    int m_GenerateFlags;
    Phase m_Phase;
    Board::Move m_HashMove;
    int m_MoveIndex;
};