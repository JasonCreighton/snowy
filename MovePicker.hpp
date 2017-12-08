#pragma once

#include "Board.hpp"

#include <vector>

class MovePicker {
public:
    MovePicker(Board& board, std::vector<Board::Move>& moveList, Board::Move* killersArray);
    void SetHashMove(Board::Move move);
    bool Next(Board::Move& out_move);

private:
    enum class Phase { HASH_MOVE, GENERATE_MOVES, READ_MOVES };

    Board& m_Board;
    std::vector<Board::Move>& m_MoveList;
    Board::Move* m_KillersArray; // XXX This is ugly, need a better way to share the "killers"
    Phase m_Phase;
    Board::Move m_HashMove;
    int m_MoveIndex;
};