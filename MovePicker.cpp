// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "MovePicker.hpp"
#include "Constants.hpp"

#include <vector>
#include <cassert>

MovePicker::MovePicker(Board& board, std::vector<Board::Move>& moveList, Board::Move* killersArray) :
    m_Board(board),
    m_MoveList(moveList),
    m_KillersArray(killersArray),
    m_Phase(Phase::GENERATE_MOVES),
    m_HashMove(),
    m_MoveIndex(0) {
}

void MovePicker::SetHashMove(Board::Move move) {
    m_HashMove = move;
    m_Phase = Phase::HASH_MOVE;
}

bool MovePicker::Next(Board::Move &out_move) {
    switch(m_Phase) {
        case Phase::HASH_MOVE:
            out_move = m_HashMove;
            m_Phase = Phase::GENERATE_MOVES;
            return true;
            break;

        case Phase::GENERATE_MOVES:
            m_Board.FindPseudoLegalMoves(m_MoveList);

            // Boost killers above other quiet moves
            for(auto& move : m_MoveList) {
                for(int i = 0; i < NUM_KILLER_MOVE_SLOTS; ++i) {
                    if(move.SrcSquare == m_KillersArray[i].SrcSquare &&
                       move.DestSquare == m_KillersArray[i].DestSquare &&
                       move.Promotion == m_KillersArray[i].Promotion) {
                        move.Score += NUM_KILLER_MOVE_SLOTS - i;
                    }
                }
            }

            m_Phase = Phase::READ_MOVES;
            // FALL THROUGH
        case Phase::READ_MOVES:
            assert(m_MoveIndex <= (int)m_MoveList.size());

            if(m_MoveIndex == (int)m_MoveList.size()) {
                // No more moves
                return false;
            }

            // Selection sort
            int maxScore = m_MoveList[m_MoveIndex].Score;
            int maxIndex = m_MoveIndex;
            for(int i = m_MoveIndex + 1; i < (int)m_MoveList.size(); ++i) {
                if(m_MoveList[i].Score > maxScore) {
                    maxScore = m_MoveList[i].Score;
                    maxIndex = i;
                }
            }

            out_move = m_MoveList[maxIndex];
            std::swap(m_MoveList[maxIndex], m_MoveList[m_MoveIndex]);
            ++m_MoveIndex;
            return true;
            break;
    }

    // Should not get here
    assert(false);

    return false;
}