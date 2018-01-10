// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "MovePicker.hpp"
#include "Constants.hpp"
#include "IO.hpp"

#include <vector>
#include <cassert>

template<int GenFlags>
MovePicker<GenFlags>::MovePicker(Board& board, std::vector<Board::Move>& moveList, Board::Move* killersArray) :
    m_Board(board),
    m_MoveList(moveList),
    m_KillersArray(killersArray),
    m_Phase(Phase::GENERATE_MOVES),
    m_HashMove(),
    m_MoveIndex(0) {
}

template<int GenFlags>
void MovePicker<GenFlags>::SetHashMove(Board::Move move) {
    if(m_Board.IsPseudoLegal(move)) {
        m_HashMove = move;
        m_Phase = Phase::HASH_MOVE;
    } else {
        IO::PutInfo("WARNING: MovePicker::SetHashMove() got move that was not pseudo-legal!");
    }
}

template<int GenFlags>
bool MovePicker<GenFlags>::Next(Board::Move &out_move) {
    switch(m_Phase) {
        case Phase::HASH_MOVE:
            out_move = m_HashMove;
            m_Phase = Phase::GENERATE_MOVES;
            return true;
            break;

        case Phase::GENERATE_MOVES:
            m_Board.FindPseudoLegalMoves<GenFlags>(m_MoveList);

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
            {
                int maxScore = 0;
                int maxIndex = -1;
                for(int i = m_MoveIndex; i < (int)m_MoveList.size(); ++i) {
                    if(m_MoveList[i].Score > maxScore) {
                        maxScore = m_MoveList[i].Score;
                        maxIndex = i;
                    }
                }

                if(maxIndex == -1) {
                    // We didn't find any moves with non-zero score, we don't have to
                    // keep trying to sort next time.
                    m_Phase = Phase::READ_MOVES_WITHOUT_SORT;

                    // Just pick an arbitrary move to return
                    maxIndex = m_MoveIndex;
                }

                out_move = m_MoveList[maxIndex];
                std::swap(m_MoveList[maxIndex], m_MoveList[m_MoveIndex]);
                ++m_MoveIndex;
            }
            return true;            
            break;
        case Phase::READ_MOVES_WITHOUT_SORT:
            assert(m_MoveIndex <= (int)m_MoveList.size());

            if(m_MoveIndex == (int)m_MoveList.size()) {
                // No more moves
                return false;
            }

            out_move = m_MoveList[m_MoveIndex++];
            return true;
            break;
    }

    // Should not get here
    assert(false);

    return false;
}


// Template instantiations
template class MovePicker<Board::GEN_ALL>;
template class MovePicker<Board::GEN_CAPTURES | Board::GEN_PROMOTIONS>;