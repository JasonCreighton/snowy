// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Board.hpp"
#include "TranspositionTable.hpp"
#include "MovePicker.hpp"
#include "Constants.hpp"

#include <vector>
#include <mutex>
#include <atomic>
#include <chrono>
#include <thread>

class Search {
public:
    static void Test();
    
    explicit Search(Board &board);

    void StartSearch(int depth, bool bruteForce, bool showHistograms, int moveTime_ms);
    void StopSearch();
    void WaitForSearch();
    int Quiesce();
    long Perft(int depth);

private:
    struct Ply {
        std::vector<Board::Move> MoveList;
        Board::Move KillerMoves[NUM_KILLER_MOVE_SLOTS];
    };

    void RunSearch(int maxDepth, bool bruteForce, bool showHistograms);
    void CheckTimeLimit();
    std::vector<Board::Move> PV(int depth);
    int MainSearch(int alpha, int beta, int plyIndex, int depth);
    void UpdateKillers(int plyIndex, Board::Move move);
    void BoostKillersInMoveOrdering(int plyIndex);
    int Quiesce(int alpha, int beta, int plyIndex);
    int BruteForceSearch(int plyIndex, int depth);
    long Perft(int depth, int plyIndex);

    std::mutex m_Lock;
    std::atomic<bool> m_StopRequested;
    std::thread m_SearchThread;

    Board &m_Board;

    std::chrono::time_point<std::chrono::high_resolution_clock> m_SearchStartTime;
    int m_SearchTimeLimit_ms;

    std::vector<long> m_BetaCutoffHistogram;
    std::vector<long> m_BestMoveHistogram;
    std::vector<long> m_MoveScoreHistogram;
    int m_HashTableScoreHits;
    int m_HashTableMoveHits;
    int m_NumMainNodes;
    int m_NumQuiesceNodes;

    std::vector<Ply> m_Plies;
    TranspositionTable m_TT;
};