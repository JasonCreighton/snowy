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
#include <condition_variable>

class Search {
public:
    struct Parameters {
        int Depth;
        bool BruteForce;
        bool ShowHistograms;
        int MoveTime_ms;
    };

    static void Test();
    
    explicit Search(Board &board);
    ~Search();

    void StartSearch(const Parameters& params);
    void StopSearch();
    void WaitForSearch();
    int Quiesce();
    std::int64_t Perft(int depth);
    void SetHashTableSize(int bytesLog2);

private:
    struct Ply {
        std::vector<Board::Move> MoveList;
        Board::Move KillerMoves[NUM_KILLER_MOVE_SLOTS];
    };

    void WorkerThreadMain();

    void RunSearch();
    void CheckTimeLimit();
    std::vector<Board::Move> PV(int depth);
    int MainSearch(int alpha, int beta, int plyIndex, int depth);
    void UpdateKillers(int plyIndex, Board::Move move);
    void BoostKillersInMoveOrdering(int plyIndex);
    int Quiesce(int alpha, int beta, int plyIndex);
    int BruteForceSearch(int plyIndex, int depth);
    std::int64_t Perft(int depth, int plyIndex);

    std::mutex m_Mutex;

    // Condition variable and flags to pass information to and from the worker
    // thread
    std::condition_variable m_Cond;
    bool m_Cond_SearchPending;
    bool m_Cond_WorkerThreadShutDownRequested;

    std::atomic<bool> m_StopSearchRequested;
    std::thread m_WorkerThread;

    Parameters m_SearchParameters;

    Board &m_Board;

    std::chrono::time_point<std::chrono::high_resolution_clock> m_SearchStartTime;

    std::vector<long> m_BetaCutoffHistogram;
    std::vector<long> m_BestMoveHistogram;
    std::vector<long> m_MoveScoreHistogram;
    std::int64_t m_HashTableScoreHits;
    std::int64_t m_HashTableMoveHits;
    std::int64_t m_NumMainNodes;
    std::int64_t m_NumQuiesceNodes;

    std::vector<Ply> m_Plies;
    TranspositionTable m_TT;
};