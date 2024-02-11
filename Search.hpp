// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Common.hpp"
#include "Board.hpp"
#include "TranspositionTable.hpp"
#include "MovePicker.hpp"
#include "Constants.hpp"

class Search {
public:
    struct Parameters {
        int Depth;
        bool BruteForce;
        bool ShowHistograms;
        int SoftMoveTime_ms;
        int HardMoveTime_ms;
    };

    static void Test();
    
    explicit Search(Board &board);
    ~Search();

    void NewGame();
    void StartSearch(const Parameters& params);
    void StopSearch();
    void WaitForSearch();
    int Quiesce();
    std::int64_t Perft(int depth);
    void SetHashTableSize(int bytesLog2);

private:
    static void TestCheckmateScore();

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
    int m_EffectiveHardMoveTime_ms;

    Board &m_Board;

    std::chrono::time_point<std::chrono::high_resolution_clock> m_SearchStartTime;
    int m_TimeLimitCounter;

    std::vector<long> m_BetaCutoffHistogram;
    std::vector<long> m_BestMoveHistogram;
    std::int64_t m_HashTableScoreHits;
    std::int64_t m_HashTableMoveHits;
    std::int64_t m_NumMainNodes;
    std::int64_t m_NumQuiesceNodes;
    std::int64_t m_NumStaticEvaluations;

    std::vector<Ply> m_Plies;
    TranspositionTable m_TT;
};