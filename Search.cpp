// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "Common.hpp"
#include "Search.hpp"
#include "Board.hpp"
#include "IO.hpp"
#include "Constants.hpp"

Search::Search(Board &board) :
    m_Cond_SearchPending(false),
    m_Cond_WorkerThreadShutDownRequested(false),
    m_StopSearchRequested(false),
    m_EffectiveHardMoveTime_ms(-1),
    m_Board(board),
    m_TimeLimitCounter(0),
    m_BetaCutoffHistogram(250),
    m_BestMoveHistogram(250),
    m_HashTableScoreHits(0),
    m_HashTableMoveHits(0),
    m_NumMainNodes(0),
    m_NumQuiesceNodes(0),
    m_Plies(MAX_PLY),
    m_TT(10) // it's expected that the hash table will get resized right away
{
    // Spawn worker thread
    m_WorkerThread = std::thread(&Search::WorkerThreadMain, this);
}

Search::~Search() {
    // Need to set this in case the worker thread is holding the mutex and
    // RunSearch() is running.
    m_StopSearchRequested = true;

    {
        // Need to grab the mutex here in part to protect the access to 
        // m_Cond_WorkerThreadShutDownRequested, but that could easily be made
        // atomic. The more fundamental reason is to ensure that the worker
        // thread is waiting on the condition variable when we call notify_one()
        // below. Without holding the mutex, the worker thread could end up
        // missing the notify and then waiting forever on the condition
        // variable.
        std::lock_guard<std::mutex> lock(m_Mutex);

        // Request thread shut down
        m_Cond_WorkerThreadShutDownRequested = true;
        m_Cond.notify_all();

        // Release lock by ending scope, so that we don't deadlock with the
        // worker thread when we join() below
    }

    // m_WorkerThread should be joinable, since we only start it in the
    // constructor, and we only join it here.
    m_WorkerThread.join();
}

void Search::WorkerThreadMain() {
    std::unique_lock<std::mutex> lock(m_Mutex);

    for(;;) {
        // Wait for a command we can handle
        while(!m_Cond_WorkerThreadShutDownRequested && !m_Cond_SearchPending) {
            m_Cond.wait(lock);
        }

        if(m_Cond_WorkerThreadShutDownRequested) {
            return;
        }

        if(m_Cond_SearchPending) {
            RunSearch();

            // The search may have terminated due to user request or timeout,
            // we need to clear the variable for next time
            m_StopSearchRequested = false;

            m_Cond_SearchPending = false;
            m_Cond.notify_all(); // WaitForSearch() might be waiting for a notify
        }
    }
}

void Search::StartSearch(const Parameters& params) {
    std::lock_guard<std::mutex> guard(m_Mutex);

    assert(!m_Cond_SearchPending);

    m_SearchParameters = params;

    m_Cond_SearchPending = true;
    m_Cond.notify_all();
}

void Search::StopSearch() {
    // Purposefully don't grab the lock. If the worker thread is running, we wont't
    // be able to. Setting this atomic will cause RunSearch() to stop soon.
    m_StopSearchRequested = true;

    WaitForSearch();
}

void Search::WaitForSearch() {
    std::unique_lock<std::mutex> lock(m_Mutex);

    // This indicates that a search has been started with StartSearch(), but
    // the worker thread has not processed it yet. The most common cause of
    // this is the -c command line option, which can be used to start a search
    // and then immediately wait on it.
    while(m_Cond_SearchPending) {
        m_Cond.wait(lock);
    }
}

void Search::RunSearch() {
    // No need to acquire mutex, SearchThreadMain() called us and is already
    // holding it.
    m_SearchStartTime = std::chrono::high_resolution_clock::now();
    m_TimeLimitCounter = 0;

    std::vector<Board::Move> pv;

    std::fill(m_BetaCutoffHistogram.begin(), m_BetaCutoffHistogram.end(), 0);
    std::fill(m_BestMoveHistogram.begin(), m_BestMoveHistogram.end(), 0);
    
    m_TT.AdvanceTime();

    for(int depth = 1; depth <= m_SearchParameters.Depth; ++depth) {
        int score;
        int minimumSafeSearchDepth = m_SearchParameters.Depth;
        
        m_NumMainNodes = 0;
        m_NumQuiesceNodes = 0;
        m_NumStaticEvaluations = 0;
        m_HashTableScoreHits = 0;
        m_HashTableMoveHits = 0;

        // Time limit is disabled for depth=1, since need to complete at least
        // the first depth in order to produce a "bestmove".
        // 
        // TODO: In theory a "stop" could come over UCI before we finish the first
        // depth, causing us to crash because our "pv" array will be empty.
        m_EffectiveHardMoveTime_ms = (depth > 1) ? m_SearchParameters.HardMoveTime_ms : -1;

        auto iterationStartTime = std::chrono::high_resolution_clock::now();
        if(!m_SearchParameters.BruteForce) {
            score = MainSearch(-SCORE_INF, SCORE_INF, 0, depth);
        } else {
            score = BruteForceSearch(0, depth);
        }
        auto iterationDuration = std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now() - iterationStartTime);
        auto iteration_us = iterationDuration.count();

        if(m_StopSearchRequested) {
            // Search was aborted or timed out, but we still need to print bestmove
            break;
        }

        // TODO: It is kinda ugly to have UCI stuff creeping into our search
        // functions, but with our threading setup, it would be pretty complicated
        // to do it some other way
        std::string infoStr = "info depth " + std::to_string(depth);

        if(abs(score) > SCORE_MIN_MATE) {
            int pliesToMate = SCORE_MATE - abs(score);
            int movesToMate = (pliesToMate + 1) / 2;

            // We don't detect checkmate in quiesce(), hence we need one extra
            // ply to find a mate
            minimumSafeSearchDepth = pliesToMate + 1;

            // Report negative moves to mate if the engine is getting mated
            movesToMate = (score > 0) ? movesToMate : -movesToMate;

            infoStr += " score mate " + std::to_string(movesToMate);
        } else {
            infoStr += " score cp " + std::to_string(score);
        }

        infoStr += " time " + std::to_string(iteration_us / 1000);

        int numNodes = m_NumMainNodes + m_NumQuiesceNodes;                
        infoStr += " nodes " + std::to_string(numNodes);

        if(iteration_us > (100 * 1000)) {
            std::int64_t nodesPerSecond = ((static_cast<std::int64_t>(numNodes) * 1000000) / iteration_us);
            infoStr += " nps " + std::to_string(nodesPerSecond);
        }

        infoStr += " pv";
        pv = PV(depth);
        for(auto& move : pv) {
            infoStr += " " + move.ToString();
        }

        IO::PutLine(infoStr);

#ifndef NDEBUG
        IO::PutInfo("STATS:"
            " M=" + std::to_string(m_NumMainNodes) +
            " Q=" + std::to_string(m_NumQuiesceNodes) +
            " SE=" + std::to_string(m_NumStaticEvaluations) +
            " TTS=" + std::to_string(m_HashTableScoreHits) +
            " TTM=" + std::to_string(m_HashTableMoveHits)
        );
#endif

        if(depth >= minimumSafeSearchDepth) {
            break;
        }

        auto timeElapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - m_SearchStartTime);
        if(m_SearchParameters.SoftMoveTime_ms != -1 && timeElapsed_ms.count() > m_SearchParameters.SoftMoveTime_ms) {
            break;
        }
    }

    IO::PutLine("bestmove " + pv[0].ToString());

    if(m_SearchParameters.ShowHistograms) {
        IO::PutInfo("Best move/Refutation move ordering histogram:");
        for(int i = 0; i < 250; ++i) {
            if(m_BestMoveHistogram[i] != 0 || m_BetaCutoffHistogram[i] != 0) {
                IO::PutInfo(std::to_string(i) + " " + std::to_string(m_BestMoveHistogram[i]) + " " + std::to_string(m_BetaCutoffHistogram[i]));
            }
        }
    }
}

void Search::CheckTimeLimit() {
    m_TimeLimitCounter = (m_TimeLimitCounter + 1) & 0xFFF;

    if(m_TimeLimitCounter == 0 && m_EffectiveHardMoveTime_ms != -1) {
        auto timeElapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - m_SearchStartTime);

        if(timeElapsed_ms.count() > m_EffectiveHardMoveTime_ms) {
            m_StopSearchRequested = true;
        }
    }
}

int Search::MainSearch(int alpha, int beta, int plyIndex, int depth) {
    Ply &ply = m_Plies[plyIndex];
    Board::Move bestMove;

    assert(alpha >= -SCORE_INF && alpha <= SCORE_INF);
    assert(beta >= -SCORE_INF && beta <= SCORE_INF);
    assert(beta > alpha); // XXX: Or should this be >=?
    assert(plyIndex >= 0 && plyIndex < MAX_PLY);
    assert(depth >= 0);

    // See if our time limit is up
    CheckTimeLimit();

    // Score repetitions (even two-fold) as draws
    // (But if we are at the root, we have to look for moves regardless)
    if(plyIndex > 0 && m_Board.IsRepetition()) {
        return SCORE_DRAW;
    }

    if(depth == 0) {
        if (m_Board.InCheck()) {
            // Extend search by 1 ply to get out of check before going to
            // qsearch. This shouldn't cause termination problems as it is
            // difficult to give check while simultaneously escaping check.
            //
            // TODO: I think it would be better to just evaluate all moves
            // when in check inside Quiesce(), but that would be kind of
            // awkward at the moment due to MovePicker being specialized at
            // compile-time on GenFlags. Maybe that should be refactored, but
            // we just do this for now.
            depth = 1;
        } else {
            return Quiesce(alpha, beta, plyIndex);
        }
    }

    Zobrist::hash_t thisNodeHash = m_Board.Hash();
    Board::Move hashMove;
    int probeResult = 0;

    // Don't probe at the root, we want to avoid a situation where we only
    // have hash hits as we increase depth, and then when we finally hit a
    // depth that triggers some work, we don't finish before time is up.
    // If that happens, it could be the case that we are walking right into
    // a repetition draw and don't even know it.
    if(plyIndex > 0) {
        int hashScore;

        probeResult = m_TT.Probe(thisNodeHash, alpha, beta, depth, plyIndex, hashScore, hashMove);

        if(probeResult & TranspositionTable::PROBE_FOUND_SCORE) {
            ++m_HashTableScoreHits;
            // Great, we found a score
            return hashScore;
        }
    }

    MovePicker<Board::GEN_ALL> movePicker(m_Board, ply.MoveList, &ply.KillerMoves[0]);

    int numLegalMoves = 0;
    bool alphaWasImproved = false;
        
    if(probeResult & TranspositionTable::PROBE_FOUND_MOVE) {
        ++m_HashTableMoveHits;
        movePicker.SetHashMove(hashMove);
    }
    
    Board::Move move;
    int bestMoveIndex = -1;
    while(movePicker.Next(move)) {
        if(m_Board.Make(move)) {
            ++m_NumMainNodes;

            // Alpha and beta switch roles for the other player
            int moveScore = -MainSearch(-beta, -alpha, plyIndex + 1, depth - 1);

            m_Board.Unmake();

            if(m_StopSearchRequested) {
                // abort search
                return 0;
            }

            if(moveScore >= beta) {
                // We have found a continuation that is "too good", and we
                // already know the opponent will not allow us to get to this
                // position in the first place, so we can quit searching.

                // This is a cut-node, so we insert a lower bound into the hash table
                m_TT.Insert(thisNodeHash, moveScore, TranspositionTable::ScoreBound::LOWER_BOUND, depth, plyIndex, &move);

                m_BetaCutoffHistogram[numLegalMoves] += 1;

                if(!move.IsCapture && move.Promotion == Board::Piece::NONE) {
                    // Was a quiet move, save it as a killer
                    UpdateKillers(plyIndex, move);
                }

                return beta;
            }

            if(moveScore > alpha) {
                alpha = moveScore;
                alphaWasImproved = true;
                bestMove = move;
                bestMoveIndex = numLegalMoves;
            }
                
            ++numLegalMoves;
        }
    }

    // We searched all the moves, so we either have a PV-node or an All-Node
    int score;
    TranspositionTable::ScoreBound bound;
    Board::Move* ttMove = nullptr;

    if(numLegalMoves == 0) {
        // There was nothing more to search, so this must be an exact score
        bound = TranspositionTable::ScoreBound::EXACT;

        if(m_Board.InCheck()) {
            // Checkmate is bad, but it's better if it's farther off
            score = -SCORE_MATE + plyIndex;
        } else {
            score = SCORE_DRAW;
        }
    } else if(m_Board.IsEligibleForFiftyMoveDraw()) {
        // Draw by 50 move rule
        // FIXME: Detecting this here means we only find draws at PV nodes,
        // but I don't have a clean way to detect checkmate at the moment
        // without running through the move list.
        bound = TranspositionTable::ScoreBound::EXACT;
        score = SCORE_DRAW;
    } else {
        // If alpha was improved, it's a PV-node and hence exact.
        // Otherwise, it's an all-node and the score is an upper bound.
        if(alphaWasImproved) {
            bound = TranspositionTable::ScoreBound::EXACT;
            ttMove = &bestMove;
        } else {
            bound = TranspositionTable::ScoreBound::UPPER_BOUND;
        }
        score = alpha;     
    }

    m_TT.Insert(thisNodeHash, score, bound, depth, plyIndex, ttMove);

    if(alphaWasImproved) {
        m_BestMoveHistogram[bestMoveIndex] += 1;
    }

    return score;
}

void Search::UpdateKillers(int plyIndex, Board::Move move) {
    assert(plyIndex < MAX_PLY);

    Ply& ply = m_Plies[plyIndex];

    bool alreadyInKillerMoves = false;

    for(auto& killer : ply.KillerMoves) {
        if(killer.SrcSquare == move.SrcSquare &&
           killer.DestSquare == move.DestSquare &&
           killer.Promotion == move.Promotion) {
               alreadyInKillerMoves = true;
               break;
           }
    }

    if(!alreadyInKillerMoves) {
        for(int i = 1; i < NUM_KILLER_MOVE_SLOTS; ++i) {
            ply.KillerMoves[i] = ply.KillerMoves[i - 1];
        }
        ply.KillerMoves[0] = move;
    }
}

int Search::Quiesce() {
    std::lock_guard<std::mutex> lock(m_Mutex);

    return Quiesce(-SCORE_INF, SCORE_INF, 0);
}

int Search::Quiesce(int alpha, int beta, int plyIndex) {
    Ply &ply = m_Plies[plyIndex];

    CheckTimeLimit();

    // We assume that we are not in zugzwang, and hence the static evaluation
    // can serve as a lower bound for our score, since we believe that at least
    // one of our moves should improve our situation.
    int standPatScore = m_Board.StaticEvaluation();
    ++m_NumStaticEvaluations;

    if(standPatScore >= beta) {
        // Our position is too good, we would not have been allowed to get here
        return beta;
    }

    if(standPatScore > alpha) {
        // Again, we assume that we'll be able to achieve at least the stand pat score
        alpha = standPatScore;
    }

    MovePicker<Board::GEN_CAPTURES | Board::GEN_PROMOTIONS> movePicker(m_Board, ply.MoveList, &ply.KillerMoves[0]);
    Board::Move move;
    while(movePicker.Next(move)) {
        if(m_Board.Make(move)) {
            ++m_NumQuiesceNodes;

            int moveScore = -Quiesce(-beta, -alpha, plyIndex + 1);
            m_Board.Unmake();

            if(m_StopSearchRequested) {
                return 0;
            }

            if(moveScore >= beta) {
                return beta;
            }

            if(moveScore > alpha) {
                alpha = moveScore;
            }
        }
    }

    return alpha;
}

// Brute force search for debugging/regression purposes.
int Search::BruteForceSearch(int plyIndex, int depth) {
    Ply &ply = m_Plies[plyIndex];
    
    if(depth == 0) {
        // We use the regular quiescent search. This could introduce some problems,
        // but there is just a search tree explosion if you don't use any pruning
        // at all
        return Quiesce(-SCORE_INF, SCORE_INF, plyIndex);
    } else {
        int score = -SCORE_INF;

        m_Board.FindPseudoLegalMoves(ply.MoveList);

        for(auto& move : ply.MoveList) {
            if(m_Board.Make(move)) {
                int moveScore = -BruteForceSearch(plyIndex + 1, depth - 1);

                if(moveScore > score) {
                    score = moveScore;
                }

                m_Board.Unmake();
            }
        }

        if(score == -SCORE_INF) {
            // We didn't find any moves
            if(m_Board.InCheck()) {
                score = -SCORE_MATE + plyIndex;
            } else {
                score = SCORE_DRAW;
            }
        }

        return score;
    }
}

std::vector<Board::Move> Search::PV(int depth) {
    std::vector<Board::Move> pv;
    
    Board::Move hashMove;
    int hashScore;
    int plyIndex = 0;
    int numMovesToUndo = 0;

    assert(depth > 0);

    // Follow chain of TT entries to try to discover PV, if it hasn't been overwritten
    while(depth > 0) {
        int probeResult = m_TT.Probe(m_Board.Hash(), -SCORE_INF, SCORE_INF, depth, plyIndex, hashScore, hashMove);
        --depth;
        ++plyIndex;

        if(probeResult & TranspositionTable::PROBE_FOUND_SCORE) {
            // The only way we get a score returned with this alpha/beta range is
            // an exact score

            // In a checkmate or stalemate situation, there will not have been a
            // best move
            if(!(probeResult & TranspositionTable::PROBE_FOUND_MOVE)) {
                break;
            }

            pv.push_back(hashMove);

            if(!m_Board.Make(hashMove)) {
                break;
            }
            ++numMovesToUndo;
        } else {
            break;
        }
    }

    // Put board back to original state
    for(int i = 0; i < numMovesToUndo; ++i) {
        m_Board.Unmake();
    }

    assert(!pv.empty());

    return pv;
}

std::int64_t Search::Perft(int depth) {
    std::lock_guard<std::mutex> lock(m_Mutex);    

    return Perft(depth, 0);
}

std::int64_t Search::Perft(int depth, int plyIndex) {
    Ply& ply = m_Plies[plyIndex];

    if(depth == 0) {
        // Leaf node
        return 1;
    }

    m_Board.FindPseudoLegalMoves(ply.MoveList);

    std::int64_t nodes = 0;

    for(auto &m : ply.MoveList) {
        if(m_Board.Make(m)) {
            nodes += Perft(depth - 1, plyIndex + 1);;
            m_Board.Unmake();
        }
    }

    return nodes;
}

void Search::SetHashTableSize(int bytesLog2) {
    m_TT.Resize(bytesLog2);
}

void Search::Test() {
#ifndef NDEBUG
    // Test that even with a hash table, the "mate in N" scores are still correct
    {
        Board board;
        Search search(board);

        // Black to move and is helpless. White can mate immediately when it is their turn.
        board.ParseFen("k7/7R/6R1/8/8/8/8/7K b - - 0 1");

        int checkmatedInTwoScore = search.MainSearch(-SCORE_INF, SCORE_INF, 0, 4);

        assert(checkmatedInTwoScore == -(SCORE_MATE - 2));

        // Make meaningless move for black king
        board.Make(Board::ParseMove("a8b8"));

        // Use a smaller depth, so we're sure to be able to use the hash table hit
        int deliverMateInOneScore = search.MainSearch(-SCORE_INF, SCORE_INF, 0, 3);

        assert(deliverMateInOneScore == (SCORE_MATE - 1));

        // Deliver mate
        board.Make(Board::ParseMove("g6g8"));

        int checkmatedScore = search.MainSearch(-SCORE_INF, SCORE_INF, 0, 2);
        assert(checkmatedScore == -SCORE_MATE);
    }
#endif
}