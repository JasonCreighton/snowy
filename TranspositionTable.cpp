#include "TranspositionTable.hpp"
#include "Zobrist.hpp"
#include "Constants.hpp"

#include <vector>
#include <cmath>
#include <cassert>
#include <limits>

TranspositionTable::TranspositionTable(int sizeLog2) :
    m_Now(0),
    m_HashMask((1 << sizeLog2) - 1),
    m_Table(1 << sizeLog2) {
}

void TranspositionTable::Insert(Zobrist::hash_t hash, int score, ScoreBound bound, int depth, int ply, const Board::Move *move) {
    Entry &entry = FindEntryToReplace(hash);
    Payload &payload = entry.Value;

    assert(score < SCORE_INF && score > -SCORE_INF);

    int nodeRelativeScore = ScoreRelativeToNode(score, ply);
    
    // I would like to be able to assert this, but at the moment the search
    // doesn't prune based on mate distance, so we end up finding mates when
    // we already know we have a better mate elsewhere, and then we put
    // something into the hash table with bounds outside this range.
    //
    // It all comes out in the wash on the probe side, as the entry will never
    // be used, but it still wastes a hash table entry.
    // 
    // assert(nodeRelativeScore < SCORE_INF && nodeRelativeScore > -SCORE_INF);

    entry.Key = hash;
    payload.Score = nodeRelativeScore;
    payload.Bound = bound;
    payload.Depth = depth;

    if(move != nullptr) {
        payload.HasMove = true;
        payload.SrcSquare = move->SrcSquare;
        payload.DestSquare = move->DestSquare;
        payload.Promotion = move->Promotion;
    } else {
        payload.HasMove = false;
    }
}

bool TranspositionTable::Lookup(Zobrist::hash_t hash, Payload& out_payload) {
    Bucket &bucket = m_Table[hash & m_HashMask];
    for(auto& entry : bucket.entries) {
        if(entry.Key == hash) {
            out_payload = entry.Value;
            return true;
        }
    }
    return false;
}

int TranspositionTable::Probe(Zobrist::hash_t hash, int alpha, int beta, int depth, int ply, int& out_score, Board::Move& out_move){
    Payload payload;
    int flags = 0;

    if(!Lookup(hash, payload)) {
        return flags;
    }

    if(payload.HasMove) {
        flags |= PROBE_FOUND_MOVE;
        out_move.SrcSquare = payload.SrcSquare;
        out_move.DestSquare = payload.DestSquare;
        out_move.Promotion = payload.Promotion;
        out_move.IsCapture = false; // XXX
        out_move.Score = 0; // XXX
    }

    if(payload.Depth < depth) {
        // No score available, bail out early
        return flags;
    }

    int score = ScoreRelativeToRoot(payload.Score, ply);

    switch(payload.Bound) {
        case ScoreBound::EXACT:
            // We stored an exact score, which we can just use directly
            out_score = score;
            flags |= PROBE_FOUND_SCORE;
            break;

        case ScoreBound::UPPER_BOUND:
            // If the upper bound from the hash is less than the lower bound from
            // the search (alpha), that means that we already have a better move
            // elsewhere, and we can stop searching this node.
            if(score <= alpha) {
                out_score = alpha;
                flags |= PROBE_FOUND_SCORE;
            }
            break;

        case ScoreBound::LOWER_BOUND:
            // If the lower bound from the hash is greater than the upper bound
            // from the search (beta), that means that our opponent already has
            // a better move elsewhere, and we can stop searching this node.
            if(score >= beta) {
                out_score = beta;
                flags |= PROBE_FOUND_SCORE;
            }
            break;
    }

    // If we found a score, it should be in range
    assert(!(flags & PROBE_FOUND_SCORE) || (out_score < SCORE_INF && out_score > -SCORE_INF));

    return flags;
}

void TranspositionTable::AdvanceTime() {
    ++m_Now;
}

TranspositionTable::Entry& TranspositionTable::FindEntryToReplace(Zobrist::hash_t hash) {
    Bucket& bucket = m_Table[hash & m_HashMask];
    Entry* bestCandidate = nullptr;
    int bestScore = std::numeric_limits<int>::min();
    for(auto& entry : bucket.entries) {
        // If there is an exact match, always use that
        if(entry.Key == hash) {
            return entry;
        }

        std::uint8_t age = m_Now - entry.Value.Timestamp;
        // Higher is "bad" (more likley to be replaced)
        // So this means older entries get replaced first, and in cases of ties
        // on age, shallowest depth will get replaced first. (This is what
        // Crafty does.)
        int score = (age << 16) - entry.Value.Depth;

        if(score > bestScore) {
            bestScore = score;
            bestCandidate = &entry;
        }
    }

    assert(bestCandidate != nullptr);

    return *bestCandidate;
}

int TranspositionTable::ScoreRelativeToNode(int scoreRelativeToRoot, int ply) {
    if(abs(scoreRelativeToRoot) >= SCORE_MIN_MATE) {
        if(scoreRelativeToRoot > 0) {
            return scoreRelativeToRoot + ply;
        } else {
            return scoreRelativeToRoot - ply;
        }
    }

    return scoreRelativeToRoot;
}

int TranspositionTable::ScoreRelativeToRoot(int scoreRelativeToNode, int ply) {
    if(abs(scoreRelativeToNode) >= SCORE_MIN_MATE) {
        if(scoreRelativeToNode > 0) {
            return scoreRelativeToNode - ply;
        } else {
            return scoreRelativeToNode + ply;
        }
    }

    return scoreRelativeToNode;
}