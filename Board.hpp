// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Zobrist.hpp"

#include <vector>
#include <string>
#include <cstdint>

class Board {
public:
    // square_t is the actual contents of a square (eg, a white knight, black
    // pawn, etc), and index_t is the number of the square according to the 0x88
    // scheme. TODO: Come up with better names, these are confusing.
    typedef std::uint8_t square_t;
    typedef std::uint8_t index_t;

    struct Move {
        index_t SrcSquare;
        index_t DestSquare;
        square_t Promotion;
        bool IsCapture;
        // TOOD: Need to figure out if I care how big the Move structs are
        int Score;

        std::string ToString() const;
    };

    enum class Feature {
        DOUBLED_PAWNS,
        ISOLATED_PAWNS,
        PASSED_PAWNS,
        PAWN_SHIELD_UNADVANCED,
        PAWN_SHIELD_ADVANCED_1,
        PAWN_SHIELD_MISSING,
        _END_SENTINEL,
    };
    static const int NUM_FEATURES = (int)Feature::_END_SENTINEL;

    static const int GEN_CAPTURES = 0x1;
    static const int GEN_NONCAPTURES = 0x2;
    static const int GEN_PROMOTIONS = 0x4;
    static const int GEN_ALL = GEN_CAPTURES | GEN_NONCAPTURES | GEN_PROMOTIONS;
    
    static void Test();    

    Board();

    void Print();
    
    Zobrist::hash_t Hash() const;
    void FindPseudoLegalMoves(std::vector<Move> &out_MoveList);
    void FindPseudoLegalMoves(int generateFlags, std::vector<Move> &out_MoveList);
    bool Make(Move m);
    void Unmake();
    static index_t CoordsToIndex(int rank, int file);
    void ParseFen(const std::string &fen);
    int StaticEvaluation();
    std::vector<int> EvaluationFeatures();
    bool InCheck();
    bool WhiteToMove();
    bool IsRepetition();
    bool IsEligibleForFiftyMoveDraw();

    static Move ParseMove(const std::string &moveStr);

    static const char *FEN_START_POSITION;

private:
    struct UndoMove {
        Zobrist::hash_t PieceHash;
        int PliesSincePawnMoveOrCapture;
        int NumSquaresUpdated;
        int NumPieceLocationsUpdated;   
        index_t Squares[4];
        square_t Contents[4];
        // Maximum piece locations modifications is 5. Worst case is pawn
        // capture and promotion on the same move:
        //     * Remove old pawn (2 slots)
        //     * Remove captured piece (2 slots)
        //     * Add promoted piece (1 slot)
        uint8_t PieceLocationIndexes[5];
        index_t PieceLocations[5];
        std::uint8_t CastlingRights;
        index_t EnPassantTargetSquare;
    };

    static const square_t SQ_PIECEMASK = 0x07;

    static const square_t SQ_EMPTY = 0x00;

    static const square_t SQ_PAWN = 0x01;
    static const square_t SQ_KNIGHT = 0x02;
    static const square_t SQ_BISHOP = 0x03;
    static const square_t SQ_ROOK = 0x04;    
    static const square_t SQ_QUEEN = 0x05;
    static const square_t SQ_KING = 0x06;
    
    static const square_t SQ_COLORMASK = 0x80;
    static const square_t SQ_BLACK = 0x00;
    static const square_t SQ_WHITE = 0x80;

    static const int CR_WHITE_KING_SIDE = 0x1;
    static const int CR_WHITE_QUEEN_SIDE = 0x2;
    static const int CR_BLACK_KING_SIDE = 0x4;
    static const int CR_BLACK_QUEEN_SIDE = 0x8;

    static const int PL_END_OF_LIST = 0x7F;

    struct SquareModification {
        index_t Square;
        square_t OldValue;
    };

    void MarkRookIneligibleForCastling(bool rookIsWhite, square_t rookSquare);
    void FindMovesInDirection(int generateFlags, square_t piece, index_t srcSquare, int direction, int slideDistance, bool isPromotion, std::vector<Move> &out_MoveList);
    void FindPawnMoves(int generateFlags, index_t srcSquare, std::vector<Move> &out_MoveList);
    void FindCastlingMoves(index_t srcSquare, std::vector<Move> &out_MoveList);
    void FindCastlingMovesHelper(index_t kingStartSquare, int kingMovementDirection, index_t rookStartSquare, std::vector<Move> &out_MoveList);
    bool IsAttacked(index_t square);
    Zobrist::hash_t SquareHashCode(index_t square);
    void SetSquare(index_t square, square_t contents);
    void SetSquareWithUndo(index_t square, square_t contents, UndoMove &undo);

    bool PieceListsConsistentWithBoard() const;
    int PieceLocationsOffset(bool white, int pieceNumber) const;
    void PieceListRemoveWithUndo(index_t location, UndoMove& undo);
    void SetPieceLocationWithUndo(int index, index_t location, UndoMove& undo);
    void MovePieceWithUndo(index_t from, index_t to, UndoMove& undo);
    void PlaceNewPiece(index_t location, square_t contents);
    void PlaceNewPieceWithUndo(index_t location, square_t contents, UndoMove& undo);
    void RemovePieceWithUndo(index_t location, UndoMove& undo);

    square_t &Square(int rank, int file);
    
    // 0x88 board representation
    square_t m_Squares[128];

    // Piece list: 2 sides * 6 pieces * 10 elements = 120
    // (10 because there are at most 9 of any given piece, and there is an extra
    // element needed as a terminator)
    index_t m_PieceLocations[120];

    bool m_WhiteToMove = true;
    std::uint8_t m_CastlingRights = 0xF;
    index_t m_EnPassantTargetSquare = 0x7F;
    Zobrist::hash_t m_PieceHash = 0;
    int m_PliesSincePawnMoveOrCapture; // for the 50 move rule
    std::vector<int> m_Features;

    // Undo stack
    std::vector<UndoMove> m_UndoStack;
};