// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Zobrist.hpp"

#include <vector>
#include <string>
#include <cstdint>

class Board {
public:
    typedef std::uint8_t piece_t;
    typedef std::uint8_t square_t;

    struct Move {
        square_t SrcSquare;
        square_t DestSquare;
        piece_t Promotion;
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

    template<int GenFlags = GEN_ALL>
    void FindPseudoLegalMoves(std::vector<Move> &out_MoveList);

    bool Make(Move m);
    void Unmake();
    static square_t CoordsToIndex(int rank, int file);
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
        square_t Squares[4];
        piece_t Contents[4];
        // Maximum piece locations modifications is 5. Worst case is pawn
        // capture and promotion on the same move:
        //     * Remove old pawn (2 slots)
        //     * Remove captured piece (2 slots)
        //     * Add promoted piece (1 slot)
        uint8_t PieceLocationIndexes[5];
        square_t PieceLocations[5];
        std::uint8_t CastlingRights;
        square_t EnPassantTargetSquare;
    };

    static const piece_t PC_PIECEMASK = 0x07;

    static const piece_t PC_NONE = 0x00;

    static const piece_t PC_PAWN = 0x01;
    static const piece_t PC_KNIGHT = 0x02;
    static const piece_t PC_BISHOP = 0x03;
    static const piece_t PC_ROOK = 0x04;    
    static const piece_t PC_QUEEN = 0x05;
    static const piece_t PC_KING = 0x06;
    
    static const piece_t PC_COLORMASK = 0x80;
    static const piece_t PC_BLACK = 0x00;
    static const piece_t PC_WHITE = 0x80;

    static const int CR_WHITE_KING_SIDE = 0x1;
    static const int CR_WHITE_QUEEN_SIDE = 0x2;
    static const int CR_BLACK_KING_SIDE = 0x4;
    static const int CR_BLACK_QUEEN_SIDE = 0x8;

    static const int PL_END_OF_LIST = 0x7F;

    struct SquareModification {
        square_t Square;
        piece_t OldValue;
    };

    template<int GenFlags>
    void FindMovesInDirection(piece_t piece, square_t srcSquare, int direction, int slideDistance, bool isPromotion, std::vector<Move> &out_MoveList);

    template<int GenFlags>
    void FindPawnMoves(square_t srcSquare, std::vector<Move> &out_MoveList);

    void MarkRookIneligibleForCastling(bool rookIsWhite, piece_t rookSquare);
    void FindCastlingMoves(square_t srcSquare, std::vector<Move> &out_MoveList);
    void FindCastlingMovesHelper(square_t kingStartSquare, int kingMovementDirection, square_t rookStartSquare, std::vector<Move> &out_MoveList);
    bool IsAttacked(square_t square);
    Zobrist::hash_t SquareHashCode(square_t square);
    void SetSquare(square_t square, piece_t contents);
    void SetSquareWithUndo(square_t square, piece_t contents, UndoMove &undo);

    bool PieceListsConsistentWithBoard() const;
    int PieceLocationsOffset(bool white, int pieceNumber) const;
    void PieceListRemoveWithUndo(square_t location, UndoMove& undo);
    void SetPieceLocationWithUndo(int index, square_t location, UndoMove& undo);
    void MovePieceWithUndo(square_t from, square_t to, UndoMove& undo);
    void PlaceNewPiece(square_t location, piece_t contents);
    void PlaceNewPieceWithUndo(square_t location, piece_t contents, UndoMove& undo);
    void RemovePieceWithUndo(square_t location, UndoMove& undo);

    piece_t& PieceOn(int rank, int file);
    
    // 0x88 board representation
    piece_t m_Pieces[128];

    // Piece list: 2 sides * 6 pieces * 10 elements = 120
    // (10 because there are at most 9 of any given piece, and there is an extra
    // element needed as a terminator)
    square_t m_PieceLocations[120];

    bool m_WhiteToMove = true;
    std::uint8_t m_CastlingRights = 0xF;
    square_t m_EnPassantTargetSquare = 0x7F;
    Zobrist::hash_t m_PieceHash = 0;
    int m_PliesSincePawnMoveOrCapture; // for the 50 move rule
    std::vector<int> m_Features;

    // Undo stack
    std::vector<UndoMove> m_UndoStack;
};