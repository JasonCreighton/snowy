// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Common.hpp"
#include "Zobrist.hpp"

class Board {
public:
    typedef int color_t;
    typedef std::uint8_t piece_t;
    typedef std::uint8_t square_t;

    class Color {
    public:
        static const color_t WHITE = 0;
        static const color_t BLACK = 1;
        static color_t OtherSide(color_t color) { return !color; };
    };

    class Piece {
    public:
        // Argument values for "pieceIndex"
        static const int PAWN = 0;
        static const int KNIGHT = 1;
        static const int BISHOP = 2;
        static const int ROOK = 3;
        static const int QUEEN = 4;
        static const int KING = 5;

        // Special piece_t value that means "no piece"
        static const piece_t NONE = 6;

        static const int COLOR_SHIFT = 3;
        static const piece_t INDEX_MASK = 0x7;

        static piece_t Create(color_t color, int pieceIndex) {
            assert(pieceIndex >= PAWN && pieceIndex <= KING);
            return (color << COLOR_SHIFT) | pieceIndex;
        }
        static color_t Color(piece_t piece) {
            assert(piece != NONE);
            color_t color = piece >> 3;

            // No high bits should be set
            assert((color & 1) == color);

            return color;
        }
        static int Index(piece_t piece) {
            // NOTE: Might return NONE, depends on calling context whether this is
            // an error condition.
            return piece & INDEX_MASK;
        }
    };

    static const int SQ_NONE = 0x7F;

    // Abuse of class to create a pseudo-namespace for square_t helper functions
    class Square {
    public:
        static square_t FromCoords(int rank, int file) { return (rank << 4) | file; }
        static bool OnBoard(square_t square) { return (square & 0x88) == 0; }
        static int Rank(square_t square) { return square >> 4; }
        static int File(square_t square) { return square & 0x7; }
        static int Index64(square_t square) { return (Rank(square) << 3) | File(square); }
        static int IndexPST64(square_t square, color_t pieceColor) {
            if(pieceColor == Color::WHITE) {
                return ((7 - Rank(square)) * 8) + File(square);
            } else {
                return (Rank(square) * 8) + File(square);
            }
        }
    };

    struct Move {
        square_t SrcSquare;
        square_t DestSquare;
        std::uint8_t Promotion;
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

    bool IsPseudoLegal(Move move);
    bool Make(Move m);
    void Unmake();
    void ParseFen(const std::string &fen);
    int StaticEvaluation();
    std::vector<int> EvaluationFeatures();
    bool InCheck();
    bool WhiteToMove() const;
    bool IsRepetition();
    bool IsEligibleForFiftyMoveDraw();
    std::int64_t NumLegalMovesMade;

    static Move ParseMove(const std::string &moveStr);

    static const char *FEN_START_POSITION;

private:
    static void TestZobristHashing();
    static void TestEnPassantSquare();
    static void TestCastlingRights();

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

    static const int CR_WHITE_KING_SIDE = 0x1;
    static const int CR_WHITE_QUEEN_SIDE = 0x2;
    static const int CR_BLACK_KING_SIDE = 0x4;
    static const int CR_BLACK_QUEEN_SIDE = 0x8;

    static const int PL_END_OF_LIST = 0x7F;

    struct SquareModification {
        square_t Square;
        piece_t OldValue;
    };

    template<int GenFlags, typename GenFunction>
    void FindMovesFromSquare(int pieceIndex, square_t srcSquare, GenFunction genMove);

    template<int GenFlags, typename GenFunction>
    void FindMovesInDirection(square_t srcSquare, int direction, int slideDistance, GenFunction genMove);

    template<int GenFlags, typename GenFunction>
    void FindPawnMoves(square_t srcSquare, GenFunction genMove);
    
    template<typename GenFunction>
    void FindCastlingMoves(square_t srcSquare, GenFunction genMove);

    template<typename GenFunction>
    void FindCastlingMovesHelper(square_t kingStartSquare, int kingMovementDirection, square_t rookStartSquare, GenFunction genMove);

    void MarkRookIneligibleForCastling(color_t rookColor, square_t rookSquare);
    bool IsAttacked(square_t square);
    Zobrist::hash_t SquareHashCode(square_t square);
    void SetSquare(square_t square, piece_t contents);
    void SetSquareWithUndo(square_t square, piece_t contents, UndoMove &undo);

    bool PieceListsConsistentWithBoard() const;
    int PieceLocationsOffset(color_t color, int pieceNumber) const;
    void PieceListRemoveWithUndo(square_t location, UndoMove& undo);
    void SetPieceLocationWithUndo(int index, square_t location, UndoMove& undo);
    void MovePieceWithUndo(square_t from, square_t to, UndoMove& undo);
    void PlaceNewPiece(square_t location, piece_t contents);
    void PlaceNewPieceWithUndo(square_t location, piece_t contents, UndoMove& undo);
    void RemovePieceWithUndo(square_t location, UndoMove& undo);

    // VisitorFunction should be: void visit(square_t square);
    template<typename VisitorFunction>
    void ForEachPiece(color_t color, int pieceNumber, VisitorFunction f);

    // 0x88 board representation
    piece_t m_Pieces[128];

    // Piece list: 2 sides * 6 pieces * 10 elements = 120
    // (10 because there are at most 9 of any given piece, and there is an extra
    // element needed as a terminator)
    square_t m_PieceLocations[120];

    color_t m_SideToMove = Color::WHITE;
    std::uint8_t m_CastlingRights = 0xF;
    square_t m_EnPassantTargetSquare = 0x7F;
    Zobrist::hash_t m_PieceHash = 0;
    int m_PliesSincePawnMoveOrCapture; // for the 50 move rule
    std::vector<int> m_Features;

    // Undo stack
    std::vector<UndoMove> m_UndoStack;
};