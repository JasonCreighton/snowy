// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Zobrist.hpp"

#include <vector>
#include <string>
#include <cstdint>

class Board {
public:
    typedef int color_t;
    typedef std::uint8_t piece_t;
    typedef std::uint8_t square_t;

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

    class Color {
    public:
        static const color_t WHITE = 0;
        static const color_t BLACK = 1;
        static color_t OtherSide(color_t color) { return !color; };
        static piece_t PieceBits(color_t color) {
            // TODO: This method is a little weird (does it belong here or in the
            // Piece class?) and I think we would be better served with some
            // more abstracted way to construct a piece_t
            static const color_t ColorToPieceBits[2] = { PC_WHITE, PC_BLACK };

            return ColorToPieceBits[color];
        }
    };

    class Piece {
    public:
        static color_t Color(piece_t piece) {
            // TODO: Tweak the piece_t bit layout to make this just a shift
            return (piece & PC_COLORMASK) == PC_WHITE ? Color::WHITE : Color::BLACK;
        }
        static int Index(piece_t piece) {
            return (piece & PC_PIECEMASK) - 1;
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
    void ParseFen(const std::string &fen);
    int StaticEvaluation();
    std::vector<int> EvaluationFeatures();
    bool InCheck();
    bool WhiteToMove() const;
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
    
    void MarkRookIneligibleForCastling(color_t rookColor, square_t rookSquare);
    void FindCastlingMoves(square_t srcSquare, std::vector<Move> &out_MoveList);
    void FindCastlingMovesHelper(square_t kingStartSquare, int kingMovementDirection, square_t rookStartSquare, std::vector<Move> &out_MoveList);
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