// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#pragma once

#include "Zobrist.hpp"

#include <vector>
#include <string>
#include <cstdint>

class Board {
public:
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
    
    static void Test();    

    Board();

    void Print();
    
    Zobrist::hash_t Hash() const;
    void FindPseudoLegalMoves(std::vector<Move> &out_MoveList);
    bool Make(Move m);
    void Unmake();
    static index_t CoordsToIndex(int rank, int file);
    void ParseFen(const std::string &fen);
    int StaticEvaluation();
    bool InCheck();
    bool WhiteToMove();
    bool IsRepetition();
    bool IsEligibleForFiftyMoveDraw();

    static Move ParseMove(const std::string &moveStr);

    static const char *FEN_START_POSITION;

private:
    struct UndoMove {
        Zobrist::hash_t PieceHash;
        int NumReversiblePlies;
        index_t Squares[4];
        square_t Contents[4];
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

    struct SquareModification {
        index_t Square;
        square_t OldValue;
    };

    void FindMovesInDirection(square_t piece, index_t srcSquare, int direction, int slideDistance, bool movementAllowed, bool capturesAllowed, bool promotionAllowed, std::vector<Move> &out_MoveList);
    void FindPawnMoves(index_t srcSquare, std::vector<Move> &out_MoveList);
    void FindCastlingMoves(index_t srcSquare, std::vector<Move> &out_MoveList);
    void FindCastlingMovesHelper(index_t kingStartSquare, int kingMovementDirection, index_t rookStartSquare, std::vector<Move> &out_MoveList);
    bool IsAttacked(index_t square);
    Zobrist::hash_t SquareHashCode(index_t square);
    void SetSquare(index_t square, square_t contents);
    void SetSquareWithUndo(index_t square, square_t contents, UndoMove &undo, int undoIndex);

    square_t &Square(int rank, int file);
    
    // 0x88 board representation
    square_t m_Squares[128];
    bool m_WhiteToMove = true;
    std::uint8_t m_CastlingRights = 0xF;
    index_t m_EnPassantTargetSquare = 0x7F;
    Zobrist::hash_t m_PieceHash = 0;
    int m_NumReversiblePlies; // for the 50 move rule

    // Undo stack
    std::vector<UndoMove> m_UndoStack;
};