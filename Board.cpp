// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "Board.hpp"
#include "IO.hpp"
#include "Util.hpp"

#include <iostream>
#include <cctype> // tolower()
#include <cstring> // memset()
#include <cassert>

namespace {
    #include "StaticEvaluationParameters.hpp"
    const int MOVE_SCORE_CASTLING = 25;

    const int ORTHOGONAL_VECTORS[4] = {0x01, -0x01, 0x10, -0x10};
    const int DIAGONAL_VECTORS[4] = {0x11, -0x11, 0x0F, -0x0F};
    const int ORTHOGONAL_AND_DIAGONAL_VECTORS[8] = {0x01, -0x01, 0x10, -0x10, 0x11, -0x11, 0x0F, -0x0F};
    const int ORTHOGONAL_AND_DIAGONAL_VECTORS_IS_DIAGONAL_MASK = 0x0F;
    const int KNIGHT_VECTORS[8] = {
         0x20 + 0x01, // two up, one right
         0x20 - 0x01, // two up, one left
         0x02 + 0x10, // two right, one up
         0x02 - 0x10, // two right, one down
        -0x02 + 0x10, // two left, one up
        -0x02 - 0x10, // two left, one down
        -0x20 + 0x01, // two down, one right
        -0x20 - 0x01  // two down, one left
    };
}

const char *Board::FEN_START_POSITION = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

Board::Board() :
    m_Features(NUM_FEATURES)
{
    ParseFen(FEN_START_POSITION);
}

void Board::Print() {
    const char* pieceMap = ".PNBRQK";
    const char* fileLabels = "  a b c d e f g h";

    IO::PutLine(fileLabels);
    for(int rank = 7; rank >= 0; --rank) {
        std::string line = std::to_string(rank + 1) + " ";
        for(int file = 0; file < 8; ++file) {
            square_t square = Square(rank, file);
            char piece = pieceMap[square & SQ_PIECEMASK];
            if(square != SQ_EMPTY && (square & SQ_COLORMASK) == SQ_BLACK) {
                // HACKY
                piece = (char)std::tolower((int)piece);
            }

            line.push_back(piece);
            line.push_back(' ');
        }
        line += std::to_string(rank + 1);
        IO::PutLine(line);
    }
    IO::PutLine(fileLabels);
    IO::PutLine("Hash: " + IntegerToHexString(Hash()));
}

Zobrist::hash_t Board::Hash() const {
    bool enPassantAvailable = m_EnPassantTargetSquare != 0x7F;
    int enPassantFile = m_EnPassantTargetSquare & 0x7;

    return
        m_PieceHash ^
        (m_WhiteToMove ? Zobrist::WhiteToMove : 0) ^
        Zobrist::CastlingRights[m_CastlingRights] ^
        (enPassantAvailable ? Zobrist::EnPassantFile[enPassantFile] : 0);
}

Board::square_t &Board::Square(int rank, int file) {
    return m_Squares[(rank << 4) | file];
}

bool Board::WhiteToMove() {
    return m_WhiteToMove;
}

bool Board::IsRepetition() {
    // Note that we return true even in cases of two-fold repetition. This is
    // what Crafty does, but I need to think more about whether it's safe.

    // We need to be careful with our indices here: m_PliesSincePawnMoveOrCapture
    // could come from a FEN position, and doesn't necessary mean there are that
    // many moves in the undo stack.
    if(m_PliesSincePawnMoveOrCapture >= 4) {
        // We only have to look at plies where it was our turn to move
        int pliesToExamine = m_PliesSincePawnMoveOrCapture / 2;
        
        for(int i = static_cast<int>(m_UndoStack.size()) - 2; i >= 0 && pliesToExamine > 0; i -= 2, --pliesToExamine) {
            assert(i >= 0 && i < (int)m_UndoStack.size());

            // TODO: Kind of ugly we have to check other metadata than the hash,
            // maybe we should bite the bullet and do full incremental updating
            // of the hash, instead of leaving non-piece elements out.
            if(m_UndoStack[i].PieceHash == m_PieceHash &&
               m_UndoStack[i].CastlingRights == m_CastlingRights &&
               m_UndoStack[i].EnPassantTargetSquare == m_EnPassantTargetSquare) {
                return true;
            }
        }
    }

    return false;
}

bool Board::IsEligibleForFiftyMoveDraw() {
    return m_PliesSincePawnMoveOrCapture >= 100;
}

void Board::FindPseudoLegalMoves(std::vector<Move> &out_MoveList) {
    FindPseudoLegalMoves(GEN_ALL, out_MoveList);
}

void Board::FindPseudoLegalMoves(int generateFlags, std::vector<Move> &out_MoveList) {
    out_MoveList.clear();

    // FIXME: Loops are too complicated and messy.
    for(int plIdx = PieceLocationsOffset(m_WhiteToMove, SQ_PAWN-1); m_PieceLocations[plIdx] != PL_END_OF_LIST; ++plIdx) {
        FindPawnMoves(generateFlags, m_PieceLocations[plIdx], out_MoveList);
    }

    for(int plIdx = PieceLocationsOffset(m_WhiteToMove, SQ_KNIGHT-1); m_PieceLocations[plIdx] != PL_END_OF_LIST; ++plIdx) {
        index_t srcSquare = m_PieceLocations[plIdx];
        square_t piece = m_Squares[srcSquare];
        for(int vector : KNIGHT_VECTORS) {
            FindMovesInDirection(generateFlags, piece, srcSquare, vector, 1, false, out_MoveList);
        }
    }

    for(int plIdx = PieceLocationsOffset(m_WhiteToMove, SQ_BISHOP-1); m_PieceLocations[plIdx] != PL_END_OF_LIST; ++plIdx) {
        index_t srcSquare = m_PieceLocations[plIdx];
        square_t piece = m_Squares[srcSquare];
        for(int vector : DIAGONAL_VECTORS) {
            FindMovesInDirection(generateFlags, piece, srcSquare, vector, 8, false, out_MoveList);
        }
    }

    for(int plIdx = PieceLocationsOffset(m_WhiteToMove, SQ_ROOK-1); m_PieceLocations[plIdx] != PL_END_OF_LIST; ++plIdx) {
        index_t srcSquare = m_PieceLocations[plIdx];
        square_t piece = m_Squares[srcSquare];
        for(int vector : ORTHOGONAL_VECTORS) {
            FindMovesInDirection(generateFlags, piece, srcSquare, vector, 8, false, out_MoveList);
        }
    }

    for(int plIdx = PieceLocationsOffset(m_WhiteToMove, SQ_QUEEN-1); m_PieceLocations[plIdx] != PL_END_OF_LIST; ++plIdx) {
        index_t srcSquare = m_PieceLocations[plIdx];
        square_t piece = m_Squares[srcSquare];
        for(int vector : ORTHOGONAL_AND_DIAGONAL_VECTORS) {
            FindMovesInDirection(generateFlags, piece, srcSquare, vector, 8, false, out_MoveList);
        }
    }

    for(int plIdx = PieceLocationsOffset(m_WhiteToMove, SQ_KING-1); m_PieceLocations[plIdx] != PL_END_OF_LIST; ++plIdx) {
        index_t srcSquare = m_PieceLocations[plIdx];
        square_t piece = m_Squares[srcSquare];
        for(int vector : ORTHOGONAL_AND_DIAGONAL_VECTORS) {
            FindMovesInDirection(generateFlags, piece, srcSquare, vector, 1, false, out_MoveList);
        }

        // Check for castling
        if(generateFlags & GEN_NONCAPTURES) {
            FindCastlingMoves(srcSquare, out_MoveList);
        }
    }
}

void Board::FindMovesInDirection(int generateFlags, square_t piece, index_t srcSquare, int direction, int slideDistance, bool isPromotion, std::vector<Move> &out_MoveList) {
    for(index_t destSquare = srcSquare + direction; (destSquare & 0x88) == 0 && slideDistance > 0; destSquare += direction, --slideDistance) {
        square_t destContents = m_Squares[destSquare];
        bool isCapture = false;

        if(destContents == SQ_EMPTY) {
            // Empty square, everything is fine
        } else if((destContents & SQ_COLORMASK) == (piece & SQ_COLORMASK)) {
            // Same color piece, we can't do anything
            break;
        } else {
            // Opposite color piece
            isCapture = true;
        }

        bool generateMove =
            ((generateFlags & GEN_CAPTURES) && isCapture) ||
            ((generateFlags & GEN_NONCAPTURES) && !isCapture);
        
        if(!generateMove) {
            if(isCapture) {
                // We can't keep sliding through a piece
                break;
            } else {
                // There might be an allowed capture later
                continue;                
            }
        }

        // Should not be able to capture an empty square or a king
        if(isCapture) {
            assert(destContents != SQ_EMPTY);
            assert((destContents & SQ_PIECEMASK) != SQ_KING);
        }

        Move m;
        m.SrcSquare = srcSquare;
        m.DestSquare = destSquare;
        m.IsCapture = isCapture;
        m.Promotion = SQ_EMPTY;
        m.Score = 0;

        if(m.IsCapture) {
            int victimPieceNumber = m_Squares[m.DestSquare] & SQ_PIECEMASK; // [1, 6]
            int aggressorPieceNumber = m_Squares[m.SrcSquare] & SQ_PIECEMASK; // [1, 6]
            // In MVV/LVA, the victim dominates the ordering, so, eg, RxR is searched before PxB
            // Scores here range from 10 to 65
            int MVV = 10 * victimPieceNumber;
            int LVA = (6 - aggressorPieceNumber);
            m.Score = MVV + LVA;
        }

        if(isPromotion) {
            square_t pieces[4] = {SQ_BISHOP, SQ_KNIGHT, SQ_ROOK, SQ_QUEEN};
            for(square_t piece : pieces) {
                m.Promotion = piece;
                // Could be both a capture and a promotion, so we modify the previous score.
                // We only boost the score of queen promotions, underpromotions
                // are probably no better than any other move
                if(m.Promotion == SQ_QUEEN) {
                    m.Score += 100;
                }
                out_MoveList.push_back(m);
            }
        } else {
            out_MoveList.push_back(m);
        }
        
        if(isCapture) {
            break;
        }
    }
}

void Board::FindPawnMoves(int generateFlags, index_t srcSquare, std::vector<Move> &out_MoveList) {
    int startingRank;
    int promotionRank;
    int movementDirection;

    if(m_Squares[srcSquare] & SQ_WHITE) {
        startingRank = 1;
        promotionRank = 7;
        movementDirection = 0x10;
    } else  {
        startingRank = 6;
        promotionRank = 0;
        movementDirection = -0x10;
    }

    int movementDistance = 1;

    if(((srcSquare >> 4) & 0x7) == startingRank) {
        movementDistance = 2;
    }


    bool isPromotion = (((srcSquare + movementDirection) >> 4) == promotionRank);
    int effectiveGenerateFlags = generateFlags;
    if(isPromotion && (generateFlags & GEN_PROMOTIONS)) {
        // GEN_PROMOTIONS is handled a little special: We generate all
        // promotions, capturing and non-capturing, regardless of whether that
        // particular type of move would otherwise be generated.
        effectiveGenerateFlags |= GEN_CAPTURES | GEN_NONCAPTURES;
    }
    int movementGenerateFlags = effectiveGenerateFlags & GEN_NONCAPTURES;
    int captureGenerateFlags = effectiveGenerateFlags & GEN_CAPTURES;

    // Movement
    FindMovesInDirection(movementGenerateFlags, m_Squares[srcSquare], srcSquare, movementDirection, movementDistance, isPromotion, out_MoveList);

    // Captures
    FindMovesInDirection(captureGenerateFlags, m_Squares[srcSquare], srcSquare, movementDirection + 0x01, 1, isPromotion, out_MoveList);
    FindMovesInDirection(captureGenerateFlags, m_Squares[srcSquare], srcSquare, movementDirection - 0x01, 1, isPromotion, out_MoveList);

    // En Passant
    for(int dfile = -1; dfile <= 1; dfile += 2) {
        index_t targetSquare = srcSquare + movementDirection + dfile;

        if(targetSquare == m_EnPassantTargetSquare) {
            Move enPassant;
            enPassant.SrcSquare = srcSquare;
            enPassant.DestSquare = m_EnPassantTargetSquare;
            enPassant.IsCapture = true;
            enPassant.Promotion = SQ_EMPTY;
            enPassant.Score = 0;

            out_MoveList.push_back(enPassant);
        }
    }
}

void Board::FindCastlingMoves(index_t srcSquare, std::vector<Move> &out_MoveList) {
    if(m_WhiteToMove) {
        if(srcSquare == CoordsToIndex(0, 4)) {
            if(m_CastlingRights & CR_WHITE_KING_SIDE) {
                FindCastlingMovesHelper(srcSquare, 1, CoordsToIndex(0, 7), out_MoveList);
            }
            if(m_CastlingRights & CR_WHITE_QUEEN_SIDE) {
                FindCastlingMovesHelper(srcSquare, -1, CoordsToIndex(0, 0), out_MoveList);
            }
        }
    } else {
        if(srcSquare == CoordsToIndex(7, 4)) {
            if(m_CastlingRights & CR_BLACK_KING_SIDE) {
                FindCastlingMovesHelper(srcSquare, 1, CoordsToIndex(7, 7), out_MoveList);
            }
            if(m_CastlingRights & CR_BLACK_QUEEN_SIDE) {
                FindCastlingMovesHelper(srcSquare, -1, CoordsToIndex(7, 0), out_MoveList);
            }
        }
    }
}

void Board::FindCastlingMovesHelper(index_t kingStartSquare, int kingMovementDirection, index_t rookStartSquare, std::vector<Move> &out_MoveList) {
    index_t kingDestSquare = kingStartSquare + (kingMovementDirection * 2);

    // Check for empty squares
    if(m_Squares[kingStartSquare + kingMovementDirection] != SQ_EMPTY ||
       m_Squares[kingStartSquare + (kingMovementDirection * 2)] != SQ_EMPTY ||
       m_Squares[rookStartSquare - kingMovementDirection] != SQ_EMPTY ||
       m_Squares[rookStartSquare - (kingMovementDirection * 2)] != SQ_EMPTY) {

        return;
    }

    // Check for attacks
    if(IsAttacked(kingStartSquare) || IsAttacked(kingStartSquare + kingMovementDirection) || IsAttacked(kingDestSquare)) {
        return;
    }

    // Castling allowed
    Move m;
    m.SrcSquare = kingStartSquare;
    m.DestSquare = kingDestSquare;
    m.IsCapture = false;
    m.Promotion = SQ_EMPTY;
    m.Score = MOVE_SCORE_CASTLING;
    out_MoveList.push_back(m);
}

bool Board::IsAttacked(index_t square) {
    square_t enemyColor = m_WhiteToMove ? SQ_BLACK : SQ_WHITE;
    index_t directionToEnemyPawns = m_WhiteToMove ? 0x10 : -0x10;
    int pawnAttackDeltas[2] = {directionToEnemyPawns + 0x01, directionToEnemyPawns - 0x01};

    // Find knight attacks
    for(int knightVec : KNIGHT_VECTORS) {
        square_t attackingSquare = square + knightVec;
        if((attackingSquare & 0x88) == 0 && m_Squares[attackingSquare] == (enemyColor | SQ_KNIGHT)) {
            return true;
        }
    }

    // Find pawn attacks
    for(int pawnVec : pawnAttackDeltas) {
        square_t attackingSquare = square + pawnVec;
        if((attackingSquare & 0x88) == 0 && m_Squares[attackingSquare] == (enemyColor | SQ_PAWN)) {
            return true;
        }
    }

    // Find sliding piece attacks
    for(int i = 0; i < 8; ++i) {
        index_t direction = ORTHOGONAL_AND_DIAGONAL_VECTORS[i];
        bool isDiagonal = (0x80 >> i) & ORTHOGONAL_AND_DIAGONAL_VECTORS_IS_DIAGONAL_MASK;
        bool isHorizontal = !isDiagonal;
        for(int j = 1; j < 8; ++j) {
            index_t attackingSquare = square + (direction * j);
            if((attackingSquare & 0x88) != 0) {
                break; // next direction
            }
            square_t attackingPiece = m_Squares[attackingSquare];
            if(attackingPiece != SQ_EMPTY) {
                // We found something, what is it?
                if(j == 1 && attackingPiece == (enemyColor | SQ_KING)) {
                    return true;
                } else if(attackingPiece == (enemyColor | SQ_QUEEN)) {
                    return true;
                } else if (isDiagonal && attackingPiece == (enemyColor | SQ_BISHOP)) {
                    return true;
                } else if (isHorizontal && attackingPiece == (enemyColor | SQ_ROOK)) {
                    return true;
                }
                // If we get here: Not an attacker, but the square isn't empty, so we should
                // check a different direction
                break; // next direction
            } 
        }
    }

    return false;
}

Zobrist::hash_t Board::SquareHashCode(index_t square) {
    square_t contents = m_Squares[square];
    if(contents == SQ_EMPTY) {
        return 0;
    } else {
        int color = ((contents & SQ_COLORMASK) == SQ_WHITE) ? 0 : 1;
        int piece = (contents & SQ_PIECEMASK) - 1;
        int squareNumber = (square & 0x7) | ((square & 0x70) >> 1);

        assert(color >= 0 && color < 2);
        assert(squareNumber >= 0 && squareNumber < 64);
        assert(piece >= 0 && piece < 6);

        return Zobrist::Piece[color][piece][squareNumber];
    }
}

void Board::SetSquare(index_t square, square_t contents) {
    m_PieceHash ^= SquareHashCode(square); // clear old hash code
    m_Squares[square] = contents;
    m_PieceHash ^= SquareHashCode(square); // set new hash code
}

void Board::SetSquareWithUndo(index_t square, square_t contents, UndoMove& undo) {
    assert(undo.NumSquaresUpdated < 4);

    // Save square contents for undo
    undo.Squares[undo.NumSquaresUpdated] = square;
    undo.Contents[undo.NumSquaresUpdated] = m_Squares[square];
    undo.NumSquaresUpdated += 1;

    // Modify square
    SetSquare(square, contents);
}

bool Board::PieceListsConsistentWithBoard() const {
    int numPiecesInPieceLists = 0;

    // Verify that all pieces in the piece list match the board
    for(int color = 0; color < 2; ++color) {
        for(int pieceNumber = 0; pieceNumber < 6; ++pieceNumber) {
            int plIdx = PieceLocationsOffset(color == 0, pieceNumber);
            while(m_PieceLocations[plIdx] != PL_END_OF_LIST) {
                square_t expectedPiece = ((color == 0) ? SQ_WHITE : SQ_BLACK) | (pieceNumber + 1);
                if(m_Squares[m_PieceLocations[plIdx]] != expectedPiece) {
                    return false;
                }
                ++plIdx;
                ++numPiecesInPieceLists;
            }
        }
    }

    // Count the number of pieces on the board
    int numPiecesOnBoard = 0;
    for(int rank = 0; rank < 8; ++rank) {
        for(int file = 0; file < 8; ++file) {
            if(m_Squares[CoordsToIndex(rank, file)] != SQ_EMPTY) {
                ++numPiecesOnBoard;
            }
        }
    }

    // Verify that there are the same number of pieces on the board and in the
    // piece lists
    if(numPiecesInPieceLists != numPiecesOnBoard) {
        return false;
    }

    // No problems found
    return true;
}

int Board::PieceLocationsOffset(bool white, int pieceNumber) const {
    // Returns an index to the start of the piece list for the given piece
    return (white ? 0 : 60) + (pieceNumber * 10);
}

void Board::PieceListRemoveWithUndo(index_t location, UndoMove& undo) {
    square_t contents = m_Squares[location];
    assert(contents != SQ_EMPTY);

    // Update piece list
    int plIdx = PieceLocationsOffset((contents & SQ_COLORMASK) == SQ_WHITE, (contents & SQ_PIECEMASK) - 1);
    int piecePlIdx = -1;

    while(m_PieceLocations[plIdx] != PL_END_OF_LIST) {
        if(m_PieceLocations[plIdx] == location) {
            piecePlIdx = plIdx;
        }
        ++plIdx;
    }

    assert(piecePlIdx != -1);

    int lastPiecePlIdx = plIdx - 1;

    // To delete a piece from somewhere in the middle of the list, we move the
    // last piece in the list to overwrite the piece we are removing, and then
    // set the last element to PL_END_OF_LIST, which has the effect of reducing
    // the length of the list by one
    SetPieceLocationWithUndo(piecePlIdx, m_PieceLocations[lastPiecePlIdx], undo);
    SetPieceLocationWithUndo(lastPiecePlIdx, PL_END_OF_LIST, undo);
}

void Board::SetPieceLocationWithUndo(int index, index_t location, UndoMove& undo) {
    assert(undo.NumPieceLocationsUpdated < (int)(sizeof(undo.PieceLocations)/sizeof(undo.PieceLocations[0])));

    // Save old value for later undo
    undo.PieceLocationIndexes[undo.NumPieceLocationsUpdated] = index;
    undo.PieceLocations[undo.NumPieceLocationsUpdated] = m_PieceLocations[index];
    undo.NumPieceLocationsUpdated += 1;

    // Modify piece list
    m_PieceLocations[index] = location;
}

void Board::MovePieceWithUndo(index_t from, index_t to, UndoMove& undo) {
    assert(m_Squares[from] != SQ_EMPTY);

    square_t fromContents = m_Squares[from];
    int fromPiece = fromContents & SQ_PIECEMASK;
    int plIdx = PieceLocationsOffset((fromContents & SQ_COLORMASK) == SQ_WHITE, fromPiece - 1);

    // Update piece lists
    while(m_PieceLocations[plIdx] != PL_END_OF_LIST) {
        if(m_PieceLocations[plIdx] == from) {
            SetPieceLocationWithUndo(plIdx, to, undo);
            break;
        }
        // It would be an error if we got to the last piece and we still had not
        // found it.
        ++plIdx;
        assert(m_PieceLocations[plIdx] != PL_END_OF_LIST);
    }

    if(m_Squares[to] != SQ_EMPTY) {
        // Capture
        PieceListRemoveWithUndo(to, undo);
    }

    // Update squares
    SetSquareWithUndo(to, fromContents, undo);
    SetSquareWithUndo(from, SQ_EMPTY, undo);
}

void Board::PlaceNewPiece(index_t location, square_t contents) {
    // FIXME: This method is almost identical to PlaceNewPieceWithUndo(), would be
    // nice to refactor this somehow.
    assert(m_Squares[location] == SQ_EMPTY);

    // Update piece list
    int plIdx = PieceLocationsOffset((contents & SQ_COLORMASK) == SQ_WHITE, (contents & SQ_PIECEMASK) - 1);
    // Find end of the piece list
    while(m_PieceLocations[plIdx] != PL_END_OF_LIST) {
        ++plIdx;
    }
    m_PieceLocations[plIdx] = location;

    SetSquare(location, contents);
}

void Board::PlaceNewPieceWithUndo(index_t location, square_t contents, UndoMove& undo) {
    if(m_Squares[location] != SQ_EMPTY) {
        PieceListRemoveWithUndo(location, undo);
    }

    // Update piece list
    int plIdx = PieceLocationsOffset((contents & SQ_COLORMASK) == SQ_WHITE, (contents & SQ_PIECEMASK) - 1);
    // Find end of the piece list
    while(m_PieceLocations[plIdx] != PL_END_OF_LIST) {
        ++plIdx;
    }
    SetPieceLocationWithUndo(plIdx, location, undo);

    // Update square
    SetSquareWithUndo(location, contents, undo);
}

void Board::RemovePieceWithUndo(index_t location, UndoMove& undo) {
    // Update piece list
    PieceListRemoveWithUndo(location, undo);

    // Update square
    SetSquareWithUndo(location, SQ_EMPTY, undo);
}

bool Board::Make(Move m) {
    assert(m.SrcSquare >= 0 && m.SrcSquare < 128);
    assert(m.DestSquare >= 0 && m.DestSquare < 128);
    assert(PieceListsConsistentWithBoard());

    m_UndoStack.emplace_back();
    UndoMove& undo = m_UndoStack.back();

    bool legalMove = true;

    bool isPawnMove = ((m_Squares[m.SrcSquare] & SQ_PIECEMASK) == SQ_PAWN);
    bool isEnPassant = isPawnMove && (m.DestSquare == m_EnPassantTargetSquare);
    bool isKingMove = ((m_Squares[m.SrcSquare] & SQ_PIECEMASK) == SQ_KING);
    bool isCastling = isKingMove && ((m.SrcSquare - m.DestSquare) == 2 || (m.SrcSquare - m.DestSquare) == -2);
    bool isCapture = (m_Squares[m.DestSquare] != SQ_EMPTY);
    bool isRookMove = ((m_Squares[m.SrcSquare] & SQ_PIECEMASK) == SQ_ROOK);
    bool capturesRook = ((m_Squares[m.DestSquare] & SQ_PIECEMASK) == SQ_ROOK);

    // Save undo state
    undo.NumSquaresUpdated = 0;
    undo.NumPieceLocationsUpdated = 0;
    undo.PliesSincePawnMoveOrCapture = m_PliesSincePawnMoveOrCapture;
    undo.EnPassantTargetSquare = m_EnPassantTargetSquare;
    undo.CastlingRights = m_CastlingRights;
    undo.PieceHash = m_PieceHash;

    // Update 50 move rule counter
    if(isCapture || isPawnMove) {
        m_PliesSincePawnMoveOrCapture = 0;
    } else {
        ++m_PliesSincePawnMoveOrCapture;     
    }
    
    // Remove captured piece, if there is one
    if(isEnPassant) {
        int srcFile = m.SrcSquare & 0x07;
        int destFile = m.DestSquare & 0x07;
        int dfile = destFile - srcFile;
        index_t enemyPawnSquare = m.SrcSquare + dfile;

        RemovePieceWithUndo(enemyPawnSquare, undo);
    }

    if(isPawnMove && ((m.DestSquare >> 4) == 7 || (m.DestSquare >> 4) == 0)) {
        // Pawn promotion

        // Remove pawn
        RemovePieceWithUndo(m.SrcSquare, undo);

        // Place promoted piece
        square_t pieceAtDest = (m.Promotion & SQ_PIECEMASK) | (m_WhiteToMove ? SQ_WHITE : SQ_BLACK);
        PlaceNewPieceWithUndo(m.DestSquare, pieceAtDest, undo);
    } else {
        // Just a regular move or capture. If castling, this moves the king,
        // the rook will be moved later.
        MovePieceWithUndo(m.SrcSquare, m.DestSquare, undo);
    }

    m_EnPassantTargetSquare = 0x7F; // generally en passant will not be possible on the next move
    
    // Update en passant target square
    if(isPawnMove) {
        // Could be a two-square pawn move, need to check that to update m_EnPassantTargetSquare
        index_t midpoint = (m.SrcSquare + m.DestSquare) / 2;
        if((midpoint & 0xf) == (m.SrcSquare & 0xf)) {
            // It was a two-square pawn
            m_EnPassantTargetSquare = midpoint;
        }
    }

    // Handle castling
    if(isCastling) {
        // Find the rook square
        int kingMovementDirection = (m.DestSquare - m.SrcSquare) > 0 ? 1 : -1;
        index_t rookDestSquare = m.DestSquare - kingMovementDirection;
        // account for king side vs queen side
        bool isKingSide = (m.DestSquare & 0x7) == 6;
        index_t rookSrcSquare = isKingSide ? (m.DestSquare + 1) : (m.DestSquare - 2);

        // Move the rook
        assert(m_Squares[rookSrcSquare] == (SQ_ROOK | (m_WhiteToMove ? SQ_WHITE : SQ_BLACK)));
        assert(m_Squares[rookDestSquare] == SQ_EMPTY);
        MovePieceWithUndo(rookSrcSquare, rookDestSquare, undo);
    }

    // Update castling rights
    if(isKingMove) {
        if(m_WhiteToMove) {
            m_CastlingRights &= ~(CR_WHITE_KING_SIDE | CR_WHITE_QUEEN_SIDE);
        } else {
            m_CastlingRights &= ~(CR_BLACK_KING_SIDE | CR_BLACK_QUEEN_SIDE);
        }
    }

    // Handle rook moves or captures that remove castling rights
    {
        if(isRookMove) {
            // We can't castle using a rook that is moved
            MarkRookIneligibleForCastling(m_WhiteToMove, m.SrcSquare);
        }
        
        if (capturesRook) {
            // We can't castle using a rook that was captured (even if another
            // rook ends up on the same square later, with the king unmoved)
            MarkRookIneligibleForCastling(!m_WhiteToMove, m.DestSquare);
        }
    }

    // ALMOST done...time to actually check if this was a legal move
    // NB: We do this *before* changing m_WhiteToMove, so InCheck() looks at the
    // right player.
    if(InCheck()) {
        legalMove = false;
        Unmake();
    }

    // XXX: Even if there was an undo above, that toggled the player to
    // the wrong color, so we need to undo that again here.
    m_WhiteToMove = !m_WhiteToMove;

    return legalMove;
}

void Board::Unmake() {
    UndoMove& undo = m_UndoStack.back();

    m_WhiteToMove = !m_WhiteToMove;
    m_PieceHash = undo.PieceHash;
    m_EnPassantTargetSquare = undo.EnPassantTargetSquare;
    m_CastlingRights = undo.CastlingRights;
    m_PliesSincePawnMoveOrCapture = undo.PliesSincePawnMoveOrCapture;

    // Set squares directly, bypassing hash updates, since we set that above
    for(int i = 0; i < undo.NumSquaresUpdated; ++i) {
        m_Squares[undo.Squares[i]] = undo.Contents[i];
    }

    // Restore piece lists
    for(int i = 0; i < undo.NumPieceLocationsUpdated; ++i) {
        m_PieceLocations[undo.PieceLocationIndexes[i]] = undo.PieceLocations[i];
    }

    m_UndoStack.pop_back();
}

void Board::MarkRookIneligibleForCastling(bool rookIsWhite, square_t rookSquare) {
    // Note that rookSquare doesn't have to be a corner square, in which case
    // we do nothing.
    if(rookIsWhite) {
        if(rookSquare == 0x00) {
            m_CastlingRights &= ~CR_WHITE_QUEEN_SIDE;
        } else if (rookSquare == 0x07) {
            m_CastlingRights &= ~CR_WHITE_KING_SIDE;
        }
    } else {
        if(rookSquare == 0x70) {
            m_CastlingRights &= ~CR_BLACK_QUEEN_SIDE;
        } else if (rookSquare == 0x77) {
            m_CastlingRights &= ~CR_BLACK_KING_SIDE;
        }
    }
}

Board::index_t Board::CoordsToIndex(int rank, int file) {
    return (rank << 4) | file;
}

void Board::ParseFen(const std::string &fen) {
    int rank = 7;
    int file = 0;
    std::size_t i = 0;

    // Clear board
    memset(m_Squares, SQ_EMPTY, sizeof(m_Squares));
    m_PieceHash = 0;

    // Clear piece lists
    memset(m_PieceLocations, PL_END_OF_LIST, sizeof(m_PieceLocations));

    // Clear undo stack, otherwise this could grow without limit
    m_UndoStack.clear();

    // Parse board portion of FEN
    while(!(rank == 0 && file > 7)) {
        square_t piece = SQ_EMPTY;
        int skip = 0;
        switch(fen[i++]) {
            case 'P': piece = SQ_WHITE | SQ_PAWN; break;
            case 'p': piece = SQ_BLACK | SQ_PAWN; break;
            case 'N': piece = SQ_WHITE | SQ_KNIGHT; break;
            case 'n': piece = SQ_BLACK | SQ_KNIGHT; break;
            case 'B': piece = SQ_WHITE | SQ_BISHOP; break;
            case 'b': piece = SQ_BLACK | SQ_BISHOP; break;
            case 'R': piece = SQ_WHITE | SQ_ROOK; break;
            case 'r': piece = SQ_BLACK | SQ_ROOK; break;
            case 'Q': piece = SQ_WHITE | SQ_QUEEN; break;
            case 'q': piece = SQ_BLACK | SQ_QUEEN; break;
            case 'K': piece = SQ_WHITE | SQ_KING; break;
            case 'k': piece = SQ_BLACK | SQ_KING; break;
            case '1': skip = 1; break;
            case '2': skip = 2; break;
            case '3': skip = 3; break;
            case '4': skip = 4; break;
            case '5': skip = 5; break;
            case '6': skip = 6; break;
            case '7': skip = 7; break;
            case '8': skip = 8; break;
            case '/': file = 0; --rank; break;
            default:
                assert(false);
                break;
        }

        if(piece != SQ_EMPTY) {
            PlaceNewPiece(CoordsToIndex(rank, file++), piece);
        } else {
            file += skip;
        }
    }

    while(fen[i] == ' ') { ++i; }

    // Side to move
    char sideToMove = fen[i++];
    m_WhiteToMove = (sideToMove == 'w');

    while(fen[i] == ' ') { ++i; }

    // Castling rights
    m_CastlingRights = 0;
    while(fen[i] != ' ') {
        switch(fen[i++]) {
            case 'K': m_CastlingRights |= CR_WHITE_KING_SIDE; break;
            case 'Q': m_CastlingRights |= CR_WHITE_QUEEN_SIDE; break;
            case 'k': m_CastlingRights |= CR_BLACK_KING_SIDE; break;
            case 'q': m_CastlingRights |= CR_BLACK_QUEEN_SIDE; break;
            case '-': break;
            default: assert(false); break;
        }
    }

    while(fen[i] == ' ') { ++i; }

    // En Passant target square
    if(fen[i] == '-') {
        // en passant not valid
        ++i;
        m_EnPassantTargetSquare = 0x7F;
    } else {
        char fileLetter = fen[i++];
        char rankNumber = fen[i++];
        // TODO: Should check that these characters are in the expected range
        int enPassantRank = rankNumber - '1';
        int enPassantFile = fileLetter - 'a';
        m_EnPassantTargetSquare = CoordsToIndex(enPassantRank, enPassantFile);
    }

    while(fen[i] == ' ') { ++i; }

    if(i == fen.size()) {
        // No more fields, quit parsing
        return;
    }

    // Halfmove clock of reversible moves (for the purposes of the 50 move rule)
    {
        std::string halfMoveClockStr;
        while(fen[i] != ' ') { halfMoveClockStr.push_back(fen[i++]); }
        m_PliesSincePawnMoveOrCapture = std::stoi(halfMoveClockStr);
    }

    // Fullmove clock
    // FIXME
}

int Board::StaticEvaluation() {
    // TODO: This function has gotten quite large, would be nice to split it
    // up somehow. I would also like to the reduce the occurence of code that
    // looks like:
    //
    //     if(white) { ... } else if (black) { ... }
    //
    // And instead prefer more color agnostic code.

    int scores[2] = {0, 0};
    int pawnsOnFile[2][8] = {{0}};
    int pawnMinRank[2][8] = {{7,7,7,7,7,7,7,7},{7,7,7,7,7,7,7,7}};
    int pawnMaxRank[2][8] = {{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0}};
    int kingRank[2] = {-1, -1};
    int kingFile[2] = {-1, -1};

    // Piece square tables and counting pawns on files
    // TODO: We're could be using piece lists more effectively here. We don't
    // need to save the locations of the kings anymore, and we could have special
    // loops to process only the pawns, etc.
    for(int colorIdx = 0; colorIdx < 2; ++colorIdx) {
        for(int pieceIdx = 0; pieceIdx < 6; ++pieceIdx) {
            int plIdx = PieceLocationsOffset(colorIdx == 0, pieceIdx);

            while(m_PieceLocations[plIdx] != PL_END_OF_LIST) {
                index_t location = m_PieceLocations[plIdx];
                square_t piece = m_Squares[location];
                assert(piece != SQ_EMPTY);

                int rank = (location >> 4) & 0x7;
                int file = (location >> 0) & 0x7;
                int squareIdx;
                if(colorIdx == 0) {
                    // white
                    squareIdx = ((7 - rank) * 8) + file;
                } else {
                    // black
                    squareIdx = (rank * 8) + file;
                }
                
                scores[colorIdx] += PIECE_VALUES[pieceIdx];
                scores[colorIdx] += PIECE_ON_SQUARE_VALUES[pieceIdx][squareIdx];

                if((piece & SQ_PIECEMASK) == SQ_PAWN) {
                    pawnsOnFile[colorIdx][file] += 1;
                    pawnMinRank[colorIdx][file] = std::min(pawnMinRank[colorIdx][file], rank);
                    pawnMaxRank[colorIdx][file] = std::max(pawnMaxRank[colorIdx][file], rank);
                }

                if((piece & SQ_PIECEMASK) == SQ_KING) {
                    assert(kingRank[colorIdx] == -1 && kingFile[colorIdx] == -1);
                    kingRank[colorIdx] = rank;
                    kingFile[colorIdx] = file;
                }

                ++plIdx;
            }
        }
    }

    // Both kings should have been found
    assert(kingRank[0] != -1 && kingFile[0] != -1 && kingRank[1] != -1 && kingFile[1] != -1);

    // Calculate board features, which are always from white's perspective.
    // (For example, if white has two doubled pawns, and black has three, the
    // number of doubled pawns would be -1)
    for(int i = 0; i < NUM_FEATURES; ++i) {
        m_Features[i] = 0;
    }

    for(int colorIdx = 0; colorIdx < 2; ++colorIdx) {
        int otherColorIdx = !colorIdx;
        int inc = colorIdx == 0 ? 1 : -1;
        int backRank = colorIdx == 0 ? 0 : 7;
        int pawnAdvancementDirection = colorIdx == 0 ? 1 : -1;

        // Doubled/Isolated/Passed pawns
        for(int file = 0; file < 8; ++file) {
            int pawnsOnThisFile = pawnsOnFile[colorIdx][file];

            if(pawnsOnThisFile == 0) {
                continue;
            }

            // Doubled pawns
            if(pawnsOnThisFile >= 2) {
                m_Features[(int)Feature::DOUBLED_PAWNS] += (pawnsOnThisFile * inc);
            }

            // Check two neighboring files to see if we are isolated
            bool isolated = true;
            for(int adjacentFile = std::max(file - 1, 0); adjacentFile <= std::min(file + 1, 7); adjacentFile += 2) {
                if(pawnsOnFile[colorIdx][adjacentFile] > 0) {
                    isolated = false;
                    break;
                }
            }

            if(isolated) {
                m_Features[(int)Feature::ISOLATED_PAWNS] += (pawnsOnThisFile * inc);
            }

            // Passed pawns. This calculation is a bit incomplete, we only test
            // the most advanced pawn in each file, if there are multiple passed
            // pawns in a file we will only register a single one as passed.
            bool passed = true;
            for(int otherFile = std::max(file - 1, 0); otherFile <= std::min(file + 1, 7); ++otherFile) {
                if(colorIdx == 0) {
                    if(pawnMaxRank[colorIdx][file] < pawnMaxRank[otherColorIdx][otherFile]) {
                        // There is a neighboring black pawn that is closer to
                        // rank 8 than our white pawn, so it is not passed
                        passed = false;
                        break;
                    }
                } else {
                    if(pawnMinRank[colorIdx][file] > pawnMinRank[otherColorIdx][otherFile]) {
                        // There is a neighboring white pawn that is closer to
                        // rank 1 than our black pawn, so it is not passed
                        passed = false;
                        break;
                    }
                }
            }

            if(passed) {
                m_Features[(int)Feature::PASSED_PAWNS] += inc;
            }
        }

        // King pawn shield
        int* myLeastAdvancedPawn = (colorIdx == 0) ? pawnMinRank[0] : pawnMaxRank[1];
        if(kingRank[colorIdx] == backRank) {
            bool considerShield = false;
            int startFile;
            int endFile;

            if(kingFile[colorIdx] > 4) {
                considerShield = true;
                startFile = 5;
                endFile = 7;
            } else if(kingFile[colorIdx] < 3) {
                considerShield = true;
                startFile = 0;
                endFile = 2;
            }

            if(considerShield) {
                for(int file = startFile; file <= endFile; ++file) {
                    if(pawnsOnFile[colorIdx][file] == 0) {
                        m_Features[(int)Feature::PAWN_SHIELD_MISSING] += inc;
                    } else if(myLeastAdvancedPawn[file] == (backRank + pawnAdvancementDirection)) {
                        m_Features[(int)Feature::PAWN_SHIELD_UNADVANCED] += inc;
                    } else if(myLeastAdvancedPawn[file] == (backRank + pawnAdvancementDirection*2)) {
                        m_Features[(int)Feature::PAWN_SHIELD_ADVANCED_1] += inc;
                    }
                }
            }
        }
    }

    int featureScore = 0;
    for(int i = 0; i < NUM_FEATURES; ++i) {
        featureScore += m_Features[i] * FEATURE_VALUES[i];
    }

    return (scores[0] - scores[1] + featureScore) * (m_WhiteToMove ? 1 : -1);
}

std::vector<int> Board::EvaluationFeatures() {
    // Kind of an ugly structure here: We call StaticEvaluation() for the side
    // effect of populating m_Features
    StaticEvaluation();

    return m_Features;
}

bool Board::InCheck() {
    // FIXME: SQ_KING - 1 is an ugly construct, and there are other places that
    // play fast and loose with "piece numbers" vs the SQ_ constants. Need to
    // harnomize the various usages somehow.
    int kingPlIdx = PieceLocationsOffset(m_WhiteToMove, SQ_KING - 1);

    // There should be exactly one king
    assert(m_PieceLocations[kingPlIdx] != PL_END_OF_LIST);
    assert(m_PieceLocations[kingPlIdx+1] == PL_END_OF_LIST);

    return IsAttacked(m_PieceLocations[kingPlIdx]);
}

Board::Move Board::ParseMove(const std::string &moveStr) {
    Move m;

    m.SrcSquare = CoordsToIndex(moveStr[1] - '1', moveStr[0] - 'a');
    m.DestSquare = CoordsToIndex(moveStr[3] - '1', moveStr[2] - 'a');
    m.IsCapture = false; // FIXME: This is a garbage value, it might be a capture for all we know
    m.Promotion = SQ_EMPTY;

    if(moveStr.size() == 5) {
        switch(moveStr[4]) {
            case 'b': m.Promotion = SQ_BISHOP; break;
            case 'n': m.Promotion = SQ_KNIGHT; break;
            case 'r': m.Promotion = SQ_ROOK; break;
            case 'q': m.Promotion = SQ_QUEEN; break;
            default: assert(false); break;
        }
    }

    return m;
}

std::string Board::Move::ToString() const {
    std::string out;

    out.push_back('a' + ((SrcSquare >> 0) & 0x7));
    out.push_back('1' + ((SrcSquare >> 4) & 0x7));
    out.push_back('a' + ((DestSquare >> 0) & 0x7));
    out.push_back('1' + ((DestSquare >> 4) & 0x7));

    switch(Promotion & SQ_PIECEMASK) {
        case SQ_BISHOP: out.push_back('b'); break;
        case SQ_KNIGHT: out.push_back('n'); break;
        case SQ_ROOK: out.push_back('r'); break;
        case SQ_QUEEN: out.push_back('q'); break;
        default: break;
    }

    return out;
}

void Board::Test() {
#ifndef NDEBUG
    // Test Zobrist hashing
    {
        Board board;
        board.ParseFen("7k/8/8/8/8/8/7P/3QKBNR w K - 0 1");

        Zobrist::hash_t originalHash =
            Zobrist::Piece[0][SQ_QUEEN - 1][3] ^
            Zobrist::Piece[0][SQ_KING - 1][4] ^
            Zobrist::Piece[0][SQ_BISHOP - 1][5] ^
            Zobrist::Piece[0][SQ_KNIGHT - 1][6] ^
            Zobrist::Piece[0][SQ_ROOK - 1][7] ^
            Zobrist::Piece[0][SQ_PAWN - 1][15] ^
            Zobrist::Piece[1][SQ_KING - 1][63] ^
            Zobrist::WhiteToMove ^
            Zobrist::CastlingRights[CR_WHITE_KING_SIDE];
        
        assert(board.Hash() == originalHash);

        Move move = ParseMove("h2h4");
        Zobrist::hash_t hashAfterMove = originalHash ^
            Zobrist::Piece[0][SQ_PAWN - 1][15] ^ // remove pawn from old square
            Zobrist::Piece[0][SQ_PAWN - 1][31] ^ // place pawn on new square
            Zobrist::EnPassantFile[7] ^ // En passant is available on file 7
            Zobrist::WhiteToMove; // toggle white to move off
        
        board.Make(move);

        assert(board.Hash() == hashAfterMove);

        board.Unmake();

        // Hash should have reverted back to the pre-move value
        assert(board.Hash() == originalHash);
    }

    // En passant square
    {
        Board board;
        board.ParseFen("r1bqnrk1/pp2npbp/3p2p1/2pPp3/2P1P3/2N1B3/PP2BPPP/R2QNRK1 w - c6");
        assert(board.m_EnPassantTargetSquare == 0x52);
    }

    // Castling rights
    {
        Board board;
        board.ParseFen("r2qk2r/1b3ppp/2pbpn2/8/1p1P4/3BPN2/1P3PPP/R1BQ1RK1 w kq - 0 1");
        assert(board.m_CastlingRights == (CR_BLACK_KING_SIDE | CR_BLACK_QUEEN_SIDE));

        board.Make(Board::ParseMove("a1a8"));

        // Should remove castling rights even if rook captures rook
        assert(board.m_CastlingRights == CR_BLACK_KING_SIDE);
    }
#endif
}