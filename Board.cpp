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

    // We need to be careful with our indices here: m_NumReversiblePlies could
    // come from a FEN position, and doesn't necessary mean there are that
    // many moves in the undo stack.
    if(m_NumReversiblePlies >= 4) {
        // We only have to look at plies where it was our turn to move
        int pliesToExamine = m_NumReversiblePlies / 2;
        
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
    return m_NumReversiblePlies >= 100;
}

void Board::FindPseudoLegalMoves(std::vector<Move> &out_MoveList) {
    out_MoveList.clear();

    square_t expectedColorMask = m_WhiteToMove ? SQ_WHITE : SQ_BLACK;

    for(index_t srcSquare = 0; srcSquare < 128; ++srcSquare) {
        if((srcSquare & 0x88) == 0 && m_Squares[srcSquare] != SQ_EMPTY && (m_Squares[srcSquare] & SQ_COLORMASK) == expectedColorMask)  {
            square_t piece = m_Squares[srcSquare];
            switch(piece & SQ_PIECEMASK) {
                case SQ_PAWN:
                    FindPawnMoves(srcSquare, out_MoveList);
                    break;
                case SQ_ROOK:
                    for(int vector : ORTHOGONAL_VECTORS) {
                        FindMovesInDirection(piece, srcSquare, vector, 8, true, true, false, out_MoveList);
                    }
                    break;
                case SQ_KNIGHT:
                    for(int vector : KNIGHT_VECTORS) {
                        FindMovesInDirection(piece, srcSquare, vector, 1, true, true, false, out_MoveList);
                    }
                    break;
                case SQ_BISHOP:
                    for(int vector : DIAGONAL_VECTORS) {
                        FindMovesInDirection(piece, srcSquare, vector, 8, true, true, false, out_MoveList);
                    }
                    break;
                case SQ_QUEEN:
                    for(int vector : ORTHOGONAL_AND_DIAGONAL_VECTORS) {
                        FindMovesInDirection(piece, srcSquare, vector, 8, true, true, false, out_MoveList);
                    }
                    break;
                case SQ_KING:
                    for(int vector : ORTHOGONAL_AND_DIAGONAL_VECTORS) {
                        FindMovesInDirection(piece, srcSquare, vector, 1, true, true, false, out_MoveList);
                    }

                    // Check for castling
                    FindCastlingMoves(srcSquare, out_MoveList);

                    break;
            }
        }
    }
}

void Board::FindMovesInDirection(square_t piece, index_t srcSquare, int direction, int slideDistance, bool movementAllowed, bool capturesAllowed, bool promotionAllowed, std::vector<Move> &out_MoveList) {
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

        if(isCapture && !capturesAllowed) {
            break;
        }

        if(!isCapture && !movementAllowed) {
            break;
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

        if(promotionAllowed) {
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

void Board::FindPawnMoves(index_t srcSquare, std::vector<Move> &out_MoveList) {
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

    bool promotionAllowed = (((srcSquare + movementDirection) >> 4) == promotionRank);
    // Movement
    FindMovesInDirection(m_Squares[srcSquare], srcSquare, movementDirection, movementDistance, true, false, promotionAllowed, out_MoveList);

    // Captures
    FindMovesInDirection(m_Squares[srcSquare], srcSquare, movementDirection + 0x01, movementDistance, false, true, promotionAllowed, out_MoveList);
    FindMovesInDirection(m_Squares[srcSquare], srcSquare, movementDirection - 0x01, movementDistance, false, true, promotionAllowed, out_MoveList);

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

void Board::SetSquareWithUndo(index_t square, square_t contents, UndoMove &undo, int undoIndex) {
    // Save square contents for undo
    undo.Squares[undoIndex] = square;
    undo.Contents[undoIndex] = m_Squares[square];

    // Modify square
    SetSquare(square, contents);
}

bool Board::Make(Move m) {
    assert(m.SrcSquare >= 0 && m.SrcSquare < 128);
    assert(m.DestSquare >= 0 && m.DestSquare < 128);

    m_UndoStack.emplace_back();
    UndoMove& undo = m_UndoStack.back();

    bool legalMove = true;
    int undoIndex = 0;

    bool isPawnMove = ((m_Squares[m.SrcSquare] & SQ_PIECEMASK) == SQ_PAWN);
    bool isEnPassant = isPawnMove && (m.DestSquare == m_EnPassantTargetSquare);
    bool isKingMove = ((m_Squares[m.SrcSquare] & SQ_PIECEMASK) == SQ_KING);
    bool isCastling = isKingMove && ((m.SrcSquare - m.DestSquare) == 2 || (m.SrcSquare - m.DestSquare) == -2);
    bool isCapture = (m_Squares[m.DestSquare] != SQ_EMPTY);
    bool isRookMove = ((m_Squares[m.SrcSquare] & SQ_PIECEMASK) == SQ_ROOK);
    bool capturesRook = ((m_Squares[m.DestSquare] & SQ_PIECEMASK) == SQ_ROOK);

    // Save undo state
    undo.NumReversiblePlies = m_NumReversiblePlies;
    undo.EnPassantTargetSquare = m_EnPassantTargetSquare;
    undo.CastlingRights = m_CastlingRights;
    undo.PieceHash = m_PieceHash;

    // Update 50 move rule counter
    if(isCapture || isPawnMove) {
        m_NumReversiblePlies = 0;
    } else {
        ++m_NumReversiblePlies;     
    }

    // Handle promotion
    square_t pieceAtDest;
    if(isPawnMove && ((m.DestSquare >> 4) == 7 || (m.DestSquare >> 4) == 0)) {
        pieceAtDest = (m.Promotion & SQ_PIECEMASK) | (m_WhiteToMove ? SQ_WHITE : SQ_BLACK);
    } else {
        pieceAtDest = m_Squares[m.SrcSquare];
    }

    // Do move
    SetSquareWithUndo(m.DestSquare, pieceAtDest, undo, undoIndex++);
    SetSquareWithUndo(m.SrcSquare, SQ_EMPTY, undo, undoIndex++);
    m_EnPassantTargetSquare = 0x7F; // generally en passant will not be possible on the next move
    
    // Handle en passant
    if(isEnPassant) {
        // Need to remove enemy pawn from board
        int srcFile = m.SrcSquare & 0x07;
        int destFile = m.DestSquare & 0x07;
        int dfile = destFile - srcFile;
        index_t enemyPawnSquare = m.SrcSquare + dfile;

        SetSquareWithUndo(enemyPawnSquare, SQ_EMPTY, undo, undoIndex++);
    }
    
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

        SetSquareWithUndo(rookDestSquare, m_Squares[rookSrcSquare], undo, undoIndex++);
        SetSquareWithUndo(rookSrcSquare, SQ_EMPTY, undo, undoIndex++);
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
        square_t rookSquare = 0x7F;
        bool considerWhite;
        if(isRookMove) {
            rookSquare = m.SrcSquare;
            considerWhite = m_WhiteToMove;
        } else if (capturesRook) {
            rookSquare = m.DestSquare;
            considerWhite = !m_WhiteToMove;
        }
        if((rookSquare & 0x88) == 0) {
            if(considerWhite) {
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
    }

    while(undoIndex < 4) {
        // Direct the remaining undo writes to a garbage square
        undo.Squares[undoIndex++] = 0x7F;
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
    m_NumReversiblePlies = undo.NumReversiblePlies;

    // Set squares directly, bypassing hash updates, since we set that above
    for(int i = 0; i < 4; ++i) {
        m_Squares[undo.Squares[i]] = undo.Contents[i];
    }

    m_UndoStack.pop_back();
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
            SetSquare(CoordsToIndex(rank, file++), piece);
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
        char rankLetter = fen[i++];
        char fileNumber = fen[i++];
        // TODO: Should check that these characters are in the expected range
        int enPassantRank = rankLetter - 'a';
        int enPassantFile = fileNumber - '1';
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
        m_NumReversiblePlies = std::stoi(halfMoveClockStr);
    }

    // Fullmove clock
    // FIXME
}

int Board::StaticEvaluation() {
    int scores[2] = {0, 0};
    int pawnsOnFile[2][8] = {{0}};
    int pawnMinRank[2][8] = {{7,7,7,7,7,7,7,7},{7,7,7,7,7,7,7,7}};
    int pawnMaxRank[2][8] = {{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0}};

    // Piece square tables and counting pawns on files
    for(int rank = 0; rank < 8; ++rank) {
        for(int file = 0; file < 8; ++file) {
            square_t piece = m_Squares[CoordsToIndex(rank, file)];
            if(piece != SQ_EMPTY) {
                int colorIdx = (piece & SQ_COLORMASK) == SQ_WHITE ? 0 : 1;
                int pieceIdx = (piece & SQ_PIECEMASK) - 1;
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
            }
        }
    }

    // Calculate board features, which are always from white's perspective.
    // (For example, if white has two doubled pawns, and black has three, the
    // number of doubled pawns would be -1)
    for(int i = 0; i < NUM_FEATURES; ++i) {
        m_Features[i] = 0;
    }

    for(int colorIdx = 0; colorIdx < 2; ++colorIdx) {
        int otherColorIdx = !colorIdx;
        int inc = colorIdx == 0 ? 1 : -1;

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
    square_t myKing = (m_WhiteToMove ? SQ_WHITE : SQ_BLACK) | SQ_KING;
    // FIXME: Shouldn't have to scan the whole board to find our king
    for(int rank = 0; rank < 8; ++rank) {
        for(int file = 0; file < 8; ++file) {
            square_t index = CoordsToIndex(rank, file);
            if(m_Squares[index] == myKing) {
                return IsAttacked(index);
            }
        }
    }

    // Should never get here
    assert(false);

    return false;
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
#endif
}