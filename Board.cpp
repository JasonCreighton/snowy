// Copyright (c) 2017 Jason Creighton
// Available under the MIT license, see included LICENSE file for details

#include "Common.hpp"
#include "Board.hpp"
#include "IO.hpp"
#include "Util.hpp"


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
    const char* pieceMap = "PNBRQK.";
    const char* fileLabels = "  a b c d e f g h";

    IO::PutLine(fileLabels);
    for(int rank = 7; rank >= 0; --rank) {
        std::string line = std::to_string(rank + 1) + " ";
        for(int file = 0; file < 8; ++file) {
            piece_t piece = m_Pieces[Square::FromCoords(rank, file)];
            char pieceLetter = pieceMap[Piece::Index(piece)];
            if(piece != Piece::NONE && Piece::Color(piece) == Color::BLACK) {
                // HACKY
                pieceLetter = (char)std::tolower((int)pieceLetter);
            }

            line.push_back(pieceLetter);
            line.push_back(' ');
        }
        line += std::to_string(rank + 1);
        IO::PutLine(line);
    }
    IO::PutLine(fileLabels);
    IO::PutLine("Hash: " + IntegerToHexString(Hash()));
}

Zobrist::hash_t Board::Hash() const {
    bool enPassantAvailable = m_EnPassantTargetSquare != SQ_NONE;
    int enPassantFile = Square::File(m_EnPassantTargetSquare);

    return
        m_PieceHash ^
        Zobrist::SideToMove[m_SideToMove] ^
        Zobrist::CastlingRights[m_CastlingRights] ^
        (enPassantAvailable ? Zobrist::EnPassantFile[enPassantFile] : 0);
}

bool Board::WhiteToMove() const {
    return m_SideToMove == Color::WHITE;
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

template<int GenFlags>
void Board::FindPseudoLegalMoves(std::vector<Move> &out_MoveList) {
    out_MoveList.clear();

    auto genMove = [&](Move m) {
        out_MoveList.push_back(m);
    };

    for(int pieceIndex = 0; pieceIndex < 6; ++pieceIndex) {
        ForEachPiece(m_SideToMove, pieceIndex, [&](square_t srcSquare) {
            FindMovesFromSquare<GenFlags>(pieceIndex, srcSquare, genMove);
        });
    }
}

template<int GenFlags, typename GenFunction>
void Board::FindMovesFromSquare(int pieceIndex, square_t srcSquare, GenFunction genMove) {
    switch(pieceIndex) {
        case Piece::PAWN:
            FindPawnMoves<GenFlags>(srcSquare, genMove);
            break;
        
        case Piece::KNIGHT:
            for(int vector : KNIGHT_VECTORS) {
                FindMovesInDirection<GenFlags>(srcSquare, vector, 1, genMove);
            }
            break;
        
        case Piece::BISHOP:
            for(int vector : DIAGONAL_VECTORS) {
                FindMovesInDirection<GenFlags>(srcSquare, vector, 8, genMove);
            }
            break;
        
        case Piece::ROOK:
            for(int vector : ORTHOGONAL_VECTORS) {
                FindMovesInDirection<GenFlags>(srcSquare, vector, 8, genMove);
            }
            break;
        
        case Piece::QUEEN:
            for(int vector : ORTHOGONAL_AND_DIAGONAL_VECTORS) {
                FindMovesInDirection<GenFlags>(srcSquare, vector, 8, genMove);
            }
            break;
        
        case Piece::KING:
            for(int vector : ORTHOGONAL_AND_DIAGONAL_VECTORS) {
                FindMovesInDirection<GenFlags>(srcSquare, vector, 1, genMove);
            }

            // Check for castling
            if(GenFlags & GEN_NONCAPTURES) {
                FindCastlingMoves(srcSquare, genMove);
            }
            break;
    }
}

template<int GenFlags, typename GenFunction>
void Board::FindMovesInDirection(square_t srcSquare, int direction, int slideDistance, GenFunction genMove) {
    for(square_t destSquare = srcSquare + direction; Square::OnBoard(destSquare) && slideDistance > 0; destSquare += direction, --slideDistance) {
        piece_t destContents = m_Pieces[destSquare];
        bool isCapture = false;

        if(destContents == Piece::NONE) {
            // Empty square, everything is fine
        } else if(Piece::Color(destContents) == m_SideToMove) {
            // Same color piece, we can't do anything
            break;
        } else {
            // Opposite color piece
            isCapture = true;
        }

        bool generateMove =
            ((GenFlags & GEN_CAPTURES) && isCapture) ||
            ((GenFlags & GEN_NONCAPTURES) && !isCapture);
        
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
            assert(destContents != Piece::NONE);
            assert(Piece::Index(destContents) != Piece::KING);
        }

        Move m;
        m.SrcSquare = srcSquare;
        m.DestSquare = destSquare;
        m.IsCapture = isCapture;
        m.Promotion = Piece::NONE;
        m.Score = 0;

        if(m.IsCapture) {
            int victimPieceIndex = Piece::Index(m_Pieces[m.DestSquare]); // [0, 5]
            int aggressorPieceIndex = Piece::Index(m_Pieces[m.SrcSquare]); // [0, 5]
            // In MVV/LVA, the victim dominates the ordering, so, eg, RxR is searched before PxB
            // Scores here range from 10 to 65
            int MVV = 10 * (victimPieceIndex + 1);
            int LVA = (5 - aggressorPieceIndex);
            m.Score = MVV + LVA;
        }

        genMove(m);
        
        if(isCapture) {
            break;
        }
    }
}

template<int GenFlags, typename GenFunction>
void Board::FindPawnMoves(square_t srcSquare, GenFunction genMove) {
    int startingRank;
    int promotionRank;
    int movementDirection;

    if(Piece::Color(m_Pieces[srcSquare]) == Color::WHITE) {
        startingRank = 1;
        promotionRank = 7;
        movementDirection = 0x10;
    } else  {
        startingRank = 6;
        promotionRank = 0;
        movementDirection = -0x10;
    }

    int movementDistance = 1;

    if(Square::Rank(srcSquare) == startingRank) {
        movementDistance = 2;
    }

    bool isPromotion = (Square::Rank(srcSquare + movementDirection) == promotionRank);

    // For pawn moves, we have to sometimes generate promotions, so we use our
    // own GenFunction that piggy-backs on the GenFunction that was passed in.
    auto pawnGenMove = [&](Move move) {
        if(isPromotion) {
            static const int pieceIndexes[4] = {Piece::BISHOP, Piece::KNIGHT, Piece::ROOK, Piece::QUEEN};
            for(int pieceIndex : pieceIndexes) {
                Move promotionMove = move;

                promotionMove.Promotion = pieceIndex;
                // Could be both a capture and a promotion, so we modify the previous score.
                // We only boost the score of queen promotions, underpromotions
                // are probably no better than any other move
                if(pieceIndex == Piece::QUEEN) {
                    promotionMove.Score += 100;
                }
                genMove(promotionMove);
            }
        } else {
            genMove(move);
        }
    };

    // GEN_PROMOTIONS is handled a little special: We generate all
    // promotions, capturing and non-capturing, regardless of whether that
    // particular type of move would otherwise be generated.
    bool generateAll = isPromotion && (GenFlags & GEN_PROMOTIONS);
    bool generateMovement = (GenFlags & GEN_NONCAPTURES) || generateAll;
    bool generateCaptures = (GenFlags & GEN_CAPTURES) || generateAll;

    if(generateMovement) {
        FindMovesInDirection<GEN_NONCAPTURES>(srcSquare, movementDirection, movementDistance, pawnGenMove);
    }

    if(generateCaptures) {
        FindMovesInDirection<GEN_CAPTURES>(srcSquare, movementDirection + 0x01, 1, pawnGenMove);
        FindMovesInDirection<GEN_CAPTURES>(srcSquare, movementDirection - 0x01, 1, pawnGenMove);
    }

    // En Passant
    for(int dfile = -1; dfile <= 1; dfile += 2) {
        square_t targetSquare = srcSquare + movementDirection + dfile;

        if(targetSquare == m_EnPassantTargetSquare) {
            Move enPassant;
            enPassant.SrcSquare = srcSquare;
            enPassant.DestSquare = m_EnPassantTargetSquare;
            enPassant.IsCapture = true;
            enPassant.Promotion = Piece::NONE;
            enPassant.Score = 0;

            genMove(enPassant);
        }
    }
}

template<typename GenFunction>
void Board::FindCastlingMoves(square_t srcSquare, GenFunction genMove) {
    if(WhiteToMove()) {
        if(srcSquare == Square::FromCoords(0, 4)) {
            if(m_CastlingRights & CR_WHITE_KING_SIDE) {
                FindCastlingMovesHelper(srcSquare, 1, Square::FromCoords(0, 7), genMove);
            }
            if(m_CastlingRights & CR_WHITE_QUEEN_SIDE) {
                FindCastlingMovesHelper(srcSquare, -1, Square::FromCoords(0, 0), genMove);
            }
        }
    } else {
        if(srcSquare == Square::FromCoords(7, 4)) {
            if(m_CastlingRights & CR_BLACK_KING_SIDE) {
                FindCastlingMovesHelper(srcSquare, 1, Square::FromCoords(7, 7), genMove);
            }
            if(m_CastlingRights & CR_BLACK_QUEEN_SIDE) {
                FindCastlingMovesHelper(srcSquare, -1, Square::FromCoords(7, 0), genMove);
            }
        }
    }
}

template<typename GenFunction>
void Board::FindCastlingMovesHelper(square_t kingStartSquare, int kingMovementDirection, square_t rookStartSquare, GenFunction genMove) {
    square_t kingDestSquare = kingStartSquare + (kingMovementDirection * 2);

    // Check for empty squares
    if(m_Pieces[kingStartSquare + kingMovementDirection] != Piece::NONE ||
       m_Pieces[kingStartSquare + (kingMovementDirection * 2)] != Piece::NONE ||
       m_Pieces[rookStartSquare - kingMovementDirection] != Piece::NONE ||
       m_Pieces[rookStartSquare - (kingMovementDirection * 2)] != Piece::NONE) {

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
    m.Promotion = Piece::NONE;
    m.Score = MOVE_SCORE_CASTLING;
    genMove(m);
}

bool Board::IsAttacked(square_t square) {
    static const int pawnAttackDeltasByColor[2][2] = {{0x10 + 0x01, 0x10 - 0x01}, {-0x10 + 0x01, -0x10 - 0x01}};
    color_t enemyColor = Color::OtherSide(m_SideToMove);

    // Find knight attacks
    for(int knightVec : KNIGHT_VECTORS) {
        piece_t attackingSquare = square + knightVec;
        if(Square::OnBoard(attackingSquare) && m_Pieces[attackingSquare] == Piece::Create(enemyColor, Piece::KNIGHT)) {
            return true;
        }
    }

    // Find pawn attacks
    for(int pawnVec : pawnAttackDeltasByColor[m_SideToMove]) {
        piece_t attackingSquare = square + pawnVec;
        if(Square::OnBoard(attackingSquare) && m_Pieces[attackingSquare] == Piece::Create(enemyColor, Piece::PAWN)) {
            return true;
        }
    }

    // Find sliding piece attacks
    for(int i = 0; i < 8; ++i) {
        square_t direction = ORTHOGONAL_AND_DIAGONAL_VECTORS[i];
        bool isDiagonal = (0x80 >> i) & ORTHOGONAL_AND_DIAGONAL_VECTORS_IS_DIAGONAL_MASK;
        bool isHorizontal = !isDiagonal;
        for(int j = 1; j < 8; ++j) {
            square_t attackingSquare = square + (direction * j);
            if(!Square::OnBoard(attackingSquare)) {
                break; // next direction
            }
            piece_t attackingPiece = m_Pieces[attackingSquare];
            if(attackingPiece != Piece::NONE) {
                // We found something, what is it?
                if(j == 1 && attackingPiece == Piece::Create(enemyColor, Piece::KING)) {
                    return true;
                } else if(attackingPiece == Piece::Create(enemyColor, Piece::QUEEN)) {
                    return true;
                } else if (isDiagonal && attackingPiece == Piece::Create(enemyColor, Piece::BISHOP)) {
                    return true;
                } else if (isHorizontal && attackingPiece == Piece::Create(enemyColor, Piece::ROOK)) {
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

Zobrist::hash_t Board::SquareHashCode(square_t square) {
    piece_t contents = m_Pieces[square];
    if(contents == Piece::NONE) {
        return 0;
    } else {
        color_t color = Piece::Color(contents);
        int pieceIndex = Piece::Index(contents);
        int squareNumber = Square::Index64(square);

        assert(color >= 0 && color < 2);
        assert(squareNumber >= 0 && squareNumber < 64);
        assert(pieceIndex >= 0 && pieceIndex < 6);

        return Zobrist::Piece[color][pieceIndex][squareNumber];
    }
}

void Board::SetSquare(square_t square, piece_t contents) {
    m_PieceHash ^= SquareHashCode(square); // clear old hash code
    m_Pieces[square] = contents;
    m_PieceHash ^= SquareHashCode(square); // set new hash code
}

void Board::SetSquareWithUndo(square_t square, piece_t contents, UndoMove& undo) {
    assert(undo.NumSquaresUpdated < 4);

    // Save square contents for undo
    undo.Squares[undo.NumSquaresUpdated] = square;
    undo.Contents[undo.NumSquaresUpdated] = m_Pieces[square];
    undo.NumSquaresUpdated += 1;

    // Modify square
    SetSquare(square, contents);
}

bool Board::PieceListsConsistentWithBoard() const {
    int numPiecesInPieceLists = 0;

    // Verify that all pieces in the piece list match the board
    for(int color = 0; color < 2; ++color) {
        for(int pieceNumber = 0; pieceNumber < 6; ++pieceNumber) {
            int plIdx = PieceLocationsOffset(color, pieceNumber);
            while(m_PieceLocations[plIdx] != PL_END_OF_LIST) {
                piece_t expectedPiece = Piece::Create(color, pieceNumber);
                if(m_Pieces[m_PieceLocations[plIdx]] != expectedPiece) {
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
            if(m_Pieces[Square::FromCoords(rank, file)] != Piece::NONE) {
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

int Board::PieceLocationsOffset(color_t color, int pieceNumber) const {
    static const int ColorOffset[2] = { 0, 60 };

    // Returns an index to the start of the piece list for the given piece
    return ColorOffset[color] + (pieceNumber * 10);
}

void Board::PieceListRemoveWithUndo(square_t location, UndoMove& undo) {
    piece_t contents = m_Pieces[location];
    assert(contents != Piece::NONE);

    // Update piece list
    int plIdx = PieceLocationsOffset(Piece::Color(contents), Piece::Index(contents));
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

void Board::SetPieceLocationWithUndo(int index, square_t location, UndoMove& undo) {
    assert(undo.NumPieceLocationsUpdated < (int)(sizeof(undo.PieceLocations)/sizeof(undo.PieceLocations[0])));

    // Save old value for later undo
    undo.PieceLocationIndexes[undo.NumPieceLocationsUpdated] = index;
    undo.PieceLocations[undo.NumPieceLocationsUpdated] = m_PieceLocations[index];
    undo.NumPieceLocationsUpdated += 1;

    // Modify piece list
    m_PieceLocations[index] = location;
}

void Board::MovePieceWithUndo(square_t from, square_t to, UndoMove& undo) {
    assert(m_Pieces[from] != Piece::NONE);

    piece_t fromContents = m_Pieces[from];
    int plIdx = PieceLocationsOffset(Piece::Color(fromContents), Piece::Index(fromContents));

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

    if(m_Pieces[to] != Piece::NONE) {
        // Capture
        PieceListRemoveWithUndo(to, undo);
    }

    // Update squares
    SetSquareWithUndo(to, fromContents, undo);
    SetSquareWithUndo(from, Piece::NONE, undo);
}

void Board::PlaceNewPiece(square_t location, piece_t contents) {
    // FIXME: This method is almost identical to PlaceNewPieceWithUndo(), would be
    // nice to refactor this somehow.
    assert(m_Pieces[location] == Piece::NONE);

    // Update piece list
    int plIdx = PieceLocationsOffset(Piece::Color(contents), Piece::Index(contents));
    // Find end of the piece list
    while(m_PieceLocations[plIdx] != PL_END_OF_LIST) {
        ++plIdx;
    }
    m_PieceLocations[plIdx] = location;

    SetSquare(location, contents);
}

void Board::PlaceNewPieceWithUndo(square_t location, piece_t contents, UndoMove& undo) {
    if(m_Pieces[location] != Piece::NONE) {
        PieceListRemoveWithUndo(location, undo);
    }

    // Update piece list
    int plIdx = PieceLocationsOffset(Piece::Color(contents), Piece::Index(contents));
    // Find end of the piece list
    while(m_PieceLocations[plIdx] != PL_END_OF_LIST) {
        ++plIdx;
    }
    SetPieceLocationWithUndo(plIdx, location, undo);

    // Update square
    SetSquareWithUndo(location, contents, undo);
}

void Board::RemovePieceWithUndo(square_t location, UndoMove& undo) {
    // Update piece list
    PieceListRemoveWithUndo(location, undo);

    // Update square
    SetSquareWithUndo(location, Piece::NONE, undo);
}

template<typename VisitorFunction>
void Board::ForEachPiece(color_t color, int pieceNumber, VisitorFunction f) {
    for(int plIdx = PieceLocationsOffset(color, pieceNumber); m_PieceLocations[plIdx] != PL_END_OF_LIST; ++plIdx) {
        f(m_PieceLocations[plIdx]);
    }
}

bool Board::IsPseudoLegal(Move m) {
    // There has to be a piece to move
    if(m_Pieces[m.SrcSquare] == Piece::NONE) {
        return false;
    }

    // We can only move one of our pieces
    if(Piece::Color(m_Pieces[m.SrcSquare]) != m_SideToMove) {
        return false;
    }

    // Kind of inefficient, we just scan through the list of moves for this piece
    // and see if it's there
    bool foundMove = false;

    FindMovesFromSquare<GEN_ALL>(Piece::Index(m_Pieces[m.SrcSquare]), m.SrcSquare, [&](Move o) {
        if((m.SrcSquare == o.SrcSquare) && (m.DestSquare == o.DestSquare) && (m.Promotion == o.Promotion)) {
            foundMove = true;
        }
    });

    return foundMove;
}

bool Board::Make(Move m) {
    assert(m.SrcSquare >= 0 && m.SrcSquare < 128);
    assert(m.DestSquare >= 0 && m.DestSquare < 128);
    assert(PieceListsConsistentWithBoard());

    m_UndoStack.emplace_back();
    UndoMove& undo = m_UndoStack.back();

    bool legalMove = true;

    bool isPawnMove = Piece::Index(m_Pieces[m.SrcSquare]) == Piece::PAWN;
    bool isEnPassant = isPawnMove && (m.DestSquare == m_EnPassantTargetSquare);
    bool isKingMove = Piece::Index(m_Pieces[m.SrcSquare]) == Piece::KING;
    bool isCastling = isKingMove && ((m.SrcSquare - m.DestSquare) == 2 || (m.SrcSquare - m.DestSquare) == -2);
    bool isCapture = (m_Pieces[m.DestSquare] != Piece::NONE);
    bool isRookMove = Piece::Index(m_Pieces[m.SrcSquare]) == Piece::ROOK;
    bool capturesRook = Piece::Index(m_Pieces[m.DestSquare]) == Piece::ROOK;

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
        square_t enemyPawnSquare = m.SrcSquare + dfile;

        RemovePieceWithUndo(enemyPawnSquare, undo);
    }

    if(isPawnMove && (Square::Rank(m.DestSquare) == 7 || Square::Rank(m.DestSquare) == 0)) {
        // Pawn promotion

        // Remove pawn
        RemovePieceWithUndo(m.SrcSquare, undo);

        // Place promoted piece
        piece_t pieceAtDest = Piece::Create(m_SideToMove, m.Promotion);
        PlaceNewPieceWithUndo(m.DestSquare, pieceAtDest, undo);
    } else {
        // Just a regular move or capture. If castling, this moves the king,
        // the rook will be moved later.
        MovePieceWithUndo(m.SrcSquare, m.DestSquare, undo);
    }

    m_EnPassantTargetSquare = SQ_NONE; // generally en passant will not be possible on the next move
    
    // Update en passant target square
    if(isPawnMove) {
        // Could be a two-square pawn move, need to check that to update m_EnPassantTargetSquare
        square_t midpoint = (m.SrcSquare + m.DestSquare) / 2;
        if((midpoint & 0xf) == (m.SrcSquare & 0xf)) {
            // It was a two-square pawn
            m_EnPassantTargetSquare = midpoint;
        }
    }

    // Handle castling
    if(isCastling) {
        // Find the rook square
        int kingMovementDirection = (m.DestSquare - m.SrcSquare) > 0 ? 1 : -1;
        square_t rookDestSquare = m.DestSquare - kingMovementDirection;
        // account for king side vs queen side
        bool isKingSide = (m.DestSquare & 0x7) == 6;
        square_t rookSrcSquare = isKingSide ? (m.DestSquare + 1) : (m.DestSquare - 2);

        // Move the rook
        assert(m_Pieces[rookSrcSquare] == Piece::Create(m_SideToMove, Piece::ROOK));
        assert(m_Pieces[rookDestSquare] == Piece::NONE);
        MovePieceWithUndo(rookSrcSquare, rookDestSquare, undo);
    }

    // Update castling rights
    if(isKingMove) {
        if(WhiteToMove()) {
            m_CastlingRights &= ~(CR_WHITE_KING_SIDE | CR_WHITE_QUEEN_SIDE);
        } else {
            m_CastlingRights &= ~(CR_BLACK_KING_SIDE | CR_BLACK_QUEEN_SIDE);
        }
    }

    // Handle rook moves or captures that remove castling rights
    {
        if(isRookMove) {
            // We can't castle using a rook that is moved
            MarkRookIneligibleForCastling(m_SideToMove, m.SrcSquare);
        }
        
        if (capturesRook) {
            // We can't castle using a rook that was captured (even if another
            // rook ends up on the same square later, with the king unmoved)
            MarkRookIneligibleForCastling(Color::OtherSide(m_SideToMove), m.DestSquare);
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
    m_SideToMove = Color::OtherSide(m_SideToMove);

    return legalMove;
}

void Board::Unmake() {
    UndoMove& undo = m_UndoStack.back();

    m_SideToMove = Color::OtherSide(m_SideToMove);
    m_PieceHash = undo.PieceHash;
    m_EnPassantTargetSquare = undo.EnPassantTargetSquare;
    m_CastlingRights = undo.CastlingRights;
    m_PliesSincePawnMoveOrCapture = undo.PliesSincePawnMoveOrCapture;

    // Set squares directly, bypassing hash updates, since we set that above
    for(int i = 0; i < undo.NumSquaresUpdated; ++i) {
        m_Pieces[undo.Squares[i]] = undo.Contents[i];
    }

    // Restore piece lists
    for(int i = 0; i < undo.NumPieceLocationsUpdated; ++i) {
        m_PieceLocations[undo.PieceLocationIndexes[i]] = undo.PieceLocations[i];
    }

    m_UndoStack.pop_back();
}

void Board::MarkRookIneligibleForCastling(color_t rookColor, square_t rookSquare) {
    // Note that rookSquare doesn't have to be a corner square, in which case
    // we do nothing.
    if(rookColor == Color::WHITE) {
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

void Board::ParseFen(const std::string &fen) {
    int rank = 7;
    int file = 0;
    std::size_t i = 0;

    // Clear board
    memset(m_Pieces, Piece::NONE, sizeof(m_Pieces));
    m_PieceHash = 0;

    // Clear piece lists
    memset(m_PieceLocations, PL_END_OF_LIST, sizeof(m_PieceLocations));

    // Clear undo stack, otherwise this could grow without limit
    m_UndoStack.clear();

    // Parse board portion of FEN
    while(!(rank == 0 && file > 7)) {
        piece_t piece = Piece::NONE;
        int skip = 0;
        switch(fen[i++]) {
            case 'P': piece = Piece::Create(Color::WHITE, Piece::PAWN); break;
            case 'p': piece = Piece::Create(Color::BLACK, Piece::PAWN); break;
            case 'N': piece = Piece::Create(Color::WHITE, Piece::KNIGHT); break;
            case 'n': piece = Piece::Create(Color::BLACK, Piece::KNIGHT); break;
            case 'B': piece = Piece::Create(Color::WHITE, Piece::BISHOP); break;
            case 'b': piece = Piece::Create(Color::BLACK, Piece::BISHOP); break;
            case 'R': piece = Piece::Create(Color::WHITE, Piece::ROOK); break;
            case 'r': piece = Piece::Create(Color::BLACK, Piece::ROOK); break;
            case 'Q': piece = Piece::Create(Color::WHITE, Piece::QUEEN); break;
            case 'q': piece = Piece::Create(Color::BLACK, Piece::QUEEN); break;
            case 'K': piece = Piece::Create(Color::WHITE, Piece::KING); break;
            case 'k': piece = Piece::Create(Color::BLACK, Piece::KING); break;
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

        if(piece != Piece::NONE) {
            PlaceNewPiece(Square::FromCoords(rank, file++), piece);
        } else {
            file += skip;
        }
    }

    while(fen[i] == ' ') { ++i; }

    // Side to move
    char sideToMove = fen[i++];
    m_SideToMove = (sideToMove == 'w') ? Color::WHITE : Color::BLACK;

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
        m_EnPassantTargetSquare = SQ_NONE;
    } else {
        char fileLetter = fen[i++];
        char rankNumber = fen[i++];
        // TODO: Should check that these characters are in the expected range
        int enPassantRank = rankNumber - '1';
        int enPassantFile = fileLetter - 'a';
        m_EnPassantTargetSquare = Square::FromCoords(enPassantRank, enPassantFile);
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

    static const int SideToMoveFactor[2] = { 1, -1 };
    int scores[2] = {0, 0};
    int pawnsOnFile[2][8] = {{0}};
    int pawnMinRank[2][8] = {{7,7,7,7,7,7,7,7},{7,7,7,7,7,7,7,7}};
    int pawnMaxRank[2][8] = {{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0}};

    // Piece square tables and counting pawns on files
    for(int colorIdx = 0; colorIdx < 2; ++colorIdx) {
        // Pawns
        ForEachPiece(colorIdx, Piece::PAWN, [&](square_t square) {
            int rank = Square::Rank(square);
            int file = Square::File(square);
            int pstIdx = Square::IndexPST64(square, colorIdx);

            scores[colorIdx] += PIECE_VALUES[Piece::PAWN];
            scores[colorIdx] += PIECE_ON_SQUARE_VALUES[Piece::PAWN][pstIdx];

            pawnsOnFile[colorIdx][file] += 1;
            pawnMinRank[colorIdx][file] = std::min(pawnMinRank[colorIdx][file], rank);
            pawnMaxRank[colorIdx][file] = std::max(pawnMaxRank[colorIdx][file], rank);
        });

        // Other pieces
        static_assert(Piece::PAWN == 0, "This loop structure requires pawns have a piece index of 0");        
        for(int pieceIdx = 1; pieceIdx <= 5; ++pieceIdx) {
            ForEachPiece(colorIdx, pieceIdx, [&](square_t square) {
                int pstIdx = Square::IndexPST64(square, colorIdx);
                
                scores[colorIdx] += PIECE_VALUES[pieceIdx];
                scores[colorIdx] += PIECE_ON_SQUARE_VALUES[pieceIdx][pstIdx];
            });
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
        square_t kingSquare = m_PieceLocations[PieceLocationsOffset(colorIdx, Piece::KING)];
        if(Square::Rank(kingSquare) == backRank) {
            bool considerShield = false;
            int startFile;
            int endFile;

            if(Square::File(kingSquare) > 4) {
                considerShield = true;
                startFile = 5;
                endFile = 7;
            } else if(Square::File(kingSquare) < 3) {
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

    return (scores[0] - scores[1] + featureScore) * SideToMoveFactor[m_SideToMove];
}

std::vector<int> Board::EvaluationFeatures() {
    // Kind of an ugly structure here: We call StaticEvaluation() for the side
    // effect of populating m_Features
    StaticEvaluation();

    return m_Features;
}

bool Board::InCheck() {
    int kingPlIdx = PieceLocationsOffset(m_SideToMove, Piece::KING);

    // There should be exactly one king
    assert(m_PieceLocations[kingPlIdx] != PL_END_OF_LIST);
    assert(m_PieceLocations[kingPlIdx+1] == PL_END_OF_LIST);

    return IsAttacked(m_PieceLocations[kingPlIdx]);
}

Board::Move Board::ParseMove(const std::string &moveStr) {
    Move m;

    m.SrcSquare = Square::FromCoords(moveStr[1] - '1', moveStr[0] - 'a');
    m.DestSquare = Square::FromCoords(moveStr[3] - '1', moveStr[2] - 'a');
    m.IsCapture = false; // FIXME: This is a garbage value, it might be a capture for all we know
    m.Promotion = Piece::NONE;

    if(moveStr.size() == 5) {
        switch(moveStr[4]) {
            case 'b': m.Promotion = Piece::BISHOP; break;
            case 'n': m.Promotion = Piece::KNIGHT; break;
            case 'r': m.Promotion = Piece::ROOK; break;
            case 'q': m.Promotion = Piece::QUEEN; break;
            default: assert(false); break;
        }
    }

    return m;
}

std::string Board::Move::ToString() const {
    std::string out;

    out.push_back('a' + Square::File(SrcSquare));
    out.push_back('1' + Square::Rank(SrcSquare));
    out.push_back('a' + Square::File(DestSquare));
    out.push_back('1' + Square::Rank(DestSquare));

    switch(Promotion) {
        case Piece::BISHOP: out.push_back('b'); break;
        case Piece::KNIGHT: out.push_back('n'); break;
        case Piece::ROOK: out.push_back('r'); break;
        case Piece::QUEEN: out.push_back('q'); break;
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
            Zobrist::Piece[0][Piece::QUEEN][3] ^
            Zobrist::Piece[0][Piece::KING][4] ^
            Zobrist::Piece[0][Piece::BISHOP][5] ^
            Zobrist::Piece[0][Piece::KNIGHT][6] ^
            Zobrist::Piece[0][Piece::ROOK][7] ^
            Zobrist::Piece[0][Piece::PAWN][15] ^
            Zobrist::Piece[1][Piece::KING][63] ^
            Zobrist::SideToMove[Color::WHITE] ^
            Zobrist::CastlingRights[CR_WHITE_KING_SIDE];
        
        assert(board.Hash() == originalHash);

        Move move = ParseMove("h2h4");
        Zobrist::hash_t hashAfterMove = originalHash ^
            Zobrist::Piece[0][Piece::PAWN][15] ^ // remove pawn from old square
            Zobrist::Piece[0][Piece::PAWN][31] ^ // place pawn on new square
            Zobrist::EnPassantFile[7] ^ // En passant is available on file 7
            Zobrist::SideToMove[Color::WHITE] ^ // unset white to move
            Zobrist::SideToMove[Color::BLACK]; // set black to move
        
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

// Template instantiations
template void Board::FindPseudoLegalMoves<Board::GEN_ALL>(std::vector<Board::Move> &out_MoveList);
template void Board::FindPseudoLegalMoves<Board::GEN_CAPTURES | Board::GEN_PROMOTIONS>(std::vector<Board::Move> &out_MoveList);