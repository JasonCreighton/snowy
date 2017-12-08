#! /usr/bin/env python3

import chess
import chess.pgn
import numpy as np
import scipy
import scipy.sparse.linalg
import collections

PieceDesc = collections.namedtuple("PieceDesc", ("chess_piece", "feature", "rank_features", "file_features"))

num_features = 0
def assign_feature_index():
    global num_features
    num_features += 1
    return num_features - 1

def non_symmetric_rank_or_file_features():
    return [assign_feature_index() for _ in range(8)]

def symmetric_rank_or_file_features():
    a = assign_feature_index()
    b = assign_feature_index()
    c = assign_feature_index()
    d = assign_feature_index()

    return [a, b, c, d, d, c, b, a]

def generic_piece_desc(chess_piece):
    return PieceDesc(
        chess_piece=chess_piece,
        feature=assign_feature_index(),
        rank_features=symmetric_rank_or_file_features(),
        file_features=symmetric_rank_or_file_features()
    )

# Pawn is special, the rank is not symmetric with respect to the center
pd_pawn = PieceDesc(
    chess_piece=chess.PAWN,
    feature=assign_feature_index(),
    rank_features=non_symmetric_rank_or_file_features(),
    file_features=symmetric_rank_or_file_features()
)

pd_knight = generic_piece_desc(chess.KNIGHT)
pd_bishop = generic_piece_desc(chess.BISHOP)
pd_rook = generic_piece_desc(chess.ROOK)
pd_queen = generic_piece_desc(chess.QUEEN)
pd_king = generic_piece_desc(chess.KING)

piece_descs = [pd_pawn, pd_knight, pd_bishop, pd_rook, pd_queen, pd_king]

def game_outcome(epd_ops):
    result = epd_ops["c9"]
    if result == "1-0":
        return 1.0
    elif result == "0-1":
        return 0.0
    elif result == "1/2-1/2":
        return 0.5
    else:
        assert False

def net_num_pieces(board, piece):
    return len(board.pieces(piece, chess.WHITE)) - len(board.pieces(piece, chess.BLACK))

def build_eqsystem(num_positions, epd_filename):
    board = chess.Board()
    i = 0

    eqsystem = np.zeros((num_positions, num_features))
    outcomes = np.zeros(num_positions)

    with open(epd_filename) as epd_file:
        for line in epd_file:
            epd_ops = board.set_epd(line)

            if epd_ops["c9"] == "1/2-1/2":
                continue

            for pd in piece_descs:
                for color, val in ((chess.WHITE, 1), (chess.BLACK, -1)):
                    for square in board.pieces(pd.chess_piece, color):
                        eqsystem[i, pd.feature] += val

                        square_file = square & 0x7
                        square_rank = square >> 3

                        # We want higher rank numbers to always mean closer to pawn promotion
                        if color == chess.BLACK:
                            square_rank = 7 - square_rank

                        eqsystem[i, pd.file_features[square_file]] += val
                        eqsystem[i, pd.rank_features[square_rank]] += val

            outcomes[i] = game_outcome(epd_ops)

            i += 1

            if i == num_positions:
                break

    print("done.")

    return (eqsystem, outcomes)


eqsystem, outcomes = build_eqsystem(500000, "quiet-labeled.epd")

from sklearn.linear_model import LogisticRegression
lr = LogisticRegression()
lr.fit(eqsystem, outcomes)
values = lr.coef_[0]
# 100 = average pawn value in starting position
pawn_value = values[pd_pawn.feature] + (sum(values[f] for f in pd_pawn.file_features) / 8) + values[pd_pawn.rank_features[1]]
values /= (pawn_value / 100)

for pd in piece_descs:
    avg_value = int(round(values[pd.feature] + np.mean([values[f] for f in pd.rank_features]) + np.mean([values[f] for f in pd.file_features])))
    print("Piece %d: %d" % (pd.chess_piece, avg_value))
    for square_rank in range(7, 0-1, -1):
        for square_file in range(8):
            square_value = int(round(values[pd.feature] + values[pd.file_features[square_file]] + values[pd.rank_features[square_rank]] - avg_value))
            print("%4d," % square_value, end='')
        print()
