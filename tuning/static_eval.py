#! /usr/bin/env python3

import chess
import chess.pgn
import numpy as np
import scipy
import scipy.sparse.linalg
import collections
import subprocess
import re
import sys

# EPD file is not included in this repository to avoid any confusion about
# licensing. See https://bitbucket.org/zurichess/tuner if you are interested in
# the data set.
EPD_FILE = "../chess-data/zurichess/quiet-labeled.epd"
SNOWY_BIN = "build/release"

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

def build_eqsystem(num_positions, num_ties, snowy_features, epd_filename):
    board = chess.Board()
    i = 0

    num_samples = num_positions - num_ties

    eqsystem = np.zeros((num_samples, num_features))
    outcomes = np.zeros(num_samples)

    with open(epd_filename) as epd_file:
        for line in epd_file:
            epd_ops = board.set_epd(line)

            # Ties are excluded at the moment
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

            for j, feature_value in enumerate(snowy_features[i]):
                eqsystem[i, extra_feature_indexes[j]] = feature_value

            outcomes[i] = game_outcome(epd_ops)

            i += 1

    assert i == num_samples

    return (eqsystem, outcomes)

def snowy_extract_features_from_positions(fen_strings):
    input_bytes = b"\n".join(b"position fen %s\nfeatures" % fen.encode() for fen in fen_strings)
    output_bytes = subprocess.run(SNOWY_BIN, input=input_bytes, stdout=subprocess.PIPE, stderr=subprocess.PIPE).stdout
    features = [[int(x) for x in line.strip().split(" ")] for line in output_bytes.decode().strip().split("\n")]

    return features

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

# Query how many extra board features there are
num_extra_features = int(subprocess.run(SNOWY_BIN, input=b"num_features\n", stdout=subprocess.PIPE).stdout)
extra_feature_indexes = [assign_feature_index() for _ in range(num_extra_features)]

# We have to take a scan through the file first in order to know how big we
# should make the numpy array, and also to find the FEN lines to pass to
# Snowy
print("Scanning EPD...", file=sys.stderr)

num_positions = 0
num_ties = 0
fen_strings = []
with open(EPD_FILE) as epd_file:
    for line in epd_file:
        num_positions += 1
        if "1/2-1/2" in line:
            num_ties += 1
        else:
            fen_strings.append(re.sub(r"c9.*", "", line.strip()))

print("Extracting features from Snowy...", file=sys.stderr)
snowy_features = snowy_extract_features_from_positions(fen_strings)

print("Building samples...", file=sys.stderr)
eqsystem, outcomes = build_eqsystem(num_positions, num_ties, snowy_features, EPD_FILE)

print("Running regression...", file=sys.stderr)
from sklearn.linear_model import LogisticRegression
lr = LogisticRegression()
lr.fit(eqsystem, outcomes)
values = lr.coef_[0]
# 100 = average pawn value in starting position
pawn_value = values[pd_pawn.feature] + (sum(values[f] for f in pd_pawn.file_features) / 8) + values[pd_pawn.rank_features[1]]
values /= (pawn_value / 100)

piece_values = []

print("// Generated by static_eval.py")
print("const int PIECE_ON_SQUARE_VALUES[6][64] = {")
for pd in piece_descs:
    print("{")
    avg_value = int(round(values[pd.feature] + np.mean([values[f] for f in pd.rank_features]) + np.mean([values[f] for f in pd.file_features])))
    piece_values.append(avg_value)
    for square_rank in range(7, 0-1, -1):
        for square_file in range(8):
            square_value = int(round(values[pd.feature] + values[pd.file_features[square_file]] + values[pd.rank_features[square_rank]] - avg_value))
            print("%6d," % square_value, end='')
        print()
    print("},")
print("};")

print("const int PIECE_VALUES[6] = {%s};" % ", ".join(str(v) for v in piece_values))
print("const int FEATURE_VALUES[%d] = {%s};" % (num_extra_features, ", ".join(str(int(round(values[i]))) for i in extra_feature_indexes)))
