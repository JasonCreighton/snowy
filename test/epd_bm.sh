#!/bin/bash

CHESS_ARTIST_DIR=$HOME/src/chess-artist

make release
python "$CHESS_ARTIST_DIR/chess-artist.py" -job test -movetime 5000 -eng ./build/release -infile "$1"
