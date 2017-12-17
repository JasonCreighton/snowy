#!/bin/bash

DEPTH=6
EPDFILE="../chess-data/test/bk_test.epd"

make release

while read pos stm castling ep discard; do
    echo "position fen $pos $stm $castling $ep 0 1"
    echo "go depth $DEPTH"
done < "$EPDFILE" | perf stat -e cycles ./build/release
