#!/bin/bash

engine1=stockfish
engine2=./build/release

setup="position fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
depth=5

mkdir -p tmp

echo -e "$setup\nperft $depth\nquit\n" | $engine1 | grep '^[a-h][1-8][a-h][1-8]' | sort > tmp/engine1.txt
echo -e "$setup\nperft $depth\nquit\n" | $engine2 | grep '^[a-h][1-8][a-h][1-8]' | sort > tmp/engine2.txt

cut -d ':' -f 1 < tmp/engine1.txt > tmp/engine1_moves.txt
cut -d ':' -f 1 < tmp/engine2.txt > tmp/engine2_moves.txt

if diff -u tmp/engine1_moves.txt tmp/engine2_moves.txt; then
    if diff -u tmp/engine1.txt tmp/engine2.txt; then
        echo "OK!"
    else
        echo "^^^ Differences found between perft counts ^^^"
    fi
else
    echo "^^^ Differences found between move list ^^^"
fi
