#!/bin/sh
# claude: how the Zstandard file here was made, once (sh make_zstd.sh,
# from this directory, with zstd 1.5.5): someone else's encoder, until
# we have our own, on a text that Unit_zstd.ml makes again to compare.
#   squares.zst   the squares of 0 to 4999, a line each, at level 19:
#                 Huffman-coded literals in four streams (the weights
#                 FSE-coded), the three tables sent, repeated offsets
set -e
seq 0 4999 | awk '{print $1*$1}' | zstd -19 -q -c > squares.zst
