#!/usr/bin/env python3
# claude: how the Brotli files here were made, once (python3
# make_brotli.py, from this directory, with Google's encoder, the
# Python module brotli 1.1.0): someone else's encoder, until we have
# our own, on texts that Unit_brotli.ml makes again to compare.
#   squares.br   the squares of 0 to 4999, a line each, at quality 11:
#                complex and simple prefix codes, several codes for the
#                literals chosen by context (the map sent with runs and
#                move-to-front), 15 types of literal blocks and their
#                switches, NPOSTFIX and NDIRECT not 0
#   mixed.br     squares, noise, French, squares again, at quality 5 in
#                a window of 2^16: several codes for the distances too,
#                the last distances (again, and nudged), the commands
#                without a distance, block switches of literals and of
#                commands
#   page.br      a small web page, at quality 11: words of the
#                dictionary and their transforms
# (what each reaches was checked with a decoder that says so; none has
# a meta-block not compressed: Unit_brotli.ml's "hi" is one)
import brotli

def squares(n):
    return "".join("%d\n" % (i * i) for i in range(n)).encode()

def noise(n):
    x, out = 1, bytearray()
    for _ in range(n):
        x = (x * 75 + 74) % 65537
        out.append(x & 255)
    return bytes(out)

french = "Les élèves français étudient à l'école, près de la forêt. ".encode() * 40
page = (b'<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n'
        b'<title>Information about the government of the United States</title>\n'
        b'</head>\n<body>\n<p>However, the following information was provided by the university.</p>\n'
        b'</body>\n</html>\n')

open("squares.br", "wb").write(brotli.compress(squares(5000), quality=11))
open("mixed.br", "wb").write(brotli.compress(squares(2000) + noise(4000) + french + squares(2000), quality=5, lgwin=16))
open("page.br", "wb").write(brotli.compress(page, quality=11, mode=brotli.MODE_TEXT))
