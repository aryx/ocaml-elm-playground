#!/usr/bin/env python3
# Claude Code
#
# Copyright (C) 2026 Yoann Padioleau
#
# Draws games3d/tomb.png, the texture atlas of games3d/TinyTombRaider.ml:
# a 2 x 2 grid of 64 x 64 tiles, addressed by (col, row) in the game.
#
#     (0,0) wall stone      (1,0) wall with hieroglyphs
#     (0,1) floor flagstone (1,1) the plinth, and the pushable block
#
# Tomb Raider's own textures were not generated: Core's artists made
# them by hand, many of them from photographs of stone, cut down to
# 64 x 64. What makes them look "random", almost fractal, is two other
# things -- they were quantised and *dithered* to an 8-bit palette, and
# the PlayStation drew them with affine mapping and no filtering, so
# they shimmered. (From memory, to check.)
#
# So this does the same thing from the other end: fractal noise (value
# noise summed over octaves, after Ken Perlin, 1985) to get irregular
# stone, a different tint for every block in the course so no two are
# alike, and then a dither down to 64 colours, which is where the
# speckle comes from here as it did there.
#
# Procedural and seeded, so the file can be made again and comes out the
# same; committed all the same, because dune builds it into the program
# as base64 (see games3d/dune) and a build should not need Python.
#
#     python3 scripts/build/make_tomb_atlas.py
import random
from PIL import Image

TILE = 64
random.seed(7)

# ---- fractal (value) noise -------------------------------------------------
# One octave is a coarse grid of random numbers, read back with smooth
# interpolation; the fractal sum adds octave after octave, each twice as
# fine and half as strong. That "half as strong each time" is the whole
# idea, and what makes the result look like stone rather than static.

def octave(size, cells):
    g = [[random.random() for _ in range(cells + 1)] for _ in range(cells + 1)]
    out = [[0.0] * size for _ in range(size)]
    step = size / cells
    for y in range(size):
        fy = y / step
        j = int(fy)
        ty = fy - j
        ty = ty * ty * (3 - 2 * ty)  # smoothstep, so the cells do not show
        for x in range(size):
            fx = x / step
            i = int(fx)
            tx = fx - i
            tx = tx * tx * (3 - 2 * tx)
            a = g[j][i] * (1 - tx) + g[j][i + 1] * tx
            b = g[j + 1][i] * (1 - tx) + g[j + 1][i + 1] * tx
            out[y][x] = a * (1 - ty) + b * ty
    return out

def fractal(size, cells=4, octaves=5):
    out = [[0.0] * size for _ in range(size)]
    amp, total = 1.0, 0.0
    for k in range(octaves):
        o = octave(size, cells * (2 ** k))
        for y in range(size):
            for x in range(size):
                out[y][x] += amp * o[y][x]
        total += amp
        amp /= 2
    return [[v / total for v in row] for row in out]

WALL_N = fractal(TILE)
FLOOR_N = fractal(TILE, cells=3)
PLINTH_N = fractal(TILE, cells=6)

def shade(base, t, amount):
    """[base] lit by the noise value [t] (0..1), by up to [amount]."""
    d = int((t - 0.5) * 2 * amount)
    return tuple(max(0, min(255, c + d)) for c in base)

SAND = (172, 150, 108)
MORTAR = (96, 80, 56)
CARVED = (124, 102, 68)
GREY = (146, 138, 122)

def wall(px, ox, oy, glyphs=False):
    # the courses: 16 high, offset every other one, and every block in
    # them a slightly different stone
    for row in range(4):
        shift = 0 if row % 2 == 0 else 16
        for block in range(-1, 3):
            bx = shift + block * 32
            tint = random.randint(-16, 16)
            for y in range(row * 16, row * 16 + 16):
                for x in range(bx, bx + 32):
                    if not (0 <= x < TILE):
                        continue
                    base = tuple(c + tint for c in SAND)
                    px[ox + x, oy + y] = shade(base, WALL_N[y][x], 34)
            # the joints around it
            for y in range(row * 16, row * 16 + 16):
                for x in (bx, bx + 31):
                    if 0 <= x < TILE:
                        px[ox + x, oy + y] = shade(MORTAR, WALL_N[y][x], 14)
        for x in range(TILE):
            px[ox + x, oy + row * 16] = shade(MORTAR, WALL_N[row * 16][x], 14)
            px[ox + x, oy + row * 16 + 1] = shade(SAND, WALL_N[row * 16 + 1][x], 40)
    if glyphs:
        for gx in range(4):
            for gy in range(2):
                bx, by = 6 + gx * 15, 10 + gy * 28
                shape = random.choice(
                    [[(0, 0, 8, 2), (3, 2, 2, 10)], [(0, 0, 2, 12), (0, 5, 8, 2)],
                     [(0, 0, 8, 8), (2, 2, 4, 4)], [(2, 0, 4, 3), (0, 4, 8, 2), (2, 7, 4, 5)],
                     [(0, 0, 8, 2), (0, 5, 8, 2), (0, 10, 8, 2)]])
                for (rx, ry, rw, rh) in shape:
                    for y in range(by + ry, min(by + ry + rh, TILE)):
                        for x in range(bx + rx, min(bx + rx + rw, TILE)):
                            px[ox + x, oy + y] = shade(CARVED, WALL_N[y][x], 16)

def flagstones(px, ox, oy):
    for y in range(TILE):
        for x in range(TILE):
            tint = (-10 if (x < 32) != (y < 32) else 8)
            px[ox + x, oy + y] = shade(tuple(c + tint for c in GREY), FLOOR_N[y][x], 44)
    for k in (0, 32):
        for x in range(TILE):
            px[ox + x, oy + k] = shade(MORTAR, FLOOR_N[k][x], 14)
        for y in range(TILE):
            px[ox + k, oy + y] = shade(MORTAR, FLOOR_N[y][k], 14)

def plinth(px, ox, oy):
    for y in range(TILE):
        for x in range(TILE):
            px[ox + x, oy + y] = shade((156, 136, 100), PLINTH_N[y][x], 26)
    for d in (4, 5):
        for x in range(d, TILE - d):
            px[ox + x, oy + d] = shade(CARVED, PLINTH_N[d][x], 12)
            px[ox + x, oy + TILE - 1 - d] = shade(CARVED, PLINTH_N[TILE - 1 - d][x], 12)
        for y in range(d, TILE - d):
            px[ox + d, oy + y] = shade(CARVED, PLINTH_N[y][d], 12)
            px[ox + TILE - 1 - d, oy + y] = shade(CARVED, PLINTH_N[y][TILE - 1 - d], 12)

img = Image.new("RGB", (TILE * 2, TILE * 2))
px = img.load()
wall(px, 0, 0)
wall(px, TILE, 0, glyphs=True)
flagstones(px, 0, TILE)
plinth(px, TILE, TILE)
# One palette for the whole page, 16 colours, Floyd-Steinberg dithered.
# That is the 1996 constraint rather than a choice: a Tomb Raider level
# held its textures in 256 x 256 "texture pages" sharing one 8-bit
# palette, and everything on them was quantised and dithered into it.
# The speckle you remember on those walls is mostly this, not the art.
img = img.convert("P", palette=Image.ADAPTIVE, colors=16, dither=Image.FLOYDSTEINBERG).convert("RGB")
img.save("games3d/tomb.png", optimize=True)
print("games3d/tomb.png", img.size)
