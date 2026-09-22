#!/usr/bin/env python3
# Claude Code
#
# Copyright (C) 2026 Yoann Padioleau
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public License
# (LGPL) as published by the Free Software Foundation; either version
# 2 of the License, or (at your option) any later version.
#
# Check TinyPacman.ml's maze (read from its source): the rows the
# same width, every dot and pellet reachable from Pac-Man's start (the
# tunnel wrapping around), no dead ends in the corridors (ghosts never
# turn back: a dead end would trap them), and the ghost house.
#
# Usage: scripts/games/check_maze.py [TinyPacman.ml]

import sys
from collections import deque
from ocaml_strings import block, strings

path = sys.argv[1] if len(sys.argv) > 1 else "TinyPacman.ml"
maze = strings(block(path, "maze_rows"))
problems = []
widths = set(len(r) for r in maze)
if len(widths) != 1: problems.append(f"rows of widths {sorted(widths)}")
W, H = max(widths), len(maze)
cell = lambda c, r: maze[r][c % W] if 0 <= r < H and len(maze[r]) > c % W else '#'
walkable = lambda c, r: cell(c, r) not in '#-'
start = next(((c, r) for r, row in enumerate(maze) for c, ch in enumerate(row) if ch == 'P'), None)
if start is None: raise SystemExit("no 'P' (Pac-Man's start)")
seen, queue = {start}, deque([start])
while queue:
    c, r = queue.popleft()
    for dc, dr in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
        n = ((c + dc) % W, r + dr)
        if walkable(*n) and n not in seen: seen.add(n); queue.append(n)
dots = [(c, r) for r, row in enumerate(maze) for c, ch in enumerate(row) if ch in '.o']
lost = [d for d in dots if d not in seen]
if lost: problems.append(f"unreachable dots: {lost}")
dead = [(c, r) for (c, r) in seen
        if sum(walkable((c + dc) % W, r + dr) for dc, dr in [(1, 0), (-1, 0), (0, 1), (0, -1)]) <= 1]
if dead: problems.append(f"dead ends: {dead}")
if not any('-' in row for row in maze): problems.append("no ghost house door ('-')")
print(f"{W}x{H}, {len(dots)} dots and pellets, {len(seen)} cells reachable")
print("ok" if not problems else "\n".join("PROBLEM: " + p for p in problems))
sys.exit(1 if problems else 0)
