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
# Solve games/TinySokoban.ml's levels (read from its source), by a
# breadth-first search over the positions (the player's cell and the
# boxes' cells): each level's shortest solution in moves, or "no
# solution" -- to check a new level before adding it. u d l r are the
# moves (up is a row up).
#
# Usage: scripts/games/sokoban_solve.py [games/TinySokoban.ml]
# A position count grows fast with the boxes: fine for small levels.

import sys
from collections import deque
from ocaml_strings import block, lists_of_strings

def solve(rows):
    walls, goals, boxes, player = set(), set(), set(), None
    for r, row in enumerate(rows):
        for c, ch in enumerate(row):
            if ch == '#': walls.add((c, r))
            if ch in '.*+': goals.add((c, r))
            if ch in '$*': boxes.add((c, r))
            if ch in '@+': player = (c, r)
    if len(boxes) != len(goals):
        return None, f"{len(boxes)} boxes for {len(goals)} goals"
    start = (player, frozenset(boxes))
    seen, queue = {start}, deque([(start, "")])
    moves = {'u': (0, -1), 'd': (0, 1), 'l': (-1, 0), 'r': (1, 0)}
    while queue:
        (p, bs), path = queue.popleft()
        if bs == goals:
            return path, f"{len(seen)} positions explored"
        for k, (dc, dr) in moves.items():
            t = (p[0] + dc, p[1] + dr)
            if t in walls: continue
            nb = bs
            if t in bs:
                beyond = (t[0] + dc, t[1] + dr)
                if beyond in walls or beyond in bs: continue
                nb = (bs - {t}) | {beyond}
            state = (t, nb)
            if state not in seen:
                seen.add(state); queue.append((state, path + k))
    return None, f"{len(seen)} positions explored"

path = sys.argv[1] if len(sys.argv) > 1 else "games/TinySokoban.ml"
for i, level in enumerate(lists_of_strings(block(path, "levels"))):
    solution, info = solve(level)
    print(f"level {i + 1}: " + (f"{len(solution)} moves: {solution}" if solution else "no solution") + f" ({info})")
