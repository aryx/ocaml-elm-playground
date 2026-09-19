#!/bin/bash
# Claude Code
#
# Copyright (C) 2026 Yoann Padioleau
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public License
# (LGPL) as published by the Free Software Foundation; either version
# 2 of the License, or (at your option) any later version.
#
# How fast can a 2D playground backend draw an example or game? Runs it
# without the 60 fps cap (-uncapped), optionally presses debug keys (e.g.
# "n" to turn the software rasterizer's antialiasing off, see
# playground/software/Playground_platform.ml) with scripts/input/xdrive.py,
# and prints the median of the fps it logs (-debug) once settled. Used
# for the numbers in docs/claude_notes/notes_opti.md.
#
# Usage:
#   [REPEAT=n] scripts/perf/bench_playground.sh <path-to-exe> [key ...]
#
# Examples:
#   scripts/perf/bench_playground.sh _build/default/games/Pong.exe            # Cairo
#   scripts/perf/bench_playground.sh _build/default/games/software/Pong.exe   # ours
#   scripts/perf/bench_playground.sh _build/default/games/software/Pong.exe n # ours, no antialiasing
#   REPEAT=3 scripts/perf/bench_playground.sh ...   # median of 3 runs (min-max)

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "usage: $0 <path-to-exe> [key ...]" >&2
  exit 1
fi

EXE="$1"
shift
NAME=$(basename "$EXE")
DIR=$(dirname "$0")
# runs are noisy (other programs, the window manager, images loaded
# over the network): repeat and take the median
REPEAT=${REPEAT:-1}

# The window title minus its fps, e.g. "Playground (software rasterizer)
# -- t:alpha=on b:boxes=off ...": it shows every debug key's state, so it
# changes when a key press took effect
toggles_title() {
  xwininfo -id "$1" | grep -o '"[^"]*"' | head -1 | sed 's/-- [0-9]* fps//'
}

# A key press sometimes doesn't reach the app (seen as runs with the
# setting unchanged); check the title changed, else press again. Keys
# that change nothing in the title (e.g. on the Cairo backend, which
# has no debug keys) are pressed once.
press_key() {
  local WID="$1" KEY="$2" BEFORE i j
  BEFORE=$(toggles_title "$WID")
  for i in 1 2 3; do
    "$DIR/../input/xdrive.py" key "$WID" "$KEY" 0.05
    # the title is updated once per frame, and a slow configuration
    # can take a quarter of a second per frame: wait for up to 3s
    # before concluding the press was lost (pressing a toggle again
    # while its first press is still pending would undo it)
    for j in $(seq 15); do
      sleep 0.2
      if [ "$(toggles_title "$WID")" != "$BEFORE" ]; then return; fi
    done
  done
  echo "(key $KEY: no effect on the window title)" >&2
}

# one run: prints the median fps logged once settled, with the keys
one_run() {
  local LOG PID WID START
  LOG=$(mktemp /tmp/bench_playground.XXXXXX.log)
  "$EXE" -uncapped -debug -debug-keys > "$LOG" 2>&1 &
  PID=$!
  WID=$("$DIR/../input/xdrive.py" find "$NAME")
  # let images download (preloaded ones before the first frame, others
  # on first use)
  sleep 2
  for key in "$@"; do
    press_key "$WID" "$key"
  done
  # the fps logged from now on are with the keys applied
  START=$(grep -c "fps" "$LOG" || true)
  sleep 4
  kill $PID 2>/dev/null || true
  # wait until it's really gone, or the next run's xdrive.py find may
  # still see this run's window
  wait $PID 2>/dev/null || true
  # an image that failed to load isn't drawn: faster, but not comparable
  if grep -q "failed to load image" "$LOG"; then echo "(image failed)" >&2; fi
  tail -n +"$((START + 1))" "$LOG" | grep -o "fps [0-9.]*" | awk '{print $2}' | sort -n \
    | awk '{v[NR] = $1} END { if (NR == 0) print 0; else print v[int((NR + 1) / 2)] }'
  rm -f "$LOG"
}

for i in $(seq "$REPEAT"); do one_run "$@"; done | sort -n \
  | awk '{v[NR] = $1} END { m = v[int((NR + 1) / 2)];
      if (NR > 1) printf "%s (%s-%s)\n", m, v[1], v[NR]; else print m }'
