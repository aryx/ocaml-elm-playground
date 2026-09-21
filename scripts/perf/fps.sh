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
# How fast a native playground program runs: n frames with no 60 fps
# cap (-uncapped), timed, startup included, game keys optionally held
# (-script). E.g. to compare backends, or before/after an optimization.
#
# Beware: on the OpenGL backends the window's vsync still caps the
# frame rate at the screen's (60 Hz), -uncapped or not: 300 frames in
# ~5.6 s means "at the cap", not "slow" (5 s of frames, plus startup).
# The software backends draw into a plain window surface, not capped.
#
# Usage:
#   scripts/perf/fps.sh <exe, from the build root> <frames> [script] [flags...]
# e.g.
#   scripts/perf/fps.sh games3d/TinyVirtuaRacing.exe 300 "space:1,up:2-300"
# Env: BUILD_DIR (default _build).

set -euo pipefail
if [ $# -lt 2 ]; then echo "usage: $0 <exe> <frames> [script] [flags...]" >&2; exit 1; fi
EXE=$1; FRAMES=$2; shift 2
SCRIPT=${1:-}; [ $# -gt 0 ] && shift
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT/${BUILD_DIR:-_build}/default"
PPM=$(mktemp --suffix=.ppm)
ARGS=(-uncapped -dump-frame "$FRAMES" "$PPM")
[ -n "$SCRIPT" ] && ARGS+=(-script "$SCRIPT")
START=$(date +%s.%N)
timeout 600 "./$EXE" "${ARGS[@]}" "$@" >/dev/null 2>&1 || true
END=$(date +%s.%N)
rm -f "$PPM"
python3 -c "t=$END-$START; print(f'$EXE: {$FRAMES} frames in {t:.2f} s, {$FRAMES/t:.1f} fps (startup included)')"
