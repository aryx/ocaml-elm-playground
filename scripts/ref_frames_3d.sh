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
# Reference frames for the 3D software rasterizer, to check that a
# refactoring doesn't change a single pixel (see
# docs/claude_notes/plan_code_reorg_teaching_3d.md, phase 0): every
# scene is rendered with its clock frozen (-fixed-time), some with debug
# keys pressed first (-keys), and one frame dumped as a PPM
# (-dump-frame), then compared byte for byte.
#
# claude: 'make test' checks the same scenes automatically, against the
# golden frames in tests/3d/golden/ (see tests/3d/Golden_frames.ml),
# all but Minecraft3d; this script is for a manual check including it.
#
# Usage:
#   scripts/ref_frames_3d.sh capture <dir>   # before: write the references
#   scripts/ref_frames_3d.sh check <dir>     # after: compare with them
#
# Not included: games3d/StarCollector3d.exe (Random.self_init: its
# stars are somewhere else on every run).

set -uo pipefail

if [ $# -ne 2 ] || { [ "$1" != capture ] && [ "$1" != check ]; }; then
  echo "usage: $0 capture|check <dir>" >&2
  exit 1
fi
MODE="$1"
DIR="$2"
mkdir -p "$DIR"
BUILD=_build/default

# executable, debug keys to press first, frame to dump
SCENES="
examples3d/Cube3d.exe - 3
examples3d/Cubes3d.exe - 3
examples3d/Cubes3d.exe f 3
examples3d/Cubes3d.exe z 3
examples3d/Cubes3d.exe b 3
examples3d/Cubes3d.exe bf 3
examples3d/Spheres3d.exe - 3
examples3d/Spheres3d.exe m 3
examples3d/Spheres3d.exe mm 3
examples3d/Spheres3d.exe mmm 3
examples3d/TexturedCube3d.exe - 3
examples3d/TexturedCube3d.exe p 3
examples3d/TexturedCube3d.exe i 3
examples3d/InteractiveCube3d.exe - 3
examples3d/PaintersAlgorithmFail3d.exe - 3
examples3d/PaintersAlgorithmFail3d.exe z 3
examples3d/FloatingCity3d.exe - 3
games3d/Minecraft3d.exe - 1
"

FAILED=0
while read -r EXE KEYS FRAME; do
  [ -z "$EXE" ] && continue
  NAME="$(basename "$EXE" .exe)_${KEYS}.ppm"
  [ "$KEYS" = - ] && KEYS=""
  OUT="$DIR/$NAME"
  [ "$MODE" = check ] && OUT="$DIR/new_$NAME"
  timeout 120 "$BUILD/$EXE" -fixed-time 1000 -keys "$KEYS" -dump-frame "$FRAME" "$OUT" > /dev/null 2>&1
  if [ "$MODE" = capture ]; then
    echo "captured $NAME"
  elif cmp -s "$DIR/$NAME" "$OUT"; then
    echo "same     $NAME"
    rm -f "$OUT"
  else
    echo "DIFFERENT $NAME (see $OUT)"
    FAILED=1
  fi
done <<< "$SCENES"
exit $FAILED
