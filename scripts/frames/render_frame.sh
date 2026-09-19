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
# Render one frame of a native playground program offscreen, as a PNG:
# SDL's dummy video driver (no window: see
# docs/claude_notes/notes_debugging_techniques.md section 9), the clock
# frozen, optional game keys held over frames (-script), frame n dumped.
#
# Usage:
#   scripts/frames/render_frame.sh <exe, from the build root> <frame> <out.png> [script] [flags...]
# e.g.
#   scripts/frames/render_frame.sh games/software/TinyMario.exe 150 /tmp/run.png "right:1-150,up:30-34"
#   scripts/frames/render_frame.sh games3d/TinyTron3d.exe 150 /tmp/t.png "1:1,up:40" 
# Env: BUILD_DIR (default _build), TIME (the frozen clock, default 1000).

set -euo pipefail
if [ $# -lt 3 ]; then
  echo "usage: $0 <exe> <frame> <out.png> [script] [flags...]" >&2; exit 1
fi
EXE=$1; FRAME=$2; OUT=$(realpath -m "$3"); shift 3
SCRIPT=${1:-}; [ $# -gt 0 ] && shift
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT/${BUILD_DIR:-_build}/default"
PPM=$(mktemp --suffix=.ppm)
ARGS=(-fixed-time "${TIME:-1000}" -dump-frame "$FRAME" "$PPM")
[ -n "$SCRIPT" ] && ARGS+=(-script "$SCRIPT")
SDL_VIDEODRIVER=dummy timeout 120 "./$EXE" "${ARGS[@]}" "$@"
python3 -c "from PIL import Image; Image.open('$PPM').save('$OUT')"
rm -f "$PPM"
echo "$OUT"
