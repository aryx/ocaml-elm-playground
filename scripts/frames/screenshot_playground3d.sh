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
# Screenshot a running playground3d (native or OpenGL) demo, on a real
# X display (DISPLAY=:1 in this sandbox). See
# docs/claude_notes/notes_debugging_techniques.md's "Screenshotting a
# playground3d window" section for why this isn't just a one-line
# `import -window <title>` -- window managers create an *outer*
# decorated frame window with the *same title* as the app's inner
# content window, and `import -window <title>` can match either one
# depending on tree order, silently producing a wrong-sized screenshot
# (e.g. 1056x1132 instead of the app's real 1000x1000) with no error.
# This instead greps xwininfo's tree for the executable's own basename
# (e.g. "Mario.exe"), which only the inner content window's WM_CLASS
# carries, to reliably find the right window.
#
# Usage:
#   scripts/frames/screenshot_playground3d.sh <path-to-exe> <output.png> [wait-seconds] [run-timeout-seconds]
#
# Example:
#   scripts/frames/screenshot_playground3d.sh _build/default/examples3d/Cubes3d.exe /tmp/cubes.png

set -euo pipefail

if [ $# -lt 2 ]; then
  echo "usage: $0 <path-to-exe> <output.png> [wait-seconds] [run-timeout-seconds]" >&2
  exit 1
fi

EXE="$1"
OUT="$2"
WAIT_SECONDS="${3:-2}"
RUN_TIMEOUT="${4:-5}"
EXE_BASENAME="$(basename "$EXE")"
DISPLAY="${DISPLAY:-:1}"

RUN_LOG="$(mktemp /tmp/screenshot_playground3d.XXXXXX.log)"
(DISPLAY="$DISPLAY" timeout "$RUN_TIMEOUT" "$EXE" > "$RUN_LOG" 2>&1 &)
sleep "$WAIT_SECONDS"

WID=$(DISPLAY="$DISPLAY" xwininfo -root -tree 2>/dev/null | grep -F "$EXE_BASENAME" | grep -oE '0x[0-9a-f]+' | head -1)
if [ -z "$WID" ]; then
  echo "error: no window found for '$EXE_BASENAME' (is it running? did it crash? see $RUN_LOG)" >&2
  cat "$RUN_LOG" >&2
  exit 1
fi

DISPLAY="$DISPLAY" import -window "$WID" "$OUT"
sleep 1
identify "$OUT"
echo "run log: $RUN_LOG" >&2
