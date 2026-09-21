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
# Run every playground3d/2D-playground native executable under a given
# directory for a few seconds each and report which ones crashed (exit
# code other than 124, the `timeout` code for "still running when the
# deadline hit", i.e. no exception was raised) -- see
# docs/claude_notes/notes_debugging_techniques.md section 1's "running
# a GUI app headlessly, without it hanging your shell" for why exit
# code 124 specifically means success here, not failure.
#
# Usage:
#   scripts/smoke/smoke_test_playground3d.sh [seconds-each] [dir ...]
#
# Example (defaults to the usual example/game dirs under _build):
#   scripts/smoke/smoke_test_playground3d.sh
#   scripts/smoke/smoke_test_playground3d.sh 5 _build/default/examples3d _build/default/games3d

set -uo pipefail

SECONDS_EACH="${1:-3}"
shift || true
DIRS=("$@")
if [ ${#DIRS[@]} -eq 0 ]; then
  DIRS=(
    _build/default/examples
    _build/default/games
    _build/default/examples3d
    _build/default/examples3d/opengl
    _build/default/games3d
    _build/default/games3d/software
  )
fi

DISPLAY="${DISPLAY:-:1}"
FAILED=0

for dir in "${DIRS[@]}"; do
  [ -d "$dir" ] || continue
  for exe in "$dir"/*.exe; do
    [ -f "$exe" ] || continue
    log="$(mktemp /tmp/smoke_test.XXXXXX.log)"
    DISPLAY="$DISPLAY" timeout "$SECONDS_EACH" "$exe" > "$log" 2>&1
    code=$?
    if [ "$code" != "124" ]; then
      echo "FAIL ($code): $exe -- see $log"
      cat "$log"
      FAILED=1
    else
      echo "OK: $exe"
    fi
  done
done

exit $FAILED
