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
# Smoke-test the web programs, without a browser window:
#   - each 2D and 2.5D game's games/<genre>/web/*.bc.js run in node with
#     a fake DOM (scripts/web/web_headless.js), 600 frames, pressing
#     space, 1, the arrows: any exception or hang is reported;
#   - each 3D game's page (WebGL, the rows of CATALOG.md whose Dir is
#     3D) screenshotted in headless Chrome (WebGL through SwiftShader),
#     into $OUT (default /tmp/web_smoke/), to look at (the pages are
#     opened as files: a texture loaded over HTTP shows as magenta; make
#     serve-build for the real thing).
# Build first: make js (or dune build games/<genre>/web for each genre)
#
# Usage: scripts/smoke/smoke_test_web.sh
# Env: BUILD_DIR (default _build), OUT (default /tmp/web_smoke).

set -uo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
B="$ROOT/${BUILD_DIR:-_build}/default"
OUT=${OUT:-/tmp/web_smoke}
mkdir -p "$OUT"
fail=0
# the 3D games, on WebGL: the names of CATALOG.md's rows whose Dir is 3D
THREE_D=" $(grep '^| \[[A-Za-z0-9]*\]([^)]*) | 3D |' "$ROOT/CATALOG.md" | sed 's/^| \[\([A-Za-z0-9]*\)\].*/\1/' | tr '\n' ' ') "
for js in "$B"/games/*/web/*.bc.js; do
  name=$(basename "$js" .bc.js)
  where=$(basename "$(dirname "$(dirname "$js")")")/web/$name
  case "$THREE_D" in *" $name "*) continue ;; esac
  res=$(timeout 60 node "$ROOT/scripts/web/web_headless.js" "$js" 600 ' ',1,ArrowUp,ArrowRight,ArrowLeft,' ' 2>&1 | tail -1)
  case "$res" in
    OK*) echo "ok     games/$where" ;;
    *) echo "FAILED games/$where: $res"; fail=1 ;;
  esac
done
CHROME=$(command -v google-chrome || command -v chromium || true)
if [ -z "$CHROME" ]; then echo "no Chrome: WebGL pages skipped"; exit $fail; fi
for html in "$B"/games/*/web/*.html; do
  name=$(basename "$html" .html)
  where=$(basename "$(dirname "$(dirname "$html")")")/web/$name
  case "$THREE_D" in *" $name "*) ;; *) continue ;; esac
  timeout 60 "$CHROME" --headless --no-sandbox --use-angle=swiftshader --enable-unsafe-swiftshader \
    --window-size=800,800 --virtual-time-budget=3000 --screenshot="$OUT/$name.png" "file://$html" >/dev/null 2>&1 \
    && echo "shot   games/$where -> $OUT/$name.png" || { echo "FAILED games/$where"; fail=1; }
done
exit $fail
