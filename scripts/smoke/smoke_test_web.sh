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
#   - each games/web/*.bc.js run in node with a fake DOM
#     (scripts/web/web_headless.js), 600 frames, pressing space,
#     1, the arrows: any exception or hang is reported;
#   - each games3d/webgl/*.html screenshotted in headless Chrome (WebGL
#     through SwiftShader), into $OUT (default /tmp/web_smoke/), to look at
#     (the pages are opened as files: a texture loaded over HTTP, like
#     TinyMinecraft's, shows as magenta; make serve-build for the real thing).
# Build first: dune build @games/web/default @games3d/webgl/default
#
# Usage: scripts/smoke/smoke_test_web.sh
# Env: BUILD_DIR (default _build), OUT (default /tmp/web_smoke).

set -uo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
B="$ROOT/${BUILD_DIR:-_build}/default"
OUT=${OUT:-/tmp/web_smoke}
mkdir -p "$OUT"
fail=0
for js in "$B"/games/web/*.bc.js; do
  name=$(basename "$js" .bc.js)
  res=$(timeout 60 node "$ROOT/scripts/web/web_headless.js" "$js" 600 ' ',1,ArrowUp,ArrowRight,ArrowLeft,' ' 2>&1 | tail -1)
  case "$res" in
    OK*) echo "ok     games/web/$name" ;;
    *) echo "FAILED games/web/$name: $res"; fail=1 ;;
  esac
done
CHROME=$(command -v google-chrome || command -v chromium || true)
if [ -z "$CHROME" ]; then echo "no Chrome: WebGL pages skipped"; exit $fail; fi
for html in "$B"/games3d/webgl/*.html; do
  name=$(basename "$html" .html)
  timeout 60 "$CHROME" --headless --no-sandbox --use-angle=swiftshader --enable-unsafe-swiftshader \
    --window-size=800,800 --virtual-time-budget=3000 --screenshot="$OUT/$name.png" "file://$html" >/dev/null 2>&1 \
    && echo "shot   games3d/webgl/$name -> $OUT/$name.png" || { echo "FAILED games3d/webgl/$name"; fail=1; }
done
exit $fail
