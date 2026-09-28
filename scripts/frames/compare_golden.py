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
# Before approving new golden frames (make approve-golden2d/3d): for
# each frame the tests wrote to _build/default/tests/<2d|3d>/actual/,
# an image with the golden frame, the new one, and their difference
# (the differing pixels in red over a dimmed golden), and on stdout
# the number of differing pixels and the box around them -- "only the
# player's box changed", or not.
#
# Usage: scripts/frames/compare_golden.py [2d|3d ...] [--out DIR]
# Env: BUILD_DIR (default _build).

import os, sys
from PIL import Image, ImageChops

args = sys.argv[1:]
out = "/tmp/golden_diff"
if "--out" in args:
    i = args.index("--out"); out = args[i + 1]; del args[i:i + 2]
dims = args or ["2d", "3d"]
build = os.environ.get("BUILD_DIR", "_build")
os.makedirs(out, exist_ok=True)
for dim in dims:
    actual_dir = f"{build}/default/tests/{dim}/actual"
    if not os.path.isdir(actual_dir): continue
    for name in sorted(os.listdir(actual_dir)):
        new = Image.open(f"{actual_dir}/{name}").convert("RGB")
        golden_path = f"tests/{dim}/golden/{name}"
        if not os.path.exists(golden_path):
            new.save(f"{out}/{dim}_{name}"); print(f"{dim}/{name}: new, no golden frame yet -> {out}/{dim}_{name}"); continue
        old = Image.open(golden_path).convert("RGB")
        diff = ImageChops.difference(old, new)
        mask = diff.convert("L").point(lambda v: 255 if v else 0)
        count = sum(1 for v in mask.getdata() if v)
        shown = Image.blend(old, Image.new("RGB", old.size, "black"), 0.6)
        shown.paste(Image.new("RGB", old.size, "red"), mask=mask)
        sheet = Image.new("RGB", (old.width * 3, old.height)); sheet.paste(old, (0, 0)); sheet.paste(new, (old.width, 0)); sheet.paste(shown, (old.width * 2, 0))
        sheet.save(f"{out}/{dim}_{name}")
        print(f"{dim}/{name}: {count} pixels differ, in the box {diff.getbbox()} -> {out}/{dim}_{name}")
