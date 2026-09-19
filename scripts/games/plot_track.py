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
# The racing kit's course (kits/racing/Road.ml's [coast], read from its
# source) in space, as TinyVirtuaRacing builds it (Road.centerline):
# the closest the road comes back near itself (two points at least 40
# segments apart), which must stay above the road's width, and a top
# view of it as a PNG. To choose the degrees per unit of curve, or check
# a changed course.
#
# Usage: scripts/games/plot_track.py [degrees_per_curve (0.8)] [segment_length (2)] [out.png]

import math, re, sys
from PIL import Image, ImageDraw
from ocaml_strings import block

k = float(sys.argv[1]) if len(sys.argv) > 1 else 0.8
L = float(sys.argv[2]) if len(sys.argv) > 2 else 2.0
out = sys.argv[3] if len(sys.argv) > 3 else "/tmp/track.png"

num = r'\(?-?[0-9.]+\)?'
text = block("kits/racing/Road.ml", "coast")
sections = []
for kind, args in re.findall(r'\b(straight|curve_hill|curve|hill) ((?:' + num + r' ?)+)', text):
    vals = [float(v) for v in re.findall(r'-?[0-9.]+', args)]
    n = int(vals[0]); curve = vals[1] if kind in ('curve', 'curve_hill') else 0.
    sections.append((n, curve))

# Road.build's curves: eased in over a quarter, held over a half, eased out
ease_in = lambda a, b, p: a + (b - a) * p * p
ease_io = lambda a, b, p: a + (b - a) * ((-math.cos(p * math.pi) / 2) + 0.5)
curves = []
for n, c in sections:
    e, h, l = n // 4, n // 2, n // 4
    curves += [ease_in(0, c, i / e) for i in range(e)] + [c] * h + [ease_io(c, 0, i / l) for i in range(l)]

x = z = heading = 0.
pts = [(0., 0.)]
for c in curves:
    heading += c * k
    x += L * math.sin(math.radians(heading)); z -= L * math.cos(math.radians(heading))
    pts.append((x, z))
closest = min((math.hypot(pts[i][0] - pts[j][0], pts[i][1] - pts[j][1]), i, j)
              for i in range(0, len(pts), 5) for j in range(i + 40, len(pts), 5))
print(f"{len(curves)} segments, final heading {heading:.0f} degrees")
print(f"closest approach: {closest[0]:.1f} (segments {closest[1]} and {closest[2]})")
xs = [p[0] for p in pts]; zs = [p[1] for p in pts]
s = 580 / max(max(xs) - min(xs), max(zs) - min(zs))
im = Image.new('RGB', (600, 600), 'white'); d = ImageDraw.Draw(im)
d.line([((p[0] - min(xs)) * s + 10, (p[1] - min(zs)) * s + 10) for p in pts], fill='black', width=2)
d.ellipse([(xs[0] - min(xs)) * s + 5, (zs[0] - min(zs)) * s + 5, (xs[0] - min(xs)) * s + 15, (zs[0] - min(zs)) * s + 15], fill='red')
im.save(out); print(out)
