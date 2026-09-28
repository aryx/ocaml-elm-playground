(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Constructive solid geometry: solids made of solids -- the union of
 * two, their intersection, one minus the other -- as a ray tracer does
 * it, which is nearly free, where a rasterizer can hardly do it at all.
 *
 * {1 Intervals}
 *
 * A solid is a region of space, with an inside. Along a ray, it is
 * where the ray is inside: a list of intervals of t, from where the
 * ray enters to where it leaves. A sphere gives one, a torus up to
 * two, a half-space a half-line. And the operations on regions are
 * operations on these lists:
 *
 *   A           ====[======]=====[=====]======
 *   B           ========[=======]==============
 *
 *   A u B       ====[================]=======   inside either
 *   A n B       ========[==]=====[==]=========   inside both
 *   A - B       ====[===]============[=]======   inside A, not B
 *
 * each a merge of two sorted lists, walking the boundaries in order
 * and keeping where [op (inside A) (inside B)] holds -- a lesson a
 * student can check by hand. The visible point is the first
 * boundary in front of the eye.
 *
 * {1 The hole's normal}
 *
 * Each boundary remembers the surface it came from, to be lit with
 * its normal. Take a cube minus a cylinder, drilled: where the ray
 * enters the result by leaving the cylinder, the surface seen is the
 * cylinder's -- from inside, the wall of the hole -- and its normal,
 * which points out of the cylinder, points *into* the result. So:
 *
 *   an operand's exit that is the result's entry, or an operand's
 *   entry that is the result's exit, flips the normal
 *
 * one rule for the three operations (only the difference ever uses
 * it: in a union or an intersection the result enters where an operand
 * enters).
 *
 * {1 Worked example}
 *
 * A blind hole: the cube from -1 to 1 minus a cylinder of radius 0.5
 * along the x axis from x = 0 to x = 2 -- drilled from the right, half
 * way in. A ray along the axis from x = 3 going left (t = 3 - x):
 *
 *   cube:              [2, 4]     (x = 1 to -1)
 *   cylinder:          [1, 3]     (x = 2 to 0)
 *   cube - cylinder:   [3, 4]     entered at t = 3, x = 0
 *
 * The ray flies down the hole and meets its bottom, at x = 0: the
 * result's entry is the cylinder's exit, its end cap, whose normal
 * points out of the cylinder, towards -x, away from the eye. Flipped,
 * (1, 0, 0), it faces the eye, and the bottom of the hole is lit as
 * it should. Moved up to y = 0.75, off the hole: the cylinder is [],
 * and the cube minus nothing is [2, 4]. (The tests,
 * graphics/tests/Unit_raytrace.ml, have these, and a point-membership
 * property on random solids.)
 *
 * References: Scott D. Roth, "Ray Casting for Modeling Solids"
 * (Computer Graphics and Image Processing, 1982), the intervals;
 * Aristides Requicha, "Representations for Rigid Solids" (ACM
 * Computing Surveys, 1980), the theory; POV-Ray's union, intersection
 * and difference, and GML's (the ICFP 2000 task's third tier). *)

(* where a ray crosses a surface: at which t, whose surface ([leaf],
 * what the normal and the colour are taken from), and whether that
 * surface's normal is to be turned round (see above) *)
type 'a boundary = { t : float; leaf : 'a; flipped : bool }

(* where the ray is inside: from an entry to an exit, the t's possibly
 * infinite (a half-space) *)
type 'a interval = 'a boundary * 'a boundary

type op = Union | Inter | Diff

(* [combine op a b]: the intervals of A op B, from A's and B's, each
 * sorted and apart *)
val combine : op -> 'a interval list -> 'a interval list -> 'a interval list

(* is a point at [t] inside some interval? *)
val inside : 'a interval list -> float -> bool
