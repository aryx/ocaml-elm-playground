(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A bounding volume hierarchy: the solids of a scene in a tree of
 * boxes, so that a ray is tested against the few solids along its way
 * rather than against all of them.
 *
 * Brute force (Raytrace.nearest) asks every solid, and a scene of n
 * solids costs n tests per ray: Cubes3d's 300 triangles, a million
 * pixels, 300 million tests, 29 seconds. But a ray that misses a box
 * misses everything in it, and a box is one cheap test (Ray.box, the
 * slabs). So put the solids in two boxes, each of those in two, and
 * so on down to a few solids per leaf:
 *
 *                     [ the whole scene ]
 *                    /                   \
 *         [ left half ]                [ right half ]      a ray here
 *          /        \                    /        \        misses the
 *     [ a b ]     [ c d ]           [ e f ]     [ g h ]    left box:
 *                                                          a..d never
 *                                                          tested
 *
 * A ray goes down only into the boxes it enters, the nearer child
 * first, and skips a box that starts beyond the nearest hit found so
 * far: about log n boxes and a handful of solids, instead of n.
 *
 * {1 Where to cut}
 *
 * The tree is built once per picture, top down, cutting each group of
 * solids in two along an axis. Two ways to choose the cut, both kept
 * to be compared:
 *
 * - [Median]: along the longest side of the group's box, half the
 *   solids on each side. Simple, and a balanced tree -- but balanced
 *   in number, not in space: a big empty region can end up inside a
 *   box that every ray then enters for nothing.
 * - [Sah], the surface area heuristic (Goldsmith and Salmon 1987;
 *   MacDonald and Booth 1990): the chance that a ray crossing a box
 *   also crosses a box inside it is the ratio of their surface areas.
 *   So a cut into L and R costs, in expected tests,
 *
 *        cost = traversal + (area L * count L + area R * count R) / area
 *
 *   and the cut with the least cost wins; a group is a leaf when no
 *   cut beats testing all of it. Tight boxes, around the solids rather
 *   than around the count. Where to try cuts: at every solid, sorted
 *   by its centre along each axis, is exact and was the first version
 *   -- and 16 s to build 100,000 triangles, a sort per axis per level.
 *   At the edges of 12 bins per axis instead (binned SAH, Wald 2007),
 *   each level is one pass over its solids: 1 s, and trees as good (4.9
 *   tests a ray, against 4.9).
 *
 * {1 A box must never say no}
 *
 * ... when the solid inside it would say yes. Found by the property
 * test: a ray aimed exactly at a triangle's corner is aimed at its
 * box's corner too, where the three slabs' intervals (Ray.box) should
 * meet in a single t -- and after rounding, miss each other by the
 * last bit, while Moller-Trumbore, whose edges include their ends,
 * hits. A tree that trusts its boxes then loses the triangle. So each
 * box is a hair larger than its solid, a billionth of its size: far
 * more than the rounding, far less than a pixel. (PBRT does the same by
 * widening each slab test's far t by its worst rounding error.)
 *
 * The planes, infinite, have no box: they sit beside the tree, tested
 * by every ray (a scene has a few).
 *
 * The tree must find exactly the hit brute force finds -- the same t,
 * the same solid -- which a property test checks on random scenes
 * (graphics/tests/Unit_raytrace.ml), and the pictures are the same
 * bytes (Raytrace's [acceleration]).
 *
 * {1 How much faster}
 *
 * Measured (graphics/tests/bench/Raytrace_bench.ml, 400 x 300, shadow
 * rays included, CPU seconds; natively, and under node, the browser's
 * engine):
 *
 *                                     tests   boxes   trace, native   node
 *                                     a ray   a ray
 *   Cubes3d's 300 triangles
 *     brute force                     583.4     0        6.55 s     20.2 s
 *     BVH, median split                 4.7    10.1      0.21 s      1.0 s
 *     BVH, SAH                          2.5     7.5      0.15 s      0.76 s
 *   102,400 triangles (100 spheres)
 *     BVH, median split (build 1.1 s)   9.1    42.0      0.71 s      3.0 s
 *     BVH, SAH (build 1.0 s)            4.9    36.1      0.57 s      2.6 s
 *
 * 43 times faster on 300 triangles, and 340 times as many triangles
 * cost 4 times as much, where brute force would cost 340 times: log n
 * against n. examples/Cubes3d ray traced at 1000 x 1000 with its
 * shadows: 38 s by brute force (-rt-brute), 1.5 s with the tree, the
 * same PNG byte for byte. The browser is 3 to 5 times slower than
 * native code. The median split tests twice the solids SAH does, and
 * is the one to read first.
 *
 * {1 Where the stack is small}
 *
 * The first build under node overflowed its stack on the 100,000
 * triangles: List.map, List.filter, List.init recurse once per element
 * there (native OCaml's stack is far larger). The build works on
 * arrays for that reason -- a browser is where the Povray way runs.
 *
 * References: Timothy L. Kay and James T. Kajiya, "Ray Tracing Complex
 * Scenes" (SIGGRAPH 1986), the hierarchy of bounding volumes and the
 * slabs; Jeffrey Goldsmith and John Salmon, "Automatic Creation of
 * Object Hierarchies for Ray Tracing" (IEEE CG&A, 1987), the surface
 * area heuristic; J. David MacDonald and Kellogg S. Booth, "Heuristics
 * for Ray Tracing Using Space Subdivision" (The Visual Computer, 1990);
 * James Arvo and David Kirk, "A Survey of Ray Tracing Acceleration
 * Techniques", chapter 6 of Glassner (ed.), An Introduction to Ray
 * Tracing (1989). And the ICFP 2000 contest's second place, Camls 'R
 * Us (INRIA), whose bounding spheres alone skipped 75% of the exact
 * tests. *)

type split = Median | Sah

type t

(* [build ~split solids]: the tree over the solids *)
val build : split:split -> Solid.t list -> t

(* what the searches cost: boxes entered, solids tested, since the
 * tree was built *)
type stats = { mutable boxes : int; mutable tests : int }

val stats : t -> stats

(* the nearest solid the ray meets between [min_t] and [max_t], and at
 * which t: Raytrace.nearest's answer, from fewer tests *)
val nearest : t -> min_t:float -> max_t:float -> Ray.t -> (float * Solid.t) option

(* does the ray meet any solid between [min_t] and [max_t]? (a shadow
 * ray: the first one found will do) *)
val any : t -> min_t:float -> max_t:float -> Ray.t -> bool

(* the tree's depth, and its leaves' largest size *)
val depth : t -> int
