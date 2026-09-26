(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Perlin noise: a smooth random function of space, the same at the
 * same point every time -- the one idea under nearly every procedural
 * texture since, marble and wood below (Solid's patterns), clouds,
 * fire, terrain.
 *
 * Random values at the points of a grid would be a checkerboard of
 * noise; smoothing them gives blobs, and blobs aligned on the grid.
 * Perlin's idea (1985): at each grid point a random *gradient*, a
 * direction the function rises in, 0 at the point itself; a point
 * between is a blend of what the eight corners' slopes say there,
 * blended by a curve flat at both ends, so that no seam shows:
 *
 *        corner's gradient
 *             \   the point: its offset from each corner, dotted
 *              \  with that corner's gradient -- eight numbers,
 *     +---------+  blended by fade (u) = 6u^5 - 15u^4 + 10u^3
 *     |    . p  |  along x, then y, then z
 *     +---------+
 *
 * "Random" by a table: 256 numbers, a permutation of 0..255, written
 * out below and hashing a grid point to one of 12 gradients -- the
 * table Perlin published, so that this noise is his, value for value,
 * and the same on every machine and every run (no Random, as the
 * project's rule). Example: noise 3.14 42 7 = 0.13691995878400012,
 * the value his reference Java code gives (a test).
 *
 * [turbulence] sums the noise at doubling frequencies and halving
 * amplitudes, the absolute value of each: detail at every scale, the
 * veins of marble.
 *
 * References: Ken Perlin, "An Image Synthesizer" (SIGGRAPH 1985), the
 * noise, turbulence, and marble as sin (x + turbulence); Ken Perlin,
 * "Improving Noise" (SIGGRAPH 2002), the fade curve and the 12
 * gradients this is, and his reference implementation's table. *)

(* the value at a point, between about -1 and 1, 0 at every integer
 * point *)
val noise : float -> float -> float -> float

(* [turbulence octaves (x, y, z)]: the sum over i < octaves of
 * |noise (2^i p)| / 2^i *)
val turbulence : int -> Vec3.t -> float

(* the table, for the test that it is a permutation *)
val permutation : int array
