(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Which pairs of bodies are worth testing exactly (see
 * notes_3d_physics.md section 8). The twin of physics/2d/Broadphase,
 * whose .mli is where the three methods are explained -- all pairs, a
 * uniform grid, sort and sweep -- since the ideas do not change with a
 * third coordinate. What changes is what they cost.
 *
 * n bodies make n (n - 1) / 2 pairs: 124,750 for 500 marbles, of which
 * (in the scene measured below) 174 actually overlap. All three
 * methods find those same 174; they differ only in how many bounding
 * boxes they compare on the way.
 *
 * {2 Measured} (500 marbles piled in a box, and 100 of them; this
 * machine, ms per call)
 *
 *                    box tests        ms        pairs found
 *   100 marbles
 *     all pairs          4,950       0.04            33
 *     grid                 282       0.10            33
 *     sweep and prune      229       0.02            33
 *   500 marbles
 *     all pairs        124,750       0.75           174
 *     grid               1,340       0.76           174
 *     sweep and prune    7,472       0.22           174
 *
 * The same shape of answer as in 2D: the grid makes far and away the
 * fewest comparisons and spends the winnings on its hash table, while
 * sweep and prune -- a sort and a list -- is the fastest of the three.
 * Counting tests is not timing them, and the .mli says both.
 *
 * {2 What the third dimension actually changes}
 *
 * **The grid's memory.** In 2D a grid over the playable area is a
 * few thousand cells and an array will do. In 3D the same resolution
 * is cubed: 100 x 100 x 100 is a million cells, nearly all of them
 * empty. [grid] therefore hashes its cells instead of allocating
 * them, and pays only for the ones something is in -- which is also
 * why it is the slower of the two clever methods here.
 *
 * **Sweeping needs an axis, and the choice matters.** In 2D one
 * sweeps x and thinks no more about it. In 3D a pile on a floor is
 * wide in x and z and thin in y, and sweeping the thin axis is nearly
 * as useless as testing everything. The same 500 marbles:
 *
 *      sweeping x      6,644 box tests
 *      sweeping y     84,750            (the pile is 0.6 m tall)
 *      sweeping z      7,472
 *
 * so [sweep_and_prune] picks the axis the bodies are most spread
 * along, by the variance of their centres -- I-COLLIDE's heuristic and
 * Bullet's. It is a heuristic: here it picks z (7,472) where x (6,644)
 * would have been slightly better, because the two are within noise of
 * each other and it has to pick one.
 *
 * {2 What is not here}
 *
 * A **dynamic AABB tree** (Box2D's [b2DynamicTree], Bullet's
 * [btDbvt]), which is what a modern engine defaults to: a
 * bounding-volume hierarchy, cheap to update as bodies move, that also
 * answers the ray queries of Collide3d -- which is why it wins in 3D
 * where in 2D it is roughly a tie. Also not here: keeping the sorted
 * order from the previous frame, where an insertion sort is nearly
 * free because almost nothing has moved past anything else (Baraff's
 * point, and the reason sweep and prune is a *pair manager* in real
 * engines rather than a function).
 *
 * References: David Baraff, "Dynamic Simulation of Non-Penetrating
 * Rigid Bodies" (PhD thesis, Cornell, 1992); Cohen, Lin, Manocha and
 * Ponamgi, "I-COLLIDE" (Symposium on Interactive 3D Graphics, 1995);
 * Christer Ericson, Real-Time Collision Detection (2005), chapter 7. *)

(* a bounding box: (min corner, max corner), as Hitbox3d.bounds gives *)
type box = Vec3.t * Vec3.t

type method_ = All_pairs | Grid | Sweep_and_prune

(* in that order, the simplest first *)
val methods : method_ list

(* e.g. "sweep and prune" *)
val name : method_ -> string

type result = {
  (* the pairs of indices (i, j), i < j, whose boxes overlap, sorted *)
  pairs : (int * int) list;
  (* how many pairs of boxes were compared to find them *)
  tests : int;
}

val all_pairs : box array -> result

(* [grid ?cell boxes]: cubic cells of [cell] on a side, by default
 * [cell_size boxes], hashed rather than allocated *)
val grid : ?cell:float -> box array -> result

(* the largest side of any box: every box then covers at most 2 x 2 x 2
 * cells *)
val cell_size : box array -> float

(* [sweep_and_prune ?axis boxes]: sorted along [axis] (0 = x, 1 = y,
 * 2 = z), by default [widest_axis] *)
val sweep_and_prune : ?axis:int -> box array -> result

(* the axis the boxes' centres are most spread along, by variance: the
 * one to sweep *)
val widest_axis : box array -> int

(* the variance of the centres along each axis, which is how
 * [widest_axis] chooses *)
val spread : box array -> float * float * float

(* one of the three *)
val pairs : method_ -> box array -> result
