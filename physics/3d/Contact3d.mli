(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A contact: how two overlapping hitboxes touch -- the 3D twin of
 * physics/2d/Contact, and the same three numbers (see
 * notes_3d_physics.md sections 7 and 9).
 *
 *            normal
 *      .----->
 *     (  a  (/)  b  )    the direction to push b away from a (a unit
 *      '---'  depth      vector), how far they overlap along it, and
 *                        a point where they touch
 *
 * Example: spheres of radius 3 at the origin and 2 at (3, 4, 0): their
 * centres are 5 apart, 0 less than 3 + 2, so they just touch -- move
 * the second to (2.4, 3.2, 0) (4 apart) and the depth is 1 with the
 * normal (0.6, 0.8, 0).
 *
 * The point matters more in 3D than in 2D: the impulse of a collision
 * uses r x n at each body's centre (section 9), so a contact off the
 * centre line is what makes a struck box spin. Resolve3d (phase 5) is
 * where that happens; this module only carries the numbers. *)

type t = {
  normal : Vec3.t; (* unit, from the first hitbox towards the second *)
  depth : float; (* > 0: how far they overlap along the normal *)
  point : Vec3.t; (* where they touch; the middle of the overlap for a face *)
}

(* [make ~normal ~depth ~point]: the normal is normalized here *)
val make : normal:Vec3.t -> depth:float -> point:Vec3.t -> t

(* the same contact seen from the other hitbox: the normal turned round *)
val flip : t -> t
