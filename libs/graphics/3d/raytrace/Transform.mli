(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A solid moved, stretched, turned: an affine transform of space, a
 * 3 x 3 matrix and a translation, p -> M p + o -- kept together with
 * its inverse.
 *
 * {1 Move the ray, not the solid}
 *
 * A ray tracer does not transform its solids. Each primitive is
 * written once, at the origin and of size one, where its intersection
 * is simplest (the unit sphere: t^2 + 2 (o.d) t + o.o - 1 = 0), and a
 * transformed one is intersected by moving the *ray* the other way,
 * into the primitive's own space:
 *
 *      world                        object space
 *                    inverse
 *      o --d-->  (ellipsoid)  ----->   o' --d'--> ( unit sphere )
 *
 *      o' = M^-1 (o - translation)      d' = M^-1 d
 *
 * which is why a sphere scaled by (2, 1, 1) is an ellipsoid for free,
 * and a box turned 30 degrees is still tested by its slabs. The t
 * found there is the t here, if d' is not normalized; Ray's directions
 * are, so the object's t is divided by |d'| on the way back (Solid).
 *
 * {1 The normal goes by the inverse transpose}
 *
 * Points go by M, and so do tangents, but not normals: stretch a
 * sphere into a flat ellipsoid and its normals must turn towards the
 * flat side, the opposite of what M does to them. The normal that
 * stays perpendicular to every transformed tangent is (M^-1)^T n:
 *
 *      n . v = 0   =>   ((M^-1)^T n) . (M v) = n^T M^-1 M v = 0
 *
 * {1 Never invert a matrix}
 *
 * The inverse is kept beside the transform, built up with it: moving
 * by v is undone by moving by -v, scaling by 2 by scaling by 1/2,
 * turning by a by turning by -a, and (A then B)'s inverse is B's
 * inverse then A's. No general inversion, no determinant, no division
 * by one close to 0.
 *
 * References: Pat Hanrahan, "A Survey of Ray-Surface Intersection
 * Algorithms", chapter 3 of Glassner (ed.), An Introduction to Ray
 * Tracing (1989); Ken Turkowski, "Properties of Surface-Normal
 * Transformations", in Graphics Gems (1990), for the transpose. *)

type t

(* nothing moved *)
val identity : t

val translate : Vec3.t -> t

(* [scale (x, y, z)]: stretched by x along the x axis, ...; a zero is
 * refused (Invalid_argument): a flattened solid has no inverse *)
val scale : Vec3.t -> t

(* [rotate axis degrees]: turned about the x (0), y (1) or z (2) axis,
 * counterclockwise seen from its positive end *)
val rotate : int -> float -> t

(* [compose a b]: b first, then a *)
val compose : t -> t -> t

(* a point, a direction (no translation), both there and back *)
val point : t -> Vec3.t -> Vec3.t
val direction : t -> Vec3.t -> Vec3.t
val inverse_point : t -> Vec3.t -> Vec3.t
val inverse_direction : t -> Vec3.t -> Vec3.t

(* [normal t n]: an object-space normal in the world, (M^-1)^T n,
 * normalized *)
val normal : t -> Vec3.t -> Vec3.t

(* is it a translation only? (the solids that can move exactly then
 * stay what they are) *)
val is_translation : t -> bool
