(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A ray, a point and a direction, and where it meets the shapes a
 * renderer or a physics engine is made of: a sphere, a plane, a
 * triangle, a box. The one primitive under two users -- the ray tracer
 * (graphics/3d/raytrace/: what does the eye see through this pixel?)
 * and physics/3d's Collide3d (what does this bullet, this mouse click,
 * this wheel touch?) -- tested once, in graphics/tests/Unit_ray.ml.
 *
 *      origin                      direction, a unit vector
 *        o-------------------------->
 *        |<--- t = 3 --->|
 *                        x  at r 3.: the point 3 units along
 *
 * {1 Two choices, both for the ray tracer}
 *
 * The direction is **a unit vector, made so once**, by [make]. Then t
 * is a distance, and the dot product of the direction with anything is
 * a projection -- which the sphere's formula below relies on. Forget to
 * normalize and every sphere comes out the wrong size: the first bug
 * of the author's own ICFP 2000 ray tracer, removed here by
 * construction rather than by care.
 *
 * Every test answers **the whole line**, not the first hit in front:
 * both points where the line crosses a sphere, even behind the origin,
 * the signed t of a plane or a triangle. A ray tracer needs the two
 * points to do constructive solid geometry (where is the ray *inside*
 * a sphere minus a box?), and a hit behind the eye is the caller's to
 * throw away -- a one-line [if t > 0.], where the question is asked.
 *
 * {1 The sphere, worked}
 *
 * A point of the ray is o + t u; it is on the sphere of centre c and
 * radius r when |o + t u - c|^2 = r^2. With m = o - c and u a unit
 * vector, that is t^2 + 2 (m.u) t + (m.m - r^2) = 0, whose roots are
 *
 *      t = -b -/+ sqrt (b^2 - k)      where b = m.u, k = m.m - r^2
 *
 * No root (b^2 < k): the line misses. Example: from (0, 0, 5) towards
 * -z, the unit sphere at the origin: m = (0, 0, 5), b = -5, k = 24,
 * t = 5 -/+ 1, entering at 4 and leaving at 6.
 *
 * References: Tomas Moller and Ben Trumbore, "Fast, Minimum Storage
 * Ray/Triangle Intersection" (Journal of Graphics Tools, 1997); Timothy
 * L. Kay and James T. Kajiya, "Ray Tracing Complex Scenes" (SIGGRAPH
 * 1986) for the slabs; Christer Ericson, Real-Time Collision Detection
 * (2005), chapter 5.3, for all four; Eric Haines, "Essential Ray
 * Tracing Algorithms", chapter 2 of Glassner (ed.), An Introduction to
 * Ray Tracing (1989). *)

type t = private { origin : Vec3.t; direction : Vec3.t (* of length 1 *) }

(* [make origin direction]: the direction normalized. A zero direction
 * is refused (Invalid_argument): it is not a ray. *)
val make : Vec3.t -> Vec3.t -> t

(* [at ray t]: the point t along it, origin + t direction *)
val at : t -> float -> Vec3.t

(*****************************************************************************)
(* {1 Intersections} *)
(*****************************************************************************)

(* [sphere ray (centre, radius)]: where the line enters and leaves the
 * sphere, t_in <= t_out, either possibly negative (behind the origin;
 * t_in < 0 <= t_out when the origin is inside), or [None] if it
 * misses. A line grazing the sphere enters and leaves at the same t. *)
val sphere : t -> Vec3.t * float -> (float * float) option

(* [plane ray (n, d)]: the plane of the points p with n.p = d (n need
 * not be a unit vector), met at t, possibly negative; [None] when the
 * ray runs parallel to it. Example: the floor y = -4, (n, d) =
 * ((0, 1, 0), -4.), from the origin downwards: t = 4. *)
val plane : t -> Vec3.t * float -> float option

(* [triangle ray (a, b, c)]: Moller and Trumbore (1997). [Some (t, u,
 * v)] when the line crosses the triangle, at the point
 *
 *      (1 - u - v) a + u b + v c   =   at ray t
 *
 * (u and v, the barycentric coordinates, are what a renderer
 * interpolates normals and texture coordinates with); t possibly
 * negative; [None] outside its edges, or when the ray runs in its
 * plane. Either side of the triangle is hit: culling back faces is the
 * caller's choice. *)
val triangle : t -> Vec3.t * Vec3.t * Vec3.t -> (float * float * float) option

(* [box ray (lo, hi)]: the slab test (Kay and Kajiya 1986) against the
 * box whose sides are parallel to the axes, from corner lo to corner
 * hi: the interval (t_in, t_out) of the line inside it, or [None].
 * Each pair of parallel sides -- a slab -- cuts the line into an
 * interval; the box is where the three intervals overlap:
 *
 *            x slab
 *          |       |
 *      ----|-------|------>        in x:  [t1, t2]
 *          |   +---+---+           in y:  [t3, t4]
 *          |   |   |   |  y slab   inside both: [max t1 t3, min t2 t4]
 *          |   +---+---+           empty when max > min: a miss
 *
 * What a bounding volume hierarchy tests at each node (the ray tracer's
 * Bvh), and what Collide3d's box is, turned into the box's own frame. *)
val box : t -> Vec3.t * Vec3.t -> (float * float) option
