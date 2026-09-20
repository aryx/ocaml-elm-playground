(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Do two hitboxes overlap, and if so how -- the narrow phase (see
 * notes_3d_physics.md section 7). physics/2d/Collide's twin, and most
 * of it carries over with a z: a distance against a sum of radii, a
 * centre clamped into a box, shadows on an axis. One idea is genuinely
 * new, and it is the box/box case.
 *
 * {1 The separating axis theorem, and the nine axes everyone forgets}
 *
 * Two convex shapes miss each other exactly when some direction sees
 * their shadows apart. In 2D the directions worth trying are the edge
 * normals of both polygons, a handful. In 3D, face normals are *not
 * enough*:
 *
 *    two boxes crossed like a +, at an angle. No face of either one
 *    separates them, and yet they do not touch: the gap lies along
 *    the direction perpendicular to one edge of each.
 *
 *        A's edge  ----> a
 *        B's edge  ----> b          try a x b as well
 *
 *    3 (A's faces) + 3 (B's faces) + 3 x 3 (edge pairs) = 15 axes
 *
 * Dropping those nine crosses is the classic 3D collision bug: boxes
 * sink into each other at the corners and look glued. [boxes] takes
 * [~edge_axes:false] so that the bug can be watched rather than
 * described -- its test builds a crossed pair that a 6-axis test calls
 * a collision and a sampled ground truth says is apart.
 *
 * {1 Capsules}
 *
 * A capsule is a segment with a radius, so every capsule test is a
 * sphere test on the closest point of a segment:
 *
 *      capsule/capsule -> the distance between two segments
 *                         ([closest_between_segments], Ericson 5.1.9,
 *                         the one piece of real geometry here)
 *      sphere/capsule  -> the closest point of one segment
 *      box/capsule     -> see [box_capsule]: gone back and forth
 *                         between the two shapes until it settles,
 *                         which is where this module stops being exact
 *
 * {1 Rays}
 *
 * 3D needs rays where 2D did not: picking a body with the mouse, a
 * gravity gun's aim, a bullet, a raycast car's wheels, a character's
 * ground check. [ray_sphere] and [ray_plane] are a quadratic and a
 * division, [ray_box] is the slab test in the box's own frame, and
 * [ray_triangle] is Moller-Trumbore (1997): no plane equation, no
 * precomputation, the barycentric coordinates falling out of one cross
 * product each. All of them answer in metres along the ray.
 *
 * {1 The honest limits}
 *
 * - **A moving body must be convex.** A concave shape is either
 *   several convex pieces glued together by the game, or static
 *   geometry. Real engines do convex decomposition; we say so and
 *   stop.
 * - **GJK is not here.** Gilbert, Johnson and Keerthi (1988), with EPA
 *   (van den Bergen, 2001) for the depth, is the general and elegant
 *   route to any pair of convex shapes, and the one a production
 *   engine takes. SAT on boxes plus the primitives covers what our
 *   games need and can be read in an afternoon; GJK is named here as
 *   the next step.
 * - **[box_capsule] is approximate**, and the only approximate test in
 *   the module: see its comment.
 * - **One contact point, not a manifold.** A box resting on a box
 *   touches along a whole face, and stacking needs all four corners
 *   (Catto's "Contact Manifolds", GDC 2007). Phase 8 clips faces
 *   properly; here the point is the middle of the overlap, which is
 *   right for a bounce and not enough for a pile.
 *
 * References: Christer Ericson, Real-Time Collision Detection (2005),
 * chapter 5 for every closest-point routine and the ray tests, chapter
 * 9 for SAT; Gottschalk, Lin and Manocha, "OBBTree" (SIGGRAPH 1996),
 * where the 15-axis test is usually cited from; Moller and Trumbore,
 * "Fast, Minimum Storage Ray/Triangle Intersection" (Journal of
 * Graphics Tools, 1997). *)

open Hitbox3d

(* {1 Closest points} *)

(* the point of a segment nearest a point *)
val closest_on_segment : Vec3.t * Vec3.t -> Vec3.t -> Vec3.t

(* the nearest pair of points of two segments, one on each. Example:
 * (0,0,0)-(1,0,0) and (0.5,1,-1)-(0.5,1,1) cross at right angles a
 * unit apart: ((0.5,0,0), (0.5,1,0)). *)
val closest_between_segments : Vec3.t * Vec3.t -> Vec3.t * Vec3.t -> Vec3.t * Vec3.t

(* the point of a box nearest a point: into the box's frame, clamped to
 * its half-extents, back out. A point inside the box is its own
 * nearest point, which is why [sphere_box] has a second case. *)
val closest_on_box : placed -> Vec3.t -> Vec3.t

(* the point of a triangle nearest a point (Ericson 5.1.5: by regions,
 * so that an edge or a corner comes out right) *)
val closest_on_triangle : Vec3.t * Vec3.t * Vec3.t -> Vec3.t -> Vec3.t

(* {1 Pairs}

   Each returns the contact if they overlap, [None] if they miss; the
   contact's normal points from the *first* argument towards the
   second.

   Touching exactly -- two spheres 5 apart with radii 3 and 2, a ball
   resting on a plane -- counts as apart, everywhere: a contact always
   has a depth greater than 0. A zero-depth contact says nothing a
   solver can use and only makes a resting body jitter. *)

(* the cheapest test there is, and the one everything else falls back
 * on: the distance against the sum of the radii *)
val spheres : Vec3.t * float -> Vec3.t * float -> Contact3d.t option

(* the sphere first. A centre inside the box is a separate case: the
 * way out is then the nearest face, which no closest point can say. *)
val sphere_box : Vec3.t * float -> placed -> Contact3d.t option

val sphere_capsule : Vec3.t * float -> placed -> Contact3d.t option
val capsules : placed -> placed -> Contact3d.t option

(* [boxes ?edge_axes a b]: the 15-axis test above. [~edge_axes:false]
 * tries only the 6 face normals -- wrong, on purpose, so that the
 * difference can be measured. *)
val boxes : ?edge_axes:bool -> placed -> placed -> Contact3d.t option

(* the box first. Approximate: the closest point of the capsule's
 * segment to the box is found by going back and forth (box, segment,
 * box, ...) four times, which converges for convex shapes but is
 * exact only when that closest feature is a point -- a capsule lying
 * flat along a face touches along a line, and this reports its middle.
 * Good enough for a character against a wall (phase 9), not for
 * stacking. *)
val box_capsule : placed -> placed -> Contact3d.t option

(* [plane_hitbox (normal, d) p]: the plane first, so the contact's
 * normal is the plane's -- the way out of a half-space. [d] is how far
 * along [normal] the plane sits. *)
val plane_hitbox : Vec3.t * float -> placed -> Contact3d.t option

val sphere_triangle : Vec3.t * float -> Vec3.t * Vec3.t * Vec3.t -> Contact3d.t option

(* any two hitboxes: dispatched to the right test above, turned round
 * when the pair arrives the other way up *)
val contact : placed -> placed -> Contact3d.t option

(* [contact] without the numbers *)
val touching : placed -> placed -> bool

(* {1 Manifolds}

   [contact] answers with one point, which is all a bounce needs. A box
   resting on a box touches along a whole *face*, and holding a stack
   still needs every corner of it: push at one point and the box tips
   about it, again and again, for ever.

   [manifold] is that face. For two boxes it is found by clipping --
   the reference face is whichever of the two is better aligned with
   the contact normal, the other box's nearest face is the incident
   one, and the incident face is cut down by the reference face's four
   side planes (Sutherland-Hodgman, the same clipping
   games2.5d/TinyDescent.ml does through its portals):

        reference face (the floor's top)      the incident face, clipped
        +----------------------+              +--------+
        |                      |              |  box   |  4 points kept,
        |     +--------+       |     ->       +--------+  the deepest and
        |     |  box   |       |                          the three most
        +-----+--------+-------+                          spread out

   Everything else answers with its single point: a sphere touches a
   plane at one point and always will, and a capsule lying flat on a
   face touches along a line that this engine reports the middle of
   (see [box_capsule]).

   Erin Catto, "Contact Manifolds" (GDC 2007) is the reference. *)

(* up to four contact points for a pair, [] when they miss *)
val manifold : placed -> placed -> Contact3d.t list

(* the pieces, exposed because they are the interesting half: a box's
 * six faces (an outward normal and four corners each), one
 * Sutherland-Hodgman clip against a plane (keeping n . p <= d), and
 * the choice of which points to keep when clipping leaves more than
 * four *)
val box_faces : placed -> (Vec3.t * Vec3.t list) list
val clip_by_plane : Vec3.t list -> Vec3.t -> float -> Vec3.t list
val spread_out : int -> Contact3d.t list -> Contact3d.t list

(* do two axis-aligned boxes overlap: the cheap test a broad phase runs
 * before any of the above (Broadphase3d, phase 6) *)
val bounds_overlap : Vec3.t * Vec3.t -> Vec3.t * Vec3.t -> bool

(* {1 Rays}

   Each answers the distance in metres from [from] along [direction]
   (which need not be a unit vector) to the first hit, or [None]. A ray
   starting inside a shape hits at 0. *)

val ray_sphere : from:Vec3.t -> direction:Vec3.t -> Vec3.t * float -> float option
val ray_plane : from:Vec3.t -> direction:Vec3.t -> Vec3.t * float -> float option
val ray_box : from:Vec3.t -> direction:Vec3.t -> placed -> float option
val ray_capsule : from:Vec3.t -> direction:Vec3.t -> placed -> float option
val ray_triangle : from:Vec3.t -> direction:Vec3.t -> Vec3.t * Vec3.t * Vec3.t -> float option

(* any hitbox *)
val ray : from:Vec3.t -> direction:Vec3.t -> placed -> float option
