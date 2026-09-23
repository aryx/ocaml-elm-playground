(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The shapes a collision is computed between -- what a body *is* to
 * the engine, as opposed to what it looks like (see
 * notes_3d_physics.md section 7). The 3D twin of physics/2d/Shape, and
 * the four here are the four nearly every 3D engine ships:
 *
 *    Sphere r          Box (hx, hy, hz)       Capsule (h, r)   Plane
 *       .--.            +-------+                 .--.          ____
 *      (    )          /       /|                ( .. )        /
 *       '--'          +-------+ |                |    |       /
 *                     |       | +                |    |      the floor,
 *    cheapest of      |       |/                 ( '' )      and static
 *    all, and the     +-------+                   '--'
 *    one everything                            a segment
 *    else is tested                            with a radius:
 *    against          a *turned* box (an       no corners to
 *                     OBB), not an AABB        catch on stairs
 *
 * A [placed] hitbox is one of these plus where it is and which way it
 * is turned; [Collide3d] takes two of them and answers with a
 * [Contact3d.t].
 *
 * The capsule earns its place: it is cheap, it has no corners to catch
 * on a staircase, and it is what nearly every game character in the
 * world is (notes_3d_physics.md section 14 is about exactly that).
 *
 * {b What is not here yet}, and why: a **convex hull** needs its
 * *faces*, not only its points -- a list of vertices does not say
 * which triples are a face, and SAT needs the face normals -- so it
 * waits for a game with one, or for GJK (Collide3d.mli names it); and
 * a **static triangle mesh** for a level waits for a level, with
 * [Collide3d.ray_triangle] and [Collide3d.sphere_triangle] already
 * there to build it out of.
 *
 * References: Christer Ericson, Real-Time Collision Detection (2005),
 * chapters 4 and 5; the capsule's inertia tensor is a cylinder plus
 * two hemispheres, moved onto the axis by the parallel-axis theorem
 * (Body3d.shifted does the same job for compound bodies). *)

type t =
  | Sphere of float (* radius *)
  | Box of Vec3.t (* half-extents: a box 2 x 1 x 4 is (1, 0.5, 2) *)
  | Capsule of float * float (* half the length of its segment, and its radius *)
  | Plane of Vec3.t * float (* a unit normal, and how far along it the plane sits *)

(* a hitbox somewhere, turned: what Collide3d works on *)
type placed = { shape : t; pos : Vec3.t; orientation : Quat.t }

val place : ?orientation:Quat.t -> Vec3.t -> t -> placed

(* the smallest axis-aligned box holding it, for a broad phase (a
 * [Plane] gives an infinite one) *)
val bounds : placed -> Vec3.t * Vec3.t

(* how much room it takes up ([Plane]: 0, it has none) *)
val volume : t -> float

(* [inertia ~mass shape]: its tensor in its own frame, about its centre
 * -- what Body3d wants. A [Plane] never turns. *)
val inertia : mass:float -> t -> Mat3.t

(*****************************************************************************)
(* {1 Reading a placed hitbox} *)
(*****************************************************************************)

(* a box's 8 corners, in the world; anything else: [] *)
val corners : placed -> Vec3.t list

(* a box's 3 face normals (its own axes, turned); a plane's normal;
 * anything else: [] *)
val face_axes : placed -> Vec3.t list

(* a capsule's segment, in the world: its two end centres *)
val segment : placed -> Vec3.t * Vec3.t

(* [support p direction]: the point of [p] farthest along [direction] --
 * the one operation every convex collision algorithm is built on
 * (SAT uses it to project, GJK would use nothing else) *)
val support : placed -> Vec3.t -> Vec3.t

(* [extent p axis]: how far [p] reaches along a *unit* [axis], as the
 * (lowest, highest) of its projection -- SAT's whole vocabulary *)
val extent : placed -> Vec3.t -> float * float
