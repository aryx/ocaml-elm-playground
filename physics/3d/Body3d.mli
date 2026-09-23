(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A body's state, in 3D: where it is, how fast it goes, which way it
 * points, how it spins, and how hard each of those is to change (see
 * notes_3d_physics.md sections 3-5). The twin of physics/2d/Body, and
 * the differences are the whole of what a dimension costs:
 *
 *      2D                              3D
 *      pos, vel : Vec2                 pos, vel : Vec3
 *      angle    : float (the caller's) orientation : Quat.t (ours)
 *      spin     : float                spin : Vec3.t (axis * rad/s)
 *      inertia  : float                inertia : Mat3.t, and it turns
 *                                                with the body
 *
 * Units are metres, seconds, kilograms (physics/2d counts in pixels):
 * gravity is 9.8, a pinball is 0.027 across. Only ratios of masses
 * matter to a collision, but writing the real ones down is what lets a
 * simulation be checked against the world.
 *
 *             vel                  spin (a vector: the axis it turns
 *            ^                    /     about, its length rad/s)
 *           /                    /
 *      pos o======o r       .---o---.       a point at r from the
 *                          (         )      centre moves at
 *                           '-------'       vel + spin x r
 *
 * Why the orientation is kept here but the 2D angle was left to the
 * caller: in 2D nothing in the engine needed the angle (a disk's
 * inertia is the same whichever way it faces). In 3D the tensor is
 * only diagonal in the body's own frame, so every step needs R to
 * carry it into the world ([inv_inertia_world]) -- the orientation is
 * no longer decoration, it is part of the equations.
 *
 * Two infinities, as in 2D: a mass of [infinity] is a body nothing can
 * push (a floor), and an inertia of [never_turns] is a body no impulse
 * can spin (a player, a pinball flipper). Both are read through their
 * inverses, where infinity becomes a harmless 0.
 *
 * References: Baraff and Witkin, "Physically Based Modeling" (SIGGRAPH
 * course notes, 1997-2001); Ian Millington, Game Physics Engine
 * Development (2007), chapters 9-10. *)

type t = {
  pos : Vec3.t;
  (* per second *)
  vel : Vec3.t;
  (* which way it points *)
  orientation : Quat.t;
  (* angular velocity, world frame: the axis it turns about, its length
   * in radians per second *)
  spin : Vec3.t;
  (* > 0; infinity for an immovable body *)
  mass : float;
  (* the inertia tensor in the *body* frame, where it is constant (and
   * diagonal, for every shape whose axes are its principal ones) *)
  inertia : Mat3.t;
  (* its inverse, kept because that is what every impulse formula uses;
   * [Mat3.zero] for a body that never turns *)
  inv_inertia : Mat3.t;
}

(* the tensor of a body no torque can spin: infinite about every axis *)
val never_turns : Mat3.t

(* [make ?vel ?orientation ?spin ?mass ?inertia pos]: at [pos], still,
 * pointing along its own axes, of mass 1, and never turning -- the
 * same defaults as physics/2d/Body.make, one dimension up *)
val make :
  ?vel:Vec3.t -> ?orientation:Quat.t -> ?spin:Vec3.t -> ?mass:float -> ?inertia:Mat3.t -> Vec3.t -> t

(* [with_inertia i b]: [b] with the body-frame tensor [i], its inverse
 * recomputed. The inverse of a diagonal tensor is taken entry by entry,
 * so an infinite axis simply becomes a 0 (and a singular tensor that is
 * not diagonal is read as "never turns" too). *)
val with_inertia : Mat3.t -> t -> t

(* R I R^T and R I^-1 R^T: the tensor and its inverse in the world
 * frame, which is where contacts and torques are. Recomputed every
 * step -- see Mat3.mli. *)
val inertia_world : t -> Mat3.t
val inv_inertia_world : t -> Mat3.t

(* [point_velocity b r]: how fast the point [r] from the centre moves,
 * vel + spin x r. Example: a wheel spinning at 2 rad/s about +y, not
 * moving: its point at (1, 0, 0) moves at (0, 0, -2). *)
val point_velocity : t -> Vec3.t -> Vec3.t

(*****************************************************************************)
(* {1 Tensors of the usual shapes} *)
(*****************************************************************************)
(* In the body frame, about the centre of mass. These live here until
   Hitbox3d (the plan's phase 4) gives every shape its own. *)

(* [box ~mass (w, h, d)]: m/12 * diag (h^2+d^2, w^2+d^2, w^2+h^2) *)
val box : mass:float -> Vec3.t -> Mat3.t

(* [solid_sphere ~mass ~radius]: 2/5 m r^2 about every axis, so a
 * sphere's tensor is the one that does not care how it is turned *)
val solid_sphere : mass:float -> radius:float -> Mat3.t

(* [shifted ~mass offset i]: the parallel-axis theorem in 3D,
 * i + m (|r|^2 E - r r^T): the tensor of a part whose centre sits
 * [offset] from the whole body's. Adding two of these is how a
 * compound body (PhysicsSpin3d.ml's T-handle) gets its tensor. *)
val shifted : mass:float -> Vec3.t -> Mat3.t -> Mat3.t
