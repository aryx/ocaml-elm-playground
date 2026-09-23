(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The conserved quantities: physics' referee, as physics/2d/Energy is,
 * and in 3D it referees something 2D has no version of (see
 * notes_3d_physics.md section 4).
 *
 * A body left alone keeps its energy and its momentum; the integrator
 * that loses them is wrong, and by how much says which integrator it
 * is. What 3D adds is that the *angular* momentum
 *
 *     L = I_world w
 *
 * is a vector, and that it, not the angular velocity, is the conserved
 * one. A free body's L is fixed in space; its w is not, because the
 * tensor between them turns with the body. So w moves -- the body
 * wobbles, and about the intermediate axis it flips over, with no
 * force acting at all (PhysicsSpin3d.ml).
 *
 *     spin about a principal axis     spin about the in-between one
 *
 *         L = I w  (parallel)            L         w wanders around L,
 *          ^                             ^   w     and the body turns
 *          |                             |  /      itself over
 *          | w                           | /
 *          o                             o
 *
 * Which makes these functions the sharpest test a 3D rigid-body core
 * has: through every flip, |L| and the kinetic energy must not move.
 * If they do, the tensor is not being rotated.
 *
 * Example: a mass of 2 at a speed of 3 has a kinetic energy of 9 and a
 * momentum of magnitude 6, exactly as in 2D. A sphere of mass 1 and
 * radius 1 (I = 2/5 about every axis) spinning at 10 rad/s has a
 * rotational energy of 0.4 * 100 / 2 = 20 and an |L| of 4. *)

(* m |vel|^2 / 2 *)
val linear_kinetic : Body3d.t -> float

(* w . (I_world w) / 2: a body spinning in place has energy too, and in
 * 3D how much depends on which way it is turned *)
val rotational_kinetic : Body3d.t -> float

(* the two together *)
val kinetic : Body3d.t -> float

(* m vel *)
val momentum : Body3d.t -> Vec3.t

(* I_world w: the spin's own momentum, about the body's centre *)
val angular : Body3d.t -> Vec3.t

(* [angular_momentum ~around b]: r x m vel, r from [around] to the
 * body, plus [angular] -- the whole of L, the quantity a free body
 * conserves and a collision hands around *)
val angular_momentum : around:Vec3.t -> Body3d.t -> Vec3.t

(* under a uniform gravity of [g] along -y: m g y, so that kinetic +
 * gravity is constant in free fall *)
val gravity : g:float -> Body3d.t -> float
