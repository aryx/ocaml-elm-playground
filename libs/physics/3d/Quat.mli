(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Which way a body points, and how to turn it a little (see
 * notes_3d_physics.md section 3).
 *
 * In 2D an orientation is an angle, one float, and spinning is adding
 * to it. In 3D three angles (pitch, yaw, roll -- what
 * Playground3d.rotate3d takes) will not do as *state*:
 *
 *   - gimbal lock: with the X-then-Y-then-Z convention a pitch of 90
 *     degrees makes the X and Z turns do the same thing, and a degree
 *     of freedom is gone;
 *   - angles do not add. Turn a book 90 degrees about x then about y,
 *     then start again the other way round: two different books.
 *     Rotations compose, and do not commute.
 *
 * A quaternion is four floats, q = (w, v), and the rotation of [angle]
 * about the unit axis [a] is
 *
 *     q = (cos (angle/2), sin (angle/2) * a)
 *
 *     a quarter turn about y:   w = cos 45 = 0.7071
 *                               v = (0, 0.7071, 0)
 *
 * The half-angle is not a decoration: rotating a vector is q v q^-1,
 * which applies the rotation twice, and the halves make it come out
 * right ([rotate]).
 *
 * A physics engine uses exactly three things from all this:
 *
 *   - composing is multiplying ([mul], 16 multiplies against a matrix
 *     product's 27);
 *   - the tensor wants a matrix, so [to_mat3] (see Mat3);
 *   - and the spin's differential equation, which is the whole
 *     orientation half of a time step:
 *
 *         q' = 1/2 (0, w) q          w the angular velocity
 *
 *     stepped as q + dt q', renormalized ([integrate]) -- or exactly,
 *     by turning |w| dt about w ([turned_by]). Integrate3d has both,
 *     and its .mli measures the difference.
 *
 * A quaternion drifts off the unit sphere as floating point
 * accumulates, and one [normalize] per step fixes it -- against
 * Gram-Schmidt on a matrix's 9 numbers, which is the practical reason
 * engines keep the quaternion as the state and the matrix as a
 * derived thing.
 *
 * References: William Rowan Hamilton (1843); Ken Shoemake, "Animating
 * Rotation with Quaternion Curves" (SIGGRAPH 1985), which brought them
 * into graphics; Baraff and Witkin, "Physically Based Modeling"
 * (SIGGRAPH course notes) for q' = 1/2 w q. *)

(*****************************************************************************)
(* {1 Making them} *)
(*****************************************************************************)

(* w is the scalar part, v the vector part; a *unit* quaternion is a
 * rotation (see [normalize]) *)
type t = { w : float; v : Vec3.t }

(* no rotation at all: (1, (0, 0, 0)) *)
val identity : t

(* [of_axis_angle axis radians]: [axis] need not be unit (it is
 * normalized here); a zero axis gives [identity] *)
val of_axis_angle : Vec3.t -> float -> t

(* the axis (unit, or (1, 0, 0) for no rotation) and the angle in
 * radians, in [0, pi] *)
val to_axis_angle : t -> Vec3.t * float

(*****************************************************************************)
(* {1 Composing and rotating} *)
(*****************************************************************************)

(* [mul a b] is the rotation "b first, then a", as matrices compose:
 * R (mul a b) = R a * R b. Not commutative, deliberately. *)
val mul : t -> t -> t

(* (w, -v): the inverse rotation, for a unit quaternion *)
val conjugate : t -> t

val length : t -> float

(* scaled to length 1 -- once a step is enough to keep a rotation a
 * rotation; the zero quaternion becomes [identity] *)
val normalize : t -> t

(* [rotate q v]: q v q^-1, the vector turned. Written as the sandwich
 * it is; the two-cross-product form (v + 2 qv x (qv x v + w v)) is the
 * fast one, and is what a renderer would use. *)
val rotate : t -> Vec3.t -> Vec3.t

(*****************************************************************************)
(* {1 As a matrix, as angles} *)
(*****************************************************************************)

(* the same rotation as a matrix, which is what an inertia tensor's
 * R I R^T needs ([Mat3.conjugate]) *)
val to_mat3 : t -> Mat3.t

(* The three angles, in *degrees*, that Playground3d.rotate3d takes:
 * turn about x, then y, then z. This is the drawing edge, and the only
 * place Euler angles are allowed here -- a one-shot conversion never
 * accumulates, so gimbal lock costs nothing (at a pitch of +/-90
 * degrees many triples describe the same orientation, and every one of
 * them draws the same picture). *)
val to_euler_xyz : t -> float * float * float

(*****************************************************************************)
(* {1 Spinning over time} *)
(*****************************************************************************)

(* [derivative ~spin q] is 1/2 (0, spin) q, the rate at which an
 * orientation changes under an angular velocity -- the equation above,
 * on its own, because seeing it is half the point *)
val derivative : spin:Vec3.t -> t -> t

(* [integrate ~spin ~dt q] = normalize (q + dt * derivative): one first
 * order step of it, which is what production engines do *)
val integrate : spin:Vec3.t -> dt:float -> t -> t

(* [turned_by ~spin ~dt q]: the same step exactly -- turn by |spin| dt
 * about spin, and compose. No cheaper, but it never shortens the
 * rotation the way the first-order step does (Integrate3d.mli has the
 * numbers). *)
val turned_by : spin:Vec3.t -> dt:float -> t -> t
