(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* 3x3 matrices, for the one job 3D physics cannot do without them: the
 * inertia tensor (see notes_3d_physics.md section 4).
 *
 * In 2D a body's resistance to being spun is one number (Body.inertia,
 * m r^2 / 2 for a disk). In 3D it is a matrix I with
 *
 *   L = I w        the angular momentum, from the angular velocity
 *
 * and the surprise is that L and w then point in *different*
 * directions unless w lies along one of the body's principal axes.
 * Everything odd about 3D rotation follows from that one line.
 *
 *          y                        I of a box, in its own frame:
 *          |    +-------+
 *          |   /       /|             m  | h^2+d^2     0       0    |
 *          |  +-------+ | h     I =  --- |    0     w^2+d^2    0    |
 *          |  |       | +            12  |    0        0    w^2+h^2 |
 *          |  |       |/  d
 *          +--+-------+------ x      diagonal: x, y, z *are* the
 *            /    w                  principal axes of a box
 *
 * A tensor written like that is in the *body* frame, where it is
 * constant. The solver wants it in the world frame, where the body has
 * turned by R:
 *
 *   I_world = R I R^T                 ([conjugate])
 *
 * which is this module's reason to exist, and the 3D engine's one
 * unavoidable per-step matrix job. (R^T is R's inverse, a rotation
 * matrix being orthogonal -- which is why the formula is two multiplies
 * and no inversion.)
 *
 * Row-major, and [mul_vec m v] is the usual m v (a column vector on the
 * right): [of_rows] takes the rows in reading order.
 *
 * References: David Baraff and Andrew Witkin, "Physically Based
 * Modeling" (SIGGRAPH course notes, 1997-2001), the rigid-body
 * derivation every engine's is downstream of; Christer Ericson,
 * Real-Time Collision Detection (2005), appendix A for the algebra. *)

(*****************************************************************************)
(* {1 Making them} *)
(*****************************************************************************)

type t = {
  m00 : float;
  m01 : float;
  m02 : float;
  m10 : float;
  m11 : float;
  m12 : float;
  m20 : float;
  m21 : float;
  m22 : float;
}

val zero : t
val identity : t

(* [of_rows (a, b, c) (d, e, f) (g, h, i)]: the matrix written out in
 * reading order, row by row *)
val of_rows : Vec3.t -> Vec3.t -> Vec3.t -> t

(* [diagonal a b c]: zero everywhere but the diagonal -- a tensor in the
 * frame of its own principal axes, which is how every [Body3d] tensor
 * here starts out *)
val diagonal : float -> float -> float -> t

(*****************************************************************************)
(* {1 Arithmetic} *)
(*****************************************************************************)

val add : t -> t -> t
val scale : float -> t -> t
val mul : t -> t -> t
val transpose : t -> t

(* m v, the vector rotated/transformed by m *)
val mul_vec : t -> Vec3.t -> Vec3.t

(* [conjugate r m] is r m r^T: [m] expressed in the frame [r] turns
 * into. With [r] a rotation this is the tensor's trip from the body
 * frame to the world frame, done every step. *)
val conjugate : t -> t -> t

(*****************************************************************************)
(* {1 Inverting} *)
(*****************************************************************************)

(* the determinant, and the general inverse (None when the determinant
 * is 0: a body that cannot be spun about some axis at all, which here
 * means "never turns" and is written as [zero] instead -- see
 * [Body3d.inv_inertia]) *)
val det : t -> float
val inverse : t -> t option

(* the inverse of a [diagonal] matrix, by hand: three divisions, and
 * what a principal-axis tensor actually needs *)
val inverse_diagonal : t -> t
