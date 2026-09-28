(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Body3d.mli *)

type t = {
  pos : Vec3.t;
  vel : Vec3.t;
  orientation : Quat.t;
  spin : Vec3.t;
  mass : float;
  inertia : Mat3.t;
  inv_inertia : Mat3.t;
}

let never_turns = Mat3.diagonal infinity infinity infinity

(* claude: a diagonal tensor is inverted entry by entry, where an
 * infinite axis gives 1/infinity = 0 ("no impulse can spin it about
 * this one"); anything else goes through the general inverse, and a
 * singular tensor means the same thing *)
let inverse_inertia (i : Mat3.t) : Mat3.t =
  let diagonal = i.Mat3.m01 = 0. && i.Mat3.m02 = 0. && i.Mat3.m10 = 0. && i.Mat3.m12 = 0. && i.Mat3.m20 = 0. && i.Mat3.m21 = 0. in
  if diagonal then Mat3.inverse_diagonal i
  else match Mat3.inverse i with Some m -> m | None -> Mat3.zero

let with_inertia i b = { b with inertia = i; inv_inertia = inverse_inertia i }

let make ?(vel = (0., 0., 0.)) ?(orientation = Quat.identity) ?(spin = (0., 0., 0.)) ?(mass = 1.) ?(inertia = never_turns) pos =
  { pos; vel; orientation; spin; mass; inertia; inv_inertia = inverse_inertia inertia }

let inertia_world b = Mat3.conjugate (Quat.to_mat3 b.orientation) b.inertia
let inv_inertia_world b = Mat3.conjugate (Quat.to_mat3 b.orientation) b.inv_inertia
let point_velocity b r = Vec3.add b.vel (Vec3.cross b.spin r)

let box ~mass (w, h, d) =
  let m = mass /. 12. in
  Mat3.diagonal (m *. ((h *. h) +. (d *. d))) (m *. ((w *. w) +. (d *. d))) (m *. ((w *. w) +. (h *. h)))

let solid_sphere ~mass ~radius =
  let i = 0.4 *. mass *. radius *. radius in
  Mat3.diagonal i i i

(* i + m (|r|^2 E - r r^T) *)
let shifted ~mass ((rx, ry, rz) as r) i =
  let r2 = Vec3.dot r r in
  let outer =
    Mat3.of_rows (rx *. rx, rx *. ry, rx *. rz) (ry *. rx, ry *. ry, ry *. rz) (rz *. rx, rz *. ry, rz *. rz)
  in
  Mat3.add i (Mat3.scale mass (Mat3.add (Mat3.scale r2 Mat3.identity) (Mat3.scale (-1.) outer)))
