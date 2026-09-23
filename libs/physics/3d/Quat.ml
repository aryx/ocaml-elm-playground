(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Quat.mli *)

type t = { w : float; v : Vec3.t }

let identity = { w = 1.; v = (0., 0., 0.) }

let of_axis_angle axis angle =
  let n = Vec3.length axis in
  if n < 1e-12 then identity
  else
    let half = angle /. 2. in
    { w = cos half; v = Vec3.scale (sin half /. n) axis }

let length q =
  let x, y, z = q.v in
  sqrt ((q.w *. q.w) +. (x *. x) +. (y *. y) +. (z *. z))

let normalize q =
  let n = length q in
  if n < 1e-12 then identity else { w = q.w /. n; v = Vec3.scale (1. /. n) q.v }

let to_axis_angle q =
  let q = normalize q in
  (* claude: the sign of w only says which way round the same rotation
   * is written; flipping it keeps the angle in [0, pi] *)
  let q = if q.w < 0. then { w = -.q.w; v = Vec3.scale (-1.) q.v } else q in
  let s = Vec3.length q.v in
  if s < 1e-12 then ((1., 0., 0.), 0.) else (Vec3.scale (1. /. s) q.v, 2. *. atan2 s q.w)

(* (w1 w2 - v1 . v2, w1 v2 + w2 v1 + v1 x v2) *)
let mul a b =
  { w = (a.w *. b.w) -. Vec3.dot a.v b.v;
    v = Vec3.add (Vec3.add (Vec3.scale a.w b.v) (Vec3.scale b.w a.v)) (Vec3.cross a.v b.v) }

let conjugate q = { q with v = Vec3.scale (-1.) q.v }

let rotate q v =
  let q = normalize q in
  (mul (mul q { w = 0.; v }) (conjugate q)).v

let to_mat3 q =
  let q = normalize q in
  let x, y, z = q.v and w = q.w in
  Mat3.of_rows
    (1. -. (2. *. ((y *. y) +. (z *. z))), 2. *. ((x *. y) -. (z *. w)), 2. *. ((x *. z) +. (y *. w)))
    (2. *. ((x *. y) +. (z *. w)), 1. -. (2. *. ((x *. x) +. (z *. z))), 2. *. ((y *. z) -. (x *. w)))
    (2. *. ((x *. z) -. (y *. w)), 2. *. ((y *. z) +. (x *. w)), 1. -. (2. *. ((x *. x) +. (y *. y))))

(* claude: from the matrix of R = Rz Ry Rx (Playground3d.rotate3d turns
 * about x first), whose entries give the angles directly:
 *   m20 = -sin y, m21 = sin x cos y, m22 = cos x cos y,
 *   m00 = cos y cos z, m10 = cos y sin z *)
let to_euler_xyz q =
  let m = to_mat3 q in
  let degrees r = r *. 180. /. Float.pi in
  let sy = Float.max (-1.) (Float.min 1. (-.m.Mat3.m20)) in
  let y = asin sy in
  if Float.abs sy > 0.99999 then
    (* claude: pitched straight up or down: x and z turn about the same
     * line and only their sum is defined, so all of it goes to x *)
    (degrees (atan2 (-.m.Mat3.m01) m.Mat3.m11), degrees y, 0.)
  else (degrees (atan2 m.Mat3.m21 m.Mat3.m22), degrees y, degrees (atan2 m.Mat3.m10 m.Mat3.m00))

let derivative ~spin q =
  let h = mul { w = 0.; v = spin } q in
  { w = 0.5 *. h.w; v = Vec3.scale 0.5 h.v }

let integrate ~spin ~dt q =
  let d = derivative ~spin q in
  normalize { w = q.w +. (dt *. d.w); v = Vec3.add q.v (Vec3.scale dt d.v) }

let turned_by ~spin ~dt q =
  let rate = Vec3.length spin in
  if rate < 1e-12 then q else normalize (mul (of_axis_angle spin (rate *. dt)) q)
