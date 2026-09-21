(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type vec = float * float * float
type t = { right : vec; up : vec; forward : vec }

let identity : t = { right = (1., 0., 0.); up = (0., 1., 0.); forward = (0., 0., -1.) }

(*****************************************************************************)
(* Vectors *)
(*****************************************************************************)

let add (x1, y1, z1) (x2, y2, z2) : vec = (x1 +. x2, y1 +. y2, z1 +. z2)
let scale k (x, y, z) : vec = (k *. x, k *. y, k *. z)
let dot (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)
let cross (x1, y1, z1) (x2, y2, z2) : vec = ((y1 *. z2) -. (z1 *. y2), (z1 *. x2) -. (x1 *. z2), (x1 *. y2) -. (y1 *. x2))

let normalize (v : vec) : vec =
  let n = sqrt (dot v v) in
  if n = 0. then v else scale (1. /. n) v

(*****************************************************************************)
(* Turning *)
(*****************************************************************************)

(* [a] and [b] turned by [angle] in their own plane, [a] towards [b]:
 *
 *     b                 a' = a cos + b sin
 *     ^    ,a'          b' = b cos - a sin
 *     |  ,'  angle
 *     +-------> a       the third vector, the axis, doesn't move
 *)
let rotate (angle : float) ((a, b) : vec * vec) : vec * vec =
  let c = cos (angle *. Float.pi /. 180.) and s = sin (angle *. Float.pi /. 180.) in
  (add (scale c a) (scale s b), add (scale c b) (scale (-.s) a))

(* the three squared up again: forward kept, right made square to it,
 * up made square to both *)
let straighten (t : t) : t =
  let forward = normalize t.forward in
  let right = normalize (cross forward t.up) in
  let up = cross right forward in
  { right; up; forward }

let turn ?(pitch = 0.) ?(yaw = 0.) ?(roll = 0.) (t : t) : t =
  (* pitch: forward towards up, around right *)
  let forward, up = rotate pitch (t.forward, t.up) in
  (* yaw: forward towards the left, around up *)
  let forward, right = rotate yaw (forward, scale (-1.) t.right) in
  let right = scale (-1.) right in
  (* roll: right towards down, around forward *)
  let right, up = rotate roll (right, scale (-1.) up) in
  straighten { right; up = scale (-1.) up; forward }

let ahead (t : t) ~(from : vec) ~(distance : float) : vec = add from (scale distance t.forward)

let along (t : t) ((a, b, c) : vec) : vec =
  add (scale a t.right) (add (scale b t.up) (scale c t.forward))
