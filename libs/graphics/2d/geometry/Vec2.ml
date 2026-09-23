(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Vec2.mli *)

type t = float * float

let add ((ax, ay) : t) ((bx, by) : t) : t = (ax +. bx, ay +. by)
let sub ((ax, ay) : t) ((bx, by) : t) : t = (ax -. bx, ay -. by)
let scale (s : float) ((x, y) : t) : t = (s *. x, s *. y)
let dot ((ax, ay) : t) ((bx, by) : t) : float = (ax *. bx) +. (ay *. by)
let cross ((ax, ay) : t) ((bx, by) : t) : float = (ax *. by) -. (ay *. bx)
let perp ((x, y) : t) : t = (-.y, x)
let length ((x, y) : t) : float = Float.hypot x y

let normalize (v : t) : t =
  let n = length v in
  if n = 0. then v else scale (1. /. n) v
