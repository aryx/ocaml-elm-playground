(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Stereographic.mli *)

(* the horizontal frame as vectors: x east, y north, z up *)
let vector (h : Celestial.horizontal) : float * float * float =
  (cos h.alt *. sin h.az, cos h.alt *. cos h.az, sin h.alt)

let project ~(view : Celestial.horizontal) (p : Celestial.horizontal) : (float * float) option =
  let fx, fy, fz = vector view in
  (* the screen's right: horizontal, towards increasing azimuth *)
  let rx, ry = (cos view.az, -.sin view.az) in
  (* its up: right x forward, towards the zenith *)
  let ux, uy, uz = (ry *. fz, -.(rx *. fz), (rx *. fy) -. (ry *. fx)) in
  let px, py, pz = vector p in
  let x = (px *. rx) +. (py *. ry) and y = (px *. ux) +. (py *. uy) +. (pz *. uz) in
  let z = (px *. fx) +. (py *. fy) +. (pz *. fz) in
  if z < -0.85 then None else Some (2. *. x /. (1. +. z), 2. *. y /. (1. +. z))

(* the horizon's nearest point is [a] below the centre, at
 * -2 tan (a/2), its furthest 180 - a above it, at 2 cot (a/2) *)
let horizon (a : float) : float * float = (2. /. tan a, 2. /. sin a)
