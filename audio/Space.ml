(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Space.mli *)

let clamp (p : float) : float = Float.max (-1.) (Float.min 1. p)

let pan (p : float) : float * float =
  let angle = (clamp p +. 1.) *. Float.pi /. 4. in
  (sqrt 2. *. cos angle, sqrt 2. *. sin angle)

let pan_linear (p : float) : float * float = (1. -. clamp p, 1. +. clamp p)

type vec = { x : float; y : float; z : float }

let vec x y z = { x; y; z }
let sub (a : vec) (b : vec) : vec = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }
let dot (a : vec) (b : vec) : float = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)
let distance (a : vec) (b : vec) : float = sqrt (dot (sub a b) (sub a b))

let direction ~(listener : vec) ~(right : vec) (source : vec) : float =
  let d = distance source listener in
  if d = 0. then 0. else clamp (dot (sub source listener) right /. d)

let attenuation ~(reference : float) (d : float) : float = if d <= reference then 1. else reference /. d

let doppler ~(speed_of_sound : float) ~(listener : vec) ~(listener_velocity : vec) ~(source : vec)
    ~(source_velocity : vec) : float =
  let c = speed_of_sound in
  (* the line from the source to the listener *)
  let sl = sub listener source in
  let d = sqrt (dot sl sl) in
  if d = 0. then 1.
  else
    let along v = Float.min (0.99 *. c) (dot sl v /. d) in
    (c -. along listener_velocity) /. (c -. along source_velocity)
