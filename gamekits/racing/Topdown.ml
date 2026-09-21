(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

(* See Topdown.mli *)

type t = { x : number; y : number; vx : number; vy : number; heading : number; speed : number; next : int }

type params = { accel : number; friction : number; grip : number; steering : number; steering_speed : number }

let toy = { accel = 900.; friction = 1.5; grip = 0.12; steering = 3.5; steering_speed = 250. }

let radians d = d *. Float.pi /. 180.

let drive (p : params) (top : number) (gas : number) (steer : number) (c : t) : t =
  let dt = 1. /. 60. in
  let speed = c.speed +. (gas *. p.accel *. dt) in
  let speed = speed -. (speed *. p.friction *. dt) in
  let speed = Float.max (-.top /. 3.) (Float.min top speed) in
  let heading = c.heading +. (steer *. p.steering *. Float.min 1. (Float.abs speed /. p.steering_speed)) in
  let a = radians heading in
  let vx = c.vx +. (p.grip *. ((speed *. cos a) -. c.vx)) and vy = c.vy +. (p.grip *. ((speed *. sin a) -. c.vy)) in
  { c with x = c.x +. (vx *. dt); y = c.y +. (vy *. dt); vx; vy; heading; speed }

type track = { points : (number * number) array; reach : number; corner : number }

let point (track : track) (i : int) : number * number =
  let n = Array.length track.points in
  track.points.(((i mod n) + n) mod n)

let start (track : track) (i : int) (side : number) : t =
  let x1, y1 = point track i and x2, y2 = point track (i + 1) in
  let heading = atan2 (y2 -. y1) (x2 -. x1) *. 180. /. Float.pi in
  let a = radians (heading +. 90.) in
  { x = x1 +. (side *. cos a); y = y1 +. (side *. sin a); vx = 0.; vy = 0.; heading; speed = 0.; next = i + 1 }

let follow (track : track) (c : t) : t =
  let px, py = point track c.next in
  if Float.hypot (px -. c.x) (py -. c.y) < track.reach then { c with next = c.next + 1 } else c

let lap (track : track) (c : t) : int = (c.next - 1) / Array.length track.points

let progress (track : track) (c : t) : number =
  let x, y = point track c.next in
  (float_of_int c.next *. 10000.) -. Float.hypot (x -. c.x) (y -. c.y)

let computer (track : track) (c : t) : number * number =
  let x1, y1 = point track c.next and x2, y2 = point track (c.next + 1) in
  let k = 0.5 *. Float.max 0. (1. -. (Float.hypot (x1 -. c.x) (y1 -. c.y) /. track.corner)) in
  let tx = ((1. -. k) *. x1) +. (k *. x2) and ty = ((1. -. k) *. y1) +. (k *. y2) in
  let wanted = atan2 (ty -. c.y) (tx -. c.x) *. 180. /. Float.pi in
  let diff = Float.rem (Float.rem (wanted -. c.heading +. 180.) 360. +. 360.) 360. -. 180. in
  let steer = Float.max (-1.) (Float.min 1. (diff /. 20.)) in
  let gas = if Float.abs diff > 50. && c.speed > track.corner then -0.3 else 0.9 in
  (gas, steer)
