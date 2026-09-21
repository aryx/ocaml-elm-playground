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

(* See Car.mli *)

type t = { position : number; x : number; speed : number; steer : number }

let start = { position = 0.; x = 0.; speed = 0.; steer = 0. }

type params = {
  max_speed : number;
  accel : number;
  brake : number;
  decel : number;
  off_road_decel : number;
  off_road_limit : number;
  centrifugal : number;
}

let fps = 60.

let params (segment_length : number) : params =
  let max_speed = segment_length *. fps in
  { max_speed;
    accel = max_speed /. 5.;
    brake = -.max_speed;
    decel = -.max_speed /. 5.;
    off_road_decel = -.max_speed /. 2.;
    off_road_limit = max_speed /. 4.;
    centrifugal = 0.3 }

let clamp lo hi v = Float.max lo (Float.min hi v)

let drive (p : params) (road : Road.t) (k : keyboard) (car : t) : t =
  let dt = 1. /. fps in
  let percent = car.speed /. p.max_speed in
  let seg = Road.segment_at road car.position in
  let steer = (if k.kleft then -1. else 0.) +. if k.kright then 1. else 0. in
  (* steering, faster at speed; the curve pushes outwards *)
  let x = car.x +. (steer *. dt *. 2. *. percent) -. (dt *. 2. *. percent *. seg.curve *. p.centrifugal) in
  let speed =
    if k.kup then car.speed +. (p.accel *. dt)
    else if k.kdown then car.speed +. (p.brake *. dt)
    else car.speed +. (p.decel *. dt)
  in
  (* on the grass, down to a crawl *)
  let speed = if Float.abs x > 1. && speed > p.off_road_limit then speed +. (p.off_road_decel *. dt) else speed in
  let speed = clamp 0. p.max_speed speed in
  { position = car.position +. (speed *. dt); x = clamp (-3.) 3. x; speed; steer }
