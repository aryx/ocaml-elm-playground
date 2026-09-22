(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ai.mli *)

let default_speed = 200.
let default_force = 400.

(* the body as ai/Steering sees it: a place, a velocity and two limits *)
let vehicle ~speed ~force (b : Physics.body) : Steering.vehicle =
  { position = (b.x, b.y); velocity = (b.vx, b.vy); max_speed = speed; max_force = force }

(* a steering force is an acceleration; [Physics.push] takes a force,
 * which it divides by the mass *)
let pushed ((fx, fy) : Steering.vec) (b : Physics.body) : Physics.body = Physics.push (fx *. b.mass) (fy *. b.mass) b

(* the body steered towards the velocity [desired] wants for it *)
let steered ?(speed = default_speed) ?(force = default_force) (desired : Steering.vehicle -> Steering.vec) (b : Physics.body) :
    Physics.body =
  let v = vehicle ~speed ~force b in
  pushed (Steering.steer v (desired v)) b

let seek ?speed ?force x y b = steered ?speed ?force (Steering.seek (x, y)) b
let flee ?speed ?force x y b = steered ?speed ?force (Steering.flee (x, y)) b
let arrive ?speed ?force ?slowing x y b = steered ?speed ?force (Steering.arrive ?slowing (x, y)) b

(* the target as a vehicle: only its place and velocity matter *)
let target_of (t : Physics.body) = vehicle ~speed:default_speed ~force:default_force t

let chase ?speed ?force target b = steered ?speed ?force (Steering.pursue (target_of target)) b
let escaping ?speed ?force target b = steered ?speed ?force (Steering.evade (target_of target)) b

(* smooth noise from the time: two slow sines, at speeds whose ratio
 * isn't a simple fraction, so the pattern takes long to repeat *)
let wandering ?speed ?force time b =
  let angle = (1.2 *. Float.sin (time *. 0.7)) +. (0.8 *. Float.sin ((time *. 1.9) +. 1.)) in
  steered ?speed ?force (Steering.wander ~angle) b

let avoiding ?speed ?force rocks b = steered ?speed ?force (Steering.avoid (List.map (fun (x, y, r) -> ((x, y), r)) rocks)) b

let flocking ?(speed = default_speed) ?(force = default_force) ?(radius = 100.) ?separation ?alignment ?cohesion
    (others : Physics.body list) (b : Physics.body) : Physics.body =
  (* the same vehicle for [b] in [others] and on its own, so that Flock
   * can leave it out of its own neighbours (by physical equality) *)
  let pairs = List.map (fun o -> (o, vehicle ~speed ~force o)) others in
  let v = match List.assq_opt b pairs with Some v -> v | None -> vehicle ~speed ~force b in
  pushed (Flock.flock ?separation ?alignment ?cohesion ~radius (List.map snd pairs) v) b

let following ?speed ?force ?(width = 20.) path b = steered ?speed ?force (Steering.follow ~width path) b

let facing (b : Physics.body) : Physics.body =
  if b.vx = 0. && b.vy = 0. then b else Physics.pointing (Float.atan2 b.vy b.vx *. 180. /. Float.pi) b
