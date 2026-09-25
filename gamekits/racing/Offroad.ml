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

type ground = { height : number -> number -> number; gravity : number; lo : number; hi : number }
type t = { body : Topdown.t; h : number; vh : number; air : bool }

let dt = 1. /. 60.

let start (g : ground) (body : Topdown.t) : t = { body; h = g.height body.x body.y; vh = 0.; air = false }

let slope (g : ground) (x : number) (y : number) : number * number =
  let e = 0.5 in
  ((g.height (x +. e) y -. g.height (x -. e) y) /. (2. *. e), (g.height x (y +. e) -. g.height x (y -. e)) /. (2. *. e))

(* bounced off the walls -- the ground higher than a car [h] high can
 * climb in a frame, or steeper than it can climb at all, uphill -- and
 * stopped at the edge (not a wall for Topdown.bounce: a car already in
 * a wall moves freely, to get out, and past the edge it would stay in
 * one) *)
let bounce (g : ground) (track : Topdown.track) (h : number) (before : Topdown.t) (after : Topdown.t) : Topdown.t =
  let steep x y =
    let gx, gy = slope g x y in
    Float.hypot gx gy > 1.2
  in
  let b = Topdown.bounce (fun x y -> g.height x y > h +. 1.2 || (steep x y && g.height x y > h +. 0.3)) before after in
  let clamp v = Float.max g.lo (Float.min g.hi v) in
  let b = if clamp b.x = b.x && clamp b.y = b.y then b else { b with x = clamp b.x; y = clamp b.y; vx = 0.; vy = 0.; speed = 0. } in
  Topdown.follow track b

let on_ground (g : ground) (track : Topdown.track) (p : Topdown.params) (top : number) (gas : number) (steer : number) (c : t) : t =
  let b = c.body in
  let after = Topdown.drive p top gas steer b in
  let gx, gy = slope g b.x b.y in
  let a = after.heading *. Float.pi /. 180. in
  let along = (gx *. cos a) +. (gy *. sin a) in
  let after =
    { after with
      speed = after.speed -. (g.gravity *. along *. dt);
      vx = after.vx -. (g.gravity *. (gx -. (along *. cos a)) *. dt);
      vy = after.vy -. (g.gravity *. (gy -. (along *. sin a)) *. dt) }
  in
  let body = bounce g track c.h b after in
  let ground = g.height body.x body.y in
  (* where it would be one frame on if it flew ([in_air]'s step); a
   * hundredth of a unit of margin, or the corners between a heightmap's
   * cells would make it hop *)
  let vh = c.vh -. (g.gravity *. dt) in
  let flying = c.h +. (vh *. dt) in
  if ground < flying -. 0.01 then { body; h = flying; vh; air = true }
  else { body; h = ground; vh = (ground -. c.h) /. dt; air = false }

let in_air (g : ground) (track : Topdown.track) (c : t) : t =
  let b = c.body in
  let moved = { b with x = b.x +. (b.vx *. dt); y = b.y +. (b.vy *. dt) } in
  let body = bounce g track c.h b moved in
  let vh = c.vh -. (g.gravity *. dt) in
  let h = c.h +. (vh *. dt) in
  let ground = g.height body.x body.y in
  if h > ground then { body; h; vh; air = true }
  else
    let hard = vh < -15. in
    (* landed, going as the ground goes: with no vertical speed, a car
     * landing on a downhill slope would be above the ground again the
     * next frame, and hop down it *)
    { body = { body with speed = (if hard then body.speed *. 0.7 else body.speed) };
      h = ground;
      vh = (ground -. g.height b.x b.y) /. dt;
      air = false }

let drive (g : ground) (track : Topdown.track) (p : Topdown.params) (top : number) (gas : number) (steer : number) (c : t) : t =
  if c.air then in_air g track c else on_ground g track p top gas steer c

let push (radius : number) (a : t) (b : t) : t * t =
  if Float.abs (a.h -. b.h) < 2. then
    let ba, bb = Topdown.push radius a.body b.body in
    ({ a with body = ba }, { b with body = bb })
  else (a, b)

let pose (g : ground) (c : t) : number * number =
  let b = c.body in
  let a = b.heading *. Float.pi /. 180. in
  let fx = cos a and fy = sin a in
  let deg v = Float.atan v *. 180. /. Float.pi in
  if c.air then (deg (c.vh /. Float.max 10. (Float.abs b.speed)), 0.)
  else
    let gx, gy = slope g b.x b.y in
    (deg ((gx *. fx) +. (gy *. fy)), deg ((gx *. fy) -. (gy *. fx)))
