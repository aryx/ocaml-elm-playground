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
open Basics

(* See Free_ball.mli *)

type t = { x : number; y : number; vx : number; vy : number }

let still (x : number) (y : number) : t = { x; y; vx = 0.; vy = 0. }

let roll ~(friction : number) ?(push = (0., 0.)) (b : t) : t =
  let ax, ay = push in
  let vx = (b.vx + ax) * friction and vy = (b.vy + ay) * friction in
  { x = b.x + vx; y = b.y + vy; vx; vy }

let touch ~(glued : bool) ~(speed : number) ~(reach : number) ~(hold : number) ((px, py) : number * number) ((dx, dy) : number * number) (b : t) : t option =
  if Float.hypot (b.x - px) (b.y - py) > reach then None
  else
    let d = Float.max 1e-9 (Float.hypot dx dy) in
    if glued then Some { x = px + (dx / d * hold); y = py + (dy / d * hold); vx = 0.; vy = 0. }
    else Some { b with vx = speed * dx / d; vy = speed * dy / d }

let bounce_in ~(half_w : number) ~(half_h : number) ~(keep : number) (b : t) : t =
  let x, vx = if b.x < 0. - half_w then (0. - half_w, Float.abs b.vx * keep) else if b.x > half_w then (half_w, 0. - (Float.abs b.vx * keep)) else (b.x, b.vx) in
  let y, vy = if b.y < 0. - half_h then (0. - half_h, Float.abs b.vy * keep) else if b.y > half_h then (half_h, 0. - (Float.abs b.vy * keep)) else (b.y, b.vy) in
  { x; y; vx; vy }

let speed (b : t) : number = Float.hypot b.vx b.vy
let near (r : number) ((x, y) : number * number) (b : t) : bool = Float.hypot (b.x - x) (b.y - y) < r
