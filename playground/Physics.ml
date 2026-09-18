(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Physics.mli *)

open Playground

type body = {
  shape : shape;
  x : number;
  y : number;
  vx : number;
  vy : number;
  angle : number;
  spin : number;
  mass : number;
  ax : number;
  ay : number;
}

let body (shape : shape) : body =
  { shape; x = 0.; y = 0.; vx = 0.; vy = 0.; angle = 0.; spin = 0.; mass = 1.; ax = 0.; ay = 0. }

let at x y (b : body) : body = { b with x; y }
let moving vx vy (b : body) : body = { b with vx; vy }
let radians degrees = degrees *. Float.pi /. 180.

let launched speed angle (b : body) : body =
  { b with vx = speed *. cos (radians angle); vy = speed *. sin (radians angle) }

let shot_from speed distance (shooter : body) (b : body) : body =
  let c = cos (radians shooter.angle) and s = sin (radians shooter.angle) in
  {
    b with
    x = shooter.x +. (distance *. c);
    y = shooter.y +. (distance *. s);
    vx = shooter.vx +. (speed *. c);
    vy = shooter.vy +. (speed *. s);
    angle = shooter.angle;
  }

let pointing angle (b : body) : body = { b with angle }
let heavy mass (b : body) : body = { b with mass }

(* the accumulator: every push adds an acceleration, [step] uses them up *)
let accelerate ax ay (b : body) : body = { b with ax = b.ax +. ax; ay = b.ay +. ay }
let fall g (b : body) : body = accelerate 0. (-.g) b
let push fx fy (b : body) : body = accelerate (fx /. b.mass) (fy /. b.mass) b
let thrust f (b : body) : body = push (f *. cos (radians b.angle)) (f *. sin (radians b.angle)) b
let slow c (b : body) : body = accelerate (-.c *. b.vx) (-.c *. b.vy) b
let turn spin (b : body) : body = { b with spin }

let attracted_by (other : body) (b : body) : body =
  let (ax, ay) = Force.gravitation ~gm:other.mass ~center:(other.x, other.y) (b.x, b.y) (b.vx, b.vy) in
  accelerate ax ay b
let tick = 1. /. 60.

let step (b : body) : body =
  (* one step of the engine's semi-implicit Euler, the pushes as a
   * constant acceleration during the step *)
  let state = Body.make ~vel:(b.vx, b.vy) ~mass:b.mass (b.x, b.y) in
  let state' = Integrate.semi_implicit_euler ~force:(Force.uniform (b.ax, b.ay)) ~dt:tick state in
  let (x, y) = state'.pos and (vx, vy) = state'.vel in
  { b with x; y; vx; vy; angle = b.angle +. (b.spin *. tick); ax = 0.; ay = 0. }

let wrap (screen : screen) (b : body) : body =
  let around v lo hi = if v < lo then v +. (hi -. lo) else if v > hi then v -. (hi -. lo) else v in
  { b with x = around b.x screen.left screen.right; y = around b.y screen.bottom screen.top }

let bounce_in (screen : screen) bounciness (b : body) : body =
  (* back inside, the velocity reversed if it was going further out *)
  let axis v p lo hi =
    if p < lo then (lo, if v < 0. then -.v *. bounciness else v)
    else if p > hi then (hi, if v > 0. then -.v *. bounciness else v)
    else (p, v)
  in
  let (x, vx) = axis b.vx b.x screen.left screen.right in
  let (y, vy) = axis b.vy b.y screen.bottom screen.top in
  { b with x; y; vx; vy }

let draw (b : body) : shape = b.shape |> rotate b.angle |> Playground.move b.x b.y
let distance (a : body) (b : body) : number = Float.hypot (a.x -. b.x) (a.y -. b.y)
let speed (b : body) : number = Float.hypot b.vx b.vy

let outside (screen : screen) (b : body) : bool =
  b.x < screen.left || b.x > screen.right || b.y < screen.bottom || b.y > screen.top
