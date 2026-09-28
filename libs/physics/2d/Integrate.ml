(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Integrate.mli *)

type method_ = Explicit_euler | Semi_implicit_euler | Verlet | Rk4

let methods = [ Explicit_euler; Semi_implicit_euler; Verlet; Rk4 ]

let name = function
  | Explicit_euler -> "explicit Euler"
  | Semi_implicit_euler -> "semi-implicit Euler"
  | Verlet -> "Verlet"
  | Rk4 -> "RK4"

(* v + dt * w, the step every method is made of *)
let ( +* ) (v : Vec2.t) ((dt, w) : float * Vec2.t) : Vec2.t = Vec2.add v (Vec2.scale dt w)

let explicit_euler ~(force : Force.t) ~(dt : float) (b : Body.t) : Body.t =
  let a = force b.pos b.vel in
  { b with pos = b.pos +* (dt, b.vel); vel = b.vel +* (dt, a) }

let semi_implicit_euler ~(force : Force.t) ~(dt : float) (b : Body.t) : Body.t =
  let a = force b.pos b.vel in
  let vel = b.vel +* (dt, a) in
  { b with pos = b.pos +* (dt, vel); vel }

let verlet ~(force : Force.t) ~(dt : float) (b : Body.t) : Body.t =
  let a = force b.pos b.vel in
  let pos = b.pos +* (dt, b.vel) +* (dt *. dt /. 2., a) in
  let a' = force pos (b.vel +* (dt, a)) in
  { b with pos; vel = b.vel +* (dt /. 2., Vec2.add a a') }

let rk4 ~(force : Force.t) ~(dt : float) (b : Body.t) : Body.t =
  (* the derivative of the state (pos, vel) is (vel, a) *)
  let deriv (pos, vel) = (vel, force pos vel) in
  let at (pos, vel) h (dpos, dvel) = (pos +* (h, dpos), vel +* (h, dvel)) in
  let s = (b.pos, b.vel) in
  let k1 = deriv s in
  let k2 = deriv (at s (dt /. 2.) k1) in
  let k3 = deriv (at s (dt /. 2.) k2) in
  let k4 = deriv (at s dt k3) in
  (* weights 1, 2, 2, 1 *)
  let avg f = Vec2.scale (1. /. 6.) (Vec2.add (Vec2.add (f k1) (Vec2.scale 2. (f k2))) (Vec2.add (Vec2.scale 2. (f k3)) (f k4))) in
  { b with pos = b.pos +* (dt, avg fst); vel = b.vel +* (dt, avg snd) }

let step (m : method_) ~(force : Force.t) ~(dt : float) (b : Body.t) : Body.t =
  match m with
  | Explicit_euler -> explicit_euler ~force ~dt b
  | Semi_implicit_euler -> semi_implicit_euler ~force ~dt b
  | Verlet -> verlet ~force ~dt b
  | Rk4 -> rk4 ~force ~dt b
