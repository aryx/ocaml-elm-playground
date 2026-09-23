(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Integrate3d.mli *)

type method_ = Explicit_euler | Semi_implicit_euler | Verlet | Rk4
type spin_law = Momentum | Spin_gyroscopic | Spin_naive
type turn = First_order | Exact
type force = Vec3.t -> Vec3.t -> Vec3.t

let methods = [ Explicit_euler; Semi_implicit_euler; Verlet; Rk4 ]

let name = function
  | Explicit_euler -> "explicit Euler"
  | Semi_implicit_euler -> "semi-implicit Euler"
  | Verlet -> "Verlet"
  | Rk4 -> "RK4"

let no_force _ _ = (0., 0., 0.)

(*****************************************************************************)
(* The linear half: physics/2d/Integrate, with a z *)
(*****************************************************************************)

let explicit_euler ~force ~dt (b : Body3d.t) =
  let a = force b.pos b.vel in
  { b with pos = Vec3.add b.pos (Vec3.scale dt b.vel); vel = Vec3.add b.vel (Vec3.scale dt a) }

let semi_implicit_euler ~force ~dt (b : Body3d.t) =
  let a = force b.pos b.vel in
  let vel = Vec3.add b.vel (Vec3.scale dt a) in
  { b with vel; pos = Vec3.add b.pos (Vec3.scale dt vel) }

let verlet ~force ~dt (b : Body3d.t) =
  let a = force b.pos b.vel in
  let pos = Vec3.add (Vec3.add b.pos (Vec3.scale dt b.vel)) (Vec3.scale (0.5 *. dt *. dt) a) in
  (* claude: a velocity-dependent force (drag) has no new velocity yet:
   * predict it, as physics/2d/Integrate.verlet does, and say so *)
  let predicted = Vec3.add b.vel (Vec3.scale dt a) in
  let a' = force pos predicted in
  { b with pos; vel = Vec3.add b.vel (Vec3.scale (0.5 *. dt) (Vec3.add a a')) }

let rk4 ~force ~dt (b : Body3d.t) =
  let deriv (pos, vel) = (vel, force pos vel) in
  let advance (p, v) s (dp, dv) = (Vec3.add p (Vec3.scale s dp), Vec3.add v (Vec3.scale s dv)) in
  let y = (b.pos, b.vel) in
  let k1 = deriv y in
  let k2 = deriv (advance y (dt /. 2.) k1) in
  let k3 = deriv (advance y (dt /. 2.) k2) in
  let k4 = deriv (advance y dt k3) in
  (* the four slopes, weighted 1, 2, 2, 1 *)
  let sixth x1 x2 x3 x4 = Vec3.scale (1. /. 6.) (Vec3.add (Vec3.add x1 (Vec3.scale 2. x2)) (Vec3.add (Vec3.scale 2. x3) x4)) in
  let dpos = sixth (fst k1) (fst k2) (fst k3) (fst k4) in
  let dvel = sixth (snd k1) (snd k2) (snd k3) (snd k4) in
  { b with pos = Vec3.add b.pos (Vec3.scale dt dpos); vel = Vec3.add b.vel (Vec3.scale dt dvel) }

let linear = function
  | Explicit_euler -> explicit_euler
  | Semi_implicit_euler -> semi_implicit_euler
  | Verlet -> verlet
  | Rk4 -> rk4

(*****************************************************************************)
(* The rotational half: what 3D adds *)
(*****************************************************************************)

let turn_by ~turn ~spin ~dt (q : Quat.t) =
  match turn with First_order -> Quat.integrate ~spin ~dt q | Exact -> Quat.turned_by ~spin ~dt q

let spin_step ?(law = Momentum) ?(turn = Exact) ~torque ~dt (b : Body3d.t) =
  if b.Body3d.inv_inertia = Mat3.zero then
    (* a body no torque can spin. It may still have been given a spin
     * (a flipper the player drives), and that spin still turns it. *)
    { b with orientation = turn_by ~turn ~spin:b.Body3d.spin ~dt b.Body3d.orientation }
  else
    match law with
    | Momentum ->
        (* dL/dt = torque, and that is the whole of it: with no torque L
         * does not move at all. The wobble and the flip come out of the
         * *other* line, w = I_world^-1 L, because I_world turns with the
         * body. Nothing here needs the gyroscopic term: it is what you
         * get when you insist on stepping w instead. *)
        let l = Vec3.add (Mat3.mul_vec (Body3d.inertia_world b) b.Body3d.spin) (Vec3.scale dt torque) in
        let b = { b with Body3d.orientation = turn_by ~turn ~spin:b.Body3d.spin ~dt b.Body3d.orientation } in
        { b with Body3d.spin = Mat3.mul_vec (Body3d.inv_inertia_world b) l }
    | Spin_gyroscopic | Spin_naive ->
        let i_world = Body3d.inertia_world b in
        let gyro = if law = Spin_gyroscopic then Vec3.cross b.Body3d.spin (Mat3.mul_vec i_world b.Body3d.spin) else (0., 0., 0.) in
        let alpha = Mat3.mul_vec (Body3d.inv_inertia_world b) (Vec3.sub torque gyro) in
        let spin = Vec3.add b.Body3d.spin (Vec3.scale dt alpha) in
        { b with Body3d.spin; orientation = turn_by ~turn ~spin ~dt b.Body3d.orientation }

let step m ?law ?turn ?(torque = (0., 0., 0.)) ~force ~dt b =
  spin_step ?law ?turn ~torque ~dt (linear m ~force ~dt b)
