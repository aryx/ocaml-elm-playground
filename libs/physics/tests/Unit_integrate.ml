(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/2d/Integrate: the worked examples of Integrate.mli, and the
 * laws: convergence orders, energy over long runs *)

let t = Testo.create

(* [n] steps of [m] *)
let run m ~force ~dt n (b : Body.t) : Body.t =
  let rec go i b = if i = 0 then b else go (i - 1) (Integrate.step m ~force ~dt b) in
  go n b

(* Integrate.mli's table: a ball thrown up at 10 m/s, g = 10, dt = 0.1,
 * its height after 5 steps *)
let test_projectile () =
  let ball = Body.make ~vel:(0., 10.) (0., 0.) in
  let height m = snd (run m ~force:(Force.uniform (0., -10.)) ~dt:0.1 5 ball).pos in
  Alcotest.(check (float 1e-9)) "explicit Euler: 0.25 too high" 4.0 (height Integrate.Explicit_euler);
  Alcotest.(check (float 1e-9)) "semi-implicit Euler: 0.25 too low" 3.5 (height Integrate.Semi_implicit_euler);
  Alcotest.(check (float 1e-9)) "Verlet: exact" 3.75 (height Integrate.Verlet);
  Alcotest.(check (float 1e-9)) "RK4: exact" 3.75 (height Integrate.Rk4)

(* a mass on a spring, k / m = 1: x(t) = cos t *)
let spring = Force.spring ~k_over_m:1. ~anchor:(0., 0.)
let at_rest_stretched = Body.make (1., 0.)

(* the error at t = 1, with dt then dt / 2: first order halves it,
 * second order quarters it, fourth order divides it by 16 *)
let test_orders () =
  let error m dt = Float.abs (fst (run m ~force:spring ~dt (int_of_float (Float.round (1. /. dt))) at_rest_stretched).pos -. cos 1.) in
  let ratio m = error m 0.1 /. error m 0.05 in
  let check name m lo hi =
    let r = ratio m in
    if r < lo || r > hi then Alcotest.failf "%s: the error ratio is %g, not in [%g, %g]" name r lo hi
  in
  check "explicit Euler, first order" Integrate.Explicit_euler 1.8 2.3;
  check "semi-implicit Euler, first order" Integrate.Semi_implicit_euler 1.8 2.3;
  check "Verlet, second order" Integrate.Verlet 3.6 4.4;
  check "RK4, fourth order" Integrate.Rk4 14. 18.

(* notes_2d_physics.md section 5: the spring's energy, 1 at the start *)
let spring_energy (b : Body.t) =
  2. *. (Energy.kinetic b +. Energy.spring ~k_over_m:1. ~anchor:(0., 0.) b)

let test_spring_energy () =
  Alcotest.(check (float 1e-6)) "explicit Euler: times (1 + dt^2) per step, 2.70 after 100" (1.01 ** 100.)
    (spring_energy (run Integrate.Explicit_euler ~force:spring ~dt:0.1 100 at_rest_stretched));
  (* semi-implicit Euler: bounded, forever *)
  let lo = ref 1. and hi = ref 1. in
  let rec go i b =
    if i > 0 then begin
      let b = Integrate.semi_implicit_euler ~force:spring ~dt:0.1 b in
      let e = spring_energy b in
      lo := Float.min !lo e;
      hi := Float.max !hi e;
      go (i - 1) b
    end
  in
  go 1000 at_rest_stretched;
  if !lo < 0.95 || !hi > 1.06 then Alcotest.failf "semi-implicit Euler: energy in [%g, %g], not ~[0.95, 1.05]" !lo !hi

(* Integrate.mli's orbit: gm = 1,000,000, r = 100, v = 100, dt = 1/60,
 * 377 steps per orbit *)
let star = (0., 0.)
let gravity = Force.gravitation ~gm:1e6 ~center:star
let planet = Body.make ~vel:(0., 100.) (100., 0.)
let radius_after m orbits = Vec2.length (run m ~force:gravity ~dt:(1. /. 60.) (377 * orbits) planet).pos

let test_orbit () =
  Alcotest.(check (float 0.1)) "explicit Euler, 1 orbit: spiraling out" 119.94 (radius_after Integrate.Explicit_euler 1);
  Alcotest.(check (float 0.1)) "explicit Euler, 10 orbits" 190.18 (radius_after Integrate.Explicit_euler 10);
  [ Integrate.Semi_implicit_euler; Integrate.Verlet; Integrate.Rk4 ]
  |> List.iter (fun m ->
         Alcotest.(check (float 0.01)) (Integrate.name m ^ ", 10 orbits: still on it") 100. (radius_after m 10))

(* Force.drag's example: falling with drag c = 0.5 under g = 10 reaches
 * the terminal speed g / c = 20 *)
let test_terminal_speed () =
  let force = Force.sum [ Force.uniform (0., -10.); Force.drag ~c:0.5 ] in
  let b = run Integrate.Rk4 ~force ~dt:0.01 3000 (Body.make (0., 0.)) in
  Alcotest.(check (float 1e-3)) "after 30 s, falling at 20" (-20.) (snd b.vel)

let tests =
  Testo.categorize "Integrate"
    [
      t "a projectile, the worked example" test_projectile;
      t "convergence orders" test_orders;
      t "a spring's energy" test_spring_energy;
      t "an orbit" test_orbit;
      t "drag: a terminal speed" test_terminal_speed;
    ]
