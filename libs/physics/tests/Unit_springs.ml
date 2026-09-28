(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/2d/Springs and Particles: the worked examples, momentum,
 * damping, the stability limit, and a pendulum's period *)

let t = Testo.create
let vec = Alcotest.(pair (float 1e-9) (float 1e-9))
let rec repeat n f x = if n = 0 then x else repeat (n - 1) f (f x)

(* Springs.mli's example *)
let test_spring_force () =
  let bodies = [| Body.make (0., 0.); Body.make (2., 0.) |] in
  let acc = Springs.accelerations bodies [ { a = 0; b = 1; rest = 1.; k = 4.; damping = 0. } ] in
  Alcotest.check vec "a pulled towards b" (4., 0.) acc.(0);
  Alcotest.check vec "b towards a" (-4., 0.) acc.(1)

(* the springs are internal: they can't change the total momentum *)
let test_momentum () =
  let st = Random.State.make [| 23 |] in
  let f lo hi = lo +. Random.State.float st (hi -. lo) in
  let bodies = Array.init 6 (fun _ -> Body.make ~mass:(f 0.5 3.) ~vel:(f (-1.) 1., f (-1.) 1.) (f (-5.) 5., f (-5.) 5.)) in
  let springs = List.init 8 (fun i -> { Springs.a = i mod 6; b = (i + 1 + (i / 6)) mod 6; rest = f 1. 3.; k = f 1. 10.; damping = f 0. 1. }) in
  let momentum bs = Array.fold_left (fun m b -> Vec2.add m (Energy.momentum b)) (0., 0.) bs in
  let after = repeat 1000 (fun bs -> Springs.step ~gravity:(0., 0.) ~dt:0.01 bs springs) bodies in
  let (dx, dy) = Vec2.sub (momentum after) (momentum bodies) in
  if Float.abs dx > 1e-9 || Float.abs dy > 1e-9 then Alcotest.failf "momentum changed by (%g, %g)" dx dy

(* a mass on a spring of rest length 10, pulled 1 away (far from the
 * anchor: through it, the spring's direction would flip); its
 * displacements, step by step *)
let spring_run ~k ~damping ~dt ~steps : float list =
  let bodies = [| Body.make ~mass:infinity (0., 0.); Body.make (11., 0.) |] in
  let springs = [ { Springs.a = 0; b = 1; rest = 10.; k; damping } ] in
  let rec go n bs acc = if n = 0 then List.rev acc else
      let bs = Springs.step ~gravity:(0., 0.) ~dt bs springs in go (n - 1) bs ((fst bs.(1).pos -. 10.) :: acc) in
  go steps bodies []

(* k / m = 1, damped (c = 0.2): the swings shrink as e^(-c t / 2);
 * after two periods (t = 4 pi), 0.285 *)
let test_damping () =
  let dt = 0.001 in
  let xs = spring_run ~k:1. ~damping:0.2 ~dt ~steps:14000 in
  let around t = List.filteri (fun i _ -> Float.abs ((float_of_int (i + 1) *. dt) -. t) < 1.) xs in
  let swing = List.fold_left (fun m x -> Float.max m (Float.abs x)) 0. (around (4. *. Float.pi)) in
  Alcotest.(check (float 0.01)) "e^(-0.2 * 4 pi / 2)" (exp (-0.4 *. Float.pi)) swing

(* at 60 steps per second, k / m under 14,400 stays bounded, over it
 * explodes (dt < 2 / sqrt (k / m)). Bounded, but not by 1: near the
 * limit semi-implicit Euler's swings grow to 1 / sqrt (1 - (w dt /
 * 2)^2) of the first one, 1.81 for k / m = 10,000 (w = 100) *)
let test_stability () =
  let biggest k = List.fold_left (fun m x -> Float.max m (Float.abs x)) 0. (spring_run ~k ~damping:0. ~dt:(1. /. 60.) ~steps:600) in
  Alcotest.(check (float 0.01)) "k / m = 10,000: bounded, 1.81" (1. /. sqrt (1. -. ((100. /. 120.) ** 2.))) (biggest 10000.);
  if biggest 20000. < 1e6 then Alcotest.failf "k / m = 20,000: should explode, swings of %g" (biggest 20000.)

(* a rope of 20 sticks hanging for 2 seconds: its pinned end stays;
 * relaxing converges slowly along a long rope, so it stretches a bit,
 * less with more iterations *)
let test_rope () =
  let (particles, sticks) = Particles.rope ~from:(0., 0.) ~towards:(200., 0.) 21 in
  let hang iterations =
    repeat 120 (fun ps -> ps |> Particles.step ~accel:(0., -800.) ~dt:(1. /. 60.) |> Particles.relax ~iterations sticks) particles
  in
  let stretch (ps : Particles.particle array) =
    List.fold_left
      (fun m (s : Particles.stick) -> Float.max m ((Vec2.length (Vec2.sub ps.(s.b).pos ps.(s.a).pos) /. s.length) -. 1.))
      0. sticks
  in
  let twenty = hang 20 and hundred = hang 100 in
  Alcotest.check vec "the pinned end stays" (0., 0.) twenty.(0).pos;
  if stretch twenty > 0.05 then Alcotest.failf "20 iterations: stretched by %g" (stretch twenty);
  if stretch hundred >= stretch twenty then Alcotest.failf "100 iterations not stiffer: %g" (stretch hundred)

(* Particles.mli's pendulum: L = 100, g = 800, a small swing (10
 * degrees): 2 pi sqrt (L / g) = 2.22 s (2.225 at 10 degrees) *)
let test_pendulum () =
  let dt = 1. /. 600. and l = 100. and a = 10. *. Float.pi /. 180. in
  let ps = [| Particles.particle ~pinned:true (0., 0.); Particles.particle (l *. sin a, -.l *. cos a) |] in
  let sticks = [ { Particles.a = 0; b = 1; length = l } ] in
  (* the times x crosses 0 going left *)
  let rec go i ps crossings =
    if i > 6000 || List.length crossings = 3 then List.rev crossings
    else
      let ps' = ps |> Particles.step ~accel:(0., -800.) ~dt |> Particles.relax ~iterations:5 sticks in
      let crossed = fst ps.(1).pos > 0. && fst ps'.(1).pos <= 0. in
      go (i + 1) ps' (if crossed then (float_of_int i *. dt) :: crossings else crossings)
  in
  match go 0 ps [] with
  | [ t1; t2; _ ] -> Alcotest.(check (float 0.02)) "the period" 2.225 (t2 -. t1)
  | _ -> Alcotest.fail "the pendulum didn't swing"

let tests =
  Testo.categorize "Springs and Particles"
    [
      t "a spring's pull, the worked example" test_spring_force;
      t "springs keep momentum" test_momentum;
      t "damping: e^(-c t / 2)" test_damping;
      t "the stability limit: k / m < 14,400 at 60 steps a second" test_stability;
      t "a rope keeps its length, its pinned end" test_rope;
      t "a pendulum's period, 2 pi sqrt (L / g)" test_pendulum;
    ]
