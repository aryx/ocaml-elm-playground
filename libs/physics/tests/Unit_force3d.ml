(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/3d/Force3d: the 2D forces still obey their laws with a third
 * coordinate, and Archimedes' waterline is where it should be *)

let t = Testo.create
let close = Alcotest.(check (float 1e-9))

let step ~force ~dt n (b : Body3d.t) =
  let rec go i b = if i = 0 then b else go (i - 1) (Integrate3d.semi_implicit_euler ~force ~dt b) in
  go n b

(* gm / r^2 towards the centre, and the speed that makes a circle *)
let gravitation () =
  let f = Force3d.gravitation ~gm:1000. ~center:(0., 0., 0.) in
  let a = f (10., 0., 0.) (0., 0., 0.) in
  close "10 away from gm = 1000: pulled at 10" (-10.) (let x, _, _ = a in x);
  close "and only towards it" 0. (let _, y, _ = a in y);
  (* a circular orbit in the x/z plane: sqrt (gm / r) = 10 *)
  let planet = Body3d.make ~vel:(0., 0., -10.) (10., 0., 0.) in
  let dt = 1. /. 600. in
  (* one period: 2 pi r / v = 2 pi seconds *)
  let after = step ~force:f ~dt (int_of_float (2. *. Float.pi /. dt)) planet in
  Alcotest.(check (float 0.01)) "one orbit later, still 10 out" 10. (Vec3.length after.Body3d.pos);
  Alcotest.(check (float 0.05)) "and back where it started" 10. (let x, _, _ = after.Body3d.pos in x)

(* two bodies pulling on each other: whatever they do, the total
 * momentum does not move (Newton's third law) *)
let two_body_momentum () =
  let dt = 1. /. 600. in
  let masses = (3., 1.) in
  let a = ref (Body3d.make ~mass:(fst masses) ~vel:(0., 0.2, 0.) (-2., 0., 0.)) in
  let b = ref (Body3d.make ~mass:(snd masses) ~vel:(0., -0.6, 0.) (2., 0., 0.)) in
  let total () = Vec3.add (Energy3d.momentum !a) (Energy3d.momentum !b) in
  let p0 = total () in
  let gm_of m = 0.5 *. m (* G = 0.5, so each pulls with G m / r^2 *) in
  for _ = 1 to 3000 do
    let fa = Force3d.gravitation ~gm:(gm_of !b.Body3d.mass) ~center:!b.Body3d.pos in
    let fb = Force3d.gravitation ~gm:(gm_of !a.Body3d.mass) ~center:!a.Body3d.pos in
    let a' = Integrate3d.semi_implicit_euler ~force:fa ~dt !a in
    let b' = Integrate3d.semi_implicit_euler ~force:fb ~dt !b in
    a := a';
    b := b'
  done;
  let px, py, pz = total () and qx, qy, qz = p0 in
  Alcotest.(check (float 1e-6)) "the pair's momentum, x" qx px;
  Alcotest.(check (float 1e-6)) "y" qy py;
  Alcotest.(check (float 1e-6)) "z" qz pz;
  Alcotest.(check bool) "and they really did move" true (Vec3.length (Vec3.sub !a.Body3d.pos (-2., 0., 0.)) > 0.1)

(* drag's terminal speed is g / c, whatever the direction *)
let terminal_speed () =
  let force = Force3d.sum [ Force3d.uniform (0., -9.8, 0.); Force3d.drag ~c:2. ] in
  let b = step ~force ~dt:(1. /. 60.) 2000 (Body3d.make (0., 0., 0.)) in
  let _, vy, _ = b.Body3d.vel in
  Alcotest.(check (float 1e-6)) "9.8 / 2" (-4.9) vy

(* Hooke: x(t) = cos t with k/m = 1, and the spring pulls towards its
 * anchor wherever it is *)
let spring () =
  let f = Force3d.spring ~k_over_m:4. ~anchor:(1., 2., 3.) in
  let x, y, z = f (1., 2., 5.) (0., 0., 0.) in
  close "no pull along x" 0. x;
  close "nor y" 0. y;
  close "4 k per unit of stretch, towards the anchor" (-8.) z;
  let b = step ~force:(Force3d.spring ~k_over_m:1. ~anchor:(0., 0., 0.)) ~dt:(1. /. 1000.) 1000 (Body3d.make (1., 0., 0.)) in
  let x, _, _ = b.Body3d.pos in
  Alcotest.(check (float 1e-3)) "one second of cos t" (cos 1.) x

(*****************************************************************************)
(* Archimedes *)
(*****************************************************************************)

(* the submerged fraction, by hand: a body 1 m tall, the water at 0 *)
let waterline () =
  let f y = Force3d.submerged ~water:0. ~half_height:0.5 y in
  close "well above: dry" 0. (f 1.);
  close "just touching: dry" 0. (f 0.5);
  close "centre at the surface: half under" 0.5 (f 0.);
  close "a quarter of the way down" 0.25 (f 0.25);
  close "well below: all of it" 1. (f (-1.))

(* a barrel of relative density 0.6 comes to rest with 60% of its
 * height under water -- the analytic waterline, which is the whole of
 * Archimedes in one number *)
let floats_at_its_density () =
  let rest density =
    let force = Force3d.buoyancy ~g:9.8 ~water:0. ~half_height:0.5 ~density () in
    (* a hundred seconds: dropped from 2 m up, a light float bobs for a
     * while (the drag is proportional to how much of it is in the
     * water, so the lighter it is the longer it takes to settle) *)
    let b = step ~force ~dt:(1. /. 120.) 12000 (Body3d.make (0., 2., 0.)) in
    let _, y, _ = b.Body3d.pos in
    Force3d.submerged ~water:0. ~half_height:0.5 y
  in
  Alcotest.(check (float 1e-4)) "wood, 0.6: 60% under" 0.6 (rest 0.6);
  Alcotest.(check (float 1e-4)) "a light float, 0.2" 0.2 (rest 0.2);
  Alcotest.(check (float 1e-4)) "and one that only just floats" 0.95 (rest 0.95);
  (* denser than water: it never comes back *)
  let force = Force3d.buoyancy ~g:9.8 ~water:0. ~half_height:0.5 ~density:2.7 () in
  let b = step ~force ~dt:(1. /. 120.) 1200 (Body3d.make (0., 0., 0.)) in
  let _, y, _ = b.Body3d.pos in
  Alcotest.(check bool) "aluminium sinks, and keeps sinking" true (y < -2.);
  (* and above the water it is a plain fall *)
  let dry = Force3d.buoyancy ~g:9.8 ~water:0. ~half_height:0.5 ~density:0.6 () (0., 5., 0.) (0., -1., 0.) in
  close "out of the water: gravity, nothing else" (-9.8) (let _, y, _ = dry in y)

let tests =
  [ t "Force3d, gravitation and a circular orbit" gravitation;
    t "Force3d, two bodies keep their momentum" two_body_momentum;
    t "Force3d, drag's terminal speed" terminal_speed;
    t "Force3d, Hooke's spring" spring;
    t "Force3d, how much of a body is under water" waterline;
    t "Force3d, Archimedes: it floats at its density" floats_at_its_density ]
