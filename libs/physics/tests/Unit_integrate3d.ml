(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/3d/Integrate3d: the worked examples of Integrate3d.mli --
 * the linear half against the 2D table it inherits, and the rotational
 * half against the one thing 3D rotation does that 2D cannot: a body
 * with nothing acting on it turning itself over *)

let t = Testo.create

let run m ~force ~dt n (b : Body3d.t) : Body3d.t =
  let rec go i b = if i = 0 then b else go (i - 1) (Integrate3d.step m ~force ~dt b) in
  go n b

(* Integrate.mli's table, one dimension up: a ball thrown up at 10 m/s,
 * g = 10, dt = 0.1, its height after 5 steps *)
let projectile () =
  let ball = Body3d.make ~vel:(0., 10., 0.) (0., 0., 0.) in
  let height m =
    let _, y, _ = (run m ~force:(fun _ _ -> (0., -10., 0.)) ~dt:0.1 5 ball).Body3d.pos in
    y
  in
  Alcotest.(check (float 1e-9)) "explicit Euler: 0.25 too high" 4.0 (height Integrate3d.Explicit_euler);
  Alcotest.(check (float 1e-9)) "semi-implicit Euler: 0.25 too low" 3.5 (height Integrate3d.Semi_implicit_euler);
  Alcotest.(check (float 1e-9)) "Verlet: exact" 3.75 (height Integrate3d.Verlet);
  Alcotest.(check (float 1e-9)) "RK4: exact" 3.75 (height Integrate3d.Rk4)

(* and the orders, on a spring pulling towards the origin in x, k/m = 1:
 * x(t) = cos t *)
let orders () =
  let spring (x, _, _) _ = (-.x, 0., 0.) in
  let stretched = Body3d.make (1., 0., 0.) in
  let error m dt =
    let x, _, _ = (run m ~force:spring ~dt (int_of_float (Float.round (1. /. dt))) stretched).Body3d.pos in
    Float.abs (x -. cos 1.)
  in
  let check name m lo hi =
    let r = error m 0.1 /. error m 0.05 in
    if r < lo || r > hi then Alcotest.failf "%s: the error ratio is %g, not in [%g, %g]" name r lo hi
  in
  check "explicit Euler, first order" Integrate3d.Explicit_euler 1.8 2.3;
  check "semi-implicit Euler, first order" Integrate3d.Semi_implicit_euler 1.8 2.3;
  check "Verlet, second order" Integrate3d.Verlet 3.6 4.4;
  check "RK4, fourth order" Integrate3d.Rk4 14. 18.

(*****************************************************************************)
(* The intermediate axis *)
(*****************************************************************************)

(* the T-handle of PhysicsSpin3d.ml, in metres and kg: a bar with a
 * stem hanging off it, whose three principal moments are all
 * different -- which is what the flip needs *)
let bar_sides = (0.30, 0.04, 0.04)
let bar_mass = 0.30
let stem_sides = (0.04, 0.16, 0.04)
let stem_mass = 0.16
let stem_centre_y = -.((0.16 /. 2.) +. (0.04 /. 2.))
let handle_mass = bar_mass +. stem_mass
let com_y = stem_mass *. stem_centre_y /. handle_mass

let handle_inertia =
  Mat3.add
    (Body3d.shifted ~mass:bar_mass (0., -.com_y, 0.) (Body3d.box ~mass:bar_mass bar_sides))
    (Body3d.shifted ~mass:stem_mass (0., stem_centre_y -. com_y, 0.) (Body3d.box ~mass:stem_mass stem_sides))

let handle ~spin = Body3d.make ~spin ~mass:handle_mass ~inertia:handle_inertia (0., 0., 0.)

(* [flight ~law ~seconds spin]: how many times the spin reverses in the
 * body's own frame (a flip), and how far |L| and the energy wander *)
let flight ?(law = Integrate3d.Momentum) ?(dt = 1. /. 600.) ~seconds spin =
  let axis (x, y, z) = if x > y && x > z then `X else if y > z then `Y else `Z in
  let along w = match axis spin with `X -> (fun (x, _, _) -> x) w | `Y -> (fun (_, y, _) -> y) w | `Z -> (fun (_, _, z) -> z) w in
  let b = ref (handle ~spin) in
  let l0 = Vec3.length (Energy3d.angular !b) and e0 = Energy3d.kinetic !b in
  let flips = ref 0 and previous = ref (along spin) and worst_l = ref 0. and worst_e = ref 0. in
  for _ = 1 to int_of_float (seconds /. dt) do
    b := Integrate3d.spin_step ~law ~torque:(0., 0., 0.) ~dt !b;
    let drift was is = Float.abs ((is -. was) /. was) in
    worst_l := Float.max !worst_l (drift l0 (Vec3.length (Energy3d.angular !b)));
    worst_e := Float.max !worst_e (drift e0 (Energy3d.kinetic !b));
    (* in the *body* frame: in the world frame w only wobbles around L *)
    let now = along (Quat.rotate (Quat.conjugate !b.Body3d.orientation) !b.Body3d.spin) in
    if now *. !previous < 0. then incr flips;
    previous := now
  done;
  (!flips, !worst_l, !worst_e)

(* the principal moments, since everything below depends on which is
 * which *)
let handle_moments () =
  let i = handle_inertia in
  Alcotest.(check bool) "diagonal: the handle's own axes are its principal ones" true (i.Mat3.m01 = 0. && i.Mat3.m02 = 0. && i.Mat3.m12 = 0.);
  Alcotest.(check bool) "x is the smallest moment (along the bar)" true (i.Mat3.m00 < i.Mat3.m11);
  Alcotest.(check bool) "y is the middle one (along the stem)" true (i.Mat3.m11 < i.Mat3.m22)

(* The tennis-racket theorem: spun about the middle axis the handle
 * turns itself over, again and again, with no torque at all; about
 * either of the other two it does not. *)
let intermediate_axis () =
  let nudge = 0.02 in
  let flips_about_x, _, _ = flight ~seconds:30. (10., nudge, nudge) in
  let flips_about_y, drift_l, _ = flight ~seconds:30. (nudge, 10., nudge) in
  let flips_about_z, _, _ = flight ~seconds:30. (nudge, nudge, 10.) in
  Alcotest.(check int) "about the smallest axis: no flip, it only wobbles" 0 flips_about_x;
  Alcotest.(check int) "about the largest: none either" 0 flips_about_z;
  Alcotest.(check bool) "about the middle one: over and over" true (flips_about_y > 10);
  (* and through every one of them, the law the step is built on *)
  Alcotest.(check bool) "|L| never moves, which is what Momentum steps" true (drift_l < 1e-9)

(* Integrate3d.mli's warning: without the gyroscopic term the body
 * spins about a fixed axis for ever, and every conservation check says
 * it is perfect *)
let a_quiet_lie () =
  let nudge = 0.02 in
  let flips, drift_l, drift_e = flight ~law:Integrate3d.Spin_naive ~seconds:30. (nudge, 10., nudge) in
  Alcotest.(check int) "no gyroscopic term: never a flip" 0 flips;
  Alcotest.(check bool) "|L| conserved to the last bit" true (drift_l < 1e-12);
  Alcotest.(check bool) "and the energy too: the checks cannot see the mistake" true (drift_e < 1e-12);
  (* the same body, stepped as Bullet does: the flips are back, and now
   * it is |L| that pays *)
  let flips, drift_l, _ = flight ~law:Integrate3d.Spin_gyroscopic ~seconds:30. (nudge, 10., nudge) in
  Alcotest.(check bool) "stepping w instead: it flips" true (flips > 0);
  Alcotest.(check bool) "but |L| drifts a percent or two" true (drift_l > 1e-3)

(* the two orientation updates agree at a frame's worth of turn, and
 * the first-order one falls behind when the turn gets big *)
let turning () =
  let after turn rate =
    let b = ref (Body3d.make ~spin:(0., rate, 0.) ~inertia:(Body3d.solid_sphere ~mass:1. ~radius:1.) (0., 0., 0.)) in
    for _ = 1 to 60 do
      b := Integrate3d.spin_step ~turn ~torque:(0., 0., 0.) ~dt:(1. /. 60.) !b
    done;
    !b.Body3d.orientation
  in
  let lag rate =
    let _, angle = Quat.to_axis_angle (Quat.mul (after Integrate3d.First_order rate) (Quat.conjugate (after Integrate3d.Exact rate))) in
    angle *. 180. /. Float.pi
  in
  Alcotest.(check bool) "1 rad/s: the two agree to a thousandth of a degree" true (lag 1. < 0.01);
  Alcotest.(check bool) "20 rad/s (19 degrees a step): the first order one is 10 degrees behind" true (lag 20. > 5.);
  (* the exact turn is exactly a turn: a full 2 pi in one second comes
   * back to where it started *)
  let round_trip = after Integrate3d.Exact (2. *. Float.pi) in
  let _, angle = Quat.to_axis_angle round_trip in
  Alcotest.(check (float 1e-9)) "and a whole turn is none" 0. (Float.min angle (Float.abs (angle -. (2. *. Float.pi))))

(* a body with an infinite tensor cannot be spun by a torque, but the
 * spin it was given still turns it -- and nothing becomes a nan *)
let never_turns () =
  let flipper = Body3d.make ~spin:(0., 3., 0.) (0., 0., 0.) in
  let stepped = Integrate3d.spin_step ~torque:(100., 200., 300.) ~dt:0.1 flipper in
  Alcotest.(check bool) "no torque changes it" true (stepped.Body3d.spin = (0., 3., 0.));
  let axis, angle = Quat.to_axis_angle stepped.Body3d.orientation in
  Alcotest.(check (float 1e-9)) "but it has turned 0.3 rad" 0.3 angle;
  let _, ay, _ = axis in
  Alcotest.(check (float 1e-9)) "about y" 1. ay

let tests =
  [ t "Integrate3d, the 2D table with a z" projectile;
    t "Integrate3d, the convergence orders" orders;
    t "Integrate3d, the T-handle's three moments" handle_moments;
    t "Integrate3d, the intermediate axis: a body that turns itself over" intermediate_axis;
    t "Integrate3d, and the same body without the gyroscopic term" a_quiet_lie;
    t "Integrate3d, the two ways of turning an orientation" turning;
    t "Integrate3d, a body no torque can spin" never_turns ]
