(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Rolling: the engine against a number this repository already had.
 *
 * A sphere let go on a slope of angle a rolls down at 5/7 g sin a, not
 * g sin a, because two sevenths of the energy go into its spin.
 * TinyMarbleMadness.ml derives that 5/7 by hand in its header
 * -- a ball on a height map was ten lines and needed no engine -- so
 * this is a cross-check in both directions: physics/3d knows only
 * tensors, contact points and friction impulses, and has to arrive at
 * the figure a game wrote down from a textbook. When it does, the
 * tensor, the contact, the impulse and the integrator are all right at
 * once. (Galileo timed balls down inclined planes for the same reason,
 * Two New Sciences, 1638, and missed the 5/7.) *)

let t = Testo.create
let g = 9.8
let radians a = a *. Float.pi /. 180.

(* a slope descending towards +x: its normal leans back *)
let slope_normal a = (sin (radians a), cos (radians a), 0.)
let down_slope a = (cos (radians a), -.sin (radians a), 0.)

(* a ball let go from rest on that slope, for [frames]; gives back how
 * fast it ends up going down the slope, and the body *)
let roll ~angle ~friction ~radius ~frames =
  let n = slope_normal angle in
  let nx, ny, _ = n in
  let floor =
    Physics3d.body (Playground3d.group3d [])
    |> Physics3d.hitbox (Hitbox3d.Plane (n, 0.))
    |> Physics3d.immovable
    |> Physics3d.rough friction
  in
  let ball =
    Physics3d.body (Playground3d.sphere Playground.red radius)
    |> Physics3d.ball
    |> Physics3d.at (radius *. nx) (radius *. ny) 0.
    |> Physics3d.rough friction
    |> Physics3d.bouncy 0.
  in
  let b = ref ball in
  for _ = 1 to frames do
    b := !b |> Physics3d.fall g |> Physics3d.step |> Physics3d.bounce_off floor
  done;
  let speed = Vec3.dot (!b.Physics3d.vx, !b.Physics3d.vy, !b.Physics3d.vz) (down_slope angle) in
  (speed /. (float_of_int frames /. 60.), !b)

let five_sevenths () =
  List.iter
    (fun angle ->
      let rolling = 5. /. 7. *. g *. sin (radians angle) and sliding = g *. sin (radians angle) in
      let a, ball = roll ~angle ~friction:0.3 ~radius:0.2 ~frames:120 in
      Alcotest.(check bool)
        (Printf.sprintf "%.0f degrees, rough: %.3f m/s^2 against 5/7 g sin a = %.3f" angle a rolling)
        true
        (Float.abs (a -. rolling) < 0.01 *. rolling);
      (* and it is really rolling: the contact point is standing still,
       * which is what v = w r means *)
      let _, _, spin = ball.Physics3d.spin in
      let speed = Vec3.dot (ball.Physics3d.vx, ball.Physics3d.vy, ball.Physics3d.vz) (down_slope angle) in
      let surface = -.spin *. Float.pi /. 180. *. 0.2 in
      Alcotest.(check bool)
        (Printf.sprintf "rolling, not slipping: v = %.3f, w r = %.3f" speed surface)
        true
        (Float.abs (speed -. surface) < 0.02 *. speed);
      (* with no friction there is nothing to spin it, and it slides at
       * the whole g sin a *)
      let a, ball = roll ~angle ~friction:0. ~radius:0.2 ~frames:120 in
      Alcotest.(check (float 1e-6)) "smooth: the full g sin a" sliding a;
      Alcotest.(check bool) "and no spin at all" true (Vec3.length ball.Physics3d.spin < 1e-9))
    [ 15.; 30. ]

(* the same formula for any shape: a = g sin a / (1 + I / m r^2), so a
 * capsule lying across the slope (a cylinder with round ends) rolls
 * faster than a sphere would if its mass sat further out, and slower
 * if nearer in. The constant comes out of its own tensor. *)
let any_rolling_shape () =
  let radius = 0.2 in
  let sphere_k =
    let i = Hitbox3d.inertia ~mass:1. (Hitbox3d.Sphere radius) in
    i.Mat3.m00 /. (1. *. radius *. radius)
  in
  Alcotest.(check (float 1e-9)) "a sphere's I / m r^2 is 2/5" 0.4 sphere_k;
  let capsule_k =
    let i = Hitbox3d.inertia ~mass:1. (Hitbox3d.Capsule (0.5, radius)) in
    (* about its own axis, which is the one it rolls about *)
    i.Mat3.m11 /. (1. *. radius *. radius)
  in
  Alcotest.(check bool) "a capsule's is between a sphere's and a cylinder's" true (capsule_k > 0.4 && capsule_k < 0.5);
  (* and the accelerations that fall out of them, in the order the
   * race ends *)
  let a_of k = g *. sin (radians 20.) /. (1. +. k) in
  Alcotest.(check bool) "a smooth one beats a rolling sphere" true (a_of 0. > a_of sphere_k);
  Alcotest.(check bool) "which beats a capsule" true (a_of sphere_k > a_of capsule_k)

(* rolling friction is the other one: not the grip that makes a ball
 * roll, but the loss that stops it. A spin with nothing acting on it
 * lasts for ever; with [spin_slow] it dies away. *)
let rolling_friction () =
  let spun =
    Physics3d.body (Playground3d.sphere Playground.red 0.2)
    |> Physics3d.ball
    |> Physics3d.turning (0., 0., 1.) 360.
  in
  let free = ref spun and slowed = ref spun in
  for _ = 1 to 120 do
    free := Physics3d.step !free;
    slowed := !slowed |> Physics3d.spin_slow 1.5 |> Physics3d.step
  done;
  let _, _, free_spin = !free.Physics3d.spin and _, _, slow_spin = !slowed.Physics3d.spin in
  Alcotest.(check (float 1e-9)) "nothing acting on it: it spins for ever" 360. free_spin;
  Alcotest.(check bool) "slowed: about e^-3 of it after two seconds" true (slow_spin > 360. *. 0.04 && slow_spin < 360. *. 0.06);
  Alcotest.(check bool) "and still the same way round" true (slow_spin > 0.)

let tests =
  [ t "Rolling, a sphere down a slope: 5/7 g sin a" five_sevenths;
    t "Rolling, the constant of any shape, from its tensor" any_rolling_shape;
    t "Rolling, and what slows it" rolling_friction ]
