(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* playground/Physics, the Evan-style API over physics/2d *)

open Playground
open Physics

let t = Testo.create

(* n steps of [f] *)
let rec repeat n f b = if n = 0 then b else repeat (n - 1) f (f b)

(* Physics.mli's first example, for one second: 200 pixels to the
 * right; 106.67 down where the exact parabola says 100 (semi-implicit
 * Euler's first-order error, g t dt / 2 = 6.67, see Integrate.mli) *)
let test_thrown_ball () =
  let ball = body (circle red 20.) |> at 0. 300. |> moving 200. 0. in
  let b = repeat 60 (fun b -> b |> fall 800. |> step) ball in
  Alcotest.(check (float 1e-9)) "x" 200. b.x;
  Alcotest.(check (float 1e-9)) "y" (300. -. (800. /. 3600. *. 1830.)) b.y;
  Alcotest.(check (float 1e-9)) "vy, exact" (-800.) b.vy

(* the pushes add up, in any order, and are used up by step *)
let test_accumulator () =
  let b = body (circle red 20.) in
  let one = b |> fall 100. |> push 30. 0. |> step and two = b |> push 30. 0. |> fall 100. |> step in
  Alcotest.(check (float 1e-12)) "order doesn't matter" one.vx two.vx;
  Alcotest.(check (float 1e-12)) "used up" 0. one.ax;
  Alcotest.(check (float 1e-12)) "heavier, pushed less" (15. *. tick) (b |> heavy 2. |> push 30. 0. |> step).vx

(* slow's top speed: fall 100, slow 0.5: 200 *)
let test_top_speed () =
  let b = repeat 3000 (fun b -> b |> fall 100. |> slow 0.5 |> step) (body (circle red 20.)) in
  Alcotest.(check (float 1e-6)) "falling at 200" (-200.) b.vy

let test_directions () =
  let b = body (circle red 20.) |> launched 100. 90. in
  Alcotest.(check (float 1e-9)) "launched up: no vx" 0. b.vx;
  Alcotest.(check (float 1e-9)) "launched up: vy" 100. b.vy;
  let rocket = body (circle red 20.) |> pointing 90. |> thrust 60. |> step in
  Alcotest.(check (float 1e-9)) "thrust along the angle" 1. rocket.vy;
  Alcotest.(check (float 1e-9)) "turn" 1.5 ((body (circle red 20.) |> turn 90. |> step).angle)

(* attracted_by's example: a star of mass 1,000,000, a body 100 pixels
 * away: pulled at 100 px/s^2 towards it; at 100 px/s, a circle *)
let test_gravitation () =
  let star = body (circle yellow 20.) |> heavy 1e6 in
  let b = body (circle red 5.) |> at 100. 0. |> attracted_by star in
  Alcotest.(check (float 1e-9)) "towards the star" (-100.) b.ax;
  let planet = body (circle red 5.) |> at 100. 0. |> moving 0. 100. in
  let after = repeat 377 (fun b -> b |> attracted_by star |> step) planet in
  Alcotest.(check (float 0.01)) "one orbit (377 steps): still 100 away" 100. (distance star after)

let test_shot_from () =
  let ship = body (circle red 10.) |> pointing 90. |> moving 10. 0. in
  let t = body (circle white 2.) |> shot_from 300. 20. ship in
  Alcotest.(check (float 1e-9)) "20 ahead: x" 0. t.x;
  Alcotest.(check (float 1e-9)) "20 ahead: y" 20. t.y;
  Alcotest.(check (float 1e-9)) "the ship's vx kept" 10. t.vx;
  Alcotest.(check (float 1e-9)) "300 forward" 300. t.vy

(* touching: the real shapes, turned the way the bodies point *)
let test_touching () =
  let point x y = body (circle white 0.5) |> at x y in
  let stick = body (rectangle red 100. 10.) |> pointing 90. in
  Alcotest.(check bool) "a stick turned upright: its top" true (touching stick (point 0. 45.));
  Alcotest.(check bool) "... not its old end" false (touching stick (point 45. 0.));
  let moon = body (group [ circle red 10. |> move 50. 0. ]) in
  Alcotest.(check bool) "a group: its moved circle counts" true (touching moon (point 55. 0.));
  Alcotest.(check bool) "... not the group's center" false (touching moon (point 0. 0.));
  let ship = body (polygon blue [ (15., 0.); (-15., 10.); (-10., 0.); (-15., -10.) ]) |> at 100. 100. in
  Alcotest.(check bool) "a concave ship: its nose" true (touching ship (point 112. 100.));
  Alcotest.(check bool) "... not in its notch" false (touching ship (point 87. 100.));
  Alcotest.(check bool) "a hexagon" true (touching (body (hexagon red 30.)) (point 0. 28.));
  Alcotest.(check bool) "... not beside its side" false (touching (body (hexagon red 30.)) (point 28. 0.))

let screen = to_screen 1000. 1000.

let test_edges () =
  let b = body (circle red 20.) |> at 510. 0. in
  Alcotest.(check (float 1e-9)) "wrap: back on the left" (-490.) (wrap screen b).x;
  let b = body (circle red 20.) |> at 510. 0. |> moving 100. 0. |> bounce_in screen 0.5 in
  Alcotest.(check (float 1e-9)) "bounce: at the edge" 500. b.x;
  Alcotest.(check (float 1e-9)) "bounce: back, half as fast" (-50.) b.vx;
  Alcotest.(check bool) "outside" true (outside screen (body (circle red 20.) |> at 0. (-600.)))

let test_bounce () =
  let ball = body (circle red 10.) |> bouncy 1. in
  (* billiards: two equal balls head-on, 15 apart (overlapping by 5) *)
  let (a, b) = bounce (ball |> moving 100. 0.) (ball |> at 15. 0.) in
  Alcotest.(check (float 1e-9)) "the first stops" 0. a.vx;
  Alcotest.(check (float 1e-9)) "the second goes on at its speed" 100. b.vx;
  Alcotest.(check (float 1e-9)) "pushed apart: 20 between their centers" 20. (distance a b);
  let (a', b') = bounce (ball |> at 100. 0.) b in
  Alcotest.(check bool) "not touching: unchanged" true ((a'.x, a'.vx, b'.x, b'.vx) = (100., 0., b.x, b.vx));
  (* a clay ball on the floor, then a superball *)
  let floor = body (rectangle green 400. 20.) |> at 0. (-20.) in
  (* 2 pixels into the floor, whose top is at -10 *)
  let falling = body (circle red 10.) |> at 0. (-2.) |> moving 30. (-200.) in
  let clay = falling |> bounce_off floor in
  Alcotest.(check (float 1e-9)) "clay: stopped" 0. clay.vy;
  Alcotest.(check (float 1e-9)) "... pushed back on the floor" 0. clay.y;
  Alcotest.(check (float 1e-9)) "... still sliding (no friction)" 30. clay.vx;
  Alcotest.(check (float 1e-9)) "a superball: back up as fast" 200. (falling |> bouncy 1. |> bounce_off floor).vy;
  let rubber = falling |> upright |> rough 1. |> bounce_off (floor |> rough 1.) in
  Alcotest.(check (float 1e-9)) "rubber on rubber, upright: the sliding stops" 0. rubber.vx;
  (* free to turn, it rolls instead: slower, spinning clockwise, its
   * bottom point (8 below its center, 2 in the floor) not sliding *)
  let rolling = falling |> rough 1. |> bounce_off (floor |> rough 1.) in
  Alcotest.(check bool) "rubber on rubber: slower" true (rolling.vx > 0. && rolling.vx < 30.);
  Alcotest.(check (float 1e-9)) "... rolling: its bottom still" 0. (rolling.vx +. (rolling.spin *. Float.pi /. 180. *. 8.));
  (* TinyPong's paddle: moving up at 600, bumper 1.05, rough 0.5; the
   * ball 2 pixels into it at 450: j = 2.05 * 450 = 922.5, and friction
   * gives the ball at most 0.5 j of the paddle's 600 *)
  let paddle = body (rectangle white 20. 120.) |> immovable |> bouncy 1.05 |> rough 0.5 |> moving 0. 600. in
  let hit = body (circle white 12.) |> upright |> bouncy 1. |> rough 0.5 |> at (-20.) 0. |> moving 450. 0. |> bounce_off paddle in
  Alcotest.(check (float 1e-9)) "back 5% faster" (-472.5) hit.vx;
  Alcotest.(check (float 1e-9)) "dragged up by the paddle" 461.25 hit.vy;
  (* a heavy ball barely slowed by a light one *)
  let (heavy_ball, _) = bounce (ball |> heavy 99. |> moving 100. 0.) (ball |> at 15. 0.) in
  Alcotest.(check (float 1e-9)) "99 against 1: 98% of the speed kept" 98. heavy_ball.vx;
  match bounce_all [ ball |> moving 100. 0.; ball |> at 15. 0.; ball |> at 100. 0. ] with
  | [ a; b; c ] ->
      Alcotest.(check (float 1e-9)) "bounce_all: the pair that touches" 100. b.vx;
      Alcotest.(check (float 1e-9)) "... the others unchanged" 0. (a.vx +. c.vx)
  | _ -> Alcotest.fail "three bodies in, three out"

(* bounce_all with each broad phase: the same bounces *)
let test_broad_phase () =
  let balls =
    List.init 40 (fun i ->
        body (circle red 10.) |> bouncy 0.8
        |> at (float_of_int (i mod 8) *. 19.) (float_of_int (i / 8) *. 19.)
        |> moving (float_of_int (i * 37 mod 50)) (float_of_int (i * 11 mod 30)))
  in
  let positions bs = List.map (fun b -> (b.x, b.y, b.vx, b.vy)) bs in
  let run m = positions (repeat 60 (fun bs -> bounce_all ~broad_phase:m (List.map step bs)) balls) in
  let all = run Broadphase.All_pairs in
  Alcotest.(check bool) "grid = all pairs, after 60 ticks" true (run Broadphase.Grid = all);
  Alcotest.(check bool) "sort and sweep = all pairs" true (run Broadphase.Sort_and_sweep = all);
  let r = broad_phase Broadphase.All_pairs balls in
  Alcotest.(check int) "all pairs: 40 * 39 / 2 tests" 780 r.tests;
  (* a grid of 8 x 5 balls 19 apart, radius 10: each touches its 2 to 4
   * neighbours (not the diagonals, 27 apart; their boxes do overlap) *)
  Alcotest.(check bool) "neighbours found" true (List.mem (0, 1) r.pairs && List.mem (0, 8) r.pairs)

let tests =
  Testo.categorize "Physics (the API)"
    [
      t "a thrown ball" test_thrown_ball;
      t "the pushes add up" test_accumulator;
      t "a top speed" test_top_speed;
      t "launched, thrust, turn" test_directions;
      t "wrap, bounce_in, outside" test_edges;
      t "attracted_by, the worked example" test_gravitation;
      t "shot_from" test_shot_from;
      t "touching, with the real shapes" test_touching;
      t "bounce, bounce_off, bounce_all" test_bounce;
      t "bounce_all, the three broad phases" test_broad_phase;
    ]
