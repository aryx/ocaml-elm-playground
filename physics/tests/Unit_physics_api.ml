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

let screen = to_screen 1000. 1000.

let test_edges () =
  let b = body (circle red 20.) |> at 510. 0. in
  Alcotest.(check (float 1e-9)) "wrap: back on the left" (-490.) (wrap screen b).x;
  let b = body (circle red 20.) |> at 510. 0. |> moving 100. 0. |> bounce_in screen 0.5 in
  Alcotest.(check (float 1e-9)) "bounce: at the edge" 500. b.x;
  Alcotest.(check (float 1e-9)) "bounce: back, half as fast" (-50.) b.vx;
  Alcotest.(check bool) "outside" true (outside screen (body (circle red 20.) |> at 0. (-600.)))

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
    ]
