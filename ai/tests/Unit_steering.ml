(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_steering.mli *)

open Steering

let t = Testo.create
let close = Alcotest.(pair (float 0.05) (float 0.05))
let still ?(max_speed = 200.) ?(max_force = 50.) position = { position; velocity = (0., 0.); max_speed; max_force }

(*****************************************************************************)
(* Steering *)
(*****************************************************************************)

(* Steering.mli's: going up, seeking up and to the right *)
let test_seek () =
  let v = { (still (0., 0.)) with velocity = (0., 100.) } in
  let desired = seek (300., 400.) v in
  Alcotest.check close "desired (120, 160)" (120., 160.) desired;
  Alcotest.check close "steer (44.7, 22.4)" (44.72, 22.36) (steer v desired);
  Alcotest.check close "flee: the other way" (-120., -160.) (flee (300., 400.) v)

let test_arrive () =
  let v = still (0., 0.) in
  Alcotest.check close "far: top speed" (200., 0.) (arrive (500., 0.) v);
  Alcotest.check close "at half the slowing distance: half speed" (100., 0.) (arrive (50., 0.) v);
  Alcotest.check close "on it: stopped" (0., 0.) (arrive (0., 0.) v)

(* Steering.mli's: catching a target 400 away takes 2 seconds, in which
 * it goes up 200 *)
let test_pursue () =
  let target = { (still (400., 0.)) with velocity = (0., 100.) } in
  let v = still (0., 0.) in
  Alcotest.check close "seek (400, 200)" (seek (400., 200.) v) (pursue target v);
  Alcotest.check close "evade: the other way" (flee (400., 200.) v) (evade target v)

let test_wander () =
  let v = { (still (0., 0.)) with velocity = (100., 0.) } in
  (* angle 0: the point straight ahead, on the far side of the circle *)
  Alcotest.check close "angle 0: straight on" (200., 0.) (wander ~angle:0. v);
  let x, y = wander ~angle:(Float.pi /. 2.) v in
  Alcotest.(check bool) "a quarter turn: ahead and to the left" true (x > 0. && y > 0.)

let test_avoid () =
  let v = { (still (0., 0.)) with velocity = (200., 0.) } in
  Alcotest.check close "a clear way: no force" (0., 0.) (steer v (avoid [ ((60., 100.), 20.) ] v));
  let _, fy = steer v (avoid [ ((60., 5.), 20.) ] v) in
  Alcotest.(check bool) "a rock a little to the left: turned right" true (fy < 0.);
  let _, fy = steer v (avoid [ ((60., -5.), 20.) ] v) in
  Alcotest.(check bool) "a little to the right: turned left" true (fy > 0.)

let test_follow () =
  let path = [ (0., 0.); (500., 0.) ] in
  let on = { (still (100., 5.)) with velocity = (200., 0.) } in
  Alcotest.check close "on the road: no force" (0., 0.) (steer on (follow ~width:20. path on));
  let off = { (still (100., 80.)) with velocity = (200., 0.) } in
  let _, fy = steer off (follow ~width:20. path off) in
  Alcotest.(check bool) "off it: back down to it" true (fy < 0.)

let test_move () =
  let v = { (still (0., 0.)) with velocity = (190., 0.) } in
  let v = move ~dt:1. (50., 0.) v in
  Alcotest.check close "clamped to the top speed" (200., 0.) v.velocity;
  Alcotest.check close "moved by the new velocity" (200., 0.) v.position;
  Alcotest.check close "a direction is a unit vector" (0.6, 0.8) (direction (3., 4.))

(*****************************************************************************)
(* Flock *)
(*****************************************************************************)

(* Flock.mli's: two boids 10 apart *)
let test_two_boids () =
  let a = still ~max_speed:100. (0., 0.) and b = still ~max_speed:100. (10., 0.) in
  let flock = [ a; b ] in
  let near = Flock.neighbours ~radius:50. flock a in
  Alcotest.(check int) "one neighbour, not itself" 1 (List.length near);
  Alcotest.check close "separation: away" (-100., 0.) (Flock.separation near a);
  Alcotest.check close "cohesion: towards the other" (100., 0.) (Flock.cohesion near a);
  Alcotest.check close "alignment, both still: nothing" (0., 0.) (Flock.alignment near a)

(* 30 boids on a grid, each going its own way (the golden angle apart);
 * after 20 seconds of the three rules, they go the same way: the length
 * of their mean heading, 0 for headings all over the place and 1 for a
 * single direction, goes from under 0.2 to over 0.9 *)
let test_emergence () =
  let boids =
    List.init 30 (fun i ->
        (* the golden angle apart: spread evenly round the circle *)
        let a = float_of_int i *. 137.508 *. Float.pi /. 180. in
        { position = (float_of_int (i mod 6) *. 25., float_of_int (i / 6) *. 25.);
          velocity = (60. *. Float.cos a, 60. *. Float.sin a);
          max_speed = 60.;
          max_force = 60. })
  in
  let order boids =
    let x, y = List.fold_left (fun (x, y) b -> let hx, hy = heading b in (x +. hx, y +. hy)) (0., 0.) boids in
    Float.hypot x y /. 30.
  in
  let before = order boids in
  let boids = ref boids in
  for _ = 1 to 20 * 60 do
    boids := List.map (fun b -> move ~dt:(1. /. 60.) (Flock.flock ~radius:80. !boids b) b) !boids
  done;
  let after = order !boids in
  Printf.printf "order: %.2f before, %.2f after\n" before after;
  Alcotest.(check bool) "scattered before" true (before < 0.2);
  Alcotest.(check bool) "one way after" true (after > 0.9)

let tests =
  [ t "Steering: seek and flee" test_seek;
    t "Steering: arrive" test_arrive;
    t "Steering: pursue and evade" test_pursue;
    t "Steering: wander" test_wander;
    t "Steering: avoid" test_avoid;
    t "Steering: follow a path" test_follow;
    t "Steering: move" test_move;
    t "Flock: two boids" test_two_boids;
    t "Flock: the flock appears" test_emergence ]
