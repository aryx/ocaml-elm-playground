(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Character3d, the character controller: one course piece
 * per test, and the character walked over it for a second or two *)

open Playground
open Playground3d

let t = Testo.create

(* a box of solid with its bottom at [y], its sides along the axes *)
let block ?(y = 0.) (x1, z1) (x2, z2) (h : number) : Physics3d.body =
  Physics3d.body (box white (x2 -. x1) h (z2 -. z1))
  |> Physics3d.at ((x1 +. x2) /. 2.) (y +. (h /. 2.)) ((z1 +. z2) /. 2.)
  |> Physics3d.immovable

(* the ground, its top at y = 0 *)
let ground = block ~y:(-1.) (-20., -20.) (20., 20.) 1.

(* a ramp rising towards +x at [degrees], from the ground at x = 1:
 * a box turned about z, its top face through (1, 0) *)
let ramp (degrees : number) : Physics3d.body =
  let a = degrees *. Float.pi /. 180. and len = 8. and thick = 1. in
  let cx = 1. +. (len /. 2. *. cos a) +. (thick /. 2. *. sin a) in
  let cy = (len /. 2. *. sin a) -. (thick /. 2. *. cos a) in
  Physics3d.body (box white len thick 4.) |> Physics3d.pointing (0., 0., 1.) degrees |> Physics3d.at cx cy 0. |> Physics3d.immovable

let rec ticks n f c = if n = 0 then c else ticks (n - 1) f (f c)
let near eps what expected got = Alcotest.(check (float eps)) what expected got

(* on flat ground, it goes at the speed asked, and stays on the ground *)
let walking () =
  let c = ticks 60 (Character3d.walk [ ground ] (2., 0.)) (Character3d.make 0. 0. 0.) in
  near 1e-6 "two metres in a second" 2. c.x;
  near 1e-3 "on the ground" 0. c.y;
  Alcotest.(check bool) "standing" true c.grounded

(* into a wall at 45 degrees: stopped across it, one radius from it,
 * and still going along it at the speed along it *)
let sliding () =
  let wall = block (1., -20.) (2., 20.) 3. in
  let c = ticks 60 (Character3d.walk [ ground; wall ] (2., 2.)) (Character3d.make 0. 0. 0.) in
  near 1e-3 "against the wall" 0.7 c.x;
  near 0.02 "along it, as if it weren't there" 2. c.z

(* a step walked up when it is lower than the step offset, and a wall
 * when it is higher: the plan's measured switch, a 0.5 m step turning
 * into a wall at 0.4 *)
let steps () =
  let up step h =
    let c = Character3d.make ~step 0. 0. 0. in
    (ticks 90 (Character3d.walk [ ground; block (1., -2.) (5., 2.) h ] (2., 0.)) c).y
  in
  near 1e-3 "0.3 m, offset 0.4: on it" 0.3 (up 0.4 0.3);
  near 1e-3 "0.5 m, offset 0.4: a wall" 0. (up 0.4 0.5);
  near 1e-3 "0.5 m, offset 0.6: on it" 0.5 (up 0.6 0.5)

(* a 30 degree ramp walked up; a 50 degree one is a wall to the feet,
 * and let go on it, the character slides down it *)
let slopes () =
  let climb deg = ticks 120 (Character3d.walk [ ground; ramp deg ] (2., 0.)) (Character3d.make 0. 0. 0.) in
  let c = climb 30. in
  Alcotest.(check bool) "up the 30 degrees" true (c.y > 1.);
  near 0.5 "standing on 30 degrees" 30. c.ground;
  let c = climb 50. in
  Alcotest.(check bool) "not up the 50 degrees" true (c.y < 0.3);
  let a = 50. *. Float.pi /. 180. in
  let on_it = Character3d.make 3. ((2. *. tan a) +. 0.2) 0. in
  let c = ticks 120 (Character3d.walk [ ground; ramp 50. ] (0., 0.)) on_it in
  Alcotest.(check bool) "let go on it, it slides down" true (c.x < 2. && c.y < 1.)

(* a jump under a low ceiling: the head stops it, and it comes back
 * down to stand *)
let ceiling () =
  let roof = block ~y:2.2 (-2., -2.) (2., 2.) 0.3 in
  let c = Character3d.walk [ ground; roof ] (0., 0.) (Character3d.make 0. 0. 0.) in
  let c = Character3d.walk ~jump:5. [ ground; roof ] (0., 0.) c in
  let highest = ref c.y and c = ref c in
  for _ = 1 to 60 do
    c := Character3d.walk [ ground; roof ] (0., 0.) !c;
    highest := Float.max !highest !c.y
  done;
  near 0.01 "the head at the ceiling" 0.4 !highest;
  Alcotest.(check bool) "back on the ground" true (!c.grounded && !c.y < 1e-3)

(* off a ledge a metre high: more than a step, so it falls, and lands *)
let ledge () =
  let shelf = block (-5., -2.) (0., 2.) 1. in
  let c = Character3d.make (-0.5) 1. 0. in
  let fell = ref false and c = ref c in
  for _ = 1 to 90 do
    c := Character3d.walk [ ground; shelf ] (2., 0.) !c;
    if not !c.grounded then fell := true
  done;
  Alcotest.(check bool) "it fell" true !fell;
  near 1e-3 "landed on the ground" 0. !c.y;
  Alcotest.(check bool) "standing" true !c.grounded

let tests =
  [ t "Character3d, walking on the ground" walking;
    t "Character3d, sliding along a wall" sliding;
    t "Character3d, the step offset" steps;
    t "Character3d, the slope limit" slopes;
    t "Character3d, a jump under a low ceiling" ceiling;
    t "Character3d, off a ledge" ledge ]
