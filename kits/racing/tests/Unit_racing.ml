(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* kits/racing: Road and Car *)

let t = Testo.create

let test_build () =
  let track = Road.build 200. [ Road.hill 4 1. ] in
  let heights = Array.to_list (Array.map (fun (s : Road.segment) -> s.y2) track.segments) in
  Alcotest.(check (list (float 0.05))) "eased up to 200" [ 29.29; 100.; 170.71; 200. ] heights;
  Alcotest.(check (float 1e-9)) "length" 800. (Road.length track);
  Alcotest.(check int) "wraps around" 1 (Road.segment_at track 1000.).index

let test_coast () =
  let track = Road.build 200. Road.coast in
  Alcotest.(check int) "1060 segments" 1060 (Array.length track.segments);
  let last = track.segments.(Array.length track.segments - 1) in
  Alcotest.(check (float 1e-6)) "back to its starting height" 0. last.y2

let test_centerline () =
  let track = Road.build 2. [ { enter = 0; hold = 1; leave = 0; curve = 1.; hill = 0. } ] in
  let p = (Road.centerline 90. track).(1) in
  Alcotest.(check (list (float 1e-9))) "turned right, gone right" [ 2.; 0.; 0.; 90. ] [ p.x; p.y; p.z; p.heading ]

let test_car () =
  let track = Road.build 200. [ Road.straight 100 ] in
  let p = Car.params 200. in
  let up = { Playground.initial_computer.keyboard with kup = true } in
  let rec drive n car = if n = 0 then car else drive (n - 1) (Car.drive p track up car) in
  Alcotest.(check (float 1e-6)) "full speed from a stop in 5 seconds" p.max_speed (drive 300 Car.start).speed;
  let on_grass = { Car.start with x = 2.; speed = p.max_speed } in
  (* accelerating (+0.2 of the top speed per second) on the grass
   * (-0.5): -0.3 per second, 0.4 of the top speed after 2 seconds *)
  let slowed = drive 120 on_grass in
  Alcotest.(check (float 1.)) "the grass slows it down" (0.4 *. p.max_speed) slowed.speed

let tests =
  Testo.categorize "kit_racing"
    [ t "build" test_build; t "the coast" test_coast; t "centerline" test_centerline; t "car" test_car ]
