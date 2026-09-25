(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gamekits/racing: Road, Car, Topdown and Offroad *)

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

let test_topdown () =
  let p = Topdown.toy in
  let still = { Topdown.x = 0.; y = 0.; vx = 0.; vy = 0.; heading = 0.; speed = 0.; next = 1 } in
  let c1 = Topdown.drive p 700. 1. 0. still in
  let c2 = Topdown.drive p 700. 1. 0. c1 in
  Alcotest.(check (list (float 0.01))) "full gas from a stop" [ 14.625; 28.88 ] [ c1.speed; c2.speed ];
  (* the velocity follows the speed only by the grip, 0.12 of the way *)
  Alcotest.(check (float 1e-9)) "sliding" (0.12 *. 14.625) c1.vx;
  let track = { Topdown.points = [| (0., 0.); (100., 0.); (100., 100.) |]; reach = 10.; corner = 50. } in
  let c = Topdown.start track 0 5. in
  Alcotest.(check (list (float 1e-9))) "facing the next, to its left" [ 0.; 5.; 0. ] [ c.x; c.y; c.heading ];
  let c = Topdown.follow track { c with x = 95.; y = 0. } in
  Alcotest.(check int) "passed" 2 c.next;
  Alcotest.(check int) "first lap" 0 (Topdown.lap track c);
  Alcotest.(check int) "second lap" 1 (Topdown.lap track { c with next = 4 });
  Alcotest.(check (float 1e-9)) "progress" (20000. -. Float.hypot 5. 100.) (Topdown.progress track c);
  Alcotest.(check (list (float 1e-9)))
    "distance to the center line" [ 10.; Float.hypot 10. 10. ]
    [ Topdown.distance track 50. 10.; Topdown.distance track 110. (-10.) ];
  Alcotest.(check (float 1e-9)) "to the second segment only" 50. (Topdown.distance_from track 1 1 50. 10.)

let test_hitting () =
  let still = { Topdown.x = 0.; y = 0.; vx = 0.; vy = 0.; heading = 0.; speed = 0.; next = 1 } in
  let wall x _y = x > 100. in
  let c = Topdown.bounce wall { still with x = 95. } { still with x = 105.; vx = 300.; speed = 300. } in
  Alcotest.(check (list (float 1e-9))) "turned back by the wall" [ 95.; 0.; -150.; 150. ] [ c.x; c.y; c.vx; c.speed ];
  let c = Topdown.bounce wall { still with x = 95. } { still with x = 105.; y = 5.; vx = 300.; vy = 300. } in
  Alcotest.(check (list (float 1e-9))) "sliding along it" [ 95.; 5.; -150.; 300. ] [ c.x; c.y; c.vx; c.vy ];
  let a, b = Topdown.push 10. { still with vx = 100. } { still with x = 10. } in
  Alcotest.(check (list (float 1e-9))) "the one hitting stops, the one hit goes" [ -5.; 0.; 15.; 100. ] [ a.x; a.vx; b.x; b.vx ]

(* Offroad.mli's rules, each on a ground made for it; a car going +x,
 * its params without engine or friction, so that only the ground acts *)
let test_offroad () =
  let coast = { Topdown.accel = 0.; friction = 0.; grip = 1.; steering = 0.; steering_speed = 1. } in
  let track = { Topdown.points = [| (1000., 0.); (2000., 0.) |]; reach = 1.; corner = 1. } in
  let ground ?(gravity = 30.) height = { Offroad.height; gravity; lo = -1000.; hi = 1000. } in
  let car (g : Offroad.ground) x speed =
    Offroad.start g { Topdown.x; y = 0.; vx = speed; vy = 0.; heading = 0.; speed; next = 0 }
  in
  let rec run g n c = if n = 0 then c else run g (n - 1) (Offroad.drive g track coast 100. 0. 0. c) in
  let flat = ground (fun _ _ -> 0.) in
  let c = run flat 60 (car flat 0. 30.) in
  Alcotest.(check (list (float 1e-6))) "flat: a second at 30" [ 30.; 0. ] [ c.body.x; c.h ];
  (* a ramp rising 1 in 3 up to x = 0, flat after: the car climbs at a
   * third of its speed, and flies off the top rising at it *)
  let ramp = ground (fun x _ -> if x < 0. then x /. 3. else 0.) in
  let rec until_air c = if c.Offroad.air then c else until_air (Offroad.drive ramp track coast 100. 0. 0. c) in
  let c = run ramp 1 (car ramp (-6.) 30.) in
  Alcotest.(check (float 0.1)) "climbing at a third of its speed" (c.body.speed /. 3.) c.vh;
  let c = until_air c in
  Alcotest.(check bool) "off at the top, not before" true (c.body.x > 0. && c.body.x < 1.);
  Alcotest.(check (float 0.6)) "rising at a third of its speed" (c.body.speed /. 3.) c.vh;
  (* a round crest, the same speed, over its top (a quarter of a
   * second): the Earth pulls the car down onto the far side; the Moon,
   * a sixth of it, lets it fly *)
  let crest gravity = ground ~gravity (fun x _ -> -0.03 *. x *. x) in
  let rec flies g n c = n > 0 && (c.Offroad.air || flies g (n - 1) (Offroad.drive g track coast 100. 0. 0. c)) in
  let over g = flies g 15 (car g (-3.) 30.) in
  Alcotest.(check bool) "Earth: stays on" false (over (crest 30.));
  Alcotest.(check bool) "Moon: flies" true (over (crest 5.));
  (* a slope going down 1 in 5, the car dropped just above it: it lands,
   * then stays on the ground all the way down *)
  let down = ground (fun x _ -> -.x /. 5.) in
  let c = { (car down 0. 30.) with h = 0.05; air = true } in
  Alcotest.(check bool) "landed, and not hopping down the slope" false (flies down 60 (run down 40 c));
  (* a cliff at x = 10: the car bounced back from it *)
  let cliff = ground (fun x _ -> if x > 10. then 20. else 0.) in
  let c = run cliff 60 (car cliff 0. 30.) in
  Alcotest.(check bool) "stopped by the cliff" true (c.body.x <= 10. && c.h = 0.)

let tests =
  Testo.categorize "kit_racing"
    [ t "build" test_build; t "the coast" test_coast; t "centerline" test_centerline; t "car" test_car; t "topdown" test_topdown;
      t "hitting" test_hitting; t "offroad" test_offroad ]
