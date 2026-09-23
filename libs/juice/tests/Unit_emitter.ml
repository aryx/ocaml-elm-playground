(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_emitter.mli *)

let near = Alcotest.float 1e-3
let dt = 1. /. 60.

(* one particle, no randomness: every pair's bounds equal *)
let one ~(speed : float) ~(direction : float) ~(gravity : float) ~(drag : float) : Emitter.recipe =
  { count = 1; speed = (speed, speed); direction; spread = 0.; life = (5., 5.); size = (4., 4.); spin = 0.; gravity; drag }

let sparks : Emitter.recipe =
  { count = 20; speed = (100., 400.); direction = 90.; spread = 360.; life = (0.2, 0.6); size = (2., 6.); spin = 90.; gravity = -400.; drag = 1. }

let steps (n : int) (t : 'a Emitter.t) : 'a Emitter.t = List.fold_left (fun t _ -> Emitter.step ~dt t) t (List.init n Fun.id)

let the_particle (t : 'a Emitter.t) : 'a Emitter.particle =
  match Emitter.particles t with [ p ] -> p | ps -> Alcotest.failf "%d particles, not 1" (List.length ps)

let tests =
  Testo.categorize "Emitter"
    [
      Testo.create "the worked example: thrown up, Euler 3.3 pixels short" (fun () ->
          let t = Emitter.empty ~seed:1 () |> Emitter.burst (one ~speed:400. ~direction:90. ~gravity:(-800.) ~drag:0.) ~data:Fun.id 0. 0. in
          Alcotest.check near "the top, frame 29" 96.667 (the_particle (steps 29 t)).y;
          Alcotest.check near "the top, frame 30" 96.667 (the_particle (steps 30 t)).y;
          Alcotest.check near "one second: below 0" (-6.667) (the_particle (steps 60 t)).y;
          Alcotest.check near "straight up: no sideways" 0. (the_particle (steps 60 t)).x);
      Testo.create "the worked example: drag" (fun () ->
          let t = Emitter.empty ~seed:1 () |> Emitter.burst (one ~speed:100. ~direction:0. ~gravity:0. ~drag:2.) ~data:Fun.id 0. 0. in
          let p = the_particle (steps 30 t) in
          Alcotest.check near "36.2 left of 100" 36.166 p.vx;
          Alcotest.check near "30.9 pixels gone" 30.853 p.x);
      Testo.create "the same seed, the same burst; another seed, another" (fun () ->
          let burst seed = Emitter.empty ~seed () |> Emitter.burst sparks ~data:Fun.id 10. 20. |> steps 10 |> Emitter.particles in
          Alcotest.(check bool) "same" true (burst 3 = burst 3);
          Alcotest.(check bool) "different" false (burst 3 = burst 4));
      Testo.create "a life spent is gone" (fun () ->
          let t = Emitter.empty ~seed:1 () |> Emitter.burst sparks ~data:Fun.id 0. 0. in
          Alcotest.(check int) "all born" 20 (List.length (Emitter.particles t));
          Alcotest.(check int) "none after 0.6 s" 0 (List.length (Emitter.particles (steps 37 t)));
          List.iter
            (fun (p : float Emitter.particle) ->
              if p.life < 0.2 || p.life > 0.6 || p.size < 2. || p.size > 6. || p.data < 0. || p.data > 1. then Alcotest.fail "out of its bounds")
            (Emitter.particles t));
      Testo.create "never more than the cap, the oldest dropped" (fun () ->
          (* three bursts of 20, a cap of 30: the third, and half the second *)
          let t = Emitter.empty ~cap:30 ~seed:1 () in
          let t = List.fold_left (fun t i -> Emitter.burst sparks ~data:(fun _ -> i) 0. 0. t) t [ 1; 2; 3 ] in
          let ps = Emitter.particles t in
          Alcotest.(check int) "30" 30 (List.length ps);
          Alcotest.(check bool) "none of the first burst" false (List.exists (fun (p : int Emitter.particle) -> p.data = 1) ps);
          Alcotest.(check int) "the oldest first: the second burst's" 2 (List.hd ps).data);
    ]
