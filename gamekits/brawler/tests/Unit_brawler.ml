(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gamekits/brawler: Hitbox, Frame_data, Stickman *)

let t = Testo.create
let pt = Alcotest.(pair (float 1e-9) (float 1e-9))

(* place's and overlap's worked examples *)
let test_hitbox () =
  let punch = Hitbox.place (-1.) (200., 0.) { x = 40.; y = 100.; w = 30.; h = 20. } in
  Alcotest.check pt "mirrored" (160., 100.) (punch.x, punch.y);
  Alcotest.(check bool) "on the body" true (Hitbox.overlap punch { x = 130.; y = 80.; w = 40.; h = 120. });
  Alcotest.(check bool) "too far" false (Hitbox.overlap punch { x = 100.; y = 80.; w = 40.; h = 120. })

(* the jab: 3 + 2 + 6; +5 on hit, +1 on block *)
let test_frames () =
  let jab : Frame_data.move = { startup = 3; active = 2; recovery = 6; damage = 5; hitstun = 12; blockstun = 8; hitbox = { x = 0.; y = 0.; w = 0.; h = 0. } } in
  Alcotest.(check (list bool)) "phases" [ true; true; true ] [ Frame_data.phase jab 3 = Startup; Frame_data.phase jab 5 = Active; Frame_data.phase jab 11 = Recovery ];
  Alcotest.(check bool) "over" true (Frame_data.phase jab 12 = Over);
  Alcotest.(check int) "on hit" 5 (Frame_data.advantage_on_hit jab);
  Alcotest.(check int) "on block" 1 (Frame_data.advantage_on_block jab)

(* hand's worked example, and at's *)
let test_stickman () =
  let straight = { Stickman.stand with front_leg = (0., 0.); back_leg = (0., 0.); front_arm = (90., 90.) } in
  Alcotest.check pt "the fist" (68., 160.) (Stickman.hand 200. straight);
  let p = Stickman.at [ (0, Stickman.stand); (10, straight) ] 5 in
  Alcotest.(check (float 1e-9)) "halfway" ((15. +. 90.) /. 2.) (fst p.front_arm);
  Alcotest.(check (float 1e-9)) "after the last" 90. (fst (Stickman.at [ (0, Stickman.stand); (10, straight) ] 20).front_arm)

let tests = Testo.categorize "kit_brawler" [ t "Hitbox" test_hitbox; t "Frame_data" test_frames; t "Stickman" test_stickman ]
