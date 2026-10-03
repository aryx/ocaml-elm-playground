(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_physics.mli *)

open Playground

let grey = rgb 128 128 128

(* a floor 4000 wide, its top at y = 0 *)
let floor : Physics.body = Physics.body (rectangle grey 4000. 200.) |> Physics.at 0. (-100.) |> Physics.immovable

(* a stick 14.8 by 2, on its end, 100 above the floor *)
let stick : Physics.body = Physics.body (rectangle grey 14.8 2.) |> Physics.at 0. 100. |> Physics.pointing 90.

(* [ticks] ticks of a world under gravity: the first body after, and
 * the fastest it went *)
let run ?(ticks = 300) ~(steps : int) (bodies : Physics.body list) : Physics.body * float =
  let w = ref (Physics.world bodies) and fastest = ref 0. in
  for _ = 1 to ticks do
    w := Physics.simulate ~gravity:216. ~steps !w;
    fastest := Float.max !fastest (Physics.speed (List.hd !w.bodies))
  done;
  (List.hd !w.bodies, !fastest)

let tests =
  Testo.categorize "Physics"
    [
      Testo.create "a thin stick on its end: thrown with one step, landed with four" (fun () ->
          (* it falls 3.3 a step when it reaches the floor, and is 2 thick *)
          let (_, fastest) = run ~steps:1 [ stick; floor ] in
          Alcotest.(check bool) (Printf.sprintf "one step a tick: thrown (%.0f pixels a second)" fastest) true (fastest > 1000.);
          let (landed, fastest) = run ~steps:4 [ stick; floor ] in
          Alcotest.(check bool) (Printf.sprintf "four: never faster than its fall (%.0f)" fastest) true (fastest < 250.);
          Alcotest.(check bool) (Printf.sprintf "and lying on the floor (y = %.1f)" landed.y) true (landed.y > 0. && landed.y < 9.);
          (* a box 4 thick needs none of it *)
          let thick = Physics.body (rectangle grey 14.8 4.) |> Physics.at 0. 100. |> Physics.pointing 90. in
          Alcotest.(check bool) "a box 4 thick lands with one" true (snd (run ~steps:1 [ thick; floor ]) < 250.));
      Testo.create "a tick in four steps: the same tick" (fun () ->
          let ball = Physics.body (circle grey 5.) |> Physics.at 0. 500. in
          let fall steps = 500. -. (fst (run ~ticks:1 ~steps [ ball ])).y in
          (* g / 3600 in one step; (1 + 2 + 3 + 4) / 16 of it in four *)
          Alcotest.(check (Alcotest.float 1e-9)) "one step: g / 3600" (216. /. 3600.) (fall 1);
          Alcotest.(check (Alcotest.float 1e-9)) "four: 0.625 of it" (0.625 *. 216. /. 3600.) (fall 4);
          (* a push lasts all the steps, and is then used up *)
          let pushed steps = List.hd (Physics.simulate ~steps (Physics.world [ ball |> Physics.push 600. 0. ])).bodies in
          Alcotest.(check (Alcotest.float 1e-9)) "a push of 600 for a tick: 10 a second, with one step" 10. (pushed 1).vx;
          Alcotest.(check (Alcotest.float 1e-9)) "and with four" 10. (pushed 4).vx;
          Alcotest.(check (Alcotest.float 1e-9)) "used up" 0. (pushed 4).ax);
      Testo.create "a group: its bodies pass through each other" (fun () ->
          let box x = Physics.body (rectangle grey 20. 20.) |> Physics.at x 30. in
          let apart (a : Physics.body) (b : Physics.body) =
            let w = ref (Physics.world [ a; b; floor ]) in
            for _ = 1 to 200 do w := Physics.simulate ~gravity:216. !w done;
            match !w.bodies with a :: b :: _ -> (Float.abs (b.x -. a.x), a.y) | _ -> Alcotest.fail "two bodies"
          in
          (* two boxes of 20, their middles 6 apart: 14 into each other *)
          let (d, _) = apart (box 0.) (box 6.) in
          Alcotest.(check bool) (Printf.sprintf "no group: pushed out of each other (%.1f apart)" d) true (d > 19.);
          let (d, y) = apart (box 0. |> Physics.grouped 7) (box 6. |> Physics.grouped 7) in
          Alcotest.(check (Alcotest.float 0.01)) "the same group: still 6 apart" 6. d;
          Alcotest.(check bool) (Printf.sprintf "and on the floor all the same (y = %.1f)" y) true (y > 9. && y < 11.);
          let (d, _) = apart (box 0. |> Physics.grouped 7) (box 6. |> Physics.grouped 8) in
          Alcotest.(check bool) "two groups: pushed apart" true (d > 19.);
          let (d, _) = apart (box 0. |> Physics.grouped 7) (box 6.) in
          Alcotest.(check bool) "one in a group, the other in none: pushed apart" true (d > 19.));
    ]
