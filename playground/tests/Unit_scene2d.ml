(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Scene2d *)

open Playground

let t = Testo.create

type scene = Title | Playing

let at (time : float) ?(space = false) (s : scene Scene2d.t) : scene Scene2d.t =
  let computer = { initial_computer with time = Time time; keyboard = { initial_computer.keyboard with kspace = space } } in
  Scene2d.update computer s

let test_elapsed () =
  let s = Scene2d.start Title |> at 100.0 |> at 100.5 |> at 101.25 in
  Alcotest.(check (float 1e-9)) "elapsed" 1.25 s.elapsed;
  Alcotest.(check int) "frames" 3 s.frames;
  let s = Scene2d.go Playing s in
  Alcotest.(check (float 1e-9)) "go: elapsed from 0" 0. s.elapsed;
  Alcotest.(check (float 1e-9)) "and counting" 0.5 (at 101.75 s).elapsed

let test_pressed () =
  let space = fun k -> k.kspace in
  let s1 = Scene2d.start Title |> at 1. ~space:true in
  let s2 = at 2. ~space:true s1 in
  let s3 = at 3. ~space:true s2 in
  Alcotest.(check (list bool)) "held 3 frames: pressed once"
    [ true; false; false ] (List.map (Scene2d.pressed space) [ s1; s2; s3 ]);
  Alcotest.(check bool) "released, then pressed again" true
    (Scene2d.pressed space (s3 |> at 4. |> at 5. ~space:true));
  Alcotest.(check bool) "held across go: not pressed" false (Scene2d.pressed space (Scene2d.go Playing s1))

let test_blink () =
  let shown elapsed = Scene2d.blink 1. { (Scene2d.start Title) with elapsed } [ circle red 1. ] <> [] in
  Alcotest.(check (list bool)) "period 1" [ true; true; false; true ] (List.map shown [ 0.; 0.4; 0.6; 1.2 ])

let tests =
  Testo.categorize "Scene2d" [ t "elapsed and frames" test_elapsed; t "pressed" test_pressed; t "blink" test_blink ]
