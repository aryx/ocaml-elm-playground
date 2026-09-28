(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gamekits/shmup: Shots and Path *)

let t = Testo.create
let pt = Alcotest.(pair (float 1e-9) (float 1e-9))

(* aimed's worked example: at speed 5 from (0, 0) to (30, -40), (3, -4)
 * each frame *)
let test_aimed () =
  let s = Shots.aimed 5. (0., 0.) (30., -40.) in
  Alcotest.check pt "speed" (3., -4.) (s.vx, s.vy);
  let s = Shots.advance s in
  Alcotest.check pt "one frame later" (3., -4.) (s.x, s.y)

(* moving by distance along a path: a straight one, 300 long, its middle
 * at 150, headed right (0 degrees, not radians -- what [rotate] wants);
 * a curved one, the same speed everywhere (a step of 5 pixels along it
 * moves 5 pixels, give or take the chords' shortcut). The curves
 * themselves are tested in graphics/tests/Unit_curve.ml *)
let test_path () =
  let line = Path.make [ (0., 0.); (100., 0.); (200., 0.); (300., 0.) ] in
  Alcotest.(check (float 1e-6)) "length" 300. (Path.length line);
  Alcotest.check pt "middle" (150., 0.) (fst (Path.at line 150.));
  Alcotest.(check (float 1e-9)) "in degrees" 0. (snd (Path.at line 150.));
  let p = Path.make [ (-560., -300.); (-300., -150.); (-100., 0.); (-100., 200.); (-250., 250.); (-350., 100.); (-200., 0.) ] in
  let at s = fst (Path.at p s) in
  List.iter
    (fun i ->
      let (x0, y0), (x1, y1) = (at (float_of_int i *. 5.), at (float_of_int (i + 1) *. 5.)) in
      Alcotest.(check (float 0.2)) "a step" 5. (Float.hypot (x1 -. x0) (y1 -. y0)))
    (List.init (int_of_float (Path.length p /. 5.) - 1) Fun.id)

let tests =
  Testo.categorize "kit_shmup"
    [ t "Shots, aimed" test_aimed; t "Path, at a constant speed" test_path ]
