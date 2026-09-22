(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let t = Testo.create

(* [n] frames of the same input, stopping early at a fall or the exit *)
let crush_run (n : int) (i : Crush.input) (p : Crush.play) : Crush.play =
  let rec go n p = if n = 0 || Crush.fell p || Crush.at_exit p then p else go (n - 1) (Crush.step i p) in
  go n p

(* a key pressed once, then the crush drawn to its end *)
let crush_press (i : Crush.input) (p : Crush.play) : Crush.play = crush_run 30 Crush.nothing (Crush.step i p)

(* the column Danny is in *)
let crush_col (p : Crush.play) : int = fst (Tilemap.cell (Crush.current p) p.u p.y)

(* The first level, the gap: too wide to walk across; crushed, the
 * bridge three slices back is under Danny's feet, and the wall of the
 * last slice is in his way; uncrushed past the bridge, he is back at depth 0, the wall gone,
 * and at the exit. On the bridge, uncrushing puts him at its depth. *)
let crush_the_gap () =
  let open Crush in
  let right = { nothing with dx = 1. } in
  let p = crush_run 20 nothing (enter 0) in
  Alcotest.(check bool) "uncrushed, the gap is a fall" true (fell (crush_run 300 right p));
  let crushed = crush_press { nothing with crush_key = true } p in
  Alcotest.(check bool) "crushed" true (crushed.crushed && crushed.squash = 0.);
  let stopped = crush_run 300 right crushed in
  Alcotest.(check bool) "across the gap, stopped by the wall behind" true ((not (fell stopped)) && crush_col stopped = 9);
  let back = crush_press { nothing with crush_key = true } stopped in
  Alcotest.(check int) "uncrushed at the depth of his floor" 0 back.depth;
  Alcotest.(check bool) "the wall was only behind: out" true (at_exit (crush_run 300 right back));
  (* on the bridge instead *)
  let rec to_col c n p = if n = 0 || crush_col p >= c then p else to_col c (n - 1) (step right p) in
  let on_bridge = crush_run 10 nothing (to_col 5 300 crushed) in
  Alcotest.(check int) "uncrushed on the bridge: its depth" 2 (crush_press { nothing with crush_key = true } on_bridge).depth

(* The second, the deep: where Danny starts, no room to crush from the
 * front; turned a quarter, the crush fills the hole in depth with the
 * floor far to the right, and a jump over the block in the way reaches
 * the exit. *)
let crush_the_deep () =
  let open Crush in
  let p = crush_run 20 nothing (enter 1) in
  let refused = step { nothing with crush_key = true } p in
  Alcotest.(check bool) "no room" true ((not refused.crushed) && refused.message = "no room to crush here");
  let turned = step { nothing with turn_key = true } p in
  Alcotest.(check int) "the side view" 1 turned.view;
  let crushed = crush_press { nothing with crush_key = true } turned in
  Alcotest.(check bool) "crushed along x" true crushed.crushed;
  let rec go n p =
    if n = 0 || fell p || at_exit p then p
    else
      (* running right, jumping at the block of slice 3 *)
      let jump = p.ground && crush_col p = 2 in
      go (n - 1) (step { nothing with dx = 1.; jump } p)
  in
  let out = go 400 crushed in
  Alcotest.(check bool) "out" true (at_exit out)

let tests = [ t "the gap: crush to cross, uncrush to pass" crush_the_gap; t "the deep: the side view" crush_the_deep ]
