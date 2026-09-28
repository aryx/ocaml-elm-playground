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

let blue = 0x0000ff

let picture (fb : Framebuffer.t) : string list =
  List.init fb.height (fun y ->
      String.init fb.width (fun x -> if Framebuffer.get_rgb fb ~x ~y = blue then '#' else '.'))

let line ~width ~height p0 p1 =
  let fb = Framebuffer.create ~width ~height in
  Framebuffer.clear fb ~rgb:0xffffff;
  Line.bresenham fb p0 p1 ~rgb:blue ~alpha:1.;
  picture fb

(* the example in Line.mli *)
let test_example () =
  Alcotest.(check (list string)) "(0, 0) -> (8, 3)"
    [ "##......."; "..###...."; ".....##.."; ".......##" ]
    (line ~width:9 ~height:4 (0, 0) (8, 3))

let test_steep () =
  Alcotest.(check (list string)) "(0, 0) -> (2, 5): one pixel per row"
    [ "#.."; "#.."; ".#."; ".#."; "..#"; "..#" ]
    (line ~width:3 ~height:6 (0, 0) (2, 5))

(* each direction is covered: the same line drawn from the other end
 * lights as many pixels, and the endpoints *)
let test_reversed () =
  let count rows = List.fold_left (fun n row -> n + List.length (String.split_on_char '#' row) - 1) 0 rows in
  let forward = line ~width:9 ~height:4 (0, 0) (8, 3) in
  let backward = line ~width:9 ~height:4 (8, 3) (0, 0) in
  Alcotest.(check int) "same number of pixels" (count forward) (count backward);
  Alcotest.(check char) "start" '#' (List.nth backward 0).[0];
  Alcotest.(check char) "end" '#' (List.nth backward 3).[8]

let point = Alcotest.(pair (float 1e-9) (float 1e-9))

let test_clip () =
  (match Line.clip ~width:10. ~height:10. (-5., 5.) (15., 5.) with
  | Some (p0, p1) ->
      Alcotest.check point "left end on x = 0" (0., 5.) p0;
      Alcotest.check point "right end on x = 10" (10., 5.) p1
  | None -> Alcotest.fail "should cross the rectangle");
  Alcotest.(check bool) "both above: dropped" true
    (Line.clip ~width:10. ~height:10. (1., -5.) (9., -1.) = None);
  (* left and above, but passing by the corner: codes 0001 and 0100
   * share no bit, so it takes clipping to find out *)
  Alcotest.(check bool) "misses the corner: dropped" true
    (Line.clip ~width:10. ~height:10. (-5., 2.) (2., -5.) = None)

(* without clipping, this would take a billion steps *)
let test_far_away_end () =
  let fb = Framebuffer.create ~width:10 ~height:3 in
  Line.draw fb (-1e9, 1.5) (5.5, 1.5) ~rgb:blue ~alpha:1.;
  Alcotest.(check (list string)) "only the visible part"
    [ ".........."; "######...."; ".........." ]
    (picture fb)

let tests =
  Testo.categorize "Line"
    [
      t "the (0, 0) -> (8, 3) example" test_example;
      t "steep line" test_steep;
      t "reversed" test_reversed;
      t "Cohen-Sutherland clipping" test_clip;
      t "far away end" test_far_away_end;
    ]
