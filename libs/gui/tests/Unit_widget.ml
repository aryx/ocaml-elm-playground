(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gui/Widget: the rectangle, the hit test and the paint -- the worked
 * examples of Widget.mli, checked. *)

let t = Testo.create
let button : Widget.box = { Widget.x = 0.; y = 100.; w = 200.; h = 40. }

(* the .mli's example: a button at (0, 100), 200 x 40 *)
let test_contains () =
  Alcotest.(check bool) "the mouse at (90, 110) is on it" true (Widget.contains button 90. 110.);
  Alcotest.(check bool)
    "the mouse at (110, 110) is past its right edge" false
    (Widget.contains button 110. 110.);
  Alcotest.(check bool)
    "and at (0, 121), one pixel above it" false
    (Widget.contains button 0. 121.);
  (* the edge belongs to the widget: a pixel is either in or out, and
   * two widgets should not share one *)
  Alcotest.(check bool) "its corner is inside" true (Widget.contains button 100. 120.)

let test_sides () =
  Alcotest.(check (float 1e-9)) "left" (-100.) (Widget.left button);
  Alcotest.(check (float 1e-9)) "right" 100. (Widget.right button);
  Alcotest.(check (float 1e-9)) "bottom" 80. (Widget.bottom button);
  Alcotest.(check (float 1e-9)) "top" 120. (Widget.top button)

let test_inset () =
  let b : Widget.box = Widget.inset 5. button in
  Alcotest.(check (float 1e-9)) "5 off each side" 190. b.w;
  Alcotest.(check (float 1e-9)) "and each end" 30. b.h;
  Alcotest.(check (float 1e-9)) "same center" 0. b.x;
  (* a border cannot eat more than there is *)
  let tiny : Widget.box = Widget.inset 100. button in
  Alcotest.(check (float 1e-9)) "never negative" 0. tiny.h

(* four bars, each inside the box, none overlapping another *)
let test_frame () =
  let bars =
    Widget.frame Color.black 4. button
    |> List.filter_map (function Widget.Fill (_, b) -> Some b | Widget.Text (_, b, _) -> Some b | Disc _ | Segment _ -> None)
  in
  Alcotest.(check int) "four sides" 4 (List.length bars);
  bars
  |> List.iter (fun (b : Widget.box) ->
         Alcotest.(check bool)
           "inside the box" true
           (Widget.left b >= Widget.left button -. 1e-9
           && Widget.right b <= Widget.right button +. 1e-9
           && Widget.bottom b >= Widget.bottom button -. 1e-9
           && Widget.top b <= Widget.top button +. 1e-9));
  match bars with
  | top :: bottom :: _ ->
      Alcotest.(check (float 1e-9)) "the top bar is 4 thick" 4. top.h;
      Alcotest.(check (float 1e-9)) "and sits at the top" 120. (Widget.top top);
      Alcotest.(check (float 1e-9)) "the bottom bar at the bottom" 80. (Widget.bottom bottom)
  | _ -> Alcotest.fail "expected four bars"

(* the .mli's example: "Save" at size 20 is about 48 wide *)
let test_text_width () =
  Alcotest.(check (float 1e-9)) "0.6 em a glyph" 48. (Widget.text_width ~size:20. "Save");
  Alcotest.(check (float 1e-9)) "nothing is nothing wide" 0. (Widget.text_width ~size:20. "")

let tests =
  [
    t "the hit test, edges included" test_contains;
    t "the sides of a box, y up" test_sides;
    t "inset never goes negative" test_inset;
    t "a frame is four bars inside the box" test_frame;
    t "how wide a label will be" test_text_width;
  ]
