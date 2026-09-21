(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/draw: a drawing as objects -- a hollow shape hit on its
 * outline only, the one in front hit first, a marquee taking what is
 * wholly inside it, the order front to back, groups, and resizing as a
 * map of the points, which a group passes down to what it holds. *)

let t = Testo.create

let hollow = { Figure.fill = None; pen = 1. }
let filled = { Figure.fill = Some 0.5; pen = 1. }
let square style x y s = Figure.Rect (Figure.box (x, y) (x +. s, y +. s), style)
let tol = 3.
let box = Alcotest.(list (float 1e-9))
let as_list (b : Figure.box) = [ b.x0; b.y0; b.x1; b.y1 ]

let test_a_hollow_shape_is_its_outline () =
  let r = square hollow 0. 0. 100. and f = square filled 0. 0. 100. in
  Alcotest.(check bool) "hollow: on the outline" true (Figure.hit ~tolerance:tol r (50., 1.));
  Alcotest.(check bool) "hollow: not in the middle" false (Figure.hit ~tolerance:tol r (50., 50.));
  Alcotest.(check bool) "filled: in the middle" true (Figure.hit ~tolerance:tol f (50., 50.));
  let o = Figure.Oval (Figure.box (0., 0.) (100., 50.), hollow) in
  Alcotest.(check bool) "a hollow oval: its rim" true (Figure.hit ~tolerance:tol o (100., 25.));
  Alcotest.(check bool) "not its middle" false (Figure.hit ~tolerance:tol o (50., 25.));
  let l = Figure.Line ((0., 0.), (100., 100.), hollow) in
  Alcotest.(check bool) "a line, near it" true (Figure.hit ~tolerance:tol l (50., 52.));
  Alcotest.(check bool) "a line, past its end" false (Figure.hit ~tolerance:tol l (110., 110.))

let test_the_one_in_front () =
  let d, back = Drawing.add (square filled 0. 0. 100.) Drawing.empty in
  let d, front = Drawing.add (square filled 50. 50. 100.) d in
  let d, ring = Drawing.add (square hollow 0. 0. 200.) d in
  Alcotest.(check (option int)) "overlapping: the front one" (Some front) (Drawing.at ~tolerance:tol d (75., 75.));
  Alcotest.(check (option int)) "through the hollow one in front" (Some back) (Drawing.at ~tolerance:tol d (25., 25.));
  Alcotest.(check (option int)) "and on its outline, it" (Some ring) (Drawing.at ~tolerance:tol d (200., 100.));
  let d = Drawing.to_front [ back ] d in
  Alcotest.(check (option int)) "brought to the front" (Some back) (Drawing.at ~tolerance:tol d (75., 75.));
  let d = Drawing.to_back [ back ] d in
  Alcotest.(check (list int)) "and sent back, the others' order kept" [ back; front; ring ] (List.map fst (Drawing.figures d))

let test_a_marquee_takes_what_is_inside () =
  let d, a = Drawing.add (square filled 0. 0. 10.) Drawing.empty in
  let d, _ = Drawing.add (square filled 5. 5. 100.) d in
  Alcotest.(check (list int)) "only the one wholly inside" [ a ] (Drawing.within d (Figure.box (-1., -1.) (50., 50.)))

(* the worked example of the .mli: the group resized to twice its size
   takes both of its squares with it, sizes and places *)
let test_resizing_a_group_scales_its_children () =
  let g = Figure.Group [ square filled 0. 0. 10.; square filled 90. 90. 10. ] in
  match Figure.fit (Figure.box (0., 0.) (200., 200.)) g with
  | Figure.Group [ a; b ] ->
      Alcotest.check box "the first, twice as big" [ 0.; 0.; 20.; 20. ] (as_list (Figure.bounds a));
      Alcotest.check box "the second, twice as far" [ 180.; 180.; 200.; 200. ] (as_list (Figure.bounds b))
  | _ -> Alcotest.fail "not a group any more"

let test_group_and_ungroup () =
  let d, a = Drawing.add (square filled 0. 0. 10.) Drawing.empty in
  let d, b = Drawing.add (square filled 20. 0. 10.) d in
  let d, c = Drawing.add (square filled 40. 0. 10.) d in
  let d, g = Drawing.group [ a; b ] d in
  let g = Option.get g in
  Alcotest.(check int) "two figures now" 2 (List.length (Drawing.figures d));
  Alcotest.(check (option int)) "a click on a member is the group" (Some g) (Drawing.at ~tolerance:tol d (5., 5.));
  let d = Drawing.move [ g ] 100. 0. d in
  let d, back = Drawing.ungroup g d in
  Alcotest.(check int) "ungrouped" 2 (List.length back);
  Alcotest.(check (list int)) "back where the group was, before c" (back @ [ c ]) (List.map fst (Drawing.figures d));
  Alcotest.check box "moved with it" [ 100.; 0.; 110.; 10. ] (as_list (Figure.bounds (Option.get (Drawing.get d (List.hd back)))))

let test_handles () =
  let r = square filled 0. 0. 100. in
  (* handle 2 is the bottom-right corner: dragged out, the top-left stays *)
  Alcotest.check box "a corner dragged" [ 0.; -50.; 150.; 100. ] (as_list (Figure.bounds (Figure.drag_handle r 2 (150., -50.))));
  (* handle 5 is the right side: only x moves *)
  Alcotest.check box "a side dragged" [ 0.; 0.; 40.; 100. ] (as_list (Figure.bounds (Figure.drag_handle r 5 (40., 999.))));
  Alcotest.check box "past the other side: the other way round" [ -30.; 0.; 0.; 100. ]
    (as_list (Figure.bounds (Figure.drag_handle r 5 (-30., 0.))));
  let l = Figure.Line ((0., 0.), (10., 10.), hollow) in
  Alcotest.(check int) "a line has two" 2 (List.length (Figure.handles l));
  Alcotest.check box "its end moved: flat now" [ 0.; 0.; 50.; 0. ] (as_list (Figure.bounds (Figure.drag_handle l 1 (50., 0.))))

let test_align () =
  let d, a = Drawing.add (square filled 10. 0. 10.) Drawing.empty in
  let d, b = Drawing.add (square filled 50. 30. 20.) d in
  let d = Drawing.align Drawing.Lefts [ a; b ] d in
  Alcotest.(check (list (float 1e-9))) "both on the leftmost's left edge" [ 10.; 10. ]
    (List.map (fun id -> (Figure.bounds (Option.get (Drawing.get d id))).x0) [ a; b ])

let tests =
  [
    t "a hollow shape is only its outline" test_a_hollow_shape_is_its_outline;
    t "the one in front is hit first" test_the_one_in_front;
    t "a marquee takes what is wholly inside" test_a_marquee_takes_what_is_inside;
    t "resizing a group scales what it holds" test_resizing_a_group_scales_its_children;
    t "group, move, ungroup" test_group_and_ungroup;
    t "handles: corners, sides, and a line's ends" test_handles;
    t "align lefts" test_align;
  ]
