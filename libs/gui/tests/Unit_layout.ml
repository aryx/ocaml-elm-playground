(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gui/Layout: constraints down, sizes up, the parent positions --
 * checked against rectangles computed by hand, which is the only way
 * to know a layout engine is right (a picture of a panel looks
 * plausible whatever the arithmetic did). *)

let t = Testo.create

(* the area everything below is laid out in: 400 x 400, centered on
 * the origin, so its left is -200 and its top 200 *)
let area : Widget.box = { Widget.x = 0.; y = 0.; w = 400.; h = 400. }
let room = Layout.loose 400. 400.
let size = (200., 40.)

let check_box name (x, y, w, h) (b : Widget.box) =
  Alcotest.(check (list (float 1e-9)))
    name [ x; y; w; h ] [ b.x; b.y; b.w; b.h ]

let place key places = List.assoc key places

(* the .mli's worked example, both passes *)
let test_column_of_three () =
  let panel =
    Layout.(center (column ~gap:10. [ leaf `A size; leaf `B size; leaf `C size ]))
  in
  Alcotest.(check (pair (float 1e-9) (float 1e-9)))
    "3 * 40 + 2 * 10 tall, as wide as its widest" (200., 140.)
    (Layout.measure room panel);
  let places = Layout.arrange area panel in
  check_box "the first, 50 above the middle" (0., 50., 200., 40.) (place `A places);
  check_box "the second, in the middle" (0., 0., 200., 40.) (place `B places);
  check_box "the third, 50 below" (0., -50., 200., 40.) (place `C places)

(* without the [center], the column starts at the top of what it is
 * given: the difference between "a panel" and "the whole screen" *)
let test_column_starts_at_the_top () =
  let places = Layout.arrange area Layout.(column ~gap:10. [ leaf `A size; leaf `B size ]) in
  check_box "20 below the top edge" (0., 180., 200., 40.) (place `A places);
  check_box "then the gap" (0., 130., 200., 40.) (place `B places)

(* glue: one spacer between two things pushes them apart *)
let test_spacer_pushes_apart () =
  let places =
    Layout.arrange area Layout.(row [ leaf `Left (100., 40.); spacer; leaf `Right (100., 40.) ])
  in
  check_box "hard left" (-150., 0., 100., 40.) (place `Left places);
  check_box "hard right" (150., 0., 100., 40.) (place `Right places)

(* two spacers center what is between them *)
let test_two_spacers_center () =
  let places = Layout.arrange area Layout.(row [ spacer; leaf `Mid (100., 40.); spacer ]) in
  check_box "in the middle" (0., 0., 100., 40.) (place `Mid places)

(* the flex rule: what is left over goes to the expanded child *)
let test_expand_takes_what_is_left () =
  let places =
    Layout.arrange area
      Layout.(row ~gap:0. [ leaf `Fixed (100., 40.); expand (leaf `Rest (50., 40.)) ])
  in
  check_box "asked for 100, got 100" (-150., 0., 100., 40.) (place `Fixed places);
  check_box "asked for 50, got the other 300" (50., 0., 300., 40.) (place `Rest places)

(* and two of them share it *)
let test_two_expands_share () =
  let places =
    Layout.arrange area Layout.(row ~gap:0. [ expand (leaf `L (10., 40.)); expand (leaf `R (10., 40.)) ])
  in
  check_box "half" (-100., 0., 200., 40.) (place `L places);
  check_box "the other half" (100., 0., 200., 40.) (place `R places)

(* across the axis: a child keeps its size and is centered, unless it
 * is stretched -- which is how a panel's widgets come out one width *)
let test_stretch_fills_the_cross_axis () =
  let panel =
    Layout.(center (column ~gap:0. [ leaf `Wide (200., 40.); stretch (leaf `Narrow (50., 40.)) ]))
  in
  let places = Layout.arrange area panel in
  check_box "the wide one sets the column's width" (0., 20., 200., 40.) (place `Wide places);
  check_box "and the narrow one fills it" (0., -20., 200., 40.) (place `Narrow places)

let test_narrow_child_is_centered () =
  let places =
    Layout.arrange area Layout.(center (column [ leaf `Wide (200., 40.); leaf `Narrow (50., 40.) ]))
  in
  check_box "centered, at its own width" (0., -20., 50., 40.) (place `Narrow places)

(* a pane in a window: the room left over along the row, *and* all of
   its height -- expand and stretch together, in either order. (Found
   by examples/TypesetParagraph, whose page pane floated at its natural height
   because stretch inside expand was ignored.) *)
let test_expand_and_stretch_together () =
  let pane k = Layout.(row ~gap:0. [ leaf `Fixed (100., 40.); k (leaf `Pane (50., 40.)) ]) in
  List.iter
    (fun (name, wrap) ->
      let places = Layout.arrange area (pane wrap) in
      check_box name (50., 0., 300., 400.) (place `Pane places))
    [
      ("expand (stretch x)", fun x -> Layout.expand (Layout.stretch x));
      ("stretch (expand x)", fun x -> Layout.stretch (Layout.expand x));
    ]

(* padding is room taken off all four sides, and what is inside fills
 * what is left (the .mli's rule: a leaf takes the box it is given) *)
let test_pad () =
  let padded = Layout.(pad 20. (leaf `Inside (100., 40.))) in
  Alcotest.(check (pair (float 1e-9) (float 1e-9)))
    "the child plus 20 on each side" (140., 80.) (Layout.measure room padded);
  check_box "filling what is left" (0., 0., 360., 360.) (place `Inside (Layout.arrange area padded))

(* constraints go down: a child cannot be wider than the room offered *)
let test_constraints_clamp () =
  Alcotest.(check (pair (float 1e-9) (float 1e-9)))
    "500 asked, 400 offered" (400., 40.)
    (Layout.measure room (Layout.leaf `Big (500., 40.)));
  Alcotest.(check (pair (float 1e-9) (float 1e-9)))
    "a tight constraint leaves no choice" (120., 120.)
    (Layout.measure (Layout.tight 120. 120.) (Layout.leaf `Small (10., 10.)))

(* a row inside a column: the two passes, nested *)
let test_nested () =
  let panel =
    Layout.(
      center
        (column ~gap:10.
           [ leaf `Title (200., 20.); row ~gap:20. [ leaf `Ok (80., 40.); leaf `Cancel (100., 40.) ] ]))
  in
  Alcotest.(check (pair (float 1e-9) (float 1e-9)))
    "as wide as the title, as tall as both rows and the gap" (200., 70.)
    (Layout.measure room panel);
  let places = Layout.arrange area panel in
  (* the panel is 70 tall, so it runs from y = 35 down: the title's 20
     put its center at 25, then the gap, then the row's 40 centers the
     buttons at -15 *)
  check_box "the title on top" (0., 25., 200., 20.) (place `Title places);
  (* the row is 200 wide (the column's width), its two buttons 80 + 20
     + 100 = 200 across it, so they exactly fill it *)
  check_box "the first button" (-60., -15., 80., 40.) (place `Ok places);
  check_box "the second" (50., -15., 100., 40.) (place `Cancel places)

let tests =
  [
    t "a column of three, measured and placed" test_column_of_three;
    t "a column without a center starts at the top" test_column_starts_at_the_top;
    t "a spacer pushes two things apart" test_spacer_pushes_apart;
    t "two spacers center what is between them" test_two_spacers_center;
    t "an expanded child takes what is left" test_expand_takes_what_is_left;
    t "two expanded children share it" test_two_expands_share;
    t "a stretched child fills the cross axis" test_stretch_fills_the_cross_axis;
    t "a narrow child is centered across the axis" test_narrow_child_is_centered;
    t "expand and stretch together fill a pane" test_expand_and_stretch_together;
    t "padding takes room off all four sides" test_pad;
    t "constraints clamp what a child asks for" test_constraints_clamp;
    t "a row inside a column" test_nested;
  ]
