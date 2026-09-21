(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gui/Grid: Tk's geometry manager, checked against rectangles
 * computed by hand -- and above all against the one thing a column of
 * rows cannot do, which is make two rows agree where their second
 * column starts. *)

let t = Testo.create
let area : Widget.box = { Widget.x = 0.; y = 0.; w = 400.; h = 400. }

let check_box name (x, y, w, h) (b : Widget.box) =
  Alcotest.(check (list (float 1e-9))) name [ x; y; w; h ] [ b.x; b.y; b.w; b.h ]

(* the .mli's worked example: a two-row form whose labels are of
   different widths, and whose fields must still line up *)
let form () =
  Grid.make ~gap:10.
    [
      Grid.item ~row:0 ~col:0 `Name (60., 36.);
      Grid.item ~row:0 ~col:1 `Name_field (120., 36.);
      Grid.item ~row:1 ~col:0 `Address (90., 36.);
      Grid.item ~row:1 ~col:1 `Address_field (120., 36.);
    ]

let test_columns_line_up () =
  let g = form () in
  Alcotest.(check (pair (float 1e-9) (float 1e-9)))
    "90 + 10 + 120 wide, two rows and a gap tall" (220., 82.) (Grid.measure g);
  let places = Grid.arrange area g in
  let name : Widget.box = List.assoc `Name_field places in
  let address : Widget.box = List.assoc `Address_field places in
  Alcotest.(check (float 1e-9)) "both fields start at the same x" (Widget.left name)
    (Widget.left address);
  (* and the labels sit in a column as wide as the wider of them,
     centered in it since nothing sticks them anywhere *)
  check_box "the short label, centered in its column" (-65., 23., 60., 36.) (List.assoc `Name places);
  check_box "the long one, which set the width" (-65., -23., 90., 36.) (List.assoc `Address places)

let test_sticky_fills_or_anchors () =
  let g =
    Grid.make ~gap:10.
      [
        Grid.item ~row:0 ~col:0 `Label (60., 36.);
        Grid.item ~row:0 ~col:1 ~sticky:"ew" `Filling (60., 36.);
        Grid.item ~row:1 ~col:1 ~sticky:"w" `Left (60., 36.);
        Grid.item ~row:1 ~col:0 `Wide (120., 36.);
      ]
  in
  let places = Grid.arrange area g in
  let filling : Widget.box = List.assoc `Filling places in
  Alcotest.(check (float 1e-9)) "ew fills its column" 60. filling.w;
  let left : Widget.box = List.assoc `Left places in
  Alcotest.(check (float 1e-9)) "w keeps its size" 60. left.w;
  Alcotest.(check (float 1e-9)) "and is pushed to the left edge of its cell"
    (Widget.left filling) (Widget.left left)

(* a cell that spans two columns and needs more room than they have
   grows the last one, which is what Tk does *)
let test_a_span_grows_the_last_column () =
  let g =
    Grid.make ~gap:10.
      [
        Grid.item ~row:0 ~col:0 ~colspan:2 `Title (300., 36.);
        Grid.item ~row:1 ~col:0 `A (60., 36.);
        Grid.item ~row:1 ~col:1 `B (60., 36.);
      ]
  in
  Alcotest.(check (list (float 1e-9))) "60, and the rest given to the second" [ 60.; 230. ]
    (Grid.columns g);
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "as wide as the title" (300., 82.)
    (Grid.measure g)

(* the room left over goes where the weights say: a form's fields
   grow and its labels do not *)
let test_weights_share_what_is_left () =
  let g =
    Grid.make ~gap:10. ~col_weights:[ (1, 1.) ]
      [ Grid.item ~row:0 ~col:0 `Label (60., 36.); Grid.item ~row:0 ~col:1 ~sticky:"ew" `Field (120., 36.) ]
  in
  let places = Grid.arrange { area with w = 300. } g in
  let field : Widget.box = List.assoc `Field places in
  (* natural 190; 110 spare, all of it to column 1 *)
  Alcotest.(check (float 1e-9)) "the field took the spare room" 230. field.w;
  let label : Widget.box = List.assoc `Label places in
  Alcotest.(check (float 1e-9)) "the label did not" 60. label.w

let test_with_no_weights_it_stays_its_own_size () =
  let g = form () in
  let places = Grid.arrange area g in
  let name : Widget.box = List.assoc `Name_field places in
  Alcotest.(check (float 1e-9)) "the field is the size it asked for" 120. name.w;
  (* and the table sits in the middle of the room it was given *)
  let address : Widget.box = List.assoc `Address places in
  Alcotest.(check (float 1e-9)) "centered: 220 wide in 400" (-110.) (Widget.left address)

let tests =
  [
    t "the columns of two rows line up" test_columns_line_up;
    t "sticky fills a cell or anchors in it" test_sticky_fills_or_anchors;
    t "a span grows the last column" test_a_span_grows_the_last_column;
    t "weights share what is left over" test_weights_share_what_is_left;
    t "with no weights it stays its own size" test_with_no_weights_it_stays_its_own_size;
  ]
