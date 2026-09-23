(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_juice.mli *)

open Playground

let near = Alcotest.float 1e-6

(* a polygon's corners' extent: (min x, max x, min y, max y) *)
let extent (s : shape) : float * float * float * float =
  match s.form with
  | Polygon (_, points) ->
      List.fold_left
        (fun (x0, x1, y0, y1) (x, y) -> (Float.min x0 x, Float.max x1 x, Float.min y0 y, Float.max y1 y))
        (infinity, neg_infinity, infinity, neg_infinity)
        points
  | _ -> Alcotest.fail "not a polygon"

let check_extent (what : string) (expected : float * float * float * float) (s : shape) : unit =
  let x0, x1, y0, y1 = extent s and ex0, ex1, ey0, ey1 = expected in
  List.iter2 (fun e v -> Alcotest.check near what e v) [ ex0; ex1; ey0; ey1 ] [ x0; x1; y0; y1 ]

let children (s : shape) : shape list = match s.form with Group shapes -> shapes | _ -> Alcotest.fail "not a group"

let computer_at (t : float) (flags : flags) : computer = { initial_computer with time = Time t; flags }

let tests =
  Testo.categorize "Juice"
    [
      Testo.create "a ball squashed against the ground: an oval, its bottom still at 0" (fun () ->
          let s = circle red 20. |> move_up 20. |> Juice.stretch (1. /. 0.6, 0.6) in
          (match s.form with
          | Oval (_, w, h) ->
              Alcotest.check near "66.7 wide" (40. /. 0.6) w;
              Alcotest.check near "24 tall" 24. h
          | _ -> Alcotest.fail "not an oval");
          Alcotest.check near "its center down to 12" 12. s.y;
          Alcotest.check near "its bottom at 0" 0. (s.y -. 12.));
      Testo.create "a rotated rectangle stretched: the polygon it becomes" (fun () ->
          (* 20 x 10, turned upright (10 x 20), then twice as wide: 20 x 20 *)
          check_extent "20 x 20" (-10., 10., -10., 10.) (rectangle red 20. 10. |> rotate 90. |> Juice.stretch (2., 1.)));
      Testo.create "inside a group: positions stretched, and a rotated group's children made polygons" (fun () ->
          (match children (group [ circle red 5. |> move 10. 0. ] |> Juice.stretch (2., 1.)) with
          | [ c ] ->
              Alcotest.check near "moved out to 20" 20. c.x;
              (match c.form with Oval (_, w, h) -> Alcotest.check near "20 wide" 20. w; Alcotest.check near "10 tall" 10. h | _ -> Alcotest.fail "not an oval")
          | _ -> Alcotest.fail "one child");
          match children (group [ rectangle red 20. 10. ] |> rotate 90. |> Juice.stretch (2., 1.)) with
          | [ c ] -> check_extent "the same 20 x 20" (-10., 10., -10., 10.) c
          | _ -> Alcotest.fail "one child");
      Testo.create "words are only scaled evenly" (fun () ->
          let s = words black "hi" |> Juice.stretch (2., 8.) in
          Alcotest.check near "by the mean, 4" 4. s.scale);
      Testo.create "whiten: every color through groups, not images" (fun () ->
          let s = group [ circle red 5.; group [ rectangle blue 1. 1. ]; image 10. 10. "a.png" ] |> Juice.whiten in
          match children s with
          | [ { form = Circle (c, _); _ }; inner; { form = Image _; _ } ] ->
              Alcotest.(check bool) "the circle" true (c = white);
              (match children inner with [ { form = Rectangle (c, _, _); _ } ] -> Alcotest.(check bool) "the rectangle" true (c = white) | _ -> Alcotest.fail "the inner group")
          | _ -> Alcotest.fail "the group");
      Testo.create "squash: flat at landing, round after, nothing with juice=off" (fun () ->
          let landed = Time 100. in
          let sx, sy = Juice.squash 0.4 0.5 landed (computer_at 100. []) in
          Alcotest.check near "wider" (1. /. 0.6) sx;
          Alcotest.check near "flatter" 0.6 sy;
          let sx, sy = Juice.squash 0.4 0.5 landed (computer_at 101. []) in
          Alcotest.check near "round again" 1. (sx *. sy);
          Alcotest.check near "and upright" 1. sy;
          let sx, sy = Juice.squash 0.4 0.5 landed (computer_at 100. [ ("juice", "off") ]) in
          Alcotest.check near "off: as it is" 1. sx;
          Alcotest.check near "off: as it is" 1. sy);
    ]
