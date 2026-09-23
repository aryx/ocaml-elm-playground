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

(* the example in Circle.mli *)
let test_octant () =
  Alcotest.(check (list (pair int int))) "r = 5" [ (0, 5); (1, 5); (2, 5); (3, 4) ] (Circle.octant 5)

let test_fill () =
  let fb = Framebuffer.create ~width:11 ~height:11 in
  Circle.fill fb ~cx:5 ~cy:5 ~r:5 ~rgb:blue ~alpha:1.;
  (* half widths 5, 5, 5, 4, 3, 2 from the middle row out, see
   * Circle.half_widths *)
  Alcotest.(check (list string)) "r = 5"
    [
      "...#####...";
      "..#######..";
      ".#########.";
      "###########";
      "###########";
      "###########";
      "###########";
      "###########";
      ".#########.";
      "..#######..";
      "...#####...";
    ]
    (picture fb)

let test_outline () =
  let fb = Framebuffer.create ~width:11 ~height:11 in
  Circle.outline fb ~cx:5 ~cy:5 ~r:5 ~rgb:blue ~alpha:1.;
  Alcotest.(check (list string)) "r = 5"
    [
      "...#####...";
      "..#.....#..";
      ".#.......#.";
      "#.........#";
      "#.........#";
      "#.........#";
      "#.........#";
      "#.........#";
      ".#.......#.";
      "..#.....#..";
      "...#####...";
    ]
    (picture fb)

(* each row painted once: with alpha 0.5, no pixel may be darker *)
let test_fill_once () =
  let fb = Framebuffer.create ~width:41 ~height:41 in
  Circle.fill fb ~cx:20 ~cy:20 ~r:20 ~rgb:blue ~alpha:0.5;
  for y = 0 to 40 do
    for x = 0 to 40 do
      let c = Framebuffer.get_rgb fb ~x ~y in
      if c <> 0xffffff && c <> 0x8080ff then Alcotest.failf "pixel (%d, %d) is 0x%06x" x y c
    done
  done

(* the examples in Circle.mli *)
let test_segments () =
  Alcotest.(check (list int)) "radius 10, 100, 400" [ 15; 45; 89 ]
    (List.map Circle.segments_for_radius [ 10.; 100.; 400. ])

(* Cairo, antialiasing off, fills the pixels whose center is inside the
 * true circle; the midpoint circle's pixels are the ones closest to
 * it. They can only disagree on pixels whose center is within about
 * half a pixel of the circle. *)
let test_close_to_cairo () =
  let size = 101 and r = 40 in
  let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:size ~h:size in
  let cr = Cairo.create surface in
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.paint cr;
  Cairo.set_antialias cr Cairo.ANTIALIAS_NONE;
  Cairo.set_source_rgb cr 0. 0. 1.;
  (* our pixel (50, 50) is the square from 50 to 51: its center 50.5 *)
  Cairo.arc cr 50.5 50.5 ~r:(float r) ~a1:0. ~a2:(2. *. Float.pi);
  Cairo.fill cr;
  Cairo.Surface.flush surface;
  let cairo = Cairo.Image.get_data32 surface in
  let fb = Framebuffer.create ~width:size ~height:size in
  Circle.fill fb ~cx:50 ~cy:50 ~r ~rgb:blue ~alpha:1.;
  for y = 0 to size - 1 do
    for x = 0 to size - 1 do
      let ours = Framebuffer.get_rgb fb ~x ~y = blue in
      let theirs = Int32.to_int cairo.{y, x} land 0xffffff = blue in
      if ours <> theirs then begin
        let distance = Float.abs (Float.hypot (float (x - 50)) (float (y - 50)) -. float r) in
        if distance > 0.6 then
          Alcotest.failf "pixel (%d, %d) differs from Cairo, %.2f pixel from the circle" x y distance
      end
    done
  done

let tests =
  Testo.categorize "Circle"
    [
      t "octant" test_octant;
      t "fill" test_fill;
      t "outline" test_outline;
      t "fill paints each pixel once" test_fill_once;
      t "segments for a radius" test_segments;
      t "close to Cairo without antialiasing" test_close_to_cairo;
    ]
