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

(* The pixels of a framebuffer as a picture, '#' for [rgb], '.' for
 * anything else, one string per row: failures then show the shape *)
let picture (fb : Framebuffer.t) ~rgb : string list =
  List.init fb.height (fun y ->
      String.init fb.width (fun x -> if Framebuffer.get_rgb fb ~x ~y = rgb then '#' else '.'))

let check_picture msg expected fb = Alcotest.(check (list string)) msg expected (picture fb ~rgb:blue)

let fill ?rule ~width ~height points =
  let fb = Framebuffer.create ~width ~height in
  Fill.polygon ?rule fb points ~rgb:blue ~alpha:1.;
  fb

(* the example in Fill.mli: centers inside the square are filled *)
let test_square () =
  check_picture "square (1, 1)-(4, 3)"
    [ "......"; ".###.."; ".###.."; "......" ]
    (fill ~width:6 ~height:4 [ (1., 1.); (4., 1.); (4., 3.); (1., 3.) ])

(* not on pixel boundaries: from x = 1.2 to 3.9, the centers 1.5, 2.5,
 * 3.5 are inside; from y = 0.6 to 2.4, only the center 1.5 is *)
let test_fractional_square () =
  check_picture "square (1.2, 0.6)-(3.9, 2.4)"
    [ "......"; ".###.."; "......" ]
    (fill ~width:6 ~height:3 [ (1.2, 0.6); (3.9, 0.6); (3.9, 2.4); (1.2, 2.4) ])

(* the concave "U" of Fill.mli: two spans on its upper rows *)
let test_u () =
  check_picture "U"
    [
      "..........";
      ".##....##.";
      ".##....##.";
      ".########.";
      "..........";
    ]
    (fill ~width:10 ~height:5
       [ (1., 1.); (3., 1.); (3., 3.); (7., 3.); (7., 1.); (9., 1.); (9., 4.); (1., 4.) ])

(* the pentagram of Fill.mli: the center is inside for Nonzero (winding
 * number 2) but not for Even_odd; a tip is inside for both *)
let test_star () =
  let star =
    List.init 5 (fun i ->
        let a = (Float.pi /. 2.) +. (4. *. Float.pi *. float i /. 5.) in
        (50. +. (40. *. cos a), 50. -. (40. *. sin a)))
  in
  let nonzero = fill ~rule:Nonzero ~width:100 ~height:100 star in
  let even_odd = fill ~rule:Even_odd ~width:100 ~height:100 star in
  let filled fb (x, y) = Framebuffer.get_rgb fb ~x ~y = blue in
  Alcotest.(check bool) "nonzero: center filled" true (filled nonzero (50, 50));
  Alcotest.(check bool) "even-odd: center empty" false (filled even_odd (50, 50));
  Alcotest.(check bool) "nonzero: top tip filled" true (filled nonzero (50, 15));
  Alcotest.(check bool) "even-odd: top tip filled" true (filled even_odd (50, 15))

(* The property the pixel-center rule is for: a quadrilateral cut in two
 * triangles along a diagonal. Drawn half-transparent, a pixel painted
 * by both triangles would be darker, and a pixel painted by neither
 * would stay white inside the quadrilateral. *)
let test_shared_edge () =
  let a = (10.3, 10.1) and b = (90.7, 20.2) and c = (80.1, 90.9) and d = (15.5, 70.4) in
  let halves = Framebuffer.create ~width:100 ~height:100 in
  Fill.polygon halves [ a; b; c ] ~rgb:blue ~alpha:0.5;
  Fill.polygon halves [ a; c; d ] ~rgb:blue ~alpha:0.5;
  let whole = Framebuffer.create ~width:100 ~height:100 in
  Fill.polygon whole [ a; b; c; d ] ~rgb:blue ~alpha:0.5;
  let differences = ref 0 in
  for y = 0 to 99 do
    for x = 0 to 99 do
      if Framebuffer.get_rgb halves ~x ~y <> Framebuffer.get_rgb whole ~x ~y then incr differences
    done
  done;
  Alcotest.(check int) "two halves = the whole, pixel for pixel" 0 !differences

(* Same polygons with Cairo, antialiasing off: Cairo (via pixman) also
 * fills the pixels whose center is inside, with the nonzero rule, so
 * the pictures should be identical -- almost: Cairo first rounds all
 * coordinates to multiples of 1/256 pixel (pixman's "24.8" fixed-point
 * numbers), so a pixel whose center is a few thousandths of a pixel
 * from an edge can land on the other side of it. We compute in floating
 * point, exactly enough to get those right. E.g. in the "rotated
 * square" below, the left edge crosses the row of pixel (20, 60) at
 * x = 20.502: the pixel's center 20.5 is outside, by 0.002 pixel, less
 * than 1/256 = 0.0039; Cairo fills it. *)
let cairo_fill ~width ~height points : Framebuffer.t =
  let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:width ~h:height in
  let cr = Cairo.create surface in
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.paint cr;
  Cairo.set_antialias cr Cairo.ANTIALIAS_NONE;
  Cairo.set_fill_rule cr Cairo.WINDING;
  Cairo.set_source_rgb cr 0. 0. 1.;
  (match points with
  | [] -> ()
  | (x, y) :: rest ->
      Cairo.move_to cr x y;
      List.iter (fun (x, y) -> Cairo.line_to cr x y) rest;
      Cairo.Path.close cr);
  Cairo.fill cr;
  Cairo.Surface.flush surface;
  let data = Cairo.Image.get_data32 surface in
  let fb = Framebuffer.create ~width ~height in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      fb.pixels.{y, x} <- Int32.logor data.{y, x} 0xFF000000l
    done
  done;
  fb

(* Distance from point p to the segment [a, b] *)
let distance_to_segment (px, py) ((ax, ay), (bx, by)) =
  let dx = bx -. ax and dy = by -. ay in
  let t = (((px -. ax) *. dx) +. ((py -. ay) *. dy)) /. ((dx *. dx) +. (dy *. dy)) in
  let t = Float.min 1. (Float.max 0. t) in
  Float.hypot (px -. (ax +. (t *. dx))) (py -. (ay +. (t *. dy)))

let edges points = List.combine points (List.tl points @ [ List.hd points ])

let test_same_as_cairo () =
  let polygons =
    [
      ("rotated square", [ (50.3, 10.2); (89.7, 49.6); (50.1, 90.4); (10.9, 50.8) ]);
      ("thin triangle", [ (5.5, 5.5); (95.2, 20.7); (6.1, 9.3) ]);
      ( "concave arrow",
        [ (10., 40.); (60.3, 40.); (60.3, 15.7); (95.1, 50.); (60.3, 84.2); (60.3, 60.); (10., 60.) ] );
      ( "pentagram",
        List.init 5 (fun i ->
            let a = (Float.pi /. 2.) +. (4. *. Float.pi *. float i /. 5.) in
            (50.2 +. (40. *. cos a), 49.7 -. (40. *. sin a))) );
    ]
  in
  polygons
  |> List.iter (fun (name, points) ->
         let ours = fill ~width:100 ~height:100 points in
         let cairo = cairo_fill ~width:100 ~height:100 points in
         for y = 0 to 99 do
           for x = 0 to 99 do
             if Framebuffer.get_rgb ours ~x ~y <> Framebuffer.get_rgb cairo ~x ~y then begin
               let center = (float x +. 0.5, float y +. 0.5) in
               let distance =
                 List.fold_left min infinity (List.map (distance_to_segment center) (edges points))
               in
               if distance > 0.01 then
                 Alcotest.failf "%s: pixel (%d, %d) differs from Cairo, %.4f pixel from the closest edge"
                   name x y distance
             end
           done
         done)

let tests =
  Testo.categorize "Fill"
    [
      t "square" test_square;
      t "fractional square" test_fractional_square;
      t "concave U" test_u;
      t "star: nonzero vs even-odd" test_star;
      t "shared edge: no gap, no overlap" test_shared_edge;
      t "same pixels as Cairo without antialiasing" test_same_as_cairo;
    ]
