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
let pt = Alcotest.(pair (float 1e-9) (float 1e-9))

(* the worked examples in Curve.mli: a Bézier is pulled towards its
 * control points without reaching them, a quadratic half way up to
 * its own *)
let test_bezier () =
  Alcotest.check pt "the middle, pulled up" (50., 75.) (Curve.cubic (0., 0.) (0., 100.) (100., 100.) (100., 0.) 0.5);
  Alcotest.check pt "t = 0" (0., 0.) (Curve.cubic (0., 0.) (0., 100.) (100., 100.) (100., 0.) 0.);
  Alcotest.check pt "t = 1" (100., 0.) (Curve.cubic (0., 0.) (0., 100.) (100., 100.) (100., 0.) 1.);
  Alcotest.check pt "quadratic's middle" (50., 50.) (Curve.quadratic (0., 0.) (50., 100.) (100., 0.) 0.5)

(* catmull_rom's worked examples: in a line, the middle; around a
 * corner, bulging out to (112.5, 50) *)
let test_catmull_rom () =
  Alcotest.check pt "line" (150., 0.) (Curve.catmull_rom (0., 0.) (100., 0.) (200., 0.) (300., 0.) 0.5);
  Alcotest.check pt "corner" (112.5, 50.) (Curve.catmull_rom (0., 0.) (100., 0.) (100., 100.) (0., 100.) 0.5);
  Alcotest.check pt "t = 0" (100., 0.) (Curve.catmull_rom (0., 0.) (100., 0.) (100., 100.) (0., 100.) 0.)

(* the same curve, named twice: a Catmull-Rom segment and the Bézier
 * curve of cubic_of_catmull_rom's control points agree everywhere *)
let test_same_curve () =
  let p0, p1, p2, p3 = ((0., 0.), (100., 0.), (100., 100.), (0., 100.)) in
  let b0, b1, b2, b3 = Curve.cubic_of_catmull_rom p0 p1 p2 p3 in
  List.iter
    (fun i ->
      let t = float_of_int i /. 8. in
      Alcotest.check pt "same point" (Curve.catmull_rom p0 p1 p2 p3 t) (Curve.cubic b0 b1 b2 b3 t))
    (List.init 9 Fun.id)

(* how far [p] is from the segment [a] -> [b]: to its nearest point,
 * which is p projected on it, or an end when the projection falls
 * outside *)
let distance_to_segment ((ax, ay) : float * float) ((bx, by) : float * float) ((px, py) : float * float) : float =
  let dx = bx -. ax and dy = by -. ay in
  let d2 = (dx *. dx) +. (dy *. dy) in
  let t = if d2 = 0. then 0. else Float.max 0. (Float.min 1. ((((px -. ax) *. dx) +. ((py -. ay) *. dy)) /. d2)) in
  Float.hypot (px -. (ax +. (t *. dx))) (py -. (ay +. (t *. dy)))

(* flattening: a curve whose control points sit on its chord is already
 * straight, so two points do; a real curve is cut until it is within
 * the tolerance of the polyline that replaces it *)
let test_flatten () =
  Alcotest.(check int) "a straight curve" 2 (List.length (Curve.flatten (0., 0.) (30., 0.) (60., 0.) (90., 0.)));
  let curve = Curve.flatten ~tolerance:0.1 (0., 0.) (0., 100.) (100., 100.) (100., 0.) in
  let coarse = Curve.flatten ~tolerance:10. (0., 0.) (0., 100.) (100., 100.) (100., 0.) in
  Alcotest.(check bool) "a tolerance costs segments" true (List.length curve > List.length coarse);
  Alcotest.check pt "starts at p0" (0., 0.) (List.hd curve);
  Alcotest.check pt "ends at p3" (100., 0.) (List.nth curve (List.length curve - 1));
  (* the tolerance is a promise: no point of the curve is further than
   * that from the polyline drawn in its place. Checked by sampling the
   * curve finely and measuring, for each sample, its distance to the
   * nearest segment of the polyline: 0.073 of a pixel at a tolerance
   * of 0.1, 0.29 at 1 *)
  let sampled = List.init 2001 (fun i -> Curve.cubic (0., 0.) (0., 100.) (100., 100.) (100., 0.) (float_of_int i /. 2000.)) in
  let stray polyline =
    let rec segments = function a :: (b :: _ as rest) -> (a, b) :: segments rest | _ -> [] in
    let segs = segments polyline in
    List.fold_left (fun worst p -> Float.max worst (List.fold_left (fun best (a, b) -> Float.min best (distance_to_segment a b p)) infinity segs)) 0. sampled
  in
  Alcotest.(check bool) "within the tolerance" true (stray curve <= 0.1);
  Alcotest.(check bool) "within a looser one" true (stray coarse <= 10.)

(* moving by distance along a curve: a straight one, 300 long, its
 * middle at 150; a curved one, the same speed everywhere (a step of 5
 * pixels along it moves 5 pixels, give or take the chords' shortcut) *)
let test_walk () =
  let line : Curve.t = Curve.measure (Curve.through [ (0., 0.); (100., 0.); (200., 0.); (300., 0.) ]) in
  Alcotest.(check int) "16 points a segment, and the last" 49 (Array.length line.pts);
  Alcotest.(check (float 1e-6)) "length" 300. (Curve.length line);
  Alcotest.check pt "middle" (150., 0.) (fst (Curve.at line 150.));
  Alcotest.(check (float 1e-9)) "heading" 0. (snd (Curve.at line 150.));
  let c = Curve.measure (Curve.through [ (-560., -300.); (-300., -150.); (-100., 0.); (-100., 200.); (-250., 250.); (-350., 100.); (-200., 0.) ]) in
  let at s = fst (Curve.at c s) in
  List.iter
    (fun i ->
      let (x0, y0), (x1, y1) = (at (float_of_int i *. 5.), at (float_of_int (i + 1) *. 5.)) in
      Alcotest.(check (float 0.2)) "a step" 5. (Float.hypot (x1 -. x0) (y1 -. y0)))
    (List.init (int_of_float (Curve.length c /. 5.) - 1) Fun.id)

let tests =
  Testo.categorize "Curve"
    [
      t "Bézier" test_bezier;
      t "Catmull-Rom" test_catmull_rom;
      t "the same curve, named twice" test_same_curve;
      t "flattening" test_flatten;
      t "walking at a constant speed" test_walk;
    ]
