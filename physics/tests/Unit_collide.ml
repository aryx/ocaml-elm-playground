(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/2d/Shape and Collide: the worked examples, and SAT against
 * the general test on random convex polygons *)

let t = Testo.create
let vec = Alcotest.(pair (float 1e-9) (float 1e-9))

let test_shape () =
  Alcotest.(check (float 1e-9)) "a box's area" 8. (Shape.area (Shape.Box (4., 2.)));
  Alcotest.(check (float 1e-9)) "the same as a polygon (shoelace)" 8.
    (Shape.area (Shape.Polygon [ (0., 0.); (4., 0.); (4., 2.); (0., 2.) ]));
  Alcotest.(check bool) "a square is convex" true (Shape.convex (Shape.box_corners 2. 2.));
  Alcotest.(check bool) "an arrowhead isn't" false (Shape.convex [ (0., 0.); (4., 2.); (0., 4.); (1., 2.) ]);
  (* the moments of inertia per unit of mass, J / A: r^2 / 2 for a
   * disk, (w^2 + h^2) / 12 for a box, and a circle moved away by 3
   * gains 3^2 (the parallel axis theorem) *)
  let per_mass p = let (a, j) = Shape.moments p in j /. a in
  Alcotest.(check (float 1e-9)) "a disk" 2. (per_mass (Shape.Circle_at ((0., 0.), 2.)));
  Alcotest.(check (float 1e-9)) "a 4 x 2 box" (5. /. 3.) (per_mass (Shape.place (0., 0.) (Shape.Box (4., 2.))));
  Alcotest.(check (float 1e-9)) "the same box, clockwise" (5. /. 3.) (per_mass (Shape.Polygon_at (List.rev (Shape.box_corners 4. 2.))));
  Alcotest.(check (float 1e-9)) "a disk 3 away" 11. (per_mass (Shape.Circle_at ((3., 0.), 2.)));
  (match Shape.place ~angle:(Float.pi /. 2.) (10., 0.) (Shape.Box (4., 2.)) with
  | Shape.Polygon_at (c :: _) -> Alcotest.check vec "a box turned a quarter, moved: its first corner" (11., -2.) c
  | _ -> Alcotest.fail "a box is placed as a polygon")

(* Collide.mli's examples *)
let test_circles () =
  Alcotest.(check bool) "radii 20 and 20, 50 apart: no" true (Collide.circles ((0., 0.), 20.) ((30., 40.), 20.) = None);
  match Collide.circles ((0., 0.), 30.) ((30., 40.), 25.) with
  | None -> Alcotest.fail "radii 30 and 25: overlapping"
  | Some c ->
      Alcotest.(check (float 1e-9)) "depth" 5. c.depth;
      Alcotest.check vec "normal" (0.6, 0.8) c.normal

let square x y s = [ (x, y); (x +. s, y); (x +. s, y +. s); (x, y +. s) ]

let test_sat () =
  let a = [ (0., 0.); (2., 0.); (2., 2.); (0., 2.) ] and b = [ (1., 3.); (3., 3.); (3., 4.); (1., 4.) ] in
  Alcotest.(check bool) "the example: apart" true (Collide.sat a b = None);
  match Collide.sat (square 0. 0. 2.) (square 1.5 0.5 2.) with
  | None -> Alcotest.fail "overlapping squares"
  | Some c ->
      Alcotest.(check (float 1e-9)) "depth 0.5, along x" 0.5 c.depth;
      Alcotest.check vec "normal: from the first to the second" (1., 0.) c.normal;
      (* the overlap is [1.5, 2] x [0.5, 2] *)
      Alcotest.check vec "point: the middle of the overlap" (1.75, 1.25) c.point

(* a U: concave; a point in its notch is outside *)
let u = [ (0., 0.); (3., 0.); (3., 3.); (2., 3.); (2., 1.); (1., 1.); (1., 3.); (0., 3.) ]

let test_point_in_polygon () =
  Alcotest.(check bool) "in the U's bottom" true (Collide.point_in_polygon (1.5, 0.5) u);
  Alcotest.(check bool) "in the U's notch: outside" false (Collide.point_in_polygon (1.5, 2.) u);
  Alcotest.(check bool) "in its left arm" true (Collide.point_in_polygon (0.5, 2.5) u);
  Alcotest.(check bool) "right of it" false (Collide.point_in_polygon (4., 1.) u)

let test_segments () =
  Alcotest.(check bool) "an X" true (Collide.segments_cross ((0., 0.), (2., 2.)) ((0., 2.), (2., 0.)));
  Alcotest.(check bool) "parallel" false (Collide.segments_cross ((0., 0.), (2., 0.)) ((0., 1.), (2., 1.)));
  Alcotest.(check bool) "a T: an end on the other" true (Collide.segments_cross ((0., 0.), (2., 0.)) ((1., 0.), (1., 2.)));
  Alcotest.(check bool) "on one line, apart" false (Collide.segments_cross ((0., 0.), (1., 0.)) ((2., 0.), (3., 0.)));
  Alcotest.(check bool) "on one line, overlapping" true (Collide.segments_cross ((0., 0.), (2., 0.)) ((1., 0.), (3., 0.)))

let test_circle_polygon () =
  Alcotest.(check bool) "a circle in the U's notch, small: no" false (Collide.circle_polygon ((1.5, 2.), 0.4) u);
  Alcotest.(check bool) "bigger: touching the arms" true (Collide.circle_polygon ((1.5, 2.), 0.6) u);
  match Collide.circle_convex ((0., 1.), 0.5) (square 0.2 0. 2.) with
  | None -> Alcotest.fail "overlapping by 0.3"
  | Some c ->
      Alcotest.(check (float 1e-9)) "depth" 0.3 c.depth;
      Alcotest.check vec "normal: from the circle to the square" (1., 0.) c.normal

(* random convex polygons: corners at sorted random angles on a circle *)
let random_convex (st : Random.State.t) : Vec2.t list =
  let cx = Random.State.float st 100. -. 50. and cy = Random.State.float st 100. -. 50. in
  let r = 10. +. Random.State.float st 30. and n = 3 + Random.State.int st 6 in
  List.init n (fun _ -> Random.State.float st (2. *. Float.pi))
  |> List.sort compare
  |> List.map (fun a -> (cx +. (r *. cos a), cy +. (r *. sin a)))

let test_sat_vs_general () =
  let st = Random.State.make [| 42 |] in
  let agree = ref 0 and touching = ref 0 in
  for _ = 1 to 2000 do
    let p = random_convex st and q = random_convex st in
    let general = Collide.polygons_touch p q and sat = Collide.sat p q <> None in
    if general = sat then incr agree;
    if general then incr touching
  done;
  Alcotest.(check int) "SAT agrees with edges-and-insides on 2000 pairs" 2000 !agree;
  (* and the pairs were a mix, not all apart *)
  if !touching < 300 || !touching > 1700 then Alcotest.failf "an unbalanced sample: %d touching" !touching

let test_circle_convex_vs_general () =
  let st = Random.State.make [| 7 |] in
  for _ = 1 to 2000 do
    let p = random_convex st in
    let c = (Random.State.float st 100. -. 50., Random.State.float st 100. -. 50.) and r = 1. +. Random.State.float st 20. in
    if Collide.circle_polygon (c, r) p <> (Collide.circle_convex (c, r) p <> None) then
      Alcotest.failf "circle (%g, %g) %g" (fst c) (snd c) r
  done

let tests =
  Testo.categorize "Collide"
    [
      t "shapes: areas, convexity, placing" test_shape;
      t "circles, the worked example" test_circles;
      t "SAT, the worked example" test_sat;
      t "point in a concave polygon" test_point_in_polygon;
      t "segments" test_segments;
      t "circle and polygon" test_circle_polygon;
      t "SAT = the general test, 2000 random pairs" test_sat_vs_general;
      t "circle_convex = circle_polygon, 2000 random pairs" test_circle_convex_vs_general;
    ]
