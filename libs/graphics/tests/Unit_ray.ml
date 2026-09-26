(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/geometry/Ray: its worked examples, and each intersection
 * against the answer computed by hand. physics/tests' Unit_collide3d
 * checks the first-hit questions Collide3d asks on top. *)

let t = Testo.create
let near msg expected actual = Alcotest.(check (float 1e-9)) msg expected actual

let pair msg (e1, e2) = function
  | None -> Alcotest.fail (msg ^ ": a miss")
  | Some (a1, a2) ->
      near (msg ^ ", in") e1 a1;
      near (msg ^ ", out") e2 a2

let down_z = Ray.make (0., 0., 5.) (0., 0., -1.)

let test_make () =
  let ray = Ray.make (1., 2., 3.) (0., 0., -10.) in
  Alcotest.(check (triple (float 0.) (float 0.) (float 0.))) "the direction, normalized" (0., 0., -1.) ray.direction;
  Alcotest.(check (triple (float 0.) (float 0.) (float 0.))) "3 along it" (1., 2., 0.) (Ray.at ray 3.);
  Alcotest.check_raises "a zero direction is no ray" (Invalid_argument "Ray.make: a zero direction") (fun () ->
      ignore (Ray.make (0., 0., 0.) (0., 0., 0.)))

let test_sphere () =
  let unit = ((0., 0., 0.), 1.) in
  pair "the .mli's example: in at 4, out at 6" (4., 6.) (Ray.sphere down_z unit);
  (* the ICFP entry's bug: a direction ten times too long would have
   * found the sphere at 0.4 -- make normalizes it away *)
  pair "the same, aimed with (0, 0, -10)" (4., 6.) (Ray.sphere (Ray.make (0., 0., 5.) (0., 0., -10.)) unit);
  pair "from inside: in behind, out in front" (-1., 1.) (Ray.sphere (Ray.make (0., 0., 0.) (0., 0., -1.)) unit);
  pair "pointing away: both behind" (-6., -4.) (Ray.sphere (Ray.make (0., 0., 5.) (0., 0., 1.)) unit);
  pair "grazing: in and out at once" (5., 5.) (Ray.sphere (Ray.make (1., 0., 5.) (0., 0., -1.)) unit);
  Alcotest.(check bool) "beside it: a miss" true (Ray.sphere (Ray.make (1.5, 0., 5.) (0., 0., -1.)) unit = None)

(* both points are on the sphere, for rays from all around it *)
let test_sphere_points () =
  let c = (1., -2., 0.5) and r = 1.5 in
  for i = 0 to 35 do
    let a = float_of_int i *. Float.pi /. 18. in
    let origin = (1. +. (6. *. cos a), -2. +. (6. *. sin a), 3.) in
    (* aimed near the centre, not at it *)
    let ray = Ray.make origin (Vec3.sub (1.2, -1.9, 0.5) origin) in
    match Ray.sphere ray (c, r) with
    | None -> Alcotest.fail "aimed through the sphere, it must hit"
    | Some (t_in, t_out) ->
        Alcotest.(check (float 1e-9)) "entering, on the sphere" r (Vec3.length (Vec3.sub (Ray.at ray t_in) c));
        Alcotest.(check (float 1e-9)) "leaving, on the sphere" r (Vec3.length (Vec3.sub (Ray.at ray t_out) c));
        Alcotest.(check bool) "in before out" true (t_in < t_out)
  done

let test_plane () =
  let floor = ((0., 1., 0.), -4.) in
  let down = Ray.make (0., 0., 0.) (0., -1., 0.) in
  near "the .mli's example: the floor y = -4, 4 below" 4. (Option.get (Ray.plane down floor));
  near "looking up: behind, -4" (-4.) (Option.get (Ray.plane (Ray.make (0., 0., 0.) (0., 1., 0.)) floor));
  near "a normal not of length 1: the same plane" 4. (Option.get (Ray.plane down ((0., 2., 0.), -8.)));
  near "at 45 degrees: sqrt 2 times further" (4. *. sqrt 2.) (Option.get (Ray.plane (Ray.make (0., 0., 0.) (1., -1., 0.)) floor));
  Alcotest.(check bool) "parallel: never" true (Ray.plane (Ray.make (0., 0., 0.) (1., 0., 0.)) floor = None)

let test_triangle () =
  let tri = ((-1., -1., 0.), (1., -1., 0.), (0., 1., 0.)) in
  (match Ray.triangle down_z tri with
  | None -> Alcotest.fail "through the middle: a hit"
  | Some (t, u, v) ->
      near "at 5" 5. t;
      (* (0, 0) = (1 - u - v) a + u b + v c: u = 1/4, v = 1/2 *)
      near "u, b's weight" 0.25 u;
      near "v, c's weight" 0.5 v);
  (match Ray.triangle (Ray.make (0., 0., -5.) (0., 0., 1.)) tri with
  | Some (t, _, _) -> near "from behind it: a hit all the same" 5. t
  | None -> Alcotest.fail "the back of a triangle is hit too");
  (match Ray.triangle (Ray.make (0., 0., -5.) (0., 0., -1.)) tri with
  | Some (t, _, _) -> near "going away: the line crosses it behind, -5" (-5.) t
  | None -> Alcotest.fail "the whole line, not only the front");
  Alcotest.(check bool) "outside its edges" true (Ray.triangle (Ray.make (0.9, 0.9, 5.) (0., 0., -1.)) tri = None);
  Alcotest.(check bool) "in its plane" true (Ray.triangle (Ray.make (0., 0., 0.) (1., 0., 0.)) tri = None);
  (* a corner: u = v = 0 is a *)
  match Ray.triangle (Ray.make (-1., -1., 5.) (0., 0., -1.)) tri with
  | Some (_, u, v) ->
      near "at corner a, u = 0" 0. u;
      near "and v = 0" 0. v
  | None -> Alcotest.fail "a corner belongs to the triangle"

let test_box () =
  let cube = ((-1., -1., -1.), (1., 1., 1.)) in
  pair "straight in: 4 to 6" (4., 6.) (Ray.box down_z cube);
  pair "from inside" (-1., 1.) (Ray.box (Ray.make (0., 0., 0.) (0., 0., -1.)) cube);
  pair "along a diagonal: corner to corner" (-.sqrt 3., sqrt 3.) (Ray.box (Ray.make (0., 0., 0.) (1., 1., 1.)) cube);
  (* the slabs overlap nowhere: in x the line is inside for t in
   * [0, 2], in y for [3, 5] *)
  Alcotest.(check bool) "the slabs apart: a miss" true (Ray.box (Ray.make (-1., -4., 0.) (1., 1., 0.)) cube = None);
  Alcotest.(check bool) "parallel to a slab, outside it" true (Ray.box (Ray.make (2., 0., 5.) (0., 0., -1.)) cube = None);
  pair "parallel to two slabs, inside them" (4., 6.) (Ray.box (Ray.make (0.5, -0.5, 5.) (0., 0., -1.)) cube)

let tests =
  Testo.categorize "Ray"
    [
      t "make: the direction normalized" test_make;
      t "sphere: both roots" test_sphere;
      t "sphere: both points on it" test_sphere_points;
      t "plane" test_plane;
      t "triangle: Moller-Trumbore, and u, v" test_triangle;
      t "box: the slabs" test_box;
    ]
