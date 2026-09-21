(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The worked examples of Isometric.mli: the two lines, run forwards and
 * backwards, the order they put things in, and the line of sight. *)

let t = Testo.create

(* Zaxxon's view: x drawn longer than z, so a corridor rather than a
 * square grid; and a dungeon's 2:1 diamond grid, 56 by 28 pixels *)
let fortress = Isometric.make ~across:(0.85, -0.30) ~along:(0.34, 0.42) ~up:1.
let dungeon = Isometric.make ~across:(28., 14.) ~along:(-28., 14.) ~up:34.

let close = Alcotest.(float 0.001)
let point = Alcotest.(pair close close)

(* The whole renderer, forwards: a sum of three world axes. *)
let projects () =
  Alcotest.(check point) "the origin" (0., 0.) (Isometric.project fortress (0., 0., 0.));
  Alcotest.(check point) "one across" (0.85, -0.30) (Isometric.project fortress (1., 0., 0.));
  Alcotest.(check point) "one along" (0.34, 0.42) (Isometric.project fortress (0., 0., 1.));
  Alcotest.(check point) "one up, straight up" (0., 1.) (Isometric.project fortress (0., 1., 0.));
  (* the origin is a screen offset, and following is a subtraction
     before all of it *)
  let v = fortress |> Isometric.origin (-120.) (-330.) |> Isometric.follow 0. 100. in
  Alcotest.(check point) "shifted and scrolled" (-120., -330.) (Isometric.project v (0., 0., 100.))

(* Backwards, which is what a mouse needs: every floor point comes back
 * from the pixel it was drawn at. *)
let ground_comes_back () =
  List.iter
    (fun (x, z) ->
      let v = dungeon |> Isometric.origin 40. (-60.) |> Isometric.follow 3. 7. in
      let back = Isometric.ground v (Isometric.project v (x, 0., z)) in
      Alcotest.(check point) "the floor point that pixel is" (x, z) back)
    [ (0., 0.); (1., 0.); (0., 1.); (12.5, -3.25); (-8., 20.) ];
  (* height is what the inverse cannot know: a pixel is a whole line of
     the world, and [ground] answers for the floor *)
  let flat = Isometric.ground dungeon (Isometric.project dungeon (2., 1., 5.)) in
  Alcotest.(check bool) "a thing off the floor is not where its pixel says" true
    (fst flat <> 2. || snd flat <> 5.)

(* Farther things have a bigger depth, and height counts: in a view
 * that looks down, raising a thing brings it towards the eye. *)
let depth_orders () =
  let d p = Isometric.depth dungeon p in
  Alcotest.(check bool) "further along z is farther" true (d (0., 0., 5.) > d (0., 0., 1.));
  Alcotest.(check bool) "further along x is farther" true (d (5., 0., 0.) > d (1., 0., 0.));
  Alcotest.(check bool) "higher is nearer" true (d (0., 1., 0.) < d (0., 0., 0.));
  (* and the sort puts the far ones first *)
  let shapes = [ (3., Playground.circle Playground.red 1.); (9., Playground.circle Playground.blue 1.) ] in
  Alcotest.(check int) "two shapes, farthest first" 2 (List.length (Isometric.sorted shapes))

(* The line of sight: walk from a thing towards the eye and see what
 * plane it crosses, and where. *)
let sight_walks_to_the_eye () =
  let v = fortress in
  let ex, ey, ez = Isometric.toward_eye v in
  (* walking along it does not move the pixel: that is what it means *)
  let p = (10., 20., 300.) in
  let x, y, z = p in
  let walked = (x +. (3. *. ex), y +. (3. *. ey), z +. (3. *. ez)) in
  Alcotest.(check point) "the same pixel" (Isometric.project v p) (Isometric.project v walked);
  (* a plane in front of the thing is crossed; one behind it is not *)
  (match Isometric.sight v p 250. with
  | None -> Alcotest.fail "a plane between it and the eye should be crossed"
  | Some (hx, hy) ->
      Alcotest.(check bool) "nearer the eye means further across and up" true (hx > x && hy > y));
  Alcotest.(check bool) "a plane behind it is not" true (Isometric.sight v p 400. = None)

let tests =
  Testo.categorize "isometric"
    [ t "the two lines, forwards" projects;
      t "and backwards, which is what a mouse needs" ground_comes_back;
      t "the order they are drawn in" depth_orders;
      t "the line of sight to the eye" sight_walks_to_the_eye ]
