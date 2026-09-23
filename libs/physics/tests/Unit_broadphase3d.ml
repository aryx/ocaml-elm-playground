(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/3d/Broadphase3d: the three methods have to find the same
 * pairs -- that is the whole contract -- and the interesting part is
 * how many boxes each compares to get there *)

let t = Testo.create

let box_at (x, y, z) r : Broadphase3d.box = ((x -. r, y -. r, z -. r), (x +. r, y +. r, z +. r))

(* the scene of Broadphase3d.mli's table: [n] marbles piled in a box,
 * wide in x and z, thin in y *)
let pile n =
  Array.init n (fun i ->
      let r = 0.1 +. (float_of_int (i mod 7) *. 0.02) in
      let x = -4. +. (float_of_int (i mod 20) *. 0.42) in
      let z = -4. +. (float_of_int (i / 20 mod 20) *. 0.42) in
      let y = 0.2 +. (float_of_int (i / 400) *. 0.5) in
      box_at (x, y, z) r)

let same_pairs (boxes : Broadphase3d.box array) =
  match List.map (fun m -> (m, Broadphase3d.pairs m boxes)) Broadphase3d.methods with
  | (_, first) :: rest ->
      List.iter
        (fun (m, (r : Broadphase3d.result)) ->
          Alcotest.(check (list (pair int int)))
            (Broadphase3d.name m ^ " finds the same pairs")
            first.Broadphase3d.pairs r.Broadphase3d.pairs)
        rest;
      first.Broadphase3d.pairs
  | [] -> []

(* a hand-checkable scene: two boxes overlapping, one beside them, one
 * above *)
let the_worked_example () =
  let boxes =
    [| box_at (0., 0., 0.) 1.; box_at (1.2, 0., 0.) 1.; box_at (6., 0., 0.) 1.; box_at (0., 6., 0.) 1. |]
  in
  let pairs = same_pairs boxes in
  Alcotest.(check (list (pair int int))) "only the first two overlap" [ (0, 1) ] pairs;
  let all = Broadphase3d.all_pairs boxes and grid = Broadphase3d.grid boxes in
  Alcotest.(check int) "four boxes make six pairs" 6 all.Broadphase3d.tests;
  Alcotest.(check bool) "the grid compares fewer" true (grid.Broadphase3d.tests < 6);
  Alcotest.(check (float 1e-9)) "its cells are as big as the biggest box" 2. (Broadphase3d.cell_size boxes)

(* the pile: all three agree, and the clever two do far less work *)
let the_pile () =
  let boxes = pile 500 in
  let pairs = same_pairs boxes in
  Alcotest.(check bool) "a few of the 124,750 pairs really do touch" true (List.length pairs > 100 && List.length pairs < 1000);
  let tests m = (Broadphase3d.pairs m boxes).Broadphase3d.tests in
  let all = tests Broadphase3d.All_pairs and grid = tests Broadphase3d.Grid and swept = tests Broadphase3d.Sweep_and_prune in
  Alcotest.(check int) "all pairs compares every pair" 124750 all;
  Alcotest.(check bool) "the grid compares a hundredth of that" true (grid * 50 < all);
  Alcotest.(check bool) "sweep and prune a twentieth" true (swept * 10 < all);
  Alcotest.(check bool) "and the grid makes the fewest comparisons of all" true (grid < swept)

(* the choice 2D never had to make: a pile is thin in y, and sweeping
 * that axis is nearly as bad as testing everything *)
let the_axis_matters () =
  let boxes = pile 500 in
  let sweeping axis = (Broadphase3d.sweep_and_prune ~axis boxes).Broadphase3d.tests in
  let x = sweeping 0 and y = sweeping 1 and z = sweeping 2 in
  Alcotest.(check bool) "sweeping the thin axis costs ten times more" true (y > 10 * x);
  Alcotest.(check bool) "and is within sight of all pairs" true (y > 124750 / 2);
  Alcotest.(check bool) "the wide axes are both cheap" true (x < 10000 && z < 10000);
  (* and the heuristic picks a wide one *)
  let chosen = Broadphase3d.widest_axis boxes in
  Alcotest.(check bool) "the heuristic does not pick the thin one" true (chosen <> 1);
  let vx, vy, vz = Broadphase3d.spread boxes in
  Alcotest.(check bool) "because the centres barely vary along it" true (vy < vx /. 10. && vy < vz /. 10.)

(* a hundred random scenes, from scattered to piled on top of each
 * other: the three must never disagree *)
let random_scenes () =
  Random.init 5;
  for _ = 1 to 100 do
    let n = 5 + Random.int 40 in
    let spread = 0.5 +. Random.float 8. in
    let boxes =
      Array.init n (fun _ ->
          box_at
            (Random.float spread -. (spread /. 2.), Random.float spread -. (spread /. 2.), Random.float spread -. (spread /. 2.))
            (0.1 +. Random.float 0.6))
    in
    ignore (same_pairs boxes)
  done

let tests =
  [ t "Broadphase3d, four boxes by hand" the_worked_example;
    t "Broadphase3d, five hundred marbles" the_pile;
    t "Broadphase3d, which axis to sweep" the_axis_matters;
    t "Broadphase3d, a hundred random scenes agree" random_scenes ]
