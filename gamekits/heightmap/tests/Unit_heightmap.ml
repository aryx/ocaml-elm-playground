(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gamekits/heightmap: Heightmap *)

let t = Testo.create

(* a 4 x 4 grid, by hand: 0 everywhere but a 4 at (1, 1) *)
let bump : Heightmap.t = { size = 4; top = 4.; sea = 0.; cells = Array.init 16 (fun k -> if k = 5 then 4. else 0.) }

let test_height () =
  Alcotest.(check (float 1e-9)) "on the cell" 4. (Heightmap.height bump 1. 1.);
  Alcotest.(check (float 1e-9)) "halfway to its neighbor" 2. (Heightmap.height bump 1.5 1.);
  Alcotest.(check (float 1e-9)) "in the middle of 4 cells" 1. (Heightmap.height bump 1.5 1.5);
  Alcotest.(check (float 1e-9)) "outside: the sea" 0. (Heightmap.height bump (-5.) 20.)

let test_clear () =
  Alcotest.(check bool) "over the bump" true (Heightmap.clear bump (0., 1., 5.) (3., 1., 5.));
  Alcotest.(check bool) "through it" false (Heightmap.clear bump (0., 1., 1.) (3., 1., 1.))

let test_random () =
  Alcotest.(check bool) "the same twice" true (Heightmap.random 1 2 3 = Heightmap.random 1 2 3);
  Alcotest.(check bool) "another cell, another number" true (Heightmap.random 1 2 3 <> Heightmap.random 1 3 2);
  let all = List.init 1000 (fun k -> Heightmap.random 7 k (k * 3)) in
  Alcotest.(check bool) "between -1 and 1" true (List.for_all (fun r -> r >= -1. && r <= 1.) all);
  let mean = List.fold_left ( +. ) 0. all /. 1000. in
  Alcotest.(check bool) "centered" true (Float.abs mean < 0.1)

let test_generate () =
  let m = Heightmap.generate ~seed:1 ~size:64 ~top:100. ~roughness:0.5 in
  Alcotest.(check (float 1e-9)) "the sea, a fifth of the top" 20. m.sea;
  Alcotest.(check (float 1e-9)) "the highest, the top" 100. (Array.fold_left Float.max 0. m.cells);
  Alcotest.(check (float 1e-9)) "nothing under the sea" 20. (Array.fold_left Float.min 100. m.cells);
  Alcotest.(check bool) "an island: the corners in the sea" true
    (List.for_all (fun (i, j) -> Heightmap.kind m i j = Sea) [ (0, 0); (63, 0); (0, 63); (63, 63) ]);
  Alcotest.(check bool) "the same island for the same seed" true
    (m.cells = (Heightmap.generate ~seed:1 ~size:64 ~top:100. ~roughness:0.5).cells)

let test_colors () =
  let m : Heightmap.t = { size = 2; top = 100.; sea = 20.; cells = [| 20.; 21.; 50.; 95. |] } in
  Alcotest.(check (list string)) "by height" [ "sea"; "sand"; "forest"; "snow" ]
    (List.map
       (fun (i, j) ->
         match Heightmap.kind m i j with
         | Sea -> "sea"
         | Sand -> "sand"
         | Grass -> "grass"
         | Forest -> "forest"
         | Rock -> "rock"
         | Snow -> "snow")
       [ (0, 0); (1, 0); (0, 1); (1, 1) ]);
  (* (0, 0) is level with the sea west of the grid: flat; the others
   * above their west neighbor (1, 30, 45; more than top / 200 = 0.5):
   * facing the sun *)
  Alcotest.(check (list int)) "lights" [ 1; 0; 0; 0 ] [ Heightmap.light m 0 0; Heightmap.light m 1 0; Heightmap.light m 0 1; Heightmap.light m 1 1 ]

let tests =
  Testo.categorize "Heightmap"
    [ t "height, between the cells" test_height;
      t "clear, a line of sight" test_clear;
      t "random, a hash" test_random;
      t "generate, an island" test_generate;
      t "kind and light" test_colors ]
