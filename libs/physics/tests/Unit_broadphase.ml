(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/2d/Broadphase: the worked example, and the three methods
 * finding the same pairs with fewer tests *)

let t = Testo.create
let pairs = Alcotest.(list (pair int int))

(* Broadphase.mli's four boxes: A, B, C, D *)
let example : Broadphase.box array =
  [| ((0., 0.), (2., 2.)); ((1., 1.), (3., 3.)); ((5., 0.), (6., 1.)); ((1.5, 5.), (2.5, 6.)) |]

let test_example () =
  [ (Broadphase.All_pairs, 6); (Broadphase.Sort_and_sweep, 3); (Broadphase.Grid, 1) ]
  |> List.iter (fun (m, tests) ->
         let r = Broadphase.pairs m example in
         Alcotest.check pairs (Broadphase.name m ^ ": only A and B") [ (0, 1) ] r.pairs;
         Alcotest.(check int) (Broadphase.name m ^ ": tests") tests r.tests)

(* [n] boxes, sides 5 to 25, in a [size] x [size] square *)
let random_boxes (st : Random.State.t) (n : int) (size : float) : Broadphase.box array =
  Array.init n (fun _ ->
      let x = Random.State.float st size and y = Random.State.float st size in
      let w = 5. +. Random.State.float st 20. and h = 5. +. Random.State.float st 20. in
      ((x, y), (x +. w, y +. h)))

let test_same_pairs () =
  let st = Random.State.make [| 3 |] in
  for _ = 1 to 200 do
    let boxes = random_boxes st (1 + Random.State.int st 60) (50. +. Random.State.float st 300.) in
    let all = Broadphase.all_pairs boxes in
    Alcotest.check pairs "grid = all pairs" all.pairs (Broadphase.grid boxes).pairs;
    Alcotest.check pairs "sort and sweep = all pairs" all.pairs (Broadphase.sort_and_sweep boxes).pairs;
    Alcotest.check pairs "any cell size" all.pairs (Broadphase.grid ~cell:7. boxes).pairs
  done

(* 300 boxes spread over 1000 x 1000: 44,850 pairs, a few touching;
 * the other two methods test only a few percent of them *)
let test_fewer_tests () =
  let boxes = random_boxes (Random.State.make [| 5 |]) 300 1000. in
  Alcotest.(check int) "all pairs" 44850 (Broadphase.all_pairs boxes).tests;
  let grid = (Broadphase.grid boxes).tests and sweep = (Broadphase.sort_and_sweep boxes).tests in
  if grid > 2000 then Alcotest.failf "the grid tested %d pairs" grid;
  if sweep > 2000 then Alcotest.failf "sort and sweep tested %d pairs" sweep

let tests =
  Testo.categorize "Broadphase"
    [
      t "the worked example: 6, 3, 1 tests" test_example;
      t "the same pairs, 200 random scenes" test_same_pairs;
      t "300 spread boxes: few tests" test_fewer_tests;
    ]
