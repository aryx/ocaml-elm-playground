(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* ai/Matrix: the worked examples of Matrix.mli, and the two products
 * measured against each other *)

let t = Testo.create
let floats = Alcotest.(list (list (float 1e-9)))

(* the matrix of the .mli's picture *)
let m23 = Matrix.of_lists [ [ 1.; 2.; 3. ]; [ 4.; 5.; 6. ] ]

let test_shape () =
  Alcotest.(check int) "rows" 2 m23.rows;
  Alcotest.(check int) "cols" 3 m23.cols;
  Alcotest.(check (float 1e-9)) "the element at (1, 2)" 6. (Matrix.get m23 1 2);
  Alcotest.(check (float 1e-9)) "which is data.(1 * 3 + 2)" 6. m23.data.((1 * 3) + 2);
  Alcotest.check floats "and back again" [ [ 1.; 2.; 3. ]; [ 4.; 5.; 6. ] ] (Matrix.to_lists m23);
  Alcotest.check floats "transposed" [ [ 1.; 4. ]; [ 2.; 5. ]; [ 3.; 6. ] ] (Matrix.to_lists (Matrix.transpose m23))

let test_arithmetic () =
  let a = Matrix.of_lists [ [ 1.; 2. ]; [ 3.; 4. ] ] and b = Matrix.of_lists [ [ 10.; 20. ]; [ 30.; 40. ] ] in
  Alcotest.check floats "added" [ [ 11.; 22. ]; [ 33.; 44. ] ] (Matrix.to_lists (Matrix.add a b));
  Alcotest.check floats "elementwise, which is what backprop wants" [ [ 10.; 40. ]; [ 90.; 160. ] ]
    (Matrix.to_lists (Matrix.times a b));
  Alcotest.check floats "scaled" [ [ 2.; 4. ]; [ 6.; 8. ] ] (Matrix.to_lists (Matrix.scale 2. a));
  Alcotest.(check (float 1e-9)) "summed" 10. (Matrix.sum a);
  (* the matrix product is not the elementwise one *)
  Alcotest.check floats "multiplied" [ [ 70.; 100. ]; [ 150.; 220. ] ] (Matrix.to_lists (Matrix.mul a b));
  Alcotest.check floats "identity changes nothing" (Matrix.to_lists a) (Matrix.to_lists (Matrix.mul a (Matrix.identity 2)));
  (* a layer's work: a 2x3 matrix on a 3-vector is a 2-vector *)
  let v = Matrix.vector [| 1.; 0.; -1. |] in
  Alcotest.(check (list (float 1e-9))) "W x" [ -2.; -2. ] (Array.to_list (Matrix.to_vector (Matrix.mul m23 v)));
  Alcotest.check_raises "shapes that do not fit" (Invalid_argument "Matrix.mul: inner dimensions differ") (fun () ->
      ignore (Matrix.mul m23 m23))

(* the two products agree, everywhere, including on shapes whose inner
   dimension is not a multiple of the four the fast one unrolls *)
let test_same_answer () =
  List.iter
    (fun (n, m, k) ->
      let a = Matrix.random ~seed:(n + m + k) n m and b = Matrix.random ~seed:(n * 7) m k in
      let simple = Matrix.mul_simple a b and fast = Matrix.mul_fast a b in
      Alcotest.(check int) "same shape" (Array.length simple.data) (Array.length fast.data);
      Array.iteri
        (fun i v -> Alcotest.(check (float 1e-9)) (Printf.sprintf "%dx%dx%d, element %d" n m k i) v fast.data.(i))
        simple.data)
    [ (1, 1, 1); (2, 3, 4); (5, 5, 5); (3, 7, 2); (8, 9, 6); (13, 17, 11) ]

(* and the fast one is faster, which is the only reason it is there.
   The numbers in Matrix.mli come from this test. *)
let test_speed () =
  let time (f : unit -> 'a) : float =
    let t0 = Unix.gettimeofday () in
    ignore (f ());
    Unix.gettimeofday () -. t0
  in
  List.iter
    (fun n ->
      let a = Matrix.random ~seed:1 n n and b = Matrix.random ~seed:2 n n in
      let simple = time (fun () -> Matrix.mul_simple a b) in
      let fast = time (fun () -> Matrix.mul_fast a b) in
      Printf.eprintf "matrix %dx%d: simple %.1f ms, fast %.1f ms (%.1fx)\n" n n (1000. *. simple) (1000. *. fast)
        (simple /. Float.max 1e-9 fast))
    [ 64; 128; 256 ];
  (* asserted loosely: the machine is not the point, the memory is *)
  let n = 256 in
  let a = Matrix.random ~seed:1 n n and b = Matrix.random ~seed:2 n n in
  let simple = time (fun () -> Matrix.mul_simple a b) in
  let fast = time (fun () -> Matrix.mul_fast a b) in
  Alcotest.(check bool) "reading along rows is worth at least twice" true (fast *. 2. < simple)

let test_random () =
  let m = Matrix.random ~seed:7 ~spread:0.5 4 4 in
  Alcotest.(check bool) "inside the spread" true (Array.for_all (fun v -> Float.abs v <= 0.5) m.data);
  Alcotest.(check bool) "and not all the same: a net that starts equal stays equal" true
    (Array.exists (fun v -> v <> m.data.(0)) m.data);
  Alcotest.check floats "the same seed, the same matrix" (Matrix.to_lists m)
    (Matrix.to_lists (Matrix.random ~seed:7 ~spread:0.5 4 4))

let tests =
  [
    t "Matrix, the shape and the flat array" test_shape;
    t "Matrix, arithmetic" test_arithmetic;
    t "Matrix, the two products agree" test_same_answer;
    t "Matrix, and the fast one is faster" test_speed;
    t "Matrix, random weights" test_random;
  ]
