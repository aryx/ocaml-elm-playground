(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Tensor: reverse mode on whole matrices *)

let t = Testo.create
let lists = Alcotest.(list (list (float 1e-12)))

(* the .mli's layer of two neurons *)
let test_example () =
  let open Tensor in
  let w = value (Matrix.of_lists [ [ 1.; 2.; 3. ]; [ 4.; 5.; 6. ] ]) in
  let x = value (Matrix.of_lists [ [ 1. ]; [ 0. ]; [ -1. ] ]) in
  let y = mul w x in
  Alcotest.check lists "what comes out" [ [ -2. ]; [ -2. ] ] (Matrix.to_lists (of_ y));
  let loss = sum y in
  Alcotest.check lists "nothing has a slope before the walk" [ [ 0. ]; [ 0. ]; [ 0. ] ] (Matrix.to_lists (slope x));
  backward loss;
  Alcotest.check lists "w's slope: x, for each row" [ [ 1.; 0.; -1. ]; [ 1.; 0.; -1. ] ] (Matrix.to_lists (slope w));
  Alcotest.check lists "x's slope: w's columns summed" [ [ 5. ]; [ 7. ]; [ 9. ] ] (Matrix.to_lists (slope x));
  Alcotest.(check int) "three nodes and the two leaves" 4 (nodes loss);
  (* a value used twice hears from both *)
  let a = value (Matrix.of_lists [ [ 3. ] ]) in
  backward (times a a);
  Alcotest.check lists "d(a a)/da is 2a" [ [ 6. ] ] (Matrix.to_lists (slope a))

(* [f] builds a 1 by 1 loss out of the inputs; its slopes by the graph
   against each input's every number moved a hair each way *)
let against_a_nudge (name : string) (inputs : Matrix.t list) (f : Tensor.t list -> Tensor.t) : unit =
  let loss_of (ms : Matrix.t list) : float = Tensor.number (f (List.map Tensor.value ms)) in
  let values = List.map Tensor.value inputs in
  Tensor.backward (f values);
  let epsilon = 1e-6 in
  List.iteri
    (fun which (m : Matrix.t) ->
      Array.iteri
        (fun i v ->
          let moved d =
            let data = Array.copy m.data in
            data.(i) <- v +. d;
            loss_of (List.mapi (fun k x -> if k = which then { m with data } else x) inputs)
          in
          let nudged = (moved epsilon -. moved (-.epsilon)) /. (2. *. epsilon) in
          Alcotest.(check (float 1e-6))
            (Printf.sprintf "%s: input %d, number %d" name which i)
            nudged (Tensor.slope (List.nth values which)).data.(i))
        m.data)
    inputs

let a = Matrix.random ~seed:1 3 4
let b = Matrix.random ~seed:2 4 2
let c = Matrix.random ~seed:3 3 4
let column = Matrix.random ~seed:4 3 1
let square = Matrix.random ~seed:5 4 4

(* a loss that weighs every number differently, so that a slope sent
   to the wrong place shows *)
let weigh (x : Tensor.t) : Tensor.t =
  let m = Tensor.of_ x in
  Tensor.sum (Tensor.times x (Tensor.value (Matrix.init m.rows m.cols (fun r c -> float_of_int (1 + (2 * r) + c)))))

let test_operations () =
  let open Tensor in
  let one f = function [ x ] -> weigh (f x) | _ -> assert false in
  let two f = function [ x; y ] -> weigh (f x y) | _ -> assert false in
  against_a_nudge "add" [ a; c ] (two add);
  against_a_nudge "sub" [ a; c ] (two sub);
  against_a_nudge "mul" [ a; b ] (two mul);
  against_a_nudge "times" [ a; c ] (two times);
  against_a_nudge "mul_t" [ a; c ] (two mul_t);
  direct := false;
  against_a_nudge "mul_t, the long way" [ a; c ] (two mul_t);
  direct := true;
  against_a_nudge "scale" [ a ] (one (scale 2.5));
  against_a_nudge "shift" [ a ] (one (shift 2.5));
  against_a_nudge "transpose" [ a ] (one transpose);
  against_a_nudge "tanh" [ a ] (one tanh_);
  against_a_nudge "pow" [ Matrix.map (fun x -> 1.5 +. x) a ] (one (fun x -> pow x (-0.5)));
  (* relu away from its corner, where a nudge is not a slope *)
  against_a_nudge "relu" [ Matrix.map (fun x -> if Float.abs x < 0.01 then 0.5 else x) a ] (one relu);
  against_a_nudge "rows, one of them twice" [ a ] (one (fun x -> rows x [| 2; 0; 2 |]));
  against_a_nudge "cols" [ a ] (one (fun x -> cols x 1 2));
  against_a_nudge "join_cols" [ a; c ] (two (fun x y -> join_cols [ x; y; x ]));
  against_a_nudge "row_mean" [ a ] (one row_mean);
  against_a_nudge "scale_rows" [ a; column ] (two scale_rows);
  against_a_nudge "softmax_rows" [ a ] (one softmax_rows);
  against_a_nudge "softmax_rows, causal" [ square ] (one (softmax_rows ~causal:true));
  against_a_nudge "cross_entropy" [ a ] (function [ x ] -> cross_entropy x [| 3; 0; 1 |] | _ -> assert false);
  against_a_nudge "add_row" [ a; Matrix.random ~seed:6 1 4 ] (two add_row);
  (* a board of 2 by 3 squares, 2 channels *)
  let squares = Matrix.random ~seed:7 6 2 in
  against_a_nudge "patches" [ squares ] (one (patches ~height:2 ~width:3));
  against_a_nudge "reshape" [ a ] (one (fun x -> reshape x 2 6));
  (* a picture of 4 by 5 pixels, 2 channels, in windows of 2 every 2
     (no overlap) and of 3 every 1 (pixels shared) *)
  let picture = Matrix.random ~seed:8 20 2 in
  against_a_nudge "windows, apart" [ picture ] (one (windows ~height:4 ~width:5 ~size:2 ~stride:2));
  against_a_nudge "windows, overlapping" [ picture ] (one (windows ~height:4 ~width:5 ~size:3 ~stride:1));
  let deserved = Matrix.of_lists [ [ 0.5; 0.5; 0.; 0. ]; [ 0.; 0.1; 0.2; 0.7 ]; [ 1.; 0.; 0.; 0. ] ] in
  against_a_nudge "cross_entropy_to" [ a ] (function [ x ] -> cross_entropy_to x deserved | _ -> assert false);
  (* all of the share on one answer: cross_entropy *)
  let one_hot = Matrix.of_lists [ [ 0.; 0.; 0.; 1. ]; [ 1.; 0.; 0.; 0. ]; [ 0.; 1.; 0.; 0. ] ] in
  Alcotest.(check (float 1e-12)) "one answer deserving everything"
    (number (cross_entropy (value a) [| 3; 0; 1 |]))
    (number (cross_entropy_to (value a) one_hot))

(* softmax and the loss, with numbers one can check: Grad.mli's *)
let test_shares () =
  let open Tensor in
  let scores = value (Matrix.of_lists [ [ 1.; 2.; 3. ]; [ 1.; 2.; 3. ] ]) in
  let p = Matrix.to_lists (of_ (softmax_rows scores)) in
  Alcotest.(check (list (list (float 0.005)))) "the shares" [ [ 0.09; 0.24; 0.67 ]; [ 0.09; 0.24; 0.67 ] ] p;
  let causal = Matrix.to_lists (of_ (softmax_rows ~causal:true (value (Matrix.of_lists [ [ 1.; 9.; 9. ]; [ 1.; 1.; 9. ]; [ 1.; 2.; 3. ] ])))) in
  Alcotest.(check (list (list (float 0.005)))) "each row sees itself and those before"
    [ [ 1.; 0.; 0. ]; [ 0.5; 0.5; 0. ]; [ 0.09; 0.24; 0.67 ] ]
    causal;
  (* the mean of -log 0.67 and -log 0.09 *)
  Alcotest.(check (float 0.005)) "the mean surprise" ((0.41 +. 2.41) /. 2.) (number (cross_entropy scores [| 2; 0 |]));
  (* the same loss as Grad's, a number at a time *)
  let by_grad answer = Grad.of_ (Grad.cross_entropy [ Grad.value 1.; Grad.value 2.; Grad.value 3. ] answer) in
  Alcotest.(check (float 1e-12)) "Grad's" ((by_grad 2 +. by_grad 0) /. 2.) (number (cross_entropy scores [| 2; 0 |]))

(* a square and what is around it: the board

       1 2 3
       4 5 6

   one channel. The middle of the top row, 2, sees nothing above it,
   1 and 3 beside it, 4 5 6 below *)
let test_patches () =
  let board = Matrix.of_lists [ [ 1. ]; [ 2. ]; [ 3. ]; [ 4. ]; [ 5. ]; [ 6. ] ] in
  let p = Matrix.patches board ~height:2 ~width:3 in
  Alcotest.(check (pair int int)) "a row per square, nine columns" (6, 9) (p.rows, p.cols);
  Alcotest.(check (array (float 0.))) "around the 2" [| 0.; 0.; 0.; 1.; 2.; 3.; 4.; 5.; 6. |] (Matrix.row p 1);
  Alcotest.(check (array (float 0.))) "around the 4, a corner" [| 0.; 1.; 2.; 0.; 4.; 5.; 0.; 0.; 0. |] (Matrix.row p 3);
  (* a convolution: one new channel that adds a square and its right
     neighbour, the same weights at every square *)
  let weights = Matrix.of_lists [ [ 0.; 0.; 0.; 0.; 1.; 1.; 0.; 0.; 0. ] ] in
  Alcotest.check lists "each square plus the one to its right" [ [ 3. ]; [ 5. ]; [ 3. ]; [ 9. ]; [ 11. ]; [ 6. ] ]
    (Matrix.to_lists (Matrix.mul_t p weights))

(* a picture cut into windows that step: the picture

       1  2  3  4
       5  6  7  8
       9 10 11 12
      13 14 15 16

   in windows of 2 every 2 is four windows, each a quarter *)
let test_windows () =
  let picture = Matrix.init 16 1 (fun r _ -> float_of_int (r + 1)) in
  let w = Matrix.windows picture ~height:4 ~width:4 ~size:2 ~stride:2 in
  Alcotest.check lists "the four quarters" [ [ 1.; 2.; 5.; 6. ]; [ 3.; 4.; 7.; 8. ]; [ 9.; 10.; 13.; 14. ]; [ 11.; 12.; 15.; 16. ] ]
    (Matrix.to_lists w);
  (* of 3 every 1: the four windows that fit, overlapping *)
  let w = Matrix.windows picture ~height:4 ~width:4 ~size:3 ~stride:1 in
  Alcotest.(check (pair int int)) "two by two windows of nine" (4, 9) (w.rows, w.cols);
  Alcotest.(check (array (float 0.))) "the last" [| 6.; 7.; 8.; 10.; 11.; 12.; 14.; 15.; 16. |] (Matrix.row w 3);
  (* the paper's first layer: 84 by 84 in windows of 8 every 4 *)
  let w = Matrix.windows (Matrix.create (84 * 84) 4) ~height:84 ~width:84 ~size:8 ~stride:4 in
  Alcotest.(check (pair int int)) "20 by 20 windows, of 8 by 8 pixels of 4 frames" (400, 256) (w.rows, w.cols)

let tests =
  [
    t "Tensor, a picture's windows" test_windows;
    t "Tensor, a board's neighbourhoods and a convolution" test_patches;
    t "Tensor, a layer and its slopes" test_example;
    t "Tensor, every operation against a nudge" test_operations;
    t "Tensor, softmax and cross-entropy" test_shares;
  ]
