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
  against_a_nudge "cross_entropy" [ a ] (function [ x ] -> cross_entropy x [| 3; 0; 1 |] | _ -> assert false)

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

let tests =
  [
    t "Tensor, a layer and its slopes" test_example;
    t "Tensor, every operation against a nudge" test_operations;
    t "Tensor, softmax and cross-entropy" test_shares;
  ]
