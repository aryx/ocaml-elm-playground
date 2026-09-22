(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* ai/Net and ai/Backprop: the worked example by hand, the backward
 * pass against finite differences, the vanishing gradient measured,
 * and XOR -- the thing one neuron could not do *)

let t = Testo.create
let close = Alcotest.(float 1e-5)

(* the one-neuron network of Backprop.mli, set by hand *)
let one_neuron (w : float) : Net.t =
  [ { w = Matrix.of_lists [ [ w ] ]; b = Matrix.create 1 1; f = Sigmoid } ]

let test_by_hand () =
  let net = one_neuron 0.5 in
  let example = ([| 1. |], [| 1. |]) in
  (* forward *)
  Alcotest.(check close) "a = sigma(0.5)" 0.62246 (Net.forward net [| 1. |]).(0);
  Alcotest.(check close) "L = 1/2 (a - y)^2" 0.07127 (Backprop.loss net [ example ]);
  (* backward: the same number for the weight and the bias, since x = 1 *)
  (match Backprop.gradient net example with
  | [ (dw, db) ] ->
      Alcotest.(check close) "dL/dw" (-0.08872) (Matrix.get dw 0 0);
      Alcotest.(check close) "dL/db" (-0.08872) (Matrix.get db 0 0)
  | _ -> Alcotest.fail "one layer, one weight");
  (* the step, at rate 1: the weight grows, the answer moves towards
     the target, the loss falls *)
  let after = Backprop.learn ~rate:1. net [ example ] in
  (match after with
  | [ l ] ->
      Alcotest.(check close) "w after one step" 0.58872 (Matrix.get l.w 0 0);
      (* the bias has a gradient too, and training takes it: hold it
         still and the numbers below are 0.64307 and 0.06370, which is
         how this example is usually written out by hand *)
      Alcotest.(check close) "b after one step" 0.08872 (Matrix.get l.b 0 0)
  | _ -> ());
  Alcotest.(check close) "and a is nearer 1" 0.66317 (Net.forward after [| 1. |]).(0);
  Alcotest.(check close) "and the loss is lower" 0.05673 (Backprop.loss after [ example ]);
  (* the weight alone, for the version done on paper *)
  let w_only = [ { (List.hd net) with w = Matrix.of_lists [ [ 0.58872 ] ] } ] in
  Alcotest.(check close) "the paper version" 0.64307 (Net.forward w_only [| 1. |]).(0);
  Alcotest.(check close) "and its loss" 0.06370 (Backprop.loss w_only [ example ])

(* the test that catches every sign error: the backward pass against
   the definition of a derivative, on every activation *)
let test_against_finite_differences () =
  List.iter
    (fun (name, hidden, last) ->
      let net = Net.make ~seed:7 ~hidden ~last [ 3; 4; 4; 2 ] in
      let examples =
        [ ([| 0.5; -0.2; 0.9 |], [| 1.; 0. |]); ([| -0.7; 0.3; 0.1 |], [| 0.; 1. |]); ([| 0.2; 0.8; -0.4 |], [| 0.3; 0.7 |]) ]
      in
      let mine = Backprop.over net examples and theirs = Backprop.numeric net examples in
      List.iteri
        (fun l ((dw : Matrix.t), (db : Matrix.t)) ->
          let (nw, nb) = List.nth theirs l in
          Array.iteri
            (fun i v -> Alcotest.(check close) (Printf.sprintf "%s: layer %d, weight %d" name l i) nw.data.(i) v)
            dw.data;
          Array.iteri (fun i v -> Alcotest.(check close) (Printf.sprintf "%s: layer %d, bias %d" name l i) nb.data.(i) v) db.data)
        mine)
    [ ("sigmoid", Net.Sigmoid, Net.Sigmoid); ("tanh", Net.Tanh, Net.Sigmoid); ("relu", Net.Relu, Net.Linear) ]

(* the vanishing gradient: how much smaller the first layer's gradient
   is than the last's, through six hidden layers of each activation.
   The numbers in Backprop.mli come from here. *)
let test_vanishing () =
  let example = ([| 0.5; -0.3; 0.8; 0.1 |], [| 1. |]) in
  let ratio (hidden : Net.activation) : float =
    let net = Net.make ~seed:3 ~hidden ~last:Net.Sigmoid [ 4; 8; 8; 8; 8; 8; 1 ] in
    match Backprop.magnitudes (Backprop.gradient net example) with
    | [] -> 0.
    | first :: _ as all -> List.nth all (List.length all - 1) /. Float.max 1e-30 first
  in
  let sigmoid = ratio Net.Sigmoid and tanh_ = ratio Net.Tanh and relu = ratio Net.Relu in
  Printf.eprintf "gradient, last layer over first: sigmoid %.0f, tanh %.1f, relu %.1f\n" sigmoid tanh_ relu;
  Alcotest.(check bool) "a sigmoid stack starves its first layer" true (sigmoid > 1000.);
  (* and the surprise: with Glorot weights a tanh stack does not vanish
     at this depth at all -- the first layer's gradient is the larger *)
  Alcotest.(check bool) "tanh does not vanish" true (tanh_ < 3.);
  Alcotest.(check bool) "nor does relu" true (relu < 3.)

(* and the payoff: the four corners one neuron could never separate
   (Neuron.mli), learned by two layers *)
let test_xor () =
  let examples =
    List.map (fun (a, b) -> ([| a; b |], [| (if a <> b then 1. else 0.) |])) [ (0., 0.); (0., 1.); (1., 0.); (1., 1.) ]
  in
  let net = Net.make ~seed:5 ~hidden:Net.Tanh ~last:Net.Sigmoid [ 2; 4; 1 ] in
  Alcotest.(check int) "a small machine: 17 numbers" 17 (Net.weights net);
  Alcotest.(check (list int)) "2 -> 4 -> 1" [ 2; 4; 1 ] (Net.sizes net);
  let before = Backprop.loss net examples in
  let rec train net n = if n = 0 then net else train (Backprop.learn ~rate:1.5 net examples) (n - 1) in
  let trained = train net 4000 in
  let after = Backprop.loss trained examples in
  Printf.eprintf "xor: loss %.4f before, %.4f after 4000 steps\n" before after;
  Alcotest.(check bool) "the loss falls" true (after < before /. 20.);
  let right ((x, y) : Backprop.example) =
    let got = (Net.forward trained x).(0) in
    if y.(0) = 1. then got > 0.5 else got < 0.5
  in
  Alcotest.(check bool) "all four corners, which no line can do" true (List.for_all right examples)

let tests =
  [
    t "Backprop, the worked example by hand" test_by_hand;
    t "Backprop, against finite differences" test_against_finite_differences;
    t "Backprop, the vanishing gradient" test_vanishing;
    t "Backprop, XOR learned" test_xor;
  ]
