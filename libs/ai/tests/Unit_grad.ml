(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Grad: the graph, the chain rule walked backwards once, and the
 * same gradient as the hand-written pass of Backprop -- which is
 * the whole claim of automatic differentiation *)

let t = Testo.create
let exact = Alcotest.(float 1e-12)

let test_graph () =
  let open Grad in
  (* the .mli's picture: y = x w + 1, at x = 3, w = 2 *)
  let x = value 3. and w = value 2. in
  let y = (x *: w) +: value 1. in
  Alcotest.(check exact) "y" 7. (of_ y);
  Alcotest.(check exact) "nothing has a slope before the walk" 0. (slope w);
  backward y;
  Alcotest.(check exact) "dy/dw is x" 3. (slope w);
  Alcotest.(check exact) "dy/dx is w" 2. (slope x);
  (* a value used twice: the slopes add, which is the one thing a
     hand-written pass gets wrong if it is not careful *)
  let a = value 4. in
  let b = a *: a in
  backward b;
  Alcotest.(check exact) "d(a*a)/da is 2a" 8. (slope a)

let test_rules () =
  let open Grad in
  let check name f df at =
    let x = value at in
    let y = f x in
    backward y;
    Alcotest.(check Alcotest.(float 1e-9)) name (df at) (slope x)
  in
  check "exp" exp_ exp 0.7;
  check "log" log_ (fun x -> 1. /. x) 2.5;
  check "tanh" tanh_ (fun x -> 1. -. (tanh x ** 2.)) 0.4;
  check "sigmoid" sigmoid (fun x -> let s = 1. /. (1. +. exp (-.x)) in s *. (1. -. s)) (-0.3);
  check "relu, above zero" relu (fun _ -> 1.) 2.;
  check "relu, below" relu (fun _ -> 0.) (-2.);
  check "square" square (fun x -> 2. *. x) 1.5;
  check "divide" (fun x -> value 6. /: x) (fun x -> -6. /. (x *. x)) 3.;
  (* and one whose derivative has to be worked out:
     f(x) = (x^2 + 3x) / (x - 1), so
     f'(x) = ((2x + 3)(x - 1) - (x^2 + 3x)) / (x - 1)^2, and at x = 2
     that is (7 - 10) / 1 = -3. Writing this test I said -4, and the
     graph said -3; the graph was right, which is the whole argument
     for having it. *)
  let x = value 2. in
  let y = (square x +: (value 3. *: x)) /: (x -: value 1.) in
  backward y;
  Alcotest.(check Alcotest.(float 1e-9)) "a quotient, differentiated for us" (-3.) (slope x)

(* the same network, the same example, the two ways: the hand-written
   backward pass of Backprop, and a graph built out of Grad values.
   They agree to the last digit, because they are the same arithmetic
   in a different order *)
let net = Net.make ~seed:11 ~hidden:Net.Tanh ~last:Net.Sigmoid [ 2; 8; 8; 1 ]
let example = ([| 0.4; -0.7 |], [| 1. |])

(* the same network and example as a graph of Grad values: its weights
   with their slopes filled in *)
let by_graph () : (float array * float array) list =
  let (x, y) = example in
  let weights = List.map (fun (l : Net.layer) -> (Array.map Grad.value l.w.data, Array.map Grad.value l.b.data, l)) net in
  let out =
    List.fold_left
      (fun input (ws, bs, (l : Net.layer)) ->
        List.init l.w.rows (fun r ->
            let z =
              Grad.sum (List.init l.w.cols (fun c -> Grad.( *: ) ws.((r * l.w.cols) + c) (List.nth input c)))
            in
            let z = Grad.( +: ) z bs.(r) in
            match l.f with
            | Net.Tanh -> Grad.tanh_ z
            | Net.Sigmoid -> Grad.sigmoid z
            | Net.Relu -> Grad.relu z
            | Net.Linear -> z))
      (Array.to_list (Array.map Grad.value x))
      weights
  in
  (* the same loss: 1/2 (a - y)^2 *)
  let loss = Grad.( *: ) (Grad.value 0.5) (Grad.square (Grad.( -: ) (List.hd out) (Grad.value y.(0)))) in
  Grad.backward loss;
  List.map (fun (ws, bs, _) -> (Array.map Grad.slope ws, Array.map Grad.slope bs)) weights

(* the two gradients side by side, weight by weight *)
let both () : (float * float) list =
  let by_hand = Backprop.gradient net example in
  List.concat
    (List.mapi
       (fun l (dw_graph, db_graph) ->
         let ((dw : Matrix.t), (db : Matrix.t)) = List.nth by_hand l in
         Array.to_list (Array.mapi (fun i g -> (dw.data.(i), g)) dw_graph)
         @ Array.to_list (Array.mapi (fun i g -> (db.data.(i), g)) db_graph))
       (by_graph ()))

let test_same_as_backprop () =
  let pairs = both () in
  Alcotest.(check int) "a 2-8-8-1 network has 105 numbers" 105 (List.length pairs);
  List.iteri (fun i (by_hand, by_graph) -> Alcotest.(check exact) (Printf.sprintf "weight %d" i) by_hand by_graph) pairs

(* what the convenience costs, which is the reason the hand-written
   pass is still the one the examples use. The numbers in Grad.mli
   come from here. *)
let test_cost () =
  let time n f =
    let t0 = Unix.gettimeofday () in
    for _ = 1 to n do
      ignore (f ())
    done;
    1e6 *. (Unix.gettimeofday () -. t0) /. float_of_int n
  in
  let hand = time 2000 (fun () -> Backprop.gradient net example) in
  let graph = time 500 (fun () -> by_graph ()) in
  Printf.eprintf "gradient: by hand %.1f us, by graph %.1f us (%.0fx)\n" hand graph (graph /. hand);
  Alcotest.(check bool) "scalars are the slow way" true (graph > hand)

(* choosing among several: the .mli's numbers, and the slope that
   comes out as "given minus deserved" without anybody deriving it *)
let test_cross_entropy () =
  let open Grad in
  let near = Alcotest.(float 1e-9) in
  let scores = [ value 1.; value 2.; value 3. ] in
  let p = List.map of_ (softmax scores) in
  Alcotest.(check (list (float 0.005))) "the shares" [ 0.09; 0.24; 0.67 ] p;
  Alcotest.(check near) "they sum to 1" 1. (List.fold_left ( +. ) 0. p);
  let loss = cross_entropy scores 2 in
  Alcotest.(check (float 0.005)) "the answer was the likeliest" 0.41 (of_ loss);
  Alcotest.(check (float 0.005)) "the answer was the least likely" 2.41 (of_ (cross_entropy scores 0));
  backward loss;
  List.iteri
    (fun i s ->
      let deserved = if i = 2 then 1. else 0. in
      Alcotest.(check near) (Printf.sprintf "score %d: p - deserved" i) (List.nth p i -. deserved) (slope s))
    scores;
  (* scores that would overflow exp by themselves *)
  let big = [ value 1000.; value 1001. ] in
  Alcotest.(check (float 1e-6)) "large scores" (log (1. +. exp (-1.))) (of_ (cross_entropy big 1));
  (* a power, and a weight changed between two graphs *)
  let x = value 3. in
  let y = pow x (-0.5) in
  backward y;
  Alcotest.(check near) "d(x^-1/2)/dx" (-0.5 *. (3. ** -1.5)) (slope x);
  set x 4.;
  let y = square x in
  backward y;
  Alcotest.(check near) "the new value" 16. (of_ y);
  Alcotest.(check near) "and its slope, the old one forgotten" 8. (slope x)

(* a graph the size of a language model's loss: a chain as long as
   the graph (a browser's stack would not hold a recursive walk of
   it), and the walk in time proportional to it. The table in Grad.ml
   comes from here. *)
let test_large_graph () =
  let chain n =
    let x = Grad.value 1.0001 in
    let y = ref x in
    for _ = 1 to n do
      y := Grad.( *: ) !y x
    done;
    (x, !y)
  in
  let time n =
    let (_, y) = chain n in
    let t0 = Unix.gettimeofday () in
    Grad.backward y;
    1e3 *. (Unix.gettimeofday () -. t0)
  in
  let (ms1, ms4, ms16) = (time 1000, time 4000, time 16000) in
  Printf.eprintf "backward on a chain: 1000 nodes %.2f ms, 4000 %.2f ms, 16000 %.2f ms\n" ms1 ms4 ms16;
  (* d(x^(n+1))/dx = (n+1) x^n *)
  let n = 100_000 in
  let (x, y) = chain n in
  Grad.backward y;
  Alcotest.(check int) "the nodes" (n + 1) (Grad.nodes y);
  (* no check of the times, which a loaded machine makes anything:
     that this ends at all is the check -- the walk through a list of
     the nodes seen took twelve seconds here *)
  Alcotest.(check (float 1e-6)) "its slope" 1. (Grad.slope x /. (float_of_int (n + 1) *. (1.0001 ** float_of_int n)))

let tests =
  [
    t "Grad, the graph and the chain rule" test_graph;
    t "Grad, one derivative per operation" test_rules;
    t "Grad, softmax and cross-entropy" test_cross_entropy;
    t "Grad, a graph of a hundred thousand nodes" test_large_graph;
    t "Grad, the same gradient as the hand-written pass" test_same_as_backprop;
    t "Grad, and what that convenience costs" test_cost;
  ]
