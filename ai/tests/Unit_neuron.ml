(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* ai/Neuron: Rosenblatt's rule, what it always learns, and what it
 * cannot learn however long it is given *)

let t = Testo.create

let test_answer () =
  (* a neuron is a line: this one answers 1 above x2 = -x1 + 1 *)
  let n : Neuron.t = { weights = [| 1.; 1. |]; bias = -1. } in
  Alcotest.(check (float 1e-9)) "on the line, the sum is zero" 0. (Neuron.sum n [| 1.; 0. |]);
  Alcotest.(check (float 1e-9)) "and the step says 0" 0. (Neuron.answer n [| 1.; 0. |]);
  Alcotest.(check (float 1e-9)) "above it, 1" 1. (Neuron.answer n [| 1.; 1. |]);
  Alcotest.(check (float 1e-9)) "below it, 0" 0. (Neuron.answer n [| 0.; 0. |]);
  Alcotest.check_raises "and it counts its inputs" (Invalid_argument "Neuron: wrong number of inputs") (fun () ->
      ignore (Neuron.answer n [| 1. |]))

(* the rule: a right answer changes nothing, a wrong one moves the
   weights by the input *)
let test_rule () =
  let n : Neuron.t = { weights = [| 0.; 0. |]; bias = 0. } in
  Alcotest.(check bool) "a right answer costs nothing" true (Neuron.learn ~rate:0.5 n ([| 1.; 1. |], 0.) == n);
  let after = Neuron.learn ~rate:0.5 n ([| 1.; 1. |], 1.) in
  Alcotest.(check (list (float 1e-9))) "a wrong one moves by the input" [ 0.5; 0.5 ] (Array.to_list after.weights);
  Alcotest.(check (float 1e-9)) "and the bias by the error" 0.5 after.bias;
  (* the error's sign decides which way it moves *)
  let back = Neuron.learn ~rate:0.5 after ([| 1.; 1. |], 0.) in
  Alcotest.(check (list (float 1e-9))) "the other way" [ 0.; 0. ] (Array.to_list back.weights)

(* Rosenblatt's theorem, seen: if a line exists it finds one, and then
   it stops -- there is nothing left to learn from *)
let test_convergence () =
  Alcotest.(check (float 1e-9)) "AND is a line" 1. (Neuron.learns Neuron.and_);
  Alcotest.(check (float 1e-9)) "so is OR" 1. (Neuron.learns Neuron.or_);
  let n = Neuron.train (Neuron.make ~inputs:2 ~seed:3) Neuron.and_ in
  Alcotest.(check int) "nothing left to get wrong" 0 (Neuron.mistakes n Neuron.and_);
  Alcotest.(check bool) "and a settled neuron stops moving" true (Neuron.epoch n Neuron.and_ == n);
  (* a hundred points either side of a line, which is the example's
     case: it separates them all *)
  let st = Random.State.make [| 5 |] in
  let points =
    List.init 100 (fun _ ->
        let x = Random.State.float st 2. -. 1. and y = Random.State.float st 2. -. 1. in
        (* a gap, so that a line exists at all *)
        let y = if y > 0. then y +. 0.2 else y -. 0.2 in
        ([| x; y |], if y > x then 1. else 0.))
  in
  Alcotest.(check (float 1e-9)) "a hundred points, all of them" 1. (Neuron.learns ~epochs:200 points)

(* and the one it cannot: half of it stays wrong, which is worse than
   the best line available -- a rule that cannot converge wanders *)
let test_xor () =
  (* no line gets more than three of the four: a scan of every line
     worth trying says the best is one mistake *)
  let best = ref 4 in
  for a = -20 to 20 do
    for b = -20 to 20 do
      for c = -20 to 20 do
        let n : Neuron.t = { weights = [| float_of_int a /. 4.; float_of_int b /. 4. |]; bias = float_of_int c /. 4. } in
        best := min !best (Neuron.mistakes n Neuron.xor)
      done
    done
  done;
  Alcotest.(check int) "the best any line does is three of four" 1 !best;
  (* and the rule does not even reach that: it moves on every mistake,
     the mistakes never stop, so it never rests on the best line *)
  Alcotest.(check (float 1e-9)) "XOR, with a hundred epochs" 0.5 (Neuron.learns Neuron.xor);
  Alcotest.(check (float 1e-9)) "XOR, with ten thousand" 0.5 (Neuron.learns ~epochs:10_000 Neuron.xor);
  Alcotest.(check bool) "and at every seed" true
    (List.for_all (fun seed -> Neuron.learns ~seed Neuron.xor = 0.5) [ 1; 2; 3; 4; 5; 6; 7; 8 ]);
  (* not stuck -- swapping: it keeps trading one mistake for another *)
  let n = Neuron.make ~inputs:2 ~seed:2 in
  let lines = List.init 12 (fun i -> Neuron.train ~epochs:i n Neuron.xor) in
  let seen = List.sort_uniq compare (List.map (fun (n : Neuron.t) -> (Array.to_list n.weights, n.bias)) lines) in
  Alcotest.(check bool) "it never settles on one line" true (List.length seen > 2);
  Alcotest.(check bool) "and always gets two of the four wrong" true
    (List.for_all (fun n -> Neuron.mistakes n Neuron.xor = 2) (List.filteri (fun i _ -> i > 0) lines))

let tests =
  [
    t "Neuron, the line it draws" test_answer;
    t "Neuron, the learning rule" test_rule;
    t "Neuron, what it always learns" test_convergence;
    t "Neuron, and what it cannot" test_xor;
  ]
