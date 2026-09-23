(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Train: the loop, the split, and the two things it is for --
 * learning something in a few epochs, and showing when it stops
 * learning and starts memorising *)

let t = Testo.create

(* a problem with a rule in it: inside the circle of radius 0.6, or
 * outside. Nothing a line can do, easy for a small network *)
let circle (seed : int) (n : int) : Backprop.example list =
  let st = Random.State.make [| seed |] in
  List.init n (fun _ ->
      let x = Random.State.float st 2. -. 1. and y = Random.State.float st 2. -. 1. in
      ([| x; y |], [| (if (x *. x) +. (y *. y) < 0.36 then 1. else 0.) |]))

let test_split () =
  let examples = circle 1 100 in
  let (train, held) = Train.split examples in
  Alcotest.(check int) "a fifth held out" 20 (List.length held);
  Alcotest.(check int) "and the rest trained on" 80 (List.length train);
  Alcotest.(check bool) "nothing is in both" true
    (List.for_all (fun e -> not (List.exists (fun h -> h == e) held)) train);
  let (train, held) = Train.split ~part:0.5 examples in
  Alcotest.(check int) "or half of them" 50 (List.length held);
  Alcotest.(check int) "and half" 50 (List.length train)

let test_learns () =
  let examples = circle 2 400 in
  let (train, held) = Train.split examples in
  let net = Net.make ~seed:4 ~hidden:Net.Tanh ~last:Net.Sigmoid [ 2; 8; 1 ] in
  let before = Train.accuracy net held ~answer:(fun o -> if o.(0) > 0.5 then 1 else 0) in
  let (trained, history) = Train.run ~epochs:200 ~rate:1.2 ~batch:16 ~held net train in
  let after = Train.accuracy trained held ~answer:(fun o -> if o.(0) > 0.5 then 1 else 0) in
  Printf.eprintf "circle: %.0f%% right before, %.0f%% after 200 epochs\n" (100. *. before) (100. *. after);
  Alcotest.(check int) "one entry per epoch" 200 (List.length history);
  Alcotest.(check bool) "it learns the rule" true (after > 0.95);
  (* the loss falls, and the held-out one falls with it while there is
     still a rule to learn *)
  let first = List.hd history and last = List.nth history (List.length history - 1) in
  Alcotest.(check bool) "the training loss falls" true (last.training < first.training /. 2.);
  Alcotest.(check bool) "and so does the held-out one" true (last.held_out < first.held_out /. 2.)

(* overfitting, made to happen: far too big a network on far too few
   examples, trained far too long. The training loss keeps falling and
   the held-out one turns round and climbs -- the picture Train.mli
   draws in ASCII *)
let test_overfitting () =
  let examples = circle 3 40 in
  let (train, held) = Train.split ~part:0.5 examples in
  let net = Net.make ~seed:9 ~hidden:Net.Tanh ~last:Net.Sigmoid [ 2; 24; 24; 1 ] in
  let (_, history) = Train.run ~epochs:400 ~rate:0.9 ~batch:4 ~held net train in
  let best_held = List.fold_left (fun m (h : Train.history) -> Float.min m h.held_out) infinity history in
  let last = List.nth history (List.length history - 1) in
  Printf.eprintf "overfit: training %.4f -> %.4f, held out best %.4f, ended %.4f\n"
    (List.hd history).training last.training best_held last.held_out;
  Alcotest.(check bool) "the training loss keeps falling" true (last.training < 0.01);
  Alcotest.(check bool) "the held-out loss turns round and climbs" true (last.held_out > best_held *. 1.2)

let test_answers () =
  Alcotest.(check int) "the biggest output wins" 2 (Train.best [| 0.1; 0.3; 0.9; 0.2 |]);
  Alcotest.(check int) "ties go to the first" 0 (Train.best [| 0.5; 0.5 |]);
  Alcotest.(check (array (float 1e-9))) "one in one place" [| 0.; 0.; 1.; 0. |] (Train.one_hot 4 2)

let tests =
  [
    t "Train, the split" test_split;
    t "Train, it learns a circle" test_learns;
    t "Train, and it overfits when made to" test_overfitting;
    t "Train, how a network says which" test_answers;
  ]
