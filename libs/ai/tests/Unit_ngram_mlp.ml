(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Ngram_mlp: three letters back, through a network *)

let t = Testo.create
let tokens = lazy (Tokenizer.of_text Makemore_names.text)

(* what it learns from, and names held out (Corpus) *)
let split =
  lazy
    (let c = Corpus.split (Tokenizer.words Makemore_names.text) in
     let part words = Ngram_mlp.examples (Lazy.force tokens) ~context:3 words in
     (part c.learn, part c.held))

let test_examples () =
  let ex = Ngram_mlp.examples (Lazy.force tokens) ~context:3 [ "emma" ] in
  let shown = Array.to_list (Array.map (fun (c, next) -> (Array.to_list c, next)) ex) in
  Alcotest.(check (list (pair (list int) int)))
    "the .mli's five"
    [ ([ 0; 0; 0 ], 5); ([ 0; 0; 5 ], 13); ([ 0; 5; 13 ], 13); ([ 5; 13; 13 ], 1); ([ 13; 13; 1 ], 0) ]
    shown;
  let (train, held) = Lazy.force split in
  Alcotest.(check int) "to learn from" 182631 (Array.length train);
  Alcotest.(check int) "held out" 22766 (Array.length held)

let test_shape () =
  let m = Ngram_mlp.make ~seed:1 27 in
  Alcotest.(check int) "makemore's 3,481 numbers" 3481 (Ngram_mlp.parameters m);
  let p = Ngram_mlp.probabilities m [| 0; 5; 13 |] in
  Alcotest.(check (float 1e-9)) "a distribution" 1. (Array.fold_left ( +. ) 0. p);
  let (_, held) = Lazy.force split in
  (* born knowing nothing, not confidently wrong *)
  Alcotest.(check (float 0.01)) "the first loss is log 27" 3.296 (Ngram_mlp.loss m (Array.sub held 0 2000))

(* the graph's step against a nudge: lower one weight by a hair in the
   direction the step took, and the loss on that batch goes down *)
let test_step_goes_down () =
  let (train, _) = Lazy.force split in
  let batch = Ngram_mlp.batch (Lehmer.make 5) train 32 in
  let m = Ngram_mlp.make ~seed:1 ~hidden:20 27 in
  let before = Ngram_mlp.loss m batch in
  let rec go m n = if n = 0 then m else go (Ngram_mlp.step m batch) (n - 1) in
  let after = Ngram_mlp.loss (go m 30) batch in
  Alcotest.(check bool) "thirty steps on one batch: it is being memorised" true (after < before -. 0.5);
  (* the two ways of writing a neuron's sum give the same step *)
  Ngram_mlp.fused := false;
  let slow = Ngram_mlp.step m batch in
  Ngram_mlp.fused := true;
  let fast = Ngram_mlp.step m batch in
  Array.iteri
    (fun i x -> Alcotest.(check (float 1e-12)) "one node or 2n: the same weight" x fast.out_w.data.(i))
    slow.out_w.data

(* a short run, a small network: already better than the table of
   pairs could be without its counts, on names never seen *)
let test_training () =
  let (train, held) = Lazy.force split in
  let held = Array.sub held 0 3000 in
  let state = Lehmer.make 7 in
  let m = ref (Ngram_mlp.make ~seed:1 ~hidden:30 27) in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to 300 do
    m := Ngram_mlp.step !m (Ngram_mlp.batch state train 32)
  done;
  let loss = Ngram_mlp.loss !m held in
  let draws = Lehmer.make 3 in
  let names = List.init 6 (fun _ -> Ngram_mlp.sample draws (Lazy.force tokens) !m) in
  Printf.eprintf "Ngram_mlp, 30 hidden, 300 steps of 32 (%.1f s): held-out loss %.3f; %s\n"
    (Unix.gettimeofday () -. t0) loss (String.concat " " names);
  Alcotest.(check bool) "well under knowing nothing (3.296)" true (loss < 2.75)

(* the numbers in Ngram_mlp.mli and Grad.mli: a step of 32 examples,
   a neuron's sum as 2n nodes and as one *)
let test_cost () =
  let (train, _) = Lazy.force split in
  let batch = Ngram_mlp.batch (Lehmer.make 5) train 32 in
  let m = Ngram_mlp.make ~seed:1 27 in
  let time fused =
    Ngram_mlp.fused := fused;
    let t0 = Unix.gettimeofday () in
    for _ = 1 to 3 do
      ignore (Ngram_mlp.step m batch)
    done;
    Ngram_mlp.fused := true;
    1e3 *. (Unix.gettimeofday () -. t0) /. 3.
  in
  let (apart, one) = (time false, time true) in
  Printf.eprintf "Ngram_mlp, a step of 32: %.0f ms with a node per product, %.0f ms with Grad.dot\n" apart one;
  Alcotest.(check bool) "fewer nodes, less time" true (one < apart)

(* the split every program shares: the trainer's 80% and the game's
   hidden names must be cut at the same places *)
let test_corpus () =
  let words = Tokenizer.words Makemore_names.text in
  let c = Corpus.split words in
  Alcotest.(check (triple int int int)) "80%, 10%, 10%" (25626, 3203, 3204) (Corpus.sizes c);
  Alcotest.(check (list string)) "every name once" (List.sort compare words) (List.sort compare (c.learn @ c.held @ c.test));
  Alcotest.(check bool) "the same cut twice" true (Corpus.split words = c);
  Alcotest.(check bool) "another seed, another cut" true (Corpus.split ~seed:7 words <> c);
  (* shuffled: the file has the commonest names first *)
  Alcotest.(check bool) "no longer starting with emma" true (List.hd c.learn <> "emma")

(* a network written and read back answers the same, to the seven
   digits a 32-bit number keeps *)
let test_weights () =
  let m = Ngram_mlp.make ~seed:1 ~hidden:12 27 in
  let w = Ngram_mlp.to_weights ~notes:[ ("seed", "1") ] m in
  let back =
    match Result.bind (Weights.of_string (Weights.to_string w)) Ngram_mlp.of_weights with
    | Ok back -> back
    | Error why -> Alcotest.fail why
  in
  Alcotest.(check int) "its context, from a note" 3 back.context;
  Alcotest.(check (array (float 1e-6))) "the same opinion" (Ngram_mlp.probabilities m [| 0; 5; 13 |])
    (Ngram_mlp.probabilities back [| 0; 5; 13 |]);
  (* matrices that do not fit one another are refused, not read
     outside their bounds later *)
  let cut : Weights.t = { w with matrices = List.map (fun (n, x) -> if n = "out.w" then (n, Matrix.create 27 5) else (n, x)) w.matrices } in
  Alcotest.(check bool) "sizes that do not chain" true (Result.is_error (Ngram_mlp.of_weights cut));
  let less : Weights.t = { w with matrices = List.tl w.matrices } in
  Alcotest.(check bool) "a matrix missing" true (Result.is_error (Ngram_mlp.of_weights less))

let tests =
  [
    t "Corpus, the split every program shares" test_corpus;
    t "Ngram_mlp, written to a file and read back" test_weights;
    t "Ngram_mlp, a word's examples" test_examples;
    t "Ngram_mlp, makemore's sizes" test_shape;
    t "Ngram_mlp, a step goes down" test_step_goes_down;
    t "Ngram_mlp, three hundred steps on the names" test_training;
    t "Ngram_mlp, what a node per number costs" test_cost;
  ]
