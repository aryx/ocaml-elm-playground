(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Tokenizer, Sampling, Bigram: a text as numbers, a table of pairs
 * counted, and the same table learned *)

let t = Testo.create
let words = lazy (Tokenizer.words Makemore_names.text)
let tokens = lazy (Tokenizer.of_text Makemore_names.text)
let counts = lazy (Bigram.counts (Lazy.force tokens) (Lazy.force words))

let test_tokenizer () =
  let tk = Lazy.force tokens in
  Alcotest.(check int) "the names" 32033 (List.length (Lazy.force words));
  Alcotest.(check int) "26 letters and the boundary" 27 (Tokenizer.size tk);
  Alcotest.(check (list int)) "emma" [ 5; 13; 13; 1 ] (Tokenizer.encode tk "emma");
  Alcotest.(check (list int)) "between its boundaries" [ 0; 5; 13; 13; 1; 0 ] (Tokenizer.bounded tk "emma");
  Alcotest.(check string) "and back" ".emma." (Tokenizer.decode tk [ 0; 5; 13; 13; 1; 0 ]);
  Alcotest.check_raises "a character never seen" Not_found (fun () -> ignore (Tokenizer.encode tk "Emma"));
  (* any text's characters, in order *)
  let small = Tokenizer.of_text "cab\nabba\n" in
  Alcotest.(check int) "a, b, c and the boundary" 4 (Tokenizer.size small);
  Alcotest.(check (list int)) "sorted" [ 3; 1; 2 ] (Tokenizer.encode small "cab")

let test_sampling () =
  let p = [| 0.5; 0.2; 0.3 |] in
  let near = Alcotest.(array (float 0.005)) in
  Alcotest.check near "cooler: the likely likelier" [| 0.66; 0.11; 0.24 |] (Sampling.temper 0.5 p);
  Alcotest.check near "warmer: the odds evened" [| 0.42; 0.26; 0.32 |] (Sampling.temper 2. p);
  Alcotest.check near "at 1, unchanged" p (Sampling.temper 1. p);
  Alcotest.(check int) "the likeliest" 0 (Sampling.best p);
  (* ten thousand draws have the odds they were drawn with *)
  let state = Lehmer.make 1 and seen = Array.make 3 0 in
  for _ = 1 to 10_000 do
    let i = Sampling.draw state p in
    seen.(i) <- seen.(i) + 1
  done;
  Alcotest.check
    Alcotest.(array (float 0.02))
    "the shares drawn" p
    (Array.map (fun n -> float_of_int n /. 10_000.) seen)

(* the .mli's table, which is makemore's *)
let test_counts () =
  let n = Lazy.force counts in
  let count a b = int_of_float (Matrix.get n a b) in
  Alcotest.(check int) "pairs in all" 228146 (int_of_float (Matrix.sum n));
  Alcotest.(check int) "names starting with a" 4410 (count 0 1);
  Alcotest.(check int) "names ending with n" 6763 (count 14 0);
  Alcotest.(check int) "names ending with a" 6640 (count 1 0);
  Alcotest.(check int) "an" 5438 (count 1 14);
  Alcotest.(check int) "no name is empty" 0 (count 0 0);
  let p = Bigram.probabilities n in
  Alcotest.(check (float 0.0005)) "after an a, the end" 0.196 (Matrix.get p 1 0);
  Array.iteri
    (fun r _ -> Alcotest.(check (float 1e-9)) "a row sums to 1" 1. (Array.fold_left ( +. ) 0. (Matrix.row p r)))
    (Array.make 27 ())

let test_loss () =
  let n = Lazy.force counts in
  let knowing_nothing = Bigram.probabilities (Matrix.create 27 27) in
  Alcotest.(check (float 0.0005)) "every token alike: log 27" 3.296 (Bigram.loss knowing_nothing n);
  let p = Bigram.probabilities n in
  Alcotest.(check (float 0.0005)) "the table: makemore's 2.454" 2.454 (Bigram.loss p n);
  Alcotest.(check (float 0.005)) "in bits a letter" 3.54 (Bigram.bits (Bigram.loss p n));
  Alcotest.(check (float 0.005)) "and without the table" 4.75 (Bigram.bits (log 27.));
  Alcotest.(check (float 0.00005)) "smoothed by 1" 2.4546 (Bigram.loss (Bigram.probabilities ~smoothing:1. n) n);
  (* why smooth: a pair the names never have, in another text *)
  let other = Bigram.counts (Lazy.force tokens) [ "qq" ] in
  Alcotest.(check int) "no name has qq" 0 (int_of_float (Matrix.get n 17 17));
  Alcotest.(check bool) "impossible, so infinitely surprising" true (Bigram.loss p other = infinity);
  Alcotest.(check bool) "smoothed, only very" true (Bigram.loss (Bigram.probabilities ~smoothing:1. n) other < 10.)

let test_sample () =
  let tk = Lazy.force tokens and p = Bigram.probabilities (Lazy.force counts) in
  let state = Lehmer.make 1 in
  let names = List.init 6 (fun _ -> Bigram.sample state tk p) in
  Printf.eprintf "bigram names: %s\n" (String.concat " " names);
  (* the same seed, the same names, on every OCaml *)
  Alcotest.(check (list string)) "the .mli's" [ "qusrh"; "a"; "vanile"; "a"; "mylin"; "jae" ] names;
  (* and every pair in them is a pair some name has *)
  let n = Lazy.force counts in
  List.iter
    (fun name ->
      let rec pairs l = match l with a :: (b :: _ as rest) -> (a, b) :: pairs rest | _ -> [] in
      List.iter
        (fun (a, b) -> Alcotest.(check bool) (name ^ ": a pair names have") true (Matrix.get n a b > 0.))
        (pairs (Tokenizer.bounded tk name)))
    names

(* the lesson: start knowing nothing, walk downhill, arrive at the
   table that counting gave. The .mli's numbers come from here. *)
let test_learned () =
  let n = Lazy.force counts in
  let counted = Bigram.probabilities n in
  let furthest (l : Bigram.learned) : float =
    let p = Bigram.learned_probabilities l in
    let worst = ref 0. in
    Array.iteri (fun i x -> worst := Float.max !worst (Float.abs (x -. counted.data.(i)))) p.data;
    !worst
  in
  let l = ref (Bigram.start 27) in
  Alcotest.(check (float 0.0005)) "before: log 27" 3.296 (Bigram.loss (Bigram.learned_probabilities !l) n);
  for step = 1 to 200 do
    l := Bigram.step n !l;
    if List.mem step [ 1; 10; 50; 200 ] then
      Printf.eprintf "bigram, learned: step %d, loss %.4f, furthest probability %.3f\n" step
        (Bigram.loss (Bigram.learned_probabilities !l) n)
        (furthest !l)
  done;
  Alcotest.(check (float 0.0002)) "the loss counting gave" (Bigram.loss counted n)
    (Bigram.loss (Bigram.learned_probabilities !l) n);
  Alcotest.(check bool) "and the same table, to three decimals" true (furthest !l < 0.001)

let tests =
  [
    t "Tokenizer, characters as numbers" test_tokenizer;
    t "Sampling, the odds and the temperature" test_sampling;
    t "Bigram, makemore's counts" test_counts;
    t "Bigram, the loss in nats and bits" test_loss;
    t "Bigram, names drawn from the table" test_sample;
    t "Bigram, learned: the table counting gave" test_learned;
  ]
