(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Gpt: a transformer on scalars, microgpt's *)

let t = Testo.create
let tokens = lazy (Tokenizer.of_text Makemore_names.text)

(* the names as token lists: what it learns from, and some held out *)
let texts =
  lazy
    (let c = Corpus.split (Tokenizer.words Makemore_names.text) in
     let bounded = List.map (Tokenizer.bounded (Lazy.force tokens)) in
     (Array.of_list (bounded c.learn), List.filteri (fun i _ -> i < 200) (bounded c.held)))

let test_sizes () =
  let m = Gpt.make ~seed:1 (Gpt.config 27) in
  Alcotest.(check int) "microgpt's 4,192 numbers" 4192 (Gpt.parameters m);
  Alcotest.(check (list string)) "its matrices"
    [ "wte"; "wpe"; "head"; "0.q"; "0.k"; "0.v"; "0.o"; "0.fc1"; "0.fc2" ]
    (List.map fst m.matrices);
  Alcotest.check_raises "heads that do not divide the width" (Invalid_argument "Gpt.make: the width is not a multiple of the heads")
    (fun () -> ignore (Gpt.make ~seed:1 (Gpt.config ~width:10 ~heads:4 27)))

(* what it answers is a distribution, and what a head looks at is one
   too, over the tokens read so far *)
let test_attention () =
  let m = Gpt.make ~seed:1 (Gpt.config 27) in
  let emm = [ 0; 5; 13; 13 ] in
  let (p, looked) = Gpt.next m emm in
  Alcotest.(check int) "a probability per token" 27 (Array.length p);
  Alcotest.(check (float 1e-9)) "summing to 1" 1. (Array.fold_left ( +. ) 0. p);
  Alcotest.(check int) "one layer of four heads" 4 (List.length looked);
  List.iter
    (fun shares ->
      Alcotest.(check int) "a share per token read" 4 (Array.length shares);
      Alcotest.(check (float 1e-9)) "the shares sum to 1" 1. (Array.fold_left ( +. ) 0. shares))
    looked;
  (* the first token has only itself to look at *)
  let (_, alone) = Gpt.next m [ 0 ] in
  List.iter (fun shares -> Alcotest.(check (array (float 1e-12))) "all of it" [| 1. |] shares) alone;
  (* without attention nothing before the last token can matter *)
  let blind = Gpt.make ~seed:1 (Gpt.config ~attention:false ~positions:false 27) in
  Alcotest.(check (array (float 1e-12))) "the same answer after m, whatever came before"
    (fst (Gpt.next blind [ 0; 5; 13 ]))
    (fst (Gpt.next blind [ 0; 1; 1; 13 ]));
  (* and with it, it does *)
  Alcotest.(check bool) "with attention, what came before changes the answer" true
    (fst (Gpt.next m [ 0; 5; 13 ]) <> fst (Gpt.next m [ 0; 1; 1; 13 ]))

(* the graph's slopes against a nudge: move one number by a hair each
   way, see how the loss moved. A small model, so that every number
   can be tried *)
let test_gradient () =
  let m = Gpt.make ~seed:3 (Gpt.config ~width:4 ~heads:2 ~block:6 27) in
  let text = [ 0; 5; 13; 13; 1; 0 ] in
  let (slopes, _) = Gpt.gradient m text in
  Alcotest.(check int) "a slope per number" (Gpt.parameters m) (Array.length slopes);
  let epsilon = 1e-5 and at = ref 0 and worst = ref 0. in
  List.iter
    (fun (name, (x : Matrix.t)) ->
      Array.iteri
        (fun i v ->
          (* every seventh number, of every matrix *)
          if i mod 7 = 0 then (
            let moved d =
              let data = Array.copy x.data in
              data.(i) <- v +. d;
              let matrices = List.map (fun (n, y) -> if n = name then (n, { x with data }) else (n, y)) m.matrices in
              Gpt.loss { m with matrices } [ text ]
            in
            let nudged = (moved epsilon -. moved (-.epsilon)) /. (2. *. epsilon) in
            worst := Float.max !worst (Float.abs (nudged -. slopes.(!at + i)))))
        x.data;
      at := !at + Array.length x.data)
    m.matrices;
  Printf.eprintf "Gpt, the graph's slopes against a nudge: furthest apart %.2g\n" !worst;
  Alcotest.(check bool) "the same slopes, to six decimals" true (!worst < 1e-6)

let train (c : Gpt.config) (steps : int) : Gpt.t =
  let (learn, _) = Lazy.force texts in
  let m = ref (Gpt.make ~seed:1 c) in
  for i = 1 to steps do
    (* the rate falling to zero over the run, as microgpt's *)
    let rate = 0.01 *. (1. -. (float_of_int (i - 1) /. float_of_int steps)) in
    m := fst (Gpt.step ~rate !m learn.(i - 1))
  done;
  !m

(* three hundred names, one a step: at the table of pairs' 2.454
   already. Without attention it is no worse yet: the ideas separate
   with more training (Gpt.mli's table, measure_gpt's 5,000 steps),
   which a test cannot wait for *)
let test_training () =
  let (_, held) = Lazy.force texts in
  let t0 = Unix.gettimeofday () in
  let m = train (Gpt.config 27) 300 in
  let seconds = Unix.gettimeofday () -. t0 in
  let loss = Gpt.loss m held in
  let blind = Gpt.loss (train (Gpt.config ~attention:false 27) 300) held in
  let draws = Lehmer.make 3 in
  let names = List.init 6 (fun _ -> Gpt.sample ~temperature:0.5 draws (Lazy.force tokens) m) in
  Printf.eprintf "Gpt, 300 names (%.1f s): held-out loss %.3f, without attention %.3f; %s\n" seconds loss blind
    (String.concat " " names);
  Alcotest.(check bool) "far under knowing nothing (3.296)" true (loss < 2.65 && blind < 2.65)

let test_weights () =
  let m = Gpt.make ~seed:1 (Gpt.config ~width:8 ~heads:2 27) in
  let back =
    match Result.bind (Weights.of_string (Weights.to_string (Gpt.to_weights m))) Gpt.of_weights with
    | Ok back -> back
    | Error why -> Alcotest.fail why
  in
  Alcotest.(check bool) "its sizes, from the notes" true (back.config = m.config);
  Alcotest.(check (array (float 1e-6))) "the same opinion" (fst (Gpt.next m [ 0; 5; 13 ])) (fst (Gpt.next back [ 0; 5; 13 ]));
  Alcotest.(check bool) "another model's file" true
    (Result.is_error (Gpt.of_weights (Ngram_mlp.to_weights (Ngram_mlp.make ~seed:1 27))))

let tests =
  [
    t "Gpt, microgpt's sizes" test_sizes;
    t "Gpt, attention's shares" test_attention;
    t "Gpt, its gradient against a nudge" test_gradient;
    t "Gpt, three hundred names" test_training;
    t "Gpt, written to a file and read back" test_weights;
  ]
