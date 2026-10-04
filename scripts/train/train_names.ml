(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Trains Ngram_mlp on makemore's names and writes what it learned:
 * the network AiShannon plays with.
 *
 *   dune exec scripts/train/train_names.exe -- data/weights/names_mlp/names_mlp.weights
 *   dune exec scripts/train/train_names.exe -- out.weights 5000    (fewer steps)
 *
 * 60,000 batches of 32 by default, about eleven minutes. The same
 * seeds give the same file, byte for byte, on every OCaml (Lehmer).
 *
 * It learns from Corpus.split's first 80% of the names and never sees
 * the rest: the loss printed, and written in the file's notes, is on
 * the 10% held out, and AiShannon's hidden names come from there too.
 * The learning rate is Adam's 0.01, a tenth of it for the last
 * quarter, where the loss settles. *)

let () =
  let out = if Array.length Sys.argv > 1 then Sys.argv.(1) else failwith "usage: train_names <out.weights> [steps]" in
  let steps = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 60_000 in
  (* a step's graph dies at its end: room for it in the minor heap
     (notes_opti_ocaml.md, section 20) *)
  Gc.set { (Gc.get ()) with minor_heap_size = 8 * 1024 * 1024 };
  let tokens = Tokenizer.of_text Makemore_names.text in
  let corpus = Corpus.split (Tokenizer.words Makemore_names.text) in
  let learn = Ngram_mlp.examples tokens ~context:3 corpus.learn in
  let held = Ngram_mlp.examples tokens ~context:3 corpus.held in
  let seed = 1 and batch = 32 in
  let draws = Lehmer.make 7 in
  let m = ref (Ngram_mlp.make ~seed (Tokenizer.size tokens)) in
  let t0 = Unix.gettimeofday () in
  for step = 1 to steps do
    let rate = if step > steps * 3 / 4 then 0.001 else 0.01 in
    m := Ngram_mlp.step ~rate !m (Ngram_mlp.batch draws learn batch);
    (* along the way, on a tenth of the held-out names: all of them
       is a second, and twenty of those would be the training's time *)
    if step mod (max 1 (steps / 20)) = 0 then
      Printf.printf "step %6d  %4.0f s  held-out loss %.4f\n%!" step (Unix.gettimeofday () -. t0)
        (Ngram_mlp.loss !m (Array.sub held 0 (Array.length held / 10)))
  done;
  let loss = Ngram_mlp.loss !m held in
  let notes =
    [
      ("model", "Ngram_mlp, makemore's MLP: 3 letters back, 2 coordinates a letter, 100 neurons");
      ("data", "data/names (makemore's names.txt), Corpus.split's first 80%");
      ("trainer", "scripts/train/train_names");
      ("seed", string_of_int seed);
      ("steps", Printf.sprintf "%d batches of %d, Adam 0.01 then 0.001 for the last quarter" steps batch);
      ("held-out-loss", Printf.sprintf "%.4f" loss);
    ]
  in
  let oc = open_out_bin out in
  output_string oc (Weights.to_string (Ngram_mlp.to_weights ~notes !m));
  close_out oc;
  Printf.printf "%s: held-out loss %.4f (the table of pairs: 2.454), %.0f s\n" out loss (Unix.gettimeofday () -. t0)
