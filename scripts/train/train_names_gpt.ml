(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Trains Gpt (microgpt's sizes) on makemore's names and writes what
 * it learned: AiShannon's third opponent.
 *
 *   dune exec scripts/train/train_names_gpt.exe -- data/weights/names_gpt/names_gpt.weights
 *   dune exec scripts/train/train_names_gpt.exe -- out.weights 2000    (fewer names)
 *
 * 30,000 names by default, one a step, about half a minute. The same
 * seed gives the same file, byte for byte, on every OCaml (Lehmer).
 *
 * It learns from Corpus.split's first 80% of the names, in their
 * shuffled order, round and round (30,000 steps are a little more than
 * once through), and never sees the rest: the loss printed, and written
 * in the file's notes, is on the 10% held out. The rate is Adam's 0.01
 * falling in a straight line to zero over the run, as microgpt's. *)

let () =
  let out = if Array.length Sys.argv > 1 then Sys.argv.(1) else failwith "usage: train_names_gpt <out.weights> [steps]" in
  let steps = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 30_000 in
  (* a step's graph dies at its end: room for it in the minor heap
     (notes_opti_ocaml.md, section 20) *)
  Gc.set { (Gc.get ()) with minor_heap_size = 8 * 1024 * 1024 };
  let tokens = Tokenizer.of_text Makemore_names.text in
  let corpus = Corpus.split (Tokenizer.words Makemore_names.text) in
  let bounded = List.map (Tokenizer.bounded tokens) in
  let learn = Array.of_list (bounded corpus.learn) and held = bounded corpus.held in
  let seed = 1 in
  let m = ref (Gpt.make ~seed (Gpt.config (Tokenizer.size tokens))) in
  let t0 = Unix.gettimeofday () in
  for step = 1 to steps do
    let rate = 0.01 *. (1. -. (float_of_int (step - 1) /. float_of_int steps)) in
    m := fst (Gpt.step ~rate !m learn.((step - 1) mod Array.length learn));
    (* along the way, on 300 of the held-out names *)
    if step mod (max 1 (steps / 15)) = 0 then
      Printf.printf "step %6d  %4.0f s  held-out loss %.4f\n%!" step (Unix.gettimeofday () -. t0)
        (Gpt.loss !m (List.filteri (fun i _ -> i < 300) held))
  done;
  let loss = Gpt.loss !m held in
  let notes =
    [
      ("model", "Gpt, microgpt's sizes: 16 wide, 4 heads, 1 layer, 16 positions");
      ("data", "data/names (makemore's names.txt), Corpus.split's first 80%");
      ("trainer", "scripts/train/train_names_gpt");
      ("seed", string_of_int seed);
      ("steps", Printf.sprintf "%d names, one a step, Adam 0.01 falling to 0" steps);
      ("held-out-loss", Printf.sprintf "%.4f" loss);
    ]
  in
  let oc = open_out_bin out in
  output_string oc (Weights.to_string (Gpt.to_weights ~notes !m));
  close_out oc;
  Printf.printf "%s: held-out loss %.4f (the table of pairs: 2.454), %.0f s\n" out loss (Unix.gettimeofday () -. t0)
