(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Where Gpt.mli's numbers come from: microgpt's model trained on
 * makemore's names, one name a step, and the same with each of its
 * ideas taken out; the loss on a thousand names held out.
 *
 *   dune exec scripts/train/measure_gpt.exe            (5,000 steps each, two minutes)
 *   dune exec scripts/train/measure_gpt.exe -- 1000
 *
 * Writes no file. The same seed gives the same losses on every
 * OCaml; the seconds are this machine's. *)

let () =
  let steps = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 5000 in
  (* a step's graph dies at its end: room for it in the minor heap
     (notes_opti_ocaml.md, section 20) *)
  Gc.set { (Gc.get ()) with minor_heap_size = 8 * 1024 * 1024 };
  let tokens = Tokenizer.of_text Makemore_names.text in
  let corpus = Corpus.split (Tokenizer.words Makemore_names.text) in
  let learn = Array.of_list (List.map (Tokenizer.bounded tokens) corpus.learn) in
  let held = List.filteri (fun i _ -> i < 1000) (List.map (Tokenizer.bounded tokens) corpus.held) in
  let run (name : string) (c : Gpt.config) : unit =
    let m = ref (Gpt.make ~seed:1 c) in
    let t0 = Unix.gettimeofday () in
    for i = 1 to steps do
      (* the rate falling to zero over the run, as microgpt's *)
      let rate = 0.01 *. (1. -. (float_of_int (i - 1) /. float_of_int steps)) in
      m := fst (Gpt.step ~rate !m learn.((i - 1) mod Array.length learn))
    done;
    let seconds = Unix.gettimeofday () -. t0 in
    Printf.printf "%-28s %d names, %5.1f s: held-out loss %.4f\n%!" name steps seconds (Gpt.loss !m held)
  in
  let n = Tokenizer.size tokens in
  run "microgpt's" (Gpt.config n);
  run "no attention" (Gpt.config ~attention:false n);
  run "no positions" (Gpt.config ~positions:false n);
  run "no attention, no positions" (Gpt.config ~attention:false ~positions:false n);
  run "one head" (Gpt.config ~heads:1 n)
