(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tb303.mli *)

let t = Testo.create
let db (x : float) : float = 20. *. log10 x

let pattern (text : string) : Sequencer.step array =
  match Voice_tb303.pattern_of_string text with Ok p -> p | Error e -> failwith e

(* a voice running [text] at 120 BPM, [samples] of it a sample at a
 * time, [probe] read after each: its values *)
let trace (text : string) (samples : int) (probe : Voice_tb303.t -> float) : float array =
  let v = Voice_tb303.create { Voice_tb303.initial with bpm = 120.; decay = 0.5; pattern = pattern text } in
  let i = Voice_tb303.instrument v in
  Voice_tb303.run v true;
  Array.init samples (fun _ ->
      i.fill { left = [| 0. |]; right = [| 0. |] };
      probe v)

let test_decay () =
  (* the note, then rests: a pattern of one step would start it again *)
  let at_200ms text = (trace text (Signal.samples 0.2) Voice_tb303.envelope).(Signal.samples 0.2 - 1) in
  Alcotest.(check (float 0.1)) "an accented note: 60 dB down after 200 ms" (-60.) (db (at_200ms "C2* . . ."));
  (* Decay 0.5: 0.2 x 10^0.5 = 0.632 s to -60 dB; after 0.2 s, -19 dB *)
  Alcotest.(check (float 0.1)) "a normal one at Decay 0.5: -19 dB" (-18.97) (db (at_200ms "C2 . . ."))

(* the sweep's highest in each of the first three steps (5512.5 samples
 * each) *)
let test_sweep () =
  let s = trace "C2* C2* C2* . . ." (3 * 5513) Voice_tb303.sweep in
  let peak k = Array.fold_left Float.max 0. (Array.sub s (k * 5512) 5512) in
  let p1 = peak 0 and p2 = peak 1 and p3 = peak 2 in
  Alcotest.(check bool) (Printf.sprintf "the accents climb: %.3f < %.3f < %.3f" p1 p2 p3) true (p1 < p2 && p2 < p3);
  Alcotest.(check (float 0.001)) "the first" 0.283 p1;
  Alcotest.(check (float 0.001)) "the second" 0.374 p2;
  Alcotest.(check (float 0.001)) "the third" 0.402 p3

(* C2 sliding into C3 (step 1, at sample 5513): 60 ms later, 63% of the
 * octave *)
let test_slide () =
  let at = 5513 + Signal.samples 0.06 in
  let p = trace "C2~ C3 . ." (at + 1) Voice_tb303.pitch in
  Alcotest.(check (float 0.05)) "the pitch 63% of the way" (36. +. (12. *. (1. -. exp (-1.)))) p.(at - 1)

(* the gate closing at half a step (sample 2757): the volume closing with
 * a 3 ms time constant, 60 dB down 25 ms later (e^(-25/3): -72 dB) *)
let test_gate () =
  let v = Voice_tb303.create { Voice_tb303.initial with bpm = 120.; pattern = pattern "C2 . . ." } in
  let i = Voice_tb303.instrument v in
  Voice_tb303.run v true;
  let out = Array.init 5000 (fun _ ->
      let b = { Signal.left = [| 0. |]; right = [| 0. |] } in
      i.fill b;
      b.left.(0)) in
  let peak a b = Array.fold_left Float.max 0. (Array.map Float.abs (Array.sub out a (b - a))) in
  Alcotest.(check bool) "60 dB under, 25 ms after the gate's close" true
    (db (peak (2757 + Signal.samples 0.025) (2757 + Signal.samples 0.035) /. peak 1000 2700) < -60.)

let test_text () =
  List.iter
    (fun (name, p) ->
      match Voice_tb303.of_string (Voice_tb303.to_string p) with
      | Ok p' -> if p' <> p then Alcotest.failf "%s: not the same after writing and reading" name
      | Error e -> Alcotest.failf "%s: %s" name e)
    Voice_tb303.presets;
  Alcotest.(check string) "a pattern written" "C2 C2~ C3* . Eb2 F#3*~" (Voice_tb303.pattern_to_string (pattern "C2 C2~ C3* - Eb2 F#3*~"));
  Alcotest.(check bool) "a bad step refused" true (Result.is_error (Voice_tb303.pattern_of_string "C2 H2"))

(* each preset: a bar of 16 steps and a little after, a frame's block at
 * a time *)
let bar (p : Voice_tb303.patch) : Signal.t =
  let v = Voice_tb303.create p in
  let i = Voice_tb303.instrument v in
  Voice_tb303.run v true;
  let frames = int_of_float (Float.ceil (16. *. Sequencer.samples_per_step p.bpm /. 735.)) + 20 in
  Array.concat
    (List.init frames (fun _ ->
         let b = { Signal.left = Array.make 735 0.; right = Array.make 735 0. } in
         i.fill b;
         b.left))

let tests =
  Testo.categorize "TB-303"
    (List.map
       (fun (name, p) -> t ("golden WAV: " ^ name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" ("tb303_" ^ name) (bar p)))
       Voice_tb303.presets
    @ [
        t "the accent: the shortest decay" test_decay;
        t "the accent sweep: three accents climbing" test_sweep;
        t "the slide: 63% after 60 ms" test_slide;
        t "the gate: silent after its close" test_gate;
        t "the patterns as text" test_text;
      ])
