(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Envelope and Mix: the .mli's worked examples, the click *)

let t = Testo.create

let adsr : Envelope.t = { attack = 0.01; decay = 0.1; sustain = 0.5; release = 0.2 }

let test_adsr () =
  [ (0.005, 0.5); (0.06, 0.75); (0.3, 0.5); (0.6, 0.25); (0.7, 0.); (0.9, 0.) ]
  |> List.iter (fun (time, level) ->
         Alcotest.(check (float 1e-9)) (Printf.sprintf "at %g s" time) level (Envelope.level adsr ~held:0.5 time));
  Alcotest.(check (float 1e-9)) "let go during the attack: released from there" 0.25
    (Envelope.level adsr ~held:0.005 (0.005 +. 0.1));
  Alcotest.(check (float 1e-9)) "silent for good at 0.7 s" 0.7 (Envelope.duration adsr ~held:0.5)

(* the click: a tone cut at once jumps by nearly its full amplitude
 * between two samples; enveloped, it ends in silence *)
let test_click () =
  let tone = Oscillator.render Sine ~frequency:440. 0.25 in
  let last s = Float.abs s.(Array.length s - 1) in
  (* 0.25 s of 440 Hz: 110 periods exactly, so it ends near 0 by luck;
   * cut it at 0.2505 s instead, near a peak *)
  let cut = Array.sub (Oscillator.render Sine ~frequency:440. 0.3) 0 (Signal.samples 0.2505) in
  if last cut < 0.5 then Alcotest.failf "the cut tone should end high, not %g" (last cut);
  let enveloped = Envelope.apply (Envelope.percussive ~attack:0.005 ~decay:0.245) ~held:0.25 tone in
  if last enveloped > 0.01 then Alcotest.failf "the enveloped one should end silent, not %g" (last enveloped)

let test_mix () =
  Alcotest.(check (float 1e-2)) "half the amplitude: -6.02 dB" (-6.02) (Mix.decibels 0.5);
  Alcotest.(check (float 1e-9)) "-20 dB: a tenth" 0.1 (Mix.of_decibels (-20.));
  let sine = Oscillator.render Sine ~frequency:440. 0.1 in
  let two = Mix.add [ sine; sine ] in
  let peak s = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. s in
  Alcotest.(check (float 1e-6)) "two full sines: a peak of 2" 2. (peak two);
  Alcotest.(check (float 1e-6)) "hard: cut at 1" 1. (peak (Mix.limit two));
  Alcotest.(check (float 1e-6)) "soft: tanh 2" (tanh 2.) (peak (Mix.limit ~soft:true two));
  Alcotest.(check int) "added: as long as the longest" 4410 (Array.length (Mix.add [ sine; Array.sub sine 0 100 ]));
  Alcotest.(check int) "delayed by 0.1 s" 8820 (Array.length (Mix.delay 0.1 sine))

let tests =
  Testo.categorize "Envelopes and mixing"
    [ t "ADSR: the worked example" test_adsr; t "the click, and the envelope's cure" test_click; t "Mix: decibels, clipping" test_mix ]
