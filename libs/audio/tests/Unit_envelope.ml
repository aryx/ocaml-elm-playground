(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Envelope and Mix: the .mli's worked examples, the click *)

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

(* the live envelope, played as the example: the gate on at 0, off at
 * 0.5 s, 0.8 s of it in blocks of 735 *)
let live (curve : Envelope.curve) : Signal.t =
  let r = Envelope.start () and block = Array.make 735 0. in
  Envelope.gate_on r;
  Array.concat
    (List.init 48 (fun b ->
         (* the gate closes at the block holding 0.5 s: sample 22,050 is
          * block 30's first *)
         if b = 30 then Envelope.gate_off r;
         Envelope.fill curve adsr r block;
         Array.copy block))

let test_live_linear () =
  let x = live Linear in
  (* the same levels as the offline envelope, within a sample's step of
   * the attack (1 / 441: the live one steps, then says) *)
  Array.iteri
    (fun i v ->
      let expected = Envelope.level adsr ~held:0.5 (float_of_int i /. float_of_int Signal.rate) in
      if Float.abs (v -. expected) > 0.0023 then Alcotest.failf "sample %d: %g, offline %g" i v expected)
    x

let at (x : Signal.t) (seconds : float) : float = x.(int_of_float (seconds *. float_of_int Signal.rate) - 1)

let test_live_exponential () =
  let x = live Exponential in
  (* the .mli's worked example *)
  (* halfway, 220 of the attack's 441 samples: 0.633, half a sample
   * short of the 0.634 at 220.5 *)
  Alcotest.(check (float 0.0015)) "halfway up: 0.634, not 0.5" 0.634 (at x 0.005);
  let first_full = ref 0 in
  (try Array.iteri (fun i v -> if v >= 1. then (first_full := i + 1; raise Exit)) x with Exit -> ());
  Alcotest.(check int) "at 1 after 441 samples, 10 ms" 441 !first_full;
  Alcotest.(check (float 0.0001)) "halfway through the decay: 0.5158" 0.5158 (at x 0.06);
  Alcotest.(check (float 0.0001)) "at its end: 0.5005" 0.5005 (at x 0.11);
  Alcotest.(check (float 0.0001)) "halfway through the release: 0.0158" 0.0158 (at x 0.6);
  Alcotest.(check (float 0.0001)) "at its end, -60 dB: 0.0005" 0.0005 (at x 0.7)

let test_gate () =
  let r = Envelope.start () and block = Array.make 100 0. in
  let e : Envelope.t = { attack = 0.01; decay = 0.1; sustain = 0.5; release = 0.01 } in
  Alcotest.(check bool) "idle at first" true (Envelope.stage r = Idle);
  Envelope.gate_on r;
  Envelope.fill Linear e r block;
  let level = Envelope.current r in
  (* let go during the attack, then pressed again: the attack goes on
   * from the level reached, down a little, no jump to 0 *)
  Envelope.gate_off r;
  Envelope.fill Linear e r block;
  Alcotest.(check bool) "released" true (Envelope.stage r = Release);
  let down = Envelope.current r in
  if not (down < level && down > 0.) then Alcotest.failf "the release from %g: %g" level down;
  Envelope.gate_on r;
  Envelope.fill Linear e r block;
  Alcotest.(check (float 1e-9)) "again from there, not from 0" (down +. (100. /. 441.)) block.(99);
  Envelope.gate_off r;
  for _ = 1 to 10 do
    Envelope.fill Exponential e r block
  done;
  Alcotest.(check bool) "silent: idle again" true (Envelope.stage r = Idle);
  Alcotest.(check (float 0.)) "at 0" 0. (Envelope.current r)

let tests =
  Testo.categorize "Envelopes and mixing"
    [
      t "ADSR: the worked example" test_adsr;
      t "the click, and the envelope's cure" test_click;
      t "Mix: decibels, clipping" test_mix;
      t "live, straight: the same levels" test_live_linear;
      t "live, exponential: the worked example" test_live_exponential;
      t "the gate: released and pressed again" test_gate;
    ]
