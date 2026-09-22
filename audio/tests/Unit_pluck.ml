(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Pluck: the .mli's example (A3's delay line, its pitch), the
 * string dying away, its brightness falling as it rings, and its tuning
 * with and without the all-pass *)

let t = Testo.create

let rms (x : Signal.t) (from : float) (until : float) : float =
  let i = Signal.samples from and j = Signal.samples until in
  let e = ref 0. in
  for k = i to j - 1 do
    e := !e +. (x.(k) *. x.(k))
  done;
  sqrt (!e /. float_of_int (j - i))

(* the pitch, precisely: the spectrum's peak near [near] (16384 samples
 * from 0.02 s, Hann-windowed: bins of 2.7 Hz), placed between its bins
 * by the parabola through the three around it (on their logarithms) *)
let pitch ~(near : float) (x : Signal.t) : float =
  let n = 16384 in
  let m = Spectrum.magnitudes (Spectrum.fft (Spectrum.hann (Array.sub x (Signal.samples 0.02) n))) in
  let bin f = int_of_float (f *. float_of_int n /. float_of_int Signal.rate) in
  let k = ref (bin (0.9 *. near)) in
  for i = bin (0.9 *. near) to bin (1.1 *. near) do
    if m.(i) > m.(!k) then k := i
  done;
  let a = log m.(!k - 1) and b = log m.(!k) and c = log m.(!k + 1) in
  Spectrum.bin_frequency ~n !k +. (0.5 *. (a -. c) /. (a -. (2. *. b) +. c) *. float_of_int Signal.rate /. float_of_int n)

let cents (f : float) (target : float) : float = 1200. *. Float.log2 (f /. target)

let test_pluck () =
  Alcotest.(check int) "A3's delay line: 200 samples" 200 (Pluck.period 220.);
  Alcotest.(check int) "A4's: 100" 100 (Pluck.period 440.);
  let x = Pluck.render ~frequency:220. 2. in
  Alcotest.(check (float 1.)) "A3, tuned: 220 Hz (cents)" 0. (cents (pitch ~near:220. x) 220.);
  (* dying away: 0.44 over the first 0.1 s, 0.09 a second later, 0.03 at
   * 2 s *)
  Alcotest.(check (float 0.001)) "the pluck's level" 0.443 (rms x 0. 0.1);
  Alcotest.(check (float 0.001)) "at 1 s" 0.092 (rms x 0.9 1.);
  Alcotest.(check (float 0.001)) "at 2 s" 0.033 (rms x 1.9 2.);
  (* the high harmonics dying first: the twang, then the hum *)
  Alcotest.(check (float 1.)) "its brightness at the pluck (Hz)" 3803. (Unit_synth.centroid (Array.sub x 0 4096));
  Alcotest.(check (float 1.)) "at 1.5 s (Hz)" 642. (Unit_synth.centroid (Array.sub x (Signal.samples 1.5) 4096))

(* the whole samples only: flat by what the .mli predicts, the line's
 * rounding (-4.7 cents at 440, -35 at 2 kHz); the all-pass's fraction:
 * within a quarter of a cent *)
let test_tuning () =
  List.iter
    (fun (target, untuned_cents) ->
      let untuned = pitch ~near:target (Pluck.render ~tuned:false ~frequency:target 0.5)
      and tuned = pitch ~near:target (Pluck.render ~frequency:target 0.5) in
      Alcotest.(check (float 0.1)) (Printf.sprintf "%.0f Hz, untuned (cents)" target) untuned_cents (cents untuned target);
      Alcotest.(check (float 0.25)) (Printf.sprintf "%.0f Hz, tuned (cents)" target) 0. (cents tuned target))
    [ (440., -4.7); (1000., -15.6); (2000., -35.0) ]

let tests =
  Testo.categorize "Pluck"
    [ t "Karplus-Strong: the delay line, the pitch, the ring" test_pluck; t "tuned: the all-pass's fraction" test_tuning ]
