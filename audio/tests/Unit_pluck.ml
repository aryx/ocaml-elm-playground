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
 * string dying away, and its brightness falling as it rings *)

let t = Testo.create

let rms (x : Signal.t) (from : float) (until : float) : float =
  let i = Signal.samples from and j = Signal.samples until in
  let e = ref 0. in
  for k = i to j - 1 do
    e := !e +. (x.(k) *. x.(k))
  done;
  sqrt (!e /. float_of_int (j - i))

let test_pluck () =
  Alcotest.(check int) "A3's delay line: 200 samples" 200 (Pluck.period 220.);
  Alcotest.(check int) "A4's: 100" 100 (Pluck.period 440.);
  let x = Pluck.render ~frequency:220. 2. in
  (* its pitch: the loudest bin of 8192 samples from 0.2 s, 44,100 /
   * 8192 = 5.4 Hz a bin, near 219.95 *)
  let m = Spectrum.magnitudes (Spectrum.fft (Array.sub x (Signal.samples 0.2) 8192)) in
  let peak = Spectrum.bin_frequency ~n:8192 (Spectrum.peak m) in
  Alcotest.(check (float (44100. /. 8192.))) "its pitch, within a bin of 219.95 Hz" 219.95 peak;
  (* dying away: 0.68, 0.23 a second later, 0.09 at 2 s *)
  (* dying away: 0.49 over the first 0.1 s, 0.09 a second later, 0.03 at
   * 2 s *)
  Alcotest.(check (float 0.001)) "the pluck's level" 0.485 (rms x 0. 0.1);
  Alcotest.(check (float 0.001)) "at 1 s" 0.090 (rms x 0.9 1.);
  Alcotest.(check (float 0.001)) "at 2 s" 0.032 (rms x 1.9 2.);
  (* the high harmonics dying first: the twang, then the hum *)
  Alcotest.(check (float 1.)) "its brightness at the pluck (Hz)" 4312. (Unit_synth.centroid (Array.sub x 0 4096));
  Alcotest.(check (float 1.)) "at 1.5 s (Hz)" 644. (Unit_synth.centroid (Array.sub x (Signal.samples 1.5) 4096))

let tests = Testo.categorize "Pluck" [ t "Karplus-Strong: the delay line, the pitch, the ring" test_pluck ]
