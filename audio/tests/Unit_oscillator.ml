(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Oscillator's band-limited waveforms: PolyBLEP's two samples
 * per jump, and the aliases gone from the spectrum *)

let t = Testo.create
let db (x : float) : float = 20. *. log10 x

let test_polyblep () =
  let dt = 0.1 in
  Alcotest.(check (float 1e-9)) "at the jump: -1" (-1.) (Oscillator.polyblep ~dt 0.);
  Alcotest.(check (float 1e-9)) "a sample after: 0" 0. (Oscillator.polyblep ~dt 0.1);
  Alcotest.(check (float 1e-9)) "half a sample after: -0.25" (-0.25) (Oscillator.polyblep ~dt 0.05);
  Alcotest.(check (float 1e-9)) "half a sample before: 0.25" 0.25 (Oscillator.polyblep ~dt 0.95);
  Alcotest.(check (float 1e-9)) "far from it: 0" 0. (Oscillator.polyblep ~dt 0.5);
  (* the square at its jump up: the middle, 0 *)
  Alcotest.(check (float 1e-9)) "the square at 0: 0" 0. (Oscillator.wave_band_limited Square ~dt 0.);
  Alcotest.(check (float 1e-9)) "the square at 0.5: 0" 0. (Oscillator.wave_band_limited Square ~dt 0.5);
  Alcotest.(check (float 1e-9)) "the sawtooth at 0: 0" 0. (Oscillator.wave_band_limited Sawtooth ~dt 0.)

(* 4096 samples; a 1001.3 Hz square, 93 periods exactly, so its
 * harmonics fall on bins 93 k, their aliases on other bins, and nothing
 * leaks *)
let n = 4096
let bin = 93

(* its spectrum, naive or band-limited *)
let spectrum ~(band_limited : bool) (w : Oscillator.waveform) : float array =
  let f = Spectrum.bin_frequency ~n bin in
  let x = Oscillator.render ~band_limited w ~frequency:f (float_of_int n /. float_of_int Signal.rate +. 0.001) in
  Spectrum.magnitudes (Spectrum.fft (Array.sub x 0 n))

(* the loudest bin that isn't a harmonic, below [below] Hz, in dB *)
let loudest_alias ?(below = Signal.nyquist) (m : float array) : float =
  let a = ref 0. in
  Array.iteri (fun k v -> if k mod bin <> 0 && Spectrum.bin_frequency ~n k < below && v > !a then a := v) m;
  db !a

(* the loudest alias, naive then band-limited, in dB, over the whole
 * band and below 5 kHz: PolyBLEP is weakest just under Nyquist (the
 * aliases folded from just above it), strongest far below it, where the
 * aliases would be out of tune among the low harmonics; the triangle's,
 * already quieter, by PolyBLAMP (its corners, not jumps) *)
let test_aliases () =
  List.iter
    (fun (w, below, naive_db, limited_db) ->
      let what = Printf.sprintf "%s, the loudest alias below %.0f Hz (dB)" (Oscillator.name w) below in
      Alcotest.(check (float 0.1)) ("naive " ^ what) naive_db (loudest_alias ~below (spectrum ~band_limited:false w));
      Alcotest.(check (float 0.1)) ("band-limited " ^ what) limited_db (loudest_alias ~below (spectrum ~band_limited:true w)))
    [
      (Square, Signal.nyquist, -25.1, -33.8);
      (Square, 5000., -30.2, -71.9);
      (Sawtooth, Signal.nyquist, -31.2, -39.8);
      (Sawtooth, 5000., -36.0, -76.0);
      (Triangle, Signal.nyquist, -56.3, -64.9);
      (Triangle, 5000., -66.3, -109.6);
    ]

(* the price: the harmonics near the top a little quieter (the
 * polynomial smooths a little more than the jump's aliases): the 5th
 * (5 kHz) -0.37 dB, the 9th -1.21, the 19th -5.69 *)
let test_harmonics_kept () =
  let naive = spectrum ~band_limited:false Square and limited = spectrum ~band_limited:true Square in
  List.iter
    (fun (k, expected) ->
      Alcotest.(check (float 0.01)) (Printf.sprintf "harmonic %d (dB)" k) expected (db limited.(bin * k) -. db naive.(bin * k)))
    [ (1, -0.01); (5, -0.37); (9, -1.21); (19, -5.69) ]

let tests =
  Testo.categorize "Oscillator"
    [
      t "PolyBLEP: the correction, and the middle of the jumps" test_polyblep;
      t "the loudest alias, naive vs band-limited" test_aliases;
      t "the harmonics kept, the top ones a little quieter" test_harmonics_kept;
    ]
