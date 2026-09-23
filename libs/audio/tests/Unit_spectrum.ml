(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Spectrum: the .mli's examples, the DFT against the FFT, a
 * sine's one peak, a square's odd harmonics, Parseval *)

let t = Testo.create
let size (re, im) = sqrt ((re *. re) +. (im *. im))

let test_examples () =
  Alcotest.(check (array (float 1e-9))) "an impulse: every bin at 1" (Array.make 8 1.)
    (Array.map size (Spectrum.dft [| 1.; 0.; 0.; 0.; 0.; 0.; 0.; 0. |]));
  let cosine = Array.init 8 (fun i -> cos (2. *. Float.pi *. float_of_int i /. 8.)) in
  Alcotest.(check (array (float 1e-9))) "a cosine, one cycle in 8: bins 1 and 7 at 4" [| 0.; 4.; 0.; 0.; 0.; 0.; 0.; 4. |]
    (Array.map size (Spectrum.dft cosine))

let test_fft_is_dft () =
  let st = Random.State.make [| 31 |] in
  List.iter
    (fun n ->
      let x = Array.init n (fun _ -> Random.State.float st 2. -. 1.) in
      let a = Spectrum.dft x and b = Spectrum.fft x in
      Array.iteri
        (fun k (re, im) ->
          let (re', im') = b.(k) in
          if Float.abs (re -. re') > 1e-9 || Float.abs (im -. im') > 1e-9 then Alcotest.failf "n = %d, bin %d differs" n k)
        a)
    [ 1; 2; 8; 64; 256 ]

(* 1024 samples: bin k is k 44,100 / 1024 = 43.07 k Hz; frequencies
 * right on a bin don't leak *)
let n = 1024

let test_sine () =
  let f = Spectrum.bin_frequency ~n 10 in
  let x = Array.init n (fun i -> 0.5 *. sin (2. *. Float.pi *. f *. float_of_int i /. float_of_int Signal.rate)) in
  let m = Spectrum.magnitudes (Spectrum.fft x) in
  Alcotest.(check int) "one peak, at bin 10 (430.7 Hz)" 10 (Spectrum.peak m);
  Alcotest.(check (float 1e-9)) "its amplitude, 0.5" 0.5 m.(10);
  Array.iteri (fun k v -> if k <> 10 && v > 1e-9 then Alcotest.failf "bin %d: %g, not 0" k v) m

(* a square of period 64 samples (689 Hz, bin 16): odd harmonics only,
 * at about 4 / (pi k) (1.27, 0.42, 0.25...; slightly more for a sampled
 * square, whose harmonics fold), the even ones at 0 *)
let test_square () =
  let x = Array.init n (fun i -> if i mod 64 < 32 then 1. else -1.) in
  let m = Spectrum.magnitudes (Spectrum.fft x) in
  List.iter
    (fun k ->
      let expected = 4. /. (Float.pi *. float_of_int k) in
      if Float.abs (m.(16 * k) -. expected) > 0.02 *. expected then
        Alcotest.failf "harmonic %d: %g, not about %g" k m.(16 * k) expected)
    [ 1; 3; 5; 7 ];
  List.iter (fun k -> if m.(16 * k) > 1e-9 then Alcotest.failf "harmonic %d: %g, not 0" k m.(16 * k)) [ 2; 4; 6 ]

(* the energy is the same in time and in frequency: sum x^2 = sum |X|^2
 * / N (Parseval, 1799; Rayleigh, 1889) *)
let test_parseval () =
  let st = Random.State.make [| 37 |] in
  let x = Array.init 512 (fun _ -> Random.State.float st 2. -. 1.) in
  let time = Array.fold_left (fun s v -> s +. (v *. v)) 0. x in
  let freq = Array.fold_left (fun s c -> s +. (size c ** 2.)) 0. (Spectrum.fft x) /. 512. in
  Alcotest.(check (float 1e-6)) "the same energy" time freq

let tests =
  Testo.categorize "Spectrum"
    [
      t "the .mli's examples: an impulse, a cosine" test_examples;
      t "the FFT = the DFT" test_fft_is_dft;
      t "a sine: one peak" test_sine;
      t "a square: odd harmonics at 4 / (pi k)" test_square;
      t "Parseval: the same energy" test_parseval;
    ]
