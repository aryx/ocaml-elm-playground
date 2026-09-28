(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_drive.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate
let db (x : float) : float = 20. *. log10 x
let sine f n = Array.init n (fun i -> sin (2. *. Float.pi *. f *. float_of_int i /. rate))

let test_curves () =
  Alcotest.(check (float 1e-9)) "hard: flat from 1" 1. (Drive.curve Hard 3.);
  Alcotest.(check (float 1e-9)) "cubic: flat from 1" 1. (Drive.curve Cubic 3.);
  Alcotest.(check (float 1e-9)) "cubic: odd" (-.Drive.curve Cubic 0.5) (Drive.curve Cubic (-0.5));
  Alcotest.(check (float 1e-9)) "asymmetric: 0 at 0" 0. (Drive.curve Asymmetric 0.);
  Alcotest.(check (float 0.01)) "asymmetric: to -1.29" (-1.29) (Drive.curve Asymmetric (-20.));
  Alcotest.(check (float 0.01)) "asymmetric: to 0.71" 0.71 (Drive.curve Asymmetric 20.)

(* the harmonics a 1 kHz sine gets (bins of 44,100 / 4096 Hz: 1 kHz is
 * not on one, so the loudest bin near each harmonic): tanh only odd
 * ones, the asymmetric curve the 2nd too *)
let harmonic shape k =
  let x = sine 1000. 6096 in
  Drive.process (Drive.create ~oversampling:4 ()) shape ~drive:6. ~mix:1. x;
  let m = Spectrum.of_signal (Array.sub x 2000 4096) in
  let near f =
    let c = Float.to_int (f *. 4096. /. rate) in
    Float.max m.(c) (Float.max m.(c - 1) m.(c + 1))
  in
  db (near (1000. *. float_of_int k) /. near 1000.)

let test_even () =
  Alcotest.(check bool) "tanh: no 2nd harmonic (under -80 dB)" true (harmonic Tanh 2 < -80.);
  Alcotest.(check bool) "tanh: a 3rd (above -30 dB)" true (harmonic Tanh 3 > -30.);
  Alcotest.(check bool) "asymmetric: a 2nd (above -30 dB)" true (harmonic Asymmetric 2 > -30.)

(* the .mli's table: a 5 kHz sine through tanh at +12 dB, the loudest
 * bin below 4 kHz under the note's *)
let alias l =
  let x = sine 5000. 5096 in
  Drive.process (Drive.create ~oversampling:l ()) Tanh ~drive:12. ~mix:1. x;
  let m = Spectrum.of_signal (Array.sub x 1000 4096) in
  let bin f = Float.to_int (Float.round (f *. 4096. /. rate)) in
  let note = ref 0. and worst = ref (0., 0) in
  for k = bin 4900. to bin 5100. do
    note := Float.max !note m.(k)
  done;
  for k = bin 30. to bin 4000. do
    if m.(k) > fst !worst then worst := (m.(k), k)
  done;
  (db (fst !worst /. !note), Spectrum.bin_frequency ~n:4096 (snd !worst))

let test_aliases () =
  let check l expected hz =
    let d, f = alias l in
    Alcotest.(check (float 0.1)) (Printf.sprintf "x%d: the loudest alias (dB)" l) expected d;
    Option.iter (fun hz -> Alcotest.(check (float 11.)) (Printf.sprintf "x%d: at (Hz)" l) hz f) hz
  in
  check 1 (-32.6) (Some 904.);
  check 2 (-58.6) (Some 3198.);
  check 4 (-81.7) None

(* the mix at 0: the sound untouched *)
let test_dry () =
  let x = sine 440. 1000 in
  let y = Array.copy x in
  Drive.process (Drive.create ~oversampling:4 ()) Hard ~drive:24. ~mix:0. y;
  Alcotest.(check (float 1e-12)) "dry" 0. (Array.fold_left Float.max 0. (Array.map2 (fun a b -> Float.abs (a -. b)) x y))

let tests =
  Testo.categorize "Drive"
    [
      t "the curves" test_curves;
      t "symmetric: odd harmonics; asymmetric: even ones too" test_even;
      t "the aliases: none, x2, x4 oversampling" test_aliases;
      t "mix 0: dry" test_dry;
    ]
