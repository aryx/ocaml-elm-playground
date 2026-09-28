(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_diode_ladder.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate
let db (x : float) : float = 20. *. log10 x

(* a quiet sine (the input's tanh straight), a second of it through the
 * filter, its gain over the second half *)
let sine f n = Array.init n (fun i -> 0.1 *. sin (2. *. Float.pi *. f *. float_of_int i /. rate))

let peak x a b =
  let p = ref 0. in
  for i = a to b do
    p := Float.max !p (Float.abs x.(i))
  done;
  !p

let diode ?(k = 0.) f =
  let x = sine f 44100 in
  Diode_ladder.process (Diode_ladder.create ()) ~cutoff:(Array.make 44100 500.) ~resonance:k x;
  db (peak x 22050 44099 /. 0.1)

let moog f =
  let x = sine f 44100 in
  Moog_ladder.process (Moog_ladder.create ()) Zero_delay ~cutoff:(Array.make 44100 500.) ~resonance:0. x;
  db (peak x 22050 44099 /. 0.1)

let test_slope () =
  Alcotest.(check (float 0.05)) "at the cutoff: -21.3 dB" (-21.33) (diode 500.);
  Alcotest.(check (float 0.05)) "1 -> 2 kHz: -15.8 dB, the \"18 dB\"" (-15.79) (diode 2000. -. diode 1000.);
  Alcotest.(check (float 0.05)) "2 -> 4 kHz" (-20.66) (diode 4000. -. diode 2000.);
  Alcotest.(check (float 0.05)) "4 -> 8 kHz" (-25.75) (diode 8000. -. diode 4000.);
  Alcotest.(check (float 0.05)) "the Moog's, 1 -> 2 kHz" (-21.43) (moog 2000. -. moog 1000.)

let test_bass () =
  let x = Array.make 44100 0.05 in
  Diode_ladder.process (Diode_ladder.create ()) ~cutoff:(Array.make 44100 500.) ~resonance:3. x;
  Alcotest.(check (float 1e-6)) "a constant at k = 3: a quarter" (tanh 0.05 /. 4.) x.(44099)

(* a click's response growing: ringing on its own *)
let grows k =
  let x = Array.make 30000 0. in
  x.(0) <- 0.1;
  Diode_ladder.process (Diode_ladder.create ()) ~cutoff:(Array.make 30000 500.) ~resonance:k x;
  let energy a b = Array.fold_left (fun s v -> s +. (v *. v)) 0. (Array.sub x a (b - a)) in
  let late = energy 25000 30000 in
  Float.is_nan late || late > energy 5000 10000

let test_resonance () =
  let lo = ref 1. and hi = ref 40. in
  for _ = 1 to 25 do
    let mid = (!lo +. !hi) /. 2. in
    if grows mid then hi := mid else lo := mid
  done;
  Alcotest.(check (float 0.01)) "rings on its own from k = 22.1" 22.105 !lo;
  (* the peak at k = 17.68, searched by 1% steps from 200 Hz *)
  let best = ref (0., neg_infinity) and f = ref 200. in
  while !f < 1500. do
    let g = diode ~k:17.684 !f in
    if g > snd !best then best := (!f, g);
    f := !f *. 1.01
  done;
  Alcotest.(check (float 5.)) "its peak above the cutoff, near 667 Hz" 667. (fst !best)

let tests =
  Testo.categorize "Diode_ladder"
    [
      t "the slope, against the Moog ladder's" test_slope;
      t "the bass: 1 / (1 + k)" test_bass;
      t "the resonance: its threshold, its peak" test_resonance;
    ]
