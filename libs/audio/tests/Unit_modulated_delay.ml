(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_modulated_delay.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate
let sine f n = Array.init n (fun i -> sin (2. *. Float.pi *. f *. float_of_int i /. rate))

(* [x] through a stereo effect in blocks of 735 *)
let blocks (f : Signal.stereo -> unit) (x : Signal.t) : Signal.stereo =
  let n = Array.length x in
  let s = { Signal.left = Array.copy x; right = Array.copy x } and k = ref 0 in
  while !k < n do
    let m = min 735 (n - !k) in
    let b = { Signal.left = Array.sub s.left !k m; right = Array.sub s.right !k m } in
    f b;
    Array.blit b.left 0 s.left !k m;
    Array.blit b.right 0 s.right !k m;
    k := !k + m
  done;
  s

let peak x a b =
  let p = ref 0. in
  for i = a to b do
    p := Float.max !p (Float.abs x.(i))
  done;
  !p

(* the frequency from the rising zero crossings in [a, b], each placed
 * between its two samples *)
let frequency (x : Signal.t) a b =
  let ups = ref [] in
  for i = a + 1 to b do
    if x.(i - 1) < 0. && x.(i) >= 0. then ups := (float_of_int (i - 1) +. (-.x.(i - 1) /. (x.(i) -. x.(i - 1)))) :: !ups
  done;
  match (!ups, List.rev !ups) with
  | last :: _, first :: _ -> float_of_int (List.length !ups - 1) *. rate /. (last -. first)
  | _ -> 0.

(* the copy alone (the output less the dry sound, mix 1) of a 1 kHz
 * sine, its frequency over 40 ms around 1 s (the delay shortening
 * fastest) and 2 s (lengthening) *)
let test_chorus () =
  let x = sine 1000. (Signal.samples 2.5) in
  let y = blocks (Modulated_delay.process (Modulated_delay.create ()) { Modulated_delay.chorus with mix = 1. }) x in
  let copy = Array.mapi (fun i v -> v -. x.(i)) y.left in
  let around seconds =
    let c = Signal.samples seconds in
    frequency copy (c - 882) (c + 882)
  in
  Alcotest.(check (float 0.01)) "up: 1009.42 Hz" 1009.42 (around 1.);
  Alcotest.(check (float 0.01)) "down: 990.58 Hz" 990.58 (around 2.);
  Alcotest.(check (float 0.05)) "+16.2 cents" 16.23 (1200. *. Float.log2 (around 1. /. 1000.))

let flanger_gain ~feedback f =
  let y =
    blocks
      (Modulated_delay.process (Modulated_delay.create ()) { center = 0.001; depth = 0.; rate = 0.; feedback; mix = 1. })
      (sine f 44100)
  in
  peak y.left 22050 44099

let test_flanger () =
  Alcotest.(check (float 0.0005)) "500 Hz: the first notch" 0.0002 (flanger_gain ~feedback:0. 500.);
  Alcotest.(check (float 0.0005)) "1 kHz: a peak" 1.9991 (flanger_gain ~feedback:0. 1000.);
  Alcotest.(check (float 0.0005)) "1.5 kHz: the second notch" 0.0021 (flanger_gain ~feedback:0. 1500.);
  Alcotest.(check (float 0.001)) "feedback 0.7: the peak at 4" 4.0532 (flanger_gain ~feedback:0.7 1000.)

let phaser_gain f =
  let y = blocks (Phaser.process (Phaser.create ()) { low = 1000.; high = 1000.; rate = 0.; feedback = 0.; mix = 1. }) (sine f 44100) in
  peak y.left 22050 44099

(* the quietest frequency in [lo, hi], by steps of 1 Hz *)
let notch lo hi =
  let best = ref (lo, Float.infinity) in
  for f = int_of_float lo to int_of_float hi do
    let g = phaser_gain (float_of_int f) in
    if g < snd !best then best := (float_of_int f, g)
  done;
  !best

let test_phaser () =
  Alcotest.(check (float 0.0001)) "1 kHz: back in phase" 2. (phaser_gain 1000.);
  let f1, g1 = notch 400. 430. and f2, g2 = notch 2380. 2410. in
  Alcotest.(check (float 0.)) "the first notch: 415 Hz" 415. f1;
  Alcotest.(check (float 0.)) "the second: 2395 Hz" 2395. f2;
  Alcotest.(check bool) "both deep (under 0.002)" true (g1 < 0.002 && g2 < 0.002);
  Alcotest.(check (float 0.1)) "5.8 times apart, not evenly spaced" 5.77 (f2 /. f1)

let tests =
  Testo.categorize "Modulated_delay and Phaser"
    [
      t "the chorus: +-16 cents" test_chorus;
      t "the flanger: notches evenly spaced" test_flanger;
      t "the phaser: two notches, 5.8 times apart" test_phaser;
    ]
