(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_modal.mli *)

let t = Testo.create
let run (m : Modal.t) (n : int) : Signal.t = Array.init n (fun _ -> Modal.next m)
let peak (s : Signal.t) : float = Array.fold_left (fun a x -> Float.max a (Float.abs x)) 0. s
let db (x : float) : float = 20. *. log10 x

(* the loudest over the period around sample [at] *)
let around (s : Signal.t) (at : int) : float = peak (Array.sub s at 101)

let test_struck () =
  let m = Modal.create ~frequency:440. ~t60:1. in
  Modal.strike m 0.5;
  let s = run m 50000 in
  (* the crest between two samples, a quarter period of decay before it *)
  Alcotest.(check (float 1e-3)) "the peak in the first period" 0.498 (around s 0);
  let spectrum = Spectrum.magnitudes (Spectrum.fft (Spectrum.hann (Array.sub s 0 4096))) in
  Alcotest.(check (float 5.4)) "440 Hz (bins of 10.8 Hz)" 440. (Spectrum.bin_frequency ~n:4096 (Spectrum.peak spectrum));
  Alcotest.(check (float 0.1)) "-60 dB after t60" (-60.) (db (around s 44100 /. 0.5))

let test_damped () =
  let m = Modal.create ~frequency:440. ~t60:1. in
  Modal.strike m 0.5;
  let before = run m 11025 in
  Modal.damp m ~t60:0.1;
  let after = run m 5000 in
  let at_damping = around before 10900 in
  (* the level before measured 25 samples early, at t60 1 s: 0.5 dB *)
  Alcotest.(check (float 0.1)) "damped: -60 dB 0.1 s later" (-60.5) (db (around after 4410 /. at_damping));
  (* struck again while it rings: the two added, as a linear system *)
  let a = Modal.create ~frequency:440. ~t60:1. and b = Modal.create ~frequency:440. ~t60:1. and c = Modal.create ~frequency:440. ~t60:1. in
  Modal.strike a 0.3;
  Modal.strike c 0.3;
  let xa = run a 100 and xc = run c 100 in
  Modal.strike a 0.2;
  Modal.strike b 0.2;
  let ya = run a 1000 and yb = run b 1000 and yc = run c 1000 in
  ignore (xa, xc);
  let d = ref 0. in
  Array.iteri (fun i y -> d := Float.max !d (Float.abs (y -. (yb.(i) +. yc.(i))))) ya;
  Alcotest.(check (float 1e-12)) "struck again: the sum" 0. !d

let tests = Testo.categorize "Modal" [ t "struck: its peak, frequency and decay" test_struck; t "damped, and struck again" test_damped ]
