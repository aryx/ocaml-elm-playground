(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_dynamics.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate
let db (x : float) : float = 20. *. log10 x
let sine ?(amp = 1.) f n = Array.init n (fun i -> amp *. sin (2. *. Float.pi *. f *. float_of_int i /. rate))

(* a square wave of 441 Hz: the same level at every sample *)
let square amp n = Array.init n (fun i -> if i / 50 mod 2 = 0 then amp else -.amp)

let peak x a b =
  let p = ref 0. in
  for i = a to b do
    p := Float.max !p (Float.abs x.(i))
  done;
  !p

let both (x : Signal.t) : Signal.stereo = { left = Array.copy x; right = Array.copy x }

let test_curve () =
  let hard = { Dynamics.compressor with knee = 0. } in
  Alcotest.(check (float 1e-9)) "-8 dB in, -17 out" (-17.) (Dynamics.curve hard (-8.));
  Alcotest.(check (float 1e-9)) "under the threshold: untouched" (-30.) (Dynamics.curve hard (-30.));
  Alcotest.(check (float 1e-4)) "a 6 dB knee, at the threshold" (-20.5625) (Dynamics.curve Dynamics.compressor (-20.));
  Alcotest.(check (float 1e-9)) "the limiter: a ceiling" (-1.) (Dynamics.curve Dynamics.limiter 3.);
  Alcotest.(check (float 1e-9)) "the gate: 10 dB under, 100 under" (-140.) (Dynamics.curve Dynamics.gate (-50.))

(* the reduction after each sample, a sample at a time *)
let test_times () =
  let n = Signal.samples 0.2 in
  let x = Array.concat [ square (Mix.of_decibels (-40.)) n; square (Mix.of_decibels (-8.)) n; square (Mix.of_decibels (-40.)) (2 * n) ] in
  let d = Dynamics.create () and s = { Dynamics.compressor with detector = Peak; knee = 0. } in
  let reduction =
    Array.init (Array.length x) (fun i ->
        Dynamics.process d s { left = [| x.(i) |]; right = [| x.(i) |] };
        Dynamics.reduction d)
  in
  let full = reduction.((2 * n) - 1) in
  let crossing from target up =
    let i = ref from in
    while if up then reduction.(!i) < target else reduction.(!i) > target do
      incr i
    done;
    float_of_int (!i - from) /. rate *. 1000.
  in
  Alcotest.(check (float 1e-3)) "9 dB of reduction" 9. full;
  Alcotest.(check (float 0.03)) "63% in the attack, 5 ms" 4.99 (crossing n (0.632 *. full) true);
  Alcotest.(check (float 0.03)) "back to 37% in the release, 100 ms" 99.96 (crossing (2 * n) (0.368 *. full) false)

let test_limiter () =
  let burst = Array.concat [ Array.make 4410 0.; sine 441. 4410; Array.make 4410 0. ] in
  let s = both burst in
  Dynamics.process (Dynamics.create ()) { Dynamics.limiter with threshold = -6. } s;
  Alcotest.(check (float 1e-4)) "no louder than -6 dB" 0.5012 (peak s.left 0 (Array.length burst - 1))

let test_gate () =
  let s = both (square (Mix.of_decibels (-50.)) 44100) in
  Dynamics.process (Dynamics.create ()) Dynamics.gate s;
  Alcotest.(check (float 0.01)) "a -50 dB hum out at -140" (-140.) (db (peak s.left 22050 44099))

let test_side_chain () =
  let n = Signal.samples 2. in
  let bass = both (sine ~amp:(Mix.of_decibels (-12.)) 55. n) in
  let kick =
    Array.init n (fun i ->
        let t = float_of_int i /. rate in
        if Float.rem t 0.5 < 0.1 then sin (2. *. Float.pi *. 60. *. t) else 0.)
  in
  Dynamics.process ~key:(both kick) (Dynamics.create ()) { Dynamics.compressor with detector = Peak } bass;
  let during = db (peak bass.left (Signal.samples 1.05) (Signal.samples 1.1))
  and between = db (peak bass.left (Signal.samples 1.4) (Signal.samples 1.5)) in
  Alcotest.(check (float 0.01)) "during a kick" (-26.) during;
  Alcotest.(check (float 0.01)) "between" (-12.27) between;
  Alcotest.(check (float 0.01)) "ducked 13.7 dB" 13.73 (between -. during)

let tests =
  Testo.categorize "Dynamics"
    [
      t "the static curve: -8 in, -17 out" test_curve;
      t "the attack and the release, measured" test_times;
      t "the limiter: a ceiling nothing passes" test_limiter;
      t "the gate: the hum silenced" test_gate;
      t "the side-chain: a bass ducked by a kick" test_side_chain;
    ]
