(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Vco, Lfo, Drift, and Oscillator's pulse: a synthesizer's
 * sources (notes_synth.md sections 3 and 4) *)

let t = Testo.create
let db (x : float) : float = 20. *. log10 x

(* as in Unit_oscillator: 4096 samples, a 1001.3 Hz fundamental on bin
 * 93, its harmonics on bins 93 k, nothing leaking *)
let n = 4096
let bin = 93
let f0 = Spectrum.bin_frequency ~n bin
let spectrum (x : Signal.t) : float array = Spectrum.magnitudes (Spectrum.fft x)

let loudest_alias ?(below = Signal.nyquist) (m : float array) : float =
  let a = ref 0. in
  Array.iteri (fun k v -> if k mod bin <> 0 && Spectrum.bin_frequency ~n k < below && v > !a then a := v) m;
  db !a

let test_pulse () =
  Alcotest.(check (float 1e-9)) "width 1/3, high: 1 less its average, -1/3" (4. /. 3.) (Oscillator.pulse ~width:(1. /. 3.) 0.1);
  Alcotest.(check (float 1e-9)) "low" (-2. /. 3.) (Oscillator.pulse ~width:(1. /. 3.) 0.5);
  (* the .mli's harmonics, (4 / (pi k)) |sin (pi k w)|, within 3%
   * (PolyBLEP dulls the higher ones a little: the 3rd, 3 kHz, by 1.5%,
   * the 4th by 2.7%); at 1/3 the 3rd missing *)
  List.iter
    (fun width ->
      let o = Vco.create () and out = Array.make n 0. in
      Vco.fill ~width:(Array.make n width) o Pulse ~frequency:(Array.make n f0) out;
      let m = spectrum out in
      List.iter
        (fun k ->
          let expected = 4. /. (Float.pi *. float_of_int k) *. Float.abs (sin (Float.pi *. float_of_int k *. width)) in
          let what = Printf.sprintf "width %.3f, harmonic %d" width k in
          if expected > 0.01 then Alcotest.(check (float (expected *. 0.03))) what expected m.(bin * k)
          else if m.(bin * k) > 1e-6 then Alcotest.failf "%s: %g, not missing" what m.(bin * k))
        [ 1; 2; 3; 4; 6 ];
      Alcotest.(check (float 1e-9)) (Printf.sprintf "width %.3f: no average" width) 0. m.(0))
    [ 0.5; 1. /. 3. ]

(* PWM: the width swept from 0.1 to 0.9 and back, the wave's average
 * stays at 0 over each period *)
let test_pwm () =
  let len = 44100 in
  let lfo = Lfo.create () and w = Array.make len 0. in
  Lfo.fill lfo Triangle ~rate:1. w;
  let width = Array.map (fun x -> 0.5 +. (0.4 *. x)) w in
  let o = Vco.create () and out = Array.make len 0. in
  (* 441 Hz: a period of 100 samples *)
  Vco.fill ~width o Pulse ~frequency:(Array.make len 441.) out;
  for p = 0 to (len / 100) - 1 do
    let sum = ref 0. in
    for i = 0 to 99 do
      sum := !sum +. out.((p * 100) + i)
    done;
    if Float.abs (!sum /. 100.) > 0.02 then Alcotest.failf "period %d: average %g" p (!sum /. 100.)
  done

let sync (fill : sync:Vco.t -> Vco.t -> Signal.t -> unit) : float array =
  (* 2000 samples to settle, then 4096 *)
  let len = n + 2000 in
  let master = Vco.create () and slave = Vco.create () in
  let m = Array.make len 0. and s = Array.make len 0. in
  Vco.fill master Sawtooth ~frequency:(Array.make len f0) m;
  fill ~sync:master slave s;
  spectrum (Array.sub s 2000 n)

let test_sync () =
  let slave = Array.make (n + 2000) (2.37 *. f0) in
  let naive = sync (fun ~sync s out -> Vco.fill ~band_limited:false ~sync s Sawtooth ~frequency:slave out) in
  let at_sample = sync (fun ~sync s out -> Vco.fill_sync_at_sample ~sync s Sawtooth ~frequency:slave out) in
  let fraction = sync (fun ~sync s out -> Vco.fill ~sync s Sawtooth ~frequency:slave out) in
  (* the master's pitch: every bin that isn't a multiple of 93 an alias *)
  Alcotest.(check (float 0.1)) "naive: the loudest alias below 5 kHz (dB)" (-29.5) (loudest_alias ~below:5000. naive);
  Alcotest.(check (float 0.1)) "corrected on the sample" (-30.7) (loudest_alias ~below:5000. at_sample);
  Alcotest.(check (float 0.1)) "corrected at the fraction" (-69.9) (loudest_alias ~below:5000. fraction);
  Alcotest.(check (float 0.002)) "the fundamental the same" naive.(bin) fraction.(bin)

let test_lfo () =
  Alcotest.(check (float 1e-9)) "sine at a quarter" 1. (Lfo.value Sine 0.25);
  Alcotest.(check (float 1e-9)) "triangle at a quarter" 1. (Lfo.value Triangle 0.25);
  Alcotest.(check (float 1e-9)) "square at a quarter" 1. (Lfo.value Square 0.25);
  Alcotest.(check (float 1e-9)) "saw down at a quarter" 0.5 (Lfo.value Saw_down 0.25);
  Alcotest.(check (float 1e-9)) "a quarter note at 120 BPM: 2 Hz" 2. (Lfo.of_tempo ~bpm:120. ~beats:1.);
  (* 5 Hz for a second, in blocks of 735: five periods, the phase going
   * on from block to block *)
  let lfo = Lfo.create () and block = Array.make 735 0. in
  let x = Array.concat (List.init 60 (fun _ -> Lfo.fill lfo Sine ~rate:5. block; Array.copy block)) in
  let ups = ref 0 in
  for i = 1 to Array.length x - 1 do
    if x.(i - 1) < 0. && x.(i) >= 0. then incr ups
  done;
  Alcotest.(check int) "5 Hz: 5 periods a second (4 new ones after the first)" 4 !ups;
  (* the vibrato of the .mli: 6 Hz, 0.3 semitone *)
  let v = Array.make 44100 0. in
  Lfo.fill (Lfo.create ()) Sine ~rate:6. v;
  let factors = Array.map (fun x -> Float.pow 2. (0.3 *. x /. 12.)) v in
  Alcotest.(check (float 1e-5)) "the vibrato, at least" 0.98282 (Array.fold_left Float.min 1. factors);
  Alcotest.(check (float 1e-5)) "at most" 1.01748 (Array.fold_left Float.max 1. factors)

let test_sample_and_hold () =
  Alcotest.(check int) "the generator from 0" 1013904223 (Noise.lcg 0);
  Alcotest.(check (float 1e-3)) "as -1..1" (-0.528) (Noise.uniform (Noise.lcg 0));
  Alcotest.(check (float 1e-3)) "the third" 0.639 (Noise.uniform (Noise.lcg (Noise.lcg (Noise.lcg 0))));
  (* 10 Hz: a value held for 4410 samples, then the next *)
  let run seed =
    let x = Array.make 44100 0. in
    Lfo.fill (Lfo.create ~seed ()) Sample_and_hold ~rate:10. x;
    x
  in
  let x = run 7 in
  let steps = ref 0 in
  for i = 1 to 44099 do
    if x.(i) <> x.(i - 1) then incr steps
  done;
  Alcotest.(check int) "a new value each period: 9 changes in a second" 9 !steps;
  Alcotest.(check (float 1e-9)) "held through its period" x.(0) x.(4409);
  Alcotest.(check bool) "the same seed, the same values" true (run 7 = x);
  Alcotest.(check bool) "another seed, others" false (run 8 = x)

let test_drift () =
  let d = Drift.create ~seed:1 () in
  let count = 200_000 in
  (* 290 s of it, a step at a time *)
  let xs = Array.init count (fun _ -> Drift.advance d Drift.step_samples; Drift.cents d) in
  let mean = Array.fold_left ( +. ) 0. xs /. float_of_int count in
  let var = Array.fold_left (fun a x -> a +. ((x -. mean) ** 2.)) 0. xs /. float_of_int count in
  Alcotest.(check (float 0.5)) "around 0" 0. mean;
  Alcotest.(check (float 0.1)) "its spread 3 cents: 2.97" 2.97 (sqrt var);
  (* tau = 2 s later, correlated by e^-1 = 0.37: 0.35 *)
  let lag = 2 * Signal.rate / Drift.step_samples in
  let c = ref 0. in
  for i = 0 to count - lag - 1 do
    c := !c +. ((xs.(i) -. mean) *. (xs.(i + lag) -. mean))
  done;
  Alcotest.(check (float 0.01)) "2 s later, correlated by 0.35" 0.35 (!c /. float_of_int (count - lag) /. var);
  (* the audio clock's steps, whatever the blocks: 73,500 samples in
   * blocks of 735 or of 500 *)
  let a = Drift.create ~seed:1 () and b = Drift.create ~seed:1 () in
  for _ = 1 to 100 do
    Drift.advance a 735
  done;
  for _ = 1 to 147 do
    Drift.advance b 500
  done;
  Alcotest.(check (float 0.)) "the same drift in blocks of 735 or 500" (Drift.cents a) (Drift.cents b);
  Alcotest.(check (float 1e-9)) "as a factor" (Float.pow 2. (Drift.cents a /. 1200.)) (Drift.factor a)

let tests =
  Testo.categorize "Vco"
    [
      t "the pulse: its harmonics by its width" test_pulse;
      t "pulse-width modulation, no thump" test_pwm;
      t "hard sync: the restarts corrected at their fraction" test_sync;
      t "the LFO: shapes, periods, a vibrato" test_lfo;
      t "sample and hold, seeded" test_sample_and_hold;
      t "drift: its spread, its time, its steps" test_drift;
    ]
