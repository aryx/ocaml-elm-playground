(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Ladder and Svf: the .mlis' measurements -- the slope, the
 * resonance's pitch and threshold per model, the thinning bass, the
 * saturation's harmonics; the SVF's stability, and the audio-rate
 * sweep against the biquad *)

let t = Testo.create
let rate = float_of_int Signal.rate
let db (x : float) : float = 20. *. log10 x

let peak (x : Signal.t) a b =
  let p = ref 0. in
  for i = a to b do
    p := Float.max !p (Float.abs x.(i))
  done;
  !p

let sine f n = Array.init n (fun i -> sin (2. *. Float.pi *. f *. float_of_int i /. rate))

(* a second of a sine through the ladder, the gain over its second half *)
let gain model ~k ~fc f =
  let x = sine f 44100 in
  Moog_ladder.process (Moog_ladder.create ()) model ~cutoff:(Array.make 44100 fc) ~resonance:k x;
  peak x 22050 44099

(* the pitch of the loudest response, in cents from the cutoff, on a
 * grid of 5 cents *)
let resonant_peak model ~k ~fc =
  let best = ref (0, 0.) in
  for c = -120 to 120 do
    let g = gain model ~k ~fc (fc *. Float.pow 2. (float_of_int (5 * c) /. 1200.)) in
    if g > snd !best then best := (5 * c, g)
  done;
  !best

(* an impulse's response growing: oscillating *)
let grows model ~k ~fc =
  let x = Array.make 20000 0. in
  x.(0) <- 1.;
  Moog_ladder.process (Moog_ladder.create ()) model ~cutoff:(Array.make 20000 fc) ~resonance:k x;
  let energy a b = Array.fold_left (fun s v -> s +. (v *. v)) 0. (Array.sub x a (b - a)) in
  let late = energy 15000 20000 in
  Float.is_nan late || late > energy 5000 10000

let threshold model ~fc =
  let lo = ref 3. and hi = ref 8. in
  for _ = 1 to 20 do
    let mid = (!lo +. !hi) /. 2. in
    if grows model ~k:mid ~fc then hi := mid else lo := mid
  done;
  !lo

let test_slope () =
  Alcotest.(check (float 0.01)) "-12.04 dB at the cutoff" (-12.04) (db (gain Zero_delay ~k:0. ~fc:1000. 1000.));
  let at f = db (gain Zero_delay ~k:0. ~fc:100. f) in
  Alcotest.(check (float 0.05)) "400 Hz to 800 Hz: -23.3 dB" (-23.33) (at 800. -. at 400.);
  Alcotest.(check (float 0.05)) "800 Hz to 1.6 kHz: -24.0 dB" (-23.99) (at 1600. -. at 800.)

let test_bass () =
  (* a constant through it: 1 / (1 + k), at k = 3 a quarter; with the
   * compensation, all of it *)
  let dc ?compensation model k =
    let x = Array.make 44100 1. in
    Moog_ladder.process ?compensation (Moog_ladder.create ()) model ~cutoff:(Array.make 44100 1000.) ~resonance:k x;
    x.(44099)
  in
  Alcotest.(check (float 1e-6)) "zero-delay, k = 3: 1/4" 0.25 (dc Zero_delay 3.);
  Alcotest.(check (float 1e-6)) "naive, k = 3: 1/4" 0.25 (dc Naive 3.);
  Alcotest.(check (float 1e-6)) "compensated: 1" 1. (dc ~compensation:1. Zero_delay 3.)

let test_tuning () =
  (* the analog peak at k = 3.5: 59 cents under, +9.25 dB; the zero-delay
   * loop at every cutoff, the naive one drifting *)
  List.iter
    (fun (fc, expected) ->
      let (cents, g) = resonant_peak Zero_delay ~k:3.5 ~fc in
      Alcotest.(check int) (Printf.sprintf "zero-delay at %.0f Hz (5-cent grid)" fc) expected cents;
      Alcotest.(check (float 0.1)) "+9.3 dB" 9.3 (db g))
    [ (440., -60); (1000., -60); (5000., -55) ];
  List.iter
    (fun (fc, expected) ->
      Alcotest.(check int) (Printf.sprintf "naive at %.0f Hz" fc) expected (fst (resonant_peak Naive ~k:3.5 ~fc)))
    [ (440., -35); (1000., -10); (5000., 130) ]

let test_oscillation () =
  List.iter
    (fun (fc, naive) ->
      Alcotest.(check (float 0.001)) (Printf.sprintf "zero-delay oscillates from k = 4 at %.0f Hz" fc) 4. (threshold Zero_delay ~fc);
      Alcotest.(check (float 0.01)) (Printf.sprintf "naive from k = %.2f" naive) naive (threshold Naive ~fc))
    [ (110., 4.06); (440., 4.26); (1000., 4.64) ];
  Alcotest.(check (float 0.01)) "naive at 5 kHz: not up to k = 8" 8. (threshold Naive ~fc:5000.);
  (* the nonlinear one past 4: a sine at its cutoff, held by the tanh *)
  List.iter
    (fun (k, level) ->
      let x = Array.make 44100 0. in
      x.(0) <- 1.;
      Moog_ladder.process (Moog_ladder.create ()) Nonlinear ~cutoff:(Array.make 44100 440.) ~resonance:k x;
      let ups = ref [] in
      for i = 2001 to 44099 do
        if x.(i - 1) < 0. && x.(i) >= 0. then ups := i :: !ups
      done;
      let f =
        match (!ups, List.rev !ups) with
        | last :: _, first :: _ -> float_of_int (List.length !ups - 1) *. rate /. float_of_int (last - first)
        | _ -> 0.
      in
      let cents = 1200. *. Float.log2 (f /. 440.) in
      if Float.abs cents > 1. then Alcotest.failf "k = %.1f: %.1f cents off" k cents;
      Alcotest.(check (float 0.01)) (Printf.sprintf "k = %.1f: its level" k) level (peak x 40000 44099))
    [ (4.2, 0.08); (4.5, 0.12) ]

let test_saturation () =
  (* a sine on FFT bin 10, 3rd harmonic on bin 30 *)
  let n = 4096 in
  let f = Spectrum.bin_frequency ~n 10 in
  List.iter
    (fun (a, below) ->
      let x = Array.map (fun v -> a *. v) (sine f (4 * n)) in
      Moog_ladder.process (Moog_ladder.create ()) Nonlinear ~cutoff:(Array.make (4 * n) 2000.) ~resonance:0. x;
      let m = Spectrum.magnitudes (Spectrum.fft (Array.sub x (3 * n) n)) in
      Alcotest.(check (float 0.1)) (Printf.sprintf "at %.1f, the 3rd harmonic (dB under)" a) below (db (m.(10) /. m.(30))))
    [ (0.1, 49.9); (0.5, 24.3); (1., 16.8); (4., 10.6) ]

let svf_gain model mode ~fc f =
  let x = sine f 44100 in
  Svf.process (Svf.create ()) model mode ~cutoff:(Array.make 44100 fc) ~q:0.707 x;
  peak x 22050 44099

let test_svf () =
  List.iter
    (fun fc ->
      List.iter
        (fun mode ->
          Alcotest.(check (float 0.01)) (Printf.sprintf "zero-delay at %.0f Hz: -3.01 dB" fc) (-3.01) (db (svf_gain Zero_delay mode ~fc fc)))
        [ Svf.Low_pass; Band_pass; High_pass ];
      if db (svf_gain Zero_delay Notch ~fc fc) > -200. then Alcotest.failf "the notch at %.0f Hz" fc)
    [ 1000.; 5000.; 8000.; 12000. ];
  Alcotest.(check (float 0.01)) "Chamberlin at 5 kHz: -3.01 dB" (-3.01) (db (svf_gain Chamberlin Low_pass ~fc:5000. 5000.));
  Alcotest.(check bool) "Chamberlin at 8 kHz: blown up" true (Float.is_nan (svf_gain Chamberlin Low_pass ~fc:8000. 8000.))

(* the .mli's worked example: the cutoff swept at audio rate *)
let test_sweep () =
  let n = 44100 in
  let loudest fm (filter : Signal.t -> Signal.t -> unit) =
    let cutoff = Array.init n (fun i -> 1000. *. Float.pow 2. (3. *. sin (2. *. Float.pi *. fm *. float_of_int i /. rate))) in
    let x = Array.sub (Oscillator.render ~band_limited:true Sawtooth ~frequency:110. 1.) 0 n in
    filter cutoff x;
    peak x 0 (n - 1)
  in
  let svf cutoff x = Svf.process (Svf.create ()) Zero_delay Low_pass ~cutoff ~q:5. x in
  let biquad cutoff x =
    let m = Filter.silence () in
    Array.iteri (fun i v -> x.(i) <- Filter.step (Filter.biquad Low_pass ~cutoff:cutoff.(i) ~q:5.) m v) x
  in
  Alcotest.(check (float 0.01)) "slowly (5 Hz): the SVF" 2.38 (loudest 5. svf);
  Alcotest.(check (float 0.01)) "and the biquad" 2.38 (loudest 5. biquad);
  Alcotest.(check (float 0.01)) "500 Hz: the SVF" 2.40 (loudest 500. svf);
  Alcotest.(check (float 0.01)) "the biquad, too loud" 6.66 (loudest 500. biquad);
  Alcotest.(check (float 0.01)) "3 kHz: the SVF" 2.33 (loudest 3000. svf);
  let blown = loudest 3000. biquad in
  if not (Float.is_nan blown || blown > 1e6) then Alcotest.failf "the biquad at 3 kHz: %g, not blown up" blown

let tests =
  Testo.categorize "Ladder"
    [
      t "24 dB an octave" test_slope;
      t "the bass thinned, and compensated" test_bass;
      t "the resonance's pitch: zero-delay in tune, naive drifting" test_tuning;
      t "self-oscillation from k = 4, and held by the tanh" test_oscillation;
      t "saturation: harmonics as it's driven" test_saturation;
      t "the SVF: its four outputs, Chamberlin's limit" test_svf;
      t "a cutoff swept at audio rate: SVF vs biquad" test_sweep;
    ]
