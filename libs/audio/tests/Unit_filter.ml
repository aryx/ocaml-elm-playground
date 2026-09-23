(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Filter: the .mli's numbers, and each filter's gain measured on
 * a sine actually filtered *)

let t = Testo.create
let db (x : float) : float = 20. *. log10 x

(* a filter's gain at [frequency], measured: a second of a full sine
 * through it, the peak of its last half (once the filter has settled) *)
let measured (filter : Signal.t -> Signal.t) (frequency : float) : float =
  let y = filter (Signal.of_function 1. (fun t -> sin (2. *. Float.pi *. frequency *. t))) in
  let peak = ref 0. in
  Array.iteri (fun i v -> if i > Array.length y / 2 then peak := Float.max !peak (Float.abs v)) y;
  !peak

let check_db what expected actual = Alcotest.(check (float 0.01)) (what ^ " (dB)") expected (db actual)

(* -3 dB at the cutoff, then towards -6 dB an octave: -7 at 2 kHz, -12
 * at 4 kHz; the high-pass (the input less the low-pass) the mirror,
 * -20 dB a decade below *)
let test_one_pole () =
  Alcotest.(check (float 0.001)) "a for 1000 Hz" 0.133 (Filter.one_pole_coefficient 1000.);
  let low = measured (Filter.low_pass ~cutoff:1000.) and high = measured (Filter.high_pass ~cutoff:1000.) in
  check_db "low-pass at 100 Hz" (-0.04) (low 100.);
  check_db "low-pass at the cutoff" (-3.00) (low 1000.);
  check_db "low-pass an octave above" (-6.96) (low 2000.);
  check_db "low-pass two octaves above" (-12.19) (low 4000.);
  check_db "high-pass a decade below" (-20.67) (high 100.)

(* the .mli's figure: Butterworth's -3 dB at the cutoff, -12 an octave
 * above; Q = 10's +20 dB; the high-pass the mirror; the band-pass's
 * peak at 0 dB; and [response], the formula, equal to the gain
 * measured on a filtered sine *)
let test_biquad () =
  let low q = Filter.biquad Low_pass ~cutoff:1000. ~q in
  let bw = low 0.707 in
  check_db "Butterworth at 250 Hz" (-0.02) (Filter.response bw 250.);
  check_db "Butterworth at the cutoff" (-3.01) (Filter.response bw 1000.);
  check_db "Butterworth an octave above" (-12.39) (Filter.response bw 2000.);
  check_db "Butterworth two octaves above" (-24.55) (Filter.response bw 4000.);
  check_db "Q = 10 at the cutoff" 20. (Filter.response (low 10.) 1000.);
  check_db "high-pass two octaves below" (-24.13) (Filter.response (Filter.biquad High_pass ~cutoff:1000. ~q:0.707) 250.);
  check_db "band-pass at its centre" 0. (Filter.response (Filter.biquad Band_pass ~cutoff:1000. ~q:0.707) 1000.);
  List.iter
    (fun (q, f) ->
      let what = Printf.sprintf "Q = %g, %.0f Hz: the formula = the filtered sine" q f in
      Alcotest.(check (float 0.01)) what (db (Filter.response (low q) f)) (db (measured (Filter.run (low q)) f)))
    [ (0.707, 250.); (0.707, 1000.); (0.707, 4000.); (10., 800.); (10., 1000.) ]

(* the wah: a cutoff moving from 200 Hz to 5 kHz over a second, evenly
 * in octaves, so at 1000 Hz halfway; a 1000 Hz sine through it: quiet
 * at first (-21 dB at 0.1 s, the cutoff 276 Hz), the resonance's boost
 * as the cutoff passes it (Q = 5: 20 log10 5 = +14 dB), then passing
 * (+0.7 dB at 0.9 s, the resonance's slope still above 0) *)
let test_sweep () =
  let sine = Signal.of_function 1. (fun t -> sin (2. *. Float.pi *. 1000. *. t)) in
  let y = Filter.sweep Low_pass ~q:5. ~from:200. ~to_:5000. sine in
  let peak_around seconds =
    let i = Signal.samples seconds and p = ref 0. in
    for j = i - 500 to i + 500 do p := Float.max !p (Float.abs y.(j)) done;
    !p
  in
  Alcotest.(check (float 0.1)) "at 0.1 s (dB)" (-21.05) (db (peak_around 0.1));
  Alcotest.(check (float 0.1)) "at 0.5 s, the cutoff at 1000 Hz (dB)" 14. (db (peak_around 0.5));
  Alcotest.(check (float 0.1)) "at 0.9 s (dB)" 0.69 (db (peak_around 0.9))

(* the EQ's table, measured on sines through [process] a block at a
 * time, and the formula equal to the measure *)
let test_eq () =
  let through f s =
    let m = Filter.silence () and y = Array.copy s in
    let k = ref 0 in
    while !k < Array.length y do
      let n = min 735 (Array.length y - !k) in
      let b = Array.sub y !k n in
      Filter.process f m b;
      Array.blit b 0 y !k n;
      k := !k + n
    done;
    y
  in
  let row name f expected =
    List.iter2
      (fun hz e ->
        let g = measured (through f) hz in
        Alcotest.(check (float 0.01)) (Printf.sprintf "%s at %.0f Hz (dB)" name hz) e (db g);
        Alcotest.(check (float 0.01)) (Printf.sprintf "%s at %.0f Hz, the formula (dB)" name hz) (db g) (db (Filter.response f hz)))
      [ 50.; 200.; 500.; 1000.; 2000.; 8000. ]
      expected
  in
  row "peaking" (Filter.peaking ~frequency:1000. ~q:1. ~gain:6.) [ 0.02; 0.27; 1.88; 6.00; 1.86; 0.08 ];
  row "low shelf" (Filter.low_shelf ~frequency:200. ~gain:6.) [ 5.97; 3.00; 0.16; 0.01; 0.00; 0.00 ];
  row "high shelf" (Filter.high_shelf ~frequency:4000. ~gain:(-6.)) [ 0.00; 0.00; 0.00; -0.02; -0.35; -5.73 ]

let tests =
  Testo.categorize "Filter"
    [
      t "the EQ: peaking and shelves, the table" test_eq;
      t "the one-pole: a, -3 dB at the cutoff, then -6 dB an octave" test_one_pole;
      t "the biquad: the cookbook's gains, the formula = the measure" test_biquad;
      t "the sweep: the wah" test_sweep;
    ]
