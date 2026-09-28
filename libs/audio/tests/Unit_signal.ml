(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Signal, Oscillator, Noise, Wav: the .mli's worked examples *)

let t = Testo.create

let test_signal () =
  Alcotest.(check (float 1e-9)) "Nyquist at 44,100: 22,050" 22050. Signal.nyquist;
  Alcotest.(check int) "half a second: 22,050 samples" 22050 (Signal.samples 0.5);
  Alcotest.(check (float 0.01)) "440 Hz: a period every 100.23 samples" 100.23 (Signal.period_in_samples 440.);
  Alcotest.(check (float 1e-9)) "30,000 Hz heard at 14,100" 14100. (Signal.alias 30000.);
  Alcotest.(check (float 1e-9)) "... below Nyquist, itself" 440. (Signal.alias 440.);
  Alcotest.(check int) "1. as 16 bits" 32767 (Signal.to_int16 1.);
  Alcotest.(check int) "too loud: clipped" (-32768) (Signal.to_int16 (-2.))

let test_oscillator () =
  let at w = Oscillator.wave w 0.25 in
  Alcotest.(check (float 1e-9)) "a quarter in: the sine at 1" 1. (at Sine);
  Alcotest.(check (float 1e-9)) "the square" 1. (at Square);
  Alcotest.(check (float 1e-9)) "the triangle" 1. (at Triangle);
  Alcotest.(check (float 1e-9)) "the sawtooth" (-0.5) (at Sawtooth);
  (* a 1000 Hz square's harmonics past Nyquist, folded back *)
  Alcotest.(check (list (float 1e-9))) "the 23rd, 25th, 27th harmonics heard at" [ 21100.; 19100.; 17100. ]
    (List.map (fun n -> Signal.alias (1000. *. float_of_int n)) [ 23; 25; 27 ]);
  (* a second of 440 Hz sine crosses 0 going down 440 times, in the
   * middle of each period (going up, 439: the first is at sample 0) *)
  let s = Oscillator.render Sine ~frequency:440. 1. in
  let downs = ref 0 in
  Array.iteri (fun i x -> if i > 0 && s.(i - 1) >= 0. && x < 0. then incr downs) s;
  Alcotest.(check int) "440 periods in a second" 440 !downs

let test_noise () =
  Alcotest.(check (list int)) "the first steps from 1" [ 16384; 8192; 4096 ]
    (List.tl (List.rev (List.fold_left (fun acc _ -> Noise.step Long (List.hd acc) :: acc) [ 1 ] [ 1; 2; 3 ])));
  Alcotest.(check int) "long: all 32,767 values" 32767 (Noise.period Long);
  Alcotest.(check int) "short: 93" 93 (Noise.period Short);
  let n = Noise.render ~rate:44100. 1. in
  let ones = Array.fold_left (fun c x -> if x > 0. then c + 1 else c) 0 n in
  (* a maximal LFSR has 16,384 ones for 16,383 zeros per period: half *)
  if abs (ones - 22050) > 300 then Alcotest.failf "unbalanced noise: %d ones in 44,100" ones

let test_wav () =
  let s = Oscillator.render Sine ~frequency:440. 1. in
  let wav = Wav.to_string s in
  Alcotest.(check int) "1 s: 44 + 88,200 bytes" 88244 (String.length wav);
  Alcotest.(check string) "RIFF" "RIFF" (String.sub wav 0 4);
  Alcotest.(check int) "36 + the data" 88236 (Int32.to_int (String.get_int32_le wav 4));
  Alcotest.(check int) "88,200 bytes a second" 88200 (Int32.to_int (String.get_int32_le wav 28));
  match Wav.of_string wav with
  | Error e -> Alcotest.fail e
  | Ok back ->
      Alcotest.(check int) "read back: 44,100 samples" 44100 (Array.length back);
      (* 16 bits: within half a step, 1 / 65,534 *)
      Array.iteri (fun i x -> if Float.abs (x -. s.(i)) > 1. /. 60000. then Alcotest.failf "sample %d: %g, not %g" i x s.(i)) back

let tests =
  Testo.categorize "Signals"
    [
      t "Signal: Nyquist, periods, aliases" test_signal;
      t "Oscillator: the waveforms, the aliases of a square" test_oscillator;
      t "Noise: the NES's LFSR" test_noise;
      t "Wav: the header, read back" test_wav;
    ]
