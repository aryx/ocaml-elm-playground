(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_juno.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate

(* Harman's curves at the sliders he measured (0, 5, 10 of 10) *)
let test_times () =
  Alcotest.(check (list (float 1e-3))) "attack at 0, 5, 10 (s; measured 0.001, 0.24, 3.25)" [ 0.001; 0.248; 3.251 ]
    (List.map Voice_juno.attack_seconds [ 0.; 0.5; 1. ]);
  Alcotest.(check (list (float 1e-3))) "decay, release at 0, 5, 10 (s; measured 0.002, 0.984, 19.783)" [ 0.002; 1.043; 17.462 ]
    (List.map Voice_juno.decay_seconds [ 0.; 0.5; 1. ])

(* the amplitude of [f] in [x] from sample [a] for [n], Hann-windowed *)
let amplitude (x : Signal.t) (f : float) (a : int) (n : int) : float =
  let re = ref 0. and im = ref 0. and sum = ref 0. in
  for i = a to a + n - 1 do
    let hann = 0.5 -. (0.5 *. cos (2. *. Float.pi *. float_of_int (i - a) /. float_of_int n)) in
    let w = 2. *. Float.pi *. f *. float_of_int i /. rate in
    re := !re +. (hann *. x.(i) *. cos w);
    im := !im +. (hann *. x.(i) *. sin w);
    sum := !sum +. hann
  done;
  2. *. sqrt ((!re *. !re) +. (!im *. !im)) /. !sum

let play (p : Voice_juno.patch) (key : int) (seconds : float) : Signal.t =
  let v = Voice_juno.create p in
  let i = Voice_juno.instrument v in
  i.note_on key 1.;
  let n = Signal.samples seconds in
  let out = Array.make n 0. in
  let k = ref 0 in
  while !k < n do
    let m = min 735 (n - !k) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    i.fill b;
    Array.blit b.left 0 out !k m;
    k := !k + m
  done;
  out

(* the sub alone, the filter open: A4's sound an octave under it *)
let test_sub () =
  let p = { Voice_juno.initial with saw = false; sub = 1.; cutoff = 1.; env = 0.; chorus = 0; attack = 0.; sustain = 1. } in
  let s = play p 69 0.5 in
  let at f = amplitude s f 8820 8192 in
  Alcotest.(check bool) (Printf.sprintf "220 Hz (%.3f) above 440 (%.3f)" (at 220.) (at 440.)) true (at 220. > 10. *. at 440.)

(* the high-pass's four positions: a sine's gain at 30 Hz and 1 kHz *)
let test_high_pass () =
  let gain position f =
    let s = Signal.of_function 1. (fun t -> sin (2. *. Float.pi *. f *. t)) in
    let y = Voice_juno.high_pass position s in
    amplitude y f 22050 16384
  in
  (* 0: the shelf's +6 dB at the bottom (1.86 at 30 Hz); 2 and 3: one
   * pole, 30 / 225 = 0.13, and 1000 / 720 only just above its corner *)
  Alcotest.(check (list (pair (float 0.01) (float 0.01)))) "positions 0-3: the gains at 30 Hz and 1 kHz"
    [ (1.86, 1.01); (1., 1.); (0.13, 0.96); (0.04, 0.77) ]
    (List.map (fun p -> (gain p 30., gain p 1000.)) [ 0; 1; 2; 3 ])

(* the chorus's delays: I at its start and a quarter of its cycle, the
 * right side the left's mirror; I+II the same on both *)
let test_chorus () =
  let quarter = 1. /. (4. *. 0.513) in
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "I at 0: 1.66 ms left, 5.35 right" (0.00166, 0.00535) (Voice_juno.chorus_delays 1 0.);
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "I a quarter in: both halfway" (0.003505, 0.003505) (Voice_juno.chorus_delays 1 quarter);
  let l, r = Voice_juno.chorus_delays 3 0.02 in
  Alcotest.(check (float 1e-12)) "I+II: mono" l r;
  Alcotest.(check bool) "I+II within 3.3 and 3.7 ms" true (l >= 0.0033 && l <= 0.0037)

let peak (s : Signal.t) : float = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. s

(* each preset on a phrase: a chord, a line, a chord *)
let riff (p : Voice_juno.patch) : Signal.t =
  let v = Voice_juno.create p in
  let i = Voice_juno.instrument v in
  let notes = [ (0, [ 48; 60; 64; 67 ]); (30, [ 72 ]); (40, [ 74 ]); (50, [ 76 ]); (65, [ 41; 57; 60; 65 ]) ] in
  Array.concat
    (List.init 130 (fun frame ->
         List.iter
           (fun (at, keys) ->
             if frame = at then List.iter (fun k -> i.note_on k 0.8) keys;
             if frame = at + 25 then List.iter i.note_off keys)
           notes;
         let b = { Signal.left = Array.make 735 0.; right = Array.make 735 0. } in
         i.fill b;
         b.left))

let test_peaks () =
  Alcotest.(check (list (pair string (float 0.01)))) "the presets' peaks"
    [ ("brass", 0.38); ("strings", 0.39); ("bass", 0.66); ("pad", 0.51); ("lead", 0.34) ]
    (List.map (fun (name, p) -> (name, peak (riff p))) Voice_juno.presets)

let test_text () =
  List.iter
    (fun (name, p) ->
      match Voice_juno.of_string (Voice_juno.to_string p) with
      | Ok q -> Alcotest.(check bool) (name ^ ": read back") true (q = p)
      | Error e -> Alcotest.failf "%s: %s" name e)
    Voice_juno.presets

let tests =
  Testo.categorize "Juno"
    (List.map (fun (name, p) -> t ("golden WAV: " ^ name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" ("juno_" ^ name) (riff p))) Voice_juno.presets
    @ [
        t "the envelope's times, Harman's curves" test_times;
        t "the sub-oscillator, an octave down" test_sub;
        t "the high-pass's four positions" test_high_pass;
        t "the chorus's delays" test_chorus;
        t "the presets' peaks" test_peaks;
        t "the patches as text" test_text;
      ])
