(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_cs80.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate
let preset name = List.assoc name Voice_cs80.presets
let frequency key = 440. *. Float.pow 2. (float_of_int (key - 69) /. 12.)

(* the filter envelope: IL -0.5, AL 0.5, attack 0.1 s, decay 0.2 s,
 * release 0.4 s (the knobs from the curve: 0.002 x 5000^k) *)
let knob_for seconds = log (seconds /. 0.002) /. log 5000.

let test_filter_envelope () =
  let l = { Voice_cs80.layer0 with il = -0.5; al = 0.5; f_attack = knob_for 0.1; f_decay = knob_for 0.2; f_release = knob_for 0.4 } in
  let at ?(held = infinity) s = Voice_cs80.filter_envelope l ~held s in
  Alcotest.(check (list (float 1e-9))) "IL at the key, AL after the attack, half down, the cutoff set, held" [ -0.5; 0.5; 0.25; 0.; 0. ]
    [ at 0.; at 0.1; at 0.2; at 0.3; at 5. ];
  Alcotest.(check (list (float 1e-9))) "let go at 1 s: back to IL in the release" [ 0.; -0.25; -0.5 ] [ at ~held:1. 1.; at ~held:1. 1.2; at ~held:1. 2. ]

(* [run v i seconds ~at]: the instrument played, [at] called at each
 * frame with the time *)
let run ?(at = fun _ -> ()) (i : Instrument.t) (seconds : float) : Signal.t =
  let n = Signal.samples seconds in
  let out = Array.make n 0. in
  let k = ref 0 in
  while !k < n do
    at (float_of_int !k /. rate);
    let m = min 735 (n - !k) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    i.fill b;
    Array.blit b.left 0 out !k m;
    k := !k + m
  done;
  out

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

(* a steady patch: no detune, no chorus, the filter's envelope flat, so
 * only the touch moves it *)
let steady =
  let l = { Voice_cs80.layer0 with il = 0.; al = 0.; lpf = 0.4; attack = 0.; sustain = 1.; after_brilliance = 1.; after_level = 0. } in
  { (Voice_cs80.initial) with layers = [| l; l |]; detune = 0.; chorus = false }

(* C E G held, E pressed from 0.5 s: each note's 5th harmonic, before
 * and after, in dB *)
let test_polyphonic_pressure () =
  let v = Voice_cs80.create steady in
  let i = Voice_cs80.instrument v in
  List.iter (fun k -> i.note_on k 0.5) [ 60; 64; 67 ];
  let s = run i 1. ~at:(fun time -> if time >= 0.5 then Voice_cs80.pressure v 64 1.) in
  let change key =
    let f = 5. *. frequency key in
    20. *. log10 (amplitude s f (Signal.samples 0.8) 8192 /. amplitude s f (Signal.samples 0.2) 8192)
  in
  (* E's filter 3 octaves open; C and G within a dB, what leaks from
   * their neighbours into the measure *)
  Alcotest.(check (list (float 0.1))) "the 5th harmonics' change, C E G (dB): E alone" [ -0.8; 31.0; 0.9 ] [ change 60; change 64; change 67 ]

(* the ribbon: A4 bent 2 semitones, B4 *)
let test_ribbon () =
  let v = Voice_cs80.create steady in
  let i = Voice_cs80.instrument v in
  i.note_on 69 0.5;
  Voice_cs80.bend v 2.;
  let s = run i 0.6 in
  let b4 = frequency 71 and a4 = frequency 69 in
  let at f = amplitude s f (Signal.samples 0.2) 8192 in
  Alcotest.(check bool) (Printf.sprintf "B4 (%.3f) louder than A4 (%.3f)" (at b4) (at a4)) true (at b4 > 10. *. at a4)

(* the resonance at its most: no oscillation once the note is gone *)
let test_no_self_oscillation () =
  let l = { Voice_cs80.layer0 with lpf_res = 1.; hpf_res = 1.; release = 0. } in
  let v = Voice_cs80.create { Voice_cs80.initial with layers = [| l; l |]; chorus = false } in
  let i = Voice_cs80.instrument v in
  i.note_on 60 1.;
  let s = run i 1.5 ~at:(fun time -> if time >= 0.5 && time < 0.52 then i.note_off 60) in
  let tail = Array.sub s (Signal.samples 1.2) (Signal.samples 0.3) in
  Alcotest.(check (float 1e-6)) "silent after the release" 0. (Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. tail);
  Alcotest.(check int) "freed" 0 (Voice_cs80.voices v)

let peak (s : Signal.t) : float = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. s

(* each preset on a phrase: a chord, the middle note pressed, the
 * ribbon, a chord *)
let riff (p : Voice_cs80.patch) : Signal.t =
  let v = Voice_cs80.create p in
  let i = Voice_cs80.instrument v in
  Array.concat
    (List.init 150 (fun frame ->
         if frame = 0 then List.iter (fun k -> i.note_on k 0.7) [ 48; 60; 64; 67 ];
         if frame >= 30 && frame < 60 then Voice_cs80.pressure v 64 (float_of_int (frame - 30) /. 30.);
         if frame = 70 then List.iter i.note_off [ 48; 60; 64; 67 ];
         if frame = 75 then i.note_on 72 0.9;
         if frame >= 85 && frame < 105 then Voice_cs80.bend v (-.float_of_int (frame - 85) /. 10.);
         if frame = 110 then i.note_off 72;
         let b = { Signal.left = Array.make 735 0.; right = Array.make 735 0. } in
         i.fill b;
         b.left))

let test_peaks () =
  Alcotest.(check (list (pair string (float 0.01)))) "the presets' peaks"
    [ ("brass", 0.67); ("strings", 0.45); ("pad", 0.36); ("ring bells", 0.34); ("lead", 0.36) ]
    (List.map (fun (name, p) -> (name, peak (riff p))) Voice_cs80.presets)

let test_text () =
  List.iter
    (fun (name, p) ->
      match Voice_cs80.of_string (Voice_cs80.to_string p) with
      | Ok q -> Alcotest.(check bool) (name ^ ": read back") true (q = p)
      | Error e -> Alcotest.failf "%s: %s" name e)
    Voice_cs80.presets

let tests =
  Testo.categorize "CS-80"
    (List.map
       (fun (name, p) ->
         let file = "cs80_" ^ String.map (fun c -> if c = ' ' then '_' else c) name in
         t ("golden WAV: " ^ name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" file (riff p)))
       Voice_cs80.presets
    @ [
        t "the filter envelope: IL, AL, the decay, the release" test_filter_envelope;
        t "polyphonic aftertouch: one note of a chord pressed" test_polyphonic_pressure;
        t "the ribbon: the held note bent" test_ribbon;
        t "the resonance: no self-oscillation" test_no_self_oscillation;
        t "the presets' peaks" test_peaks;
        t "the patches as text" test_text;
      ])
