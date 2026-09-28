(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_hammond.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate
let db (x : float) : float = 20. *. log10 x

let test_wheels () =
  Alcotest.(check int) "91 wheels" 91 Tonewheel.count;
  Alcotest.(check (float 1e-9)) "A4: 440 exactly" 440. (Tonewheel.frequency (Tonewheel.of_note 69));
  Alcotest.(check (float 1e-3)) "C4: 20 x 85/104 x 16" 261.538 (Tonewheel.frequency (Tonewheel.of_note 60));
  Alcotest.(check (float 0.01)) "C4: cents off equal temperament" (-0.58) (Tonewheel.cents (Tonewheel.of_note 60));
  Alcotest.(check (float 1e-2)) "the top wheel, F#8: 192 bumps" 5924.57 (Tonewheel.frequency 91);
  Alcotest.(check int) "C1 is wheel 1" 1 (Tonewheel.of_note 24);
  Alcotest.(check int) "below C1: an octave up (the 16' foldback)" (Tonewheel.of_note 32) (Tonewheel.of_note 20);
  Alcotest.(check int) "above F#8: an octave down (the 1')" (Tonewheel.of_note 108) (Tonewheel.of_note 120);
  (* the tempered harmonic: C4's 2 2/3' is G5's wheel *)
  let g5 = Tonewheel.frequency (Tonewheel.of_note 79) and third = 3. *. Tonewheel.frequency (Tonewheel.of_note 60) in
  Alcotest.(check (float 1e-9)) "G5's wheel: 784 Hz" 784. g5;
  Alcotest.(check (float 0.01)) "the true third harmonic's 784.62, 1.36 cents sharper" (-1.36) (1200. *. Float.log2 (g5 /. third))

(* [seconds] of the organ, C4 pressed at 0 (and [others] after), the
 * left side *)
let play ?(others = []) (p : Voice_hammond.patch) (seconds : float) : Signal.t =
  let v = Voice_hammond.create p in
  let i = Voice_hammond.instrument v in
  i.note_on 60 1.;
  List.iter (fun k -> i.note_on k 1.) others;
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

(* the amplitude of [f] in [x] from sample [a] for [n]: the sine's
 * correlation, Hann-windowed (without it, a short window lets the loud
 * lines nearby leak into a quiet one) *)
let amplitude (x : Signal.t) (f : float) (a : int) (n : int) : float =
  let re = ref 0. and im = ref 0. and sum = ref 0. in
  for i = a to a + n - 1 do
    let hann = 0.5 -. (0.5 *. cos (2. *. Float.pi *. float_of_int (i - a) /. float_of_int n)) in
    let w = 2. *. Float.pi *. f *. float_of_int i /. rate in
    re := !re +. (hann *. x.(i) *. cos w);
    im := !im +. (hann *. x.(i) *. sin w);
    sum := !sum +. hann
  done;
  2. *. Float.hypot !re !im /. !sum

let quiet = { Voice_hammond.initial with click = 0. }
let wheel n = Tonewheel.frequency (Tonewheel.of_note n)

let test_drawbars () =
  let x = play (Voice_hammond.registration "888000000" quiet) 1.1 in
  let at f = amplitude x f 2205 44100 in
  let a16 = at (wheel 48) and a5 = at (wheel 67) and a8 = at (wheel 60) in
  Alcotest.(check (float 0.01)) "16' as loud as 8'" 1. (a16 /. a8);
  Alcotest.(check (float 0.01)) "5 1/3' as loud as 8'" 1. (a5 /. a8);
  Alcotest.(check bool) "nothing at 4' (under -60 dB)" true (db (at (wheel 72) /. a8) < -60.);
  (* a drawbar at 6: two steps, 6 dB under *)
  let y = play (Voice_hammond.registration "006000000" quiet) 1.1 and z = play (Voice_hammond.registration "008000000" quiet) 1.1 in
  Alcotest.(check (float 0.05)) "a drawbar two steps in: -6 dB" (-6.) (db (amplitude y (wheel 60) 2205 44100 /. amplitude z (wheel 60) 2205 44100))

let test_percussion () =
  let p = { (Voice_hammond.registration "888000000" quiet) with percussion = true; third = true; fast = true } in
  let x = play p 1.2 in
  let g5 = wheel 79 in
  let early = amplitude x g5 0 2205 and late = amplitude x g5 (Signal.samples 1.0) 2205 in
  Alcotest.(check bool) "the third harmonic struck (over 0.02)" true (early > 0.02);
  Alcotest.(check bool) "and gone a second later (60 dB down)" true (db (late /. early) < -60.);
  (* E4 pressed while C4 is held: its third harmonic, B5, never struck *)
  let y = play ~others:[ 64 ] p 0.05 in
  let b5 = amplitude y (wheel 83) 0 2205 and g5' = amplitude y g5 0 2205 in
  Alcotest.(check bool) "single-trigger: E4, pressed second, without it" true (db (b5 /. g5') < -40.)

let test_voices () =
  let v = Voice_hammond.create quiet in
  let i = Voice_hammond.instrument v in
  let run blocks =
    for _ = 1 to blocks do
      i.fill { left = Array.make 735 0.; right = Array.make 735 0. }
    done
  in
  List.iter (fun k -> i.note_on k 1.) [ 60; 64; 67 ];
  run 1;
  Alcotest.(check int) "a chord: three voices" 3 (Voice_hammond.voices v);
  List.iter i.note_off [ 60; 64; 67 ];
  run 1;
  Alcotest.(check int) "let go: none (2 ms of contacts)" 0 (Voice_hammond.voices v)

let test_text () =
  List.iter
    (fun (name, p) ->
      match Voice_hammond.of_string (Voice_hammond.to_string p) with
      | Ok p' -> if p' <> p then Alcotest.failf "%s: not the same after writing and reading" name
      | Error e -> Alcotest.failf "%s: %s" name e)
    (("initial", Voice_hammond.initial) :: Voice_hammond.presets);
  Alcotest.(check string) "a registration" "838000000" (Voice_hammond.of_registration (Voice_hammond.registration "838" quiet));
  Alcotest.(check (float 1e-9)) "drawbar 8: full" 1. (Voice_hammond.drawbar_gain 8);
  Alcotest.(check (float 0.)) "drawbar 0: silent" 0. (Voice_hammond.drawbar_gain 0)

(* each preset playing C, F and G major, 0.5 s each, a frame's block
 * at a time, then 0.3 s after *)
let riff (p : Voice_hammond.patch) : Signal.t =
  let v = Voice_hammond.create p in
  let i = Voice_hammond.instrument v in
  let chords = [ (0, [ 60; 64; 67 ]); (30, [ 65; 69; 72 ]); (60, [ 67; 71; 74 ]) ] in
  Array.concat
    (List.init 108 (fun frame ->
         List.iter
           (fun (at, keys) ->
             if frame = at then List.iter (fun k -> i.note_on k 1.) keys;
             if frame = at + 28 then List.iter i.note_off keys)
           chords;
         let b = { Signal.left = Array.make 735 0.; right = Array.make 735 0. } in
         i.fill b;
         b.left))

let tests =
  Testo.categorize "Hammond"
    (List.map
       (fun (name, p) -> t ("golden WAV: " ^ name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" ("hammond_" ^ name) (riff p)))
       Voice_hammond.presets
    @ [
      t "the tonewheels: gears, cents, foldback, the tempered harmonic" test_wheels;
      t "the drawbars: three equal lines, 3 dB a step" test_drawbars;
      t "the percussion: struck, dying, single-triggered" test_percussion;
      t "the voices: a chord, freed" test_voices;
      t "the patches as text, the registrations" test_text;
    ])
