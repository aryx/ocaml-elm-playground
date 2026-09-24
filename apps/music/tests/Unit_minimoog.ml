(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Voice_minimoog: the patches as text, the knobs' laws, the keyboard
 * (low note, legato), the filter tracking the keys, the mixer
 * overloading the filter; and each preset playing the same riff, as a
 * golden WAV *)

let t = Testo.create

(* the voice played through its instrument, a pull of 735 samples a
 * frame, [events] (frame, key, down?) before their frame's pull *)
let play ?options (p : Voice_minimoog.patch) (events : (int * int * bool) list) (frames : int) : Voice_minimoog.t * Signal.t =
  let v = Voice_minimoog.create ?options p in
  let i = Voice_minimoog.instrument v in
  let block : Signal.stereo = { left = Array.make 735 0.; right = Array.make 735 0. } in
  let x =
    Array.concat
      (List.init frames (fun f ->
           List.iter (fun (at, k, down) -> if at = f then if down then i.note_on k 1. else i.note_off k) events;
           i.fill block;
           Array.copy block.left))
  in
  (v, x)

let peak (x : Signal.t) a b =
  let p = ref 0. in
  for i = a to b - 1 do
    p := Float.max !p (Float.abs x.(i))
  done;
  !p

let pitch (x : Signal.t) a b =
  let ups = ref [] in
  for i = a + 1 to b - 1 do
    if x.(i - 1) < 0. && x.(i) >= 0. then ups := i :: !ups
  done;
  match (!ups, List.rev !ups) with
  | last :: _, first :: _ when last > first -> float_of_int (List.length !ups - 1) *. 44100. /. float_of_int (last - first)
  | _ -> 0.

let test_text () =
  List.iter
    (fun (name, p) ->
      match Voice_minimoog.of_string (Voice_minimoog.to_string p) with
      | Ok p' -> if p' <> p then Alcotest.failf "%s: not the same after writing and reading" name
      | Error e -> Alcotest.failf "%s: %s" name e)
    (("initial", Voice_minimoog.initial) :: Voice_minimoog.presets);
  (* the effects too *)
  let turned = [ ("drive.on", 1.); ("drive.shape", 3.); ("eq.on", 1.); ("eq.bass", -3.5); ("reverb.kind", 1.); ("reverb.time", 4.2) ] in
  let p =
    { Voice_minimoog.initial with
      effects = List.map (fun (n, x) -> (n, Option.value (List.assoc_opt n turned) ~default:x)) Voice_minimoog.initial.effects }
  in
  Alcotest.(check bool) "the effects, turned" true (List.for_all (fun (n, x) -> List.assoc n p.effects = x) turned);
  (match Voice_minimoog.of_string (Voice_minimoog.to_string p) with
  | Ok p' -> if p' <> p then Alcotest.fail "the effects: not the same after writing and reading"
  | Error e -> Alcotest.fail e);
  (* the panel's controls and the rack's under different names (the
   * rack's modulation once shared "mod.mix" with the Model D's own) *)
  let names = List.map (fun (k : Voice_minimoog.knob) -> k.name) Voice_minimoog.knobs in
  Alcotest.(check int) "every control's name its own" (List.length names) (List.length (List.sort_uniq compare names));
  let error s = match Voice_minimoog.of_string s with Ok _ -> "" | Error e -> e in
  Alcotest.(check string) "an unknown control" "no such control: osc4.range" (error "osc4.range = 8'");
  Alcotest.(check string) "a bad value" "osc1.range: not a value: 7'" (error "osc1.range = 7'");
  Alcotest.(check string) "a comment and a blank line" "" (error "# a comment\n\nosc1.on = on # the only one")

let test_laws () =
  (* the .mli's worked example *)
  Alcotest.(check (float 0.01)) "the cutoff at 0.5: 632 Hz" 632.46 (Voice_minimoog.cutoff_hz 0.5);
  Alcotest.(check (float 1e-9)) "the attack at 0: 1 ms" 0.001 (Voice_minimoog.attack_seconds 0.);
  Alcotest.(check (float 1e-9)) "at 0.5: 100 ms" 0.1 (Voice_minimoog.attack_seconds 0.5);
  Alcotest.(check (float 1e-9)) "at 1: 10 s" 10. (Voice_minimoog.attack_seconds 1.);
  Alcotest.(check (float 1e-9)) "the decay at 1: 35 s" 35. (Voice_minimoog.decay_seconds 1.);
  Alcotest.(check (float 1e-9)) "LO: 6 octaves under 8'" (-6.) (Voice_minimoog.range_octaves 0);
  Alcotest.(check (float 1e-9)) "2': 2 over" 2. (Voice_minimoog.range_octaves 5);
  let p = Voice_minimoog.initial in
  List.iter
    (fun (k1, k2, expected) ->
      Alcotest.(check (float 1e-9)) "the tracking" expected (Voice_minimoog.tracking { p with keyboard_1 = k1; keyboard_2 = k2 }))
    [ (false, false, 0.); (true, false, 1. /. 3.); (false, true, 2. /. 3.); (true, true, 1.) ]

(* low note, legato: C3 held, E3 pressed over it: still C3, and the
 * contour not restarted; C3 let go: E3 *)
let test_keyboard () =
  let p = { Voice_minimoog.initial with loudness_contour = { attack = 0.3; decay = 0.5; sustain = 1. } } in
  let v, x = play p [ (0, 48, true); (20, 52, true); (40, 48, false) ] 60 in
  ignore v;
  Alcotest.(check (float 0.5)) "C3 alone" 130.8 (pitch x (10 * 735) (20 * 735));
  Alcotest.(check (float 0.5)) "E3 pressed over it: still C3" 130.8 (pitch x (22 * 735) (40 * 735));
  Alcotest.(check (float 0.5)) "C3 let go: E3" 164.8 (pitch x (42 * 735) (60 * 735));
  (* the attack (0.03 s) long over by frame 20: legato, the level goes on,
   * no new attack from where it was *)
  if peak x (20 * 735) (21 * 735) < 0.8 *. peak x (18 * 735) (19 * 735) then Alcotest.fail "the contour restarted"

(* the whistle: no oscillator, the filter oscillating and tracking the
 * keys fully, in tune within 2.5 cents *)
let test_whistle () =
  let w = List.assoc "whistle" Voice_minimoog.presets in
  List.iter
    (fun n ->
      let _, x = play w [ (0, n, true) ] 80 in
      let cents = 1200. *. Float.log2 (pitch x (40 * 735) (80 * 735) /. Voicing.frequency (float_of_int n)) in
      if Float.abs cents > 2.5 then Alcotest.failf "key %d: %.1f cents out" n cents)
    [ 48; 60; 67; 72 ];
  (* the cutoff an octave up for a key an octave up *)
  let v, _ = play w [ (0, 60, true) ] 5 in
  let v', _ = play w [ (0, 72, true) ] 5 in
  Alcotest.(check (float 1e-6)) "an octave up, the cutoff doubled" 2. (Voice_minimoog.cutoff_now v' /. Voice_minimoog.cutoff_now v)

(* the mixer into the filter: three sawtooths' levels from 0.1 to 1,
 * the output from 0.123 to 0.446 -- 3.3 times the input gives 2.5
 * times the output, 3 times more only 1.4: the filter saturating *)
let test_overload () =
  let base =
    { Voice_minimoog.initial with cutoff = 1.; emphasis = 0.; contour_amount = 0.; loudness_contour = { attack = 0.; decay = 0.5; sustain = 1. } }
  in
  let o l = { base.osc1 with on = true; level = l; wave = 2 } in
  let loudness l =
    let p = { base with osc1 = o l; osc2 = { (o l) with frequency = 0.01 }; osc3 = { (o l) with frequency = -0.01 } } in
    let _, x = play ~options:{ Voice_minimoog.analog with drift = false } p [ (0, 45, true) ] 60 in
    peak x (30 * 735) (60 * 735)
  in
  Alcotest.(check (float 0.002)) "levels at 0.1" 0.123 (loudness 0.1);
  Alcotest.(check (float 0.002)) "at 0.33" 0.312 (loudness 0.33);
  Alcotest.(check (float 0.002)) "at 1" 0.446 (loudness 1.)

(* the riff every preset plays: C3, E3, then G3, and C4 pressed over it
 * (still G3, the lower), G3 let go (C4, legato), C4 let go; 2.5 s *)
let riff = [ (0, 48, true); (30, 48, false); (34, 52, true); (50, 52, false); (55, 55, true); (66, 60, true); (70, 55, false); (110, 60, false) ]

let tests =
  Testo.categorize "Minimoog"
    ([
       t "patches as text, and back" test_text;
       t "the knobs' laws" test_laws;
       t "the keyboard: low note, legato" test_keyboard;
       t "the whistle: the filter played, in tune" test_whistle;
       t "the mixer overloading the filter" test_overload;
     ]
    @ List.map
        (fun (name, p) -> t ("golden WAV: " ^ name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" ("minimoog_" ^ name) (snd (play p riff 150))))
        Voice_minimoog.presets)
