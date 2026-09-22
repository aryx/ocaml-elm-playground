(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Effect and Sfx: the pitch effects' factors, the echo's impulse
 * response and tail, the presets measured *)

let t = Testo.create

let test_pitch () =
  let vibrato = Effect.Vibrato { rate = 5.; depth = 1. } in
  Alcotest.(check (float 1e-9)) "a vibrato at 0: 1" 1. (Effect.factor vibrato 0.);
  (* a quarter of its period (0.05 s): the top, a semitone up *)
  Alcotest.(check (float 1e-9)) "at its top: a semitone up" (2. ** (1. /. 12.)) (Effect.factor vibrato 0.05);
  Alcotest.(check (float 1e-9)) "at its bottom: a semitone down" (2. ** (-1. /. 12.)) (Effect.factor vibrato 0.15);
  let jump = Effect.Jump { semitones = 12.; at = 0.1 } in
  Alcotest.(check (float 1e-9)) "before the jump" 1. (Effect.factor jump 0.09);
  Alcotest.(check (float 1e-9)) "after: an octave" 2. (Effect.factor jump 0.1);
  let arpeggio = Effect.Arpeggio { semitones = [ 0.; 4.; 7. ]; step = 0.1 } in
  Alcotest.(check (list (float 1e-9)))
    "an arpeggio: 0, 4, 7, 0, ... semitones"
    (List.map (fun k -> 2. ** (k /. 12.)) [ 0.; 4.; 7.; 0.; 4. ])
    (List.map (Effect.factor arpeggio) [ 0.05; 0.15; 0.25; 0.35; 0.45 ])

(* an impulse echoed: 1, then 0.5 one delay later, 0.25, ...; the tail
 * 2.5 s for a delay of 0.25 s and feedback 0.5 (the .mli's example) *)
let test_echo () =
  Alcotest.(check (float 1e-9)) "the tail" 2.5 (Effect.tail ~delay:0.25 ~feedback:0.5);
  let impulse = Array.init 10 (fun i -> if i = 0 then 1. else 0.) in
  let y = Effect.echo ~delay:0.25 ~feedback:0.5 impulse in
  let d = Signal.samples 0.25 in
  Alcotest.(check int) "its length" (10 + Signal.samples 2.5) (Array.length y);
  List.iteri (fun k expected -> Alcotest.(check (float 1e-9)) (Printf.sprintf "echo %d" k) expected y.(k * d)) [ 1.; 0.5; 0.25; 0.125 ];
  Alcotest.(check (float 1e-9)) "nothing between them" 0. y.(d / 2)

(* the zero crossings going down in [lo, hi) seconds, as a frequency *)
let frequency (x : Signal.t) (lo : float) (hi : float) : float =
  let c = ref 0 in
  for i = Signal.samples lo + 1 to Signal.samples hi - 1 do
    if x.(i - 1) >= 0. && x.(i) < 0. then incr c
  done;
  float_of_int !c /. (hi -. lo)

let test_sfx () =
  (* the coin: C6 (1047 Hz) then, at 0.07 s, a fifth up: G6 (1568 Hz) *)
  let coin = Synth.render (Sfx.to_sound Sfx.coin) in
  Alcotest.(check (float 1e-9)) "the coin's length: attack (5 ms), sustain, decay" 0.305 (Sfx.duration Sfx.coin);
  Alcotest.(check (float 30.)) "the coin's first note (Hz)" 1047. (frequency coin 0.01 0.07);
  Alcotest.(check (float 30.)) "its second" 1568. (frequency coin 0.08 0.2);
  (* an echo lengthens a sound by its tail *)
  let echoed = { Sfx.blip with echo = 0.1 } in
  Alcotest.(check (float 1e-6)) "an echoed blip: + 0.8 s of echoes (0.4^8 < 0.001)" (Sfx.duration Sfx.blip +. 0.8) (Sfx.duration echoed);
  (* vary: the same seed, the same sound; 0, the sound itself *)
  Alcotest.(check bool) "seed 0: itself" true (Sfx.vary ~seed:0 Sfx.laser = Sfx.laser);
  Alcotest.(check bool) "the same seed: the same" true (Sfx.vary ~seed:7 Sfx.laser = Sfx.vary ~seed:7 Sfx.laser);
  Alcotest.(check bool) "another: another" true (Sfx.vary ~seed:7 Sfx.laser <> Sfx.vary ~seed:8 Sfx.laser);
  (* the explosion darkens, its low-pass falling: the spectrum's centroid
   * from 1126 Hz at 0.02 s to 175 Hz at 0.5 s *)
  let e = Synth.render (Sfx.to_sound Sfx.explosion) in
  let early = Unit_synth.centroid (Array.sub e (Signal.samples 0.02) 4096)
  and late = Unit_synth.centroid (Array.sub e (Signal.samples 0.5) 4096) in
  Alcotest.(check (float 1.)) "the explosion's burst, its brightness (Hz)" 1126. early;
  Alcotest.(check (float 1.)) "its rumble at 0.5 s (Hz)" 175. late

let tests =
  Testo.categorize "Effect and Sfx"
    [
      t "the pitch effects: vibrato, jump, arpeggio" test_pitch;
      t "the echo: an impulse's echoes, the tail" test_echo;
      t "the presets measured, vary" test_sfx;
    ]
