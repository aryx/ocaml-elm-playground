(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Pitch_effect, Synth's echo and reverb, and Sfx: the pitch
 * effects' factors, the echo's impulse response and tail, the presets
 * measured *)

let t = Testo.create

let test_pitch () =
  let vibrato = Pitch_effect.Vibrato { rate = 5.; depth = 1. } in
  Alcotest.(check (float 1e-9)) "a vibrato at 0: 1" 1. (Pitch_effect.factor vibrato 0.);
  (* a quarter of its period (0.05 s): the top, a semitone up *)
  Alcotest.(check (float 1e-9)) "at its top: a semitone up" (2. ** (1. /. 12.)) (Pitch_effect.factor vibrato 0.05);
  Alcotest.(check (float 1e-9)) "at its bottom: a semitone down" (2. ** (-1. /. 12.)) (Pitch_effect.factor vibrato 0.15);
  let jump = Pitch_effect.Jump { semitones = 12.; at = 0.1 } in
  Alcotest.(check (float 1e-9)) "before the jump" 1. (Pitch_effect.factor jump 0.09);
  Alcotest.(check (float 1e-9)) "after: an octave" 2. (Pitch_effect.factor jump 0.1);
  let arpeggio = Pitch_effect.Arpeggio { semitones = [ 0.; 4.; 7. ]; step = 0.1 } in
  Alcotest.(check (list (float 1e-9)))
    "an arpeggio: 0, 4, 7, 0, ... semitones"
    (List.map (fun k -> 2. ** (k /. 12.)) [ 0.; 4.; 7.; 0.; 4. ])
    (List.map (Pitch_effect.factor arpeggio) [ 0.05; 0.15; 0.25; 0.35; 0.45 ])

(* an impulse echoed: 1, then 0.5 one delay later, 0.25, ...; the tail
 * 2.5 s for a delay of 0.25 s and feedback 0.5 (the .mli's example) *)
let test_echo () =
  Alcotest.(check (float 1e-9)) "the tail" 2.5 (Synth.tail ~delay:0.25 ~feedback:0.5);
  let impulse = Array.init 10 (fun i -> if i = 0 then 1. else 0.) in
  let y = Synth.echo ~delay:0.25 ~feedback:0.5 impulse in
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

(* The ready-made sounds, three generations (notes_audio.md section 8):
 * 1. phase 3's recipes (Audio.ml until phase 7, kept here as
 *    the record), on the naive oscillators;
 * 2. the same recipes, band-limited (phase 6: PolyBLEP, PolyBLAMP);
 * 3. sfxr's numbers (phase 7, Sfx's presets): envelopes, a jump,
 *    filters.
 * What each step changed, measured: the energy PolyBLEP took out (the
 * naive sound less the band-limited one), the brightness (the
 * spectrum's centroid) near the start and the end, the coin's level
 * where its two notes meet. *)
let first_generation : (string * Synth.t) list =
  let sq = Synth.voice (Wave Square) and saw = Synth.voice (Wave Sawtooth) and tri = Synth.voice (Wave Triangle) in
  let noise = Synth.voice Noise in
  Synth.
    [
      ("blip", sq 880. |> lasting 0.06 |> fading);
      ("coin", After [ sq 1047. |> lasting 0.07; sq 1568. |> lasting 0.25 |> fading ] |> louder 0.8);
      ("jump", sq 300. |> sliding 650. |> lasting 0.18 |> fading |> louder 0.8);
      ("laser", saw 1200. |> sliding 200. |> lasting 0.2 |> fading);
      ("hit", noise 3000. |> lasting 0.1 |> fading);
      ("explosion", noise 1500. |> sliding 150. |> lasting 0.7 |> fading |> louder 1.5);
      ("step", tri 150. |> sliding 90. |> lasting 0.05 |> fading |> louder 0.6);
    ]

let naive (s : Synth.t) : Signal.t =
  Synth.band_limited := false;
  let x = Synth.render s in
  Synth.band_limited := true;
  x

let energy (x : Signal.t) : float = Array.fold_left (fun e v -> e +. (v *. v)) 0. x

(* the centroid of the 2048 samples from [seconds] (or the last 2048) *)
let brightness (x : Signal.t) (seconds : float) : float =
  let n = Array.length x in
  let i = min (Signal.samples seconds) (max 0 (n - 2048)) in
  Unit_synth.centroid (Array.sub x i (min 2048 (n - i)))

let rms (x : Signal.t) (from : float) (until : float) : float =
  let i = Signal.samples from and j = Signal.samples until in
  sqrt (energy (Array.sub x i (j - i)) /. float_of_int (j - i))

let test_generations () =
  let sound name = List.assoc name first_generation in
  let third name = Synth.render (Sfx.to_sound (List.assoc name Sfx.presets)) in
  let hz what expected actual = Alcotest.(check (float 1.)) (what ^ " (Hz)") expected actual in
  (* 1 -> 2: what PolyBLEP took out, in dB below the sound: little
   * energy, but the blip's and the coin's brightness nearly halved --
   * most of what was high in them were aliases; the triangle's corners
   * (PolyBLAMP, the step) -69 dB, inaudible; the noises untouched *)
  List.iter
    (fun (name, expected) ->
      let n1 = naive (sound name) and b1 = Synth.render (sound name) in
      Alcotest.(check (float 0.1)) (name ^ ": the energy band-limiting took out (dB)") expected
        (10. *. log10 (energy (Array.map2 ( -. ) n1 b1) /. energy n1)))
    [ ("blip", -18.2); ("coin", -16.1); ("jump", -21.4); ("laser", -15.8); ("step", -69.3) ];
  hz "the blip's brightness, naive" 7623. (brightness (naive (sound "blip")) 0.);
  hz "band-limited" 4476. (brightness (Synth.render (sound "blip")) 0.);
  hz "the coin's, naive" 7869. (brightness (naive (sound "coin")) 0.);
  hz "band-limited" 4656. (brightness (Synth.render (sound "coin")) 0.);
  (* 2 -> 3: the filters. The laser darker as it falls (its low-pass
   * following it down), the explosion from a burst to a rumble: its noise
   * slowed tenfold hardly dulled it (the LFSR's steps are square: their
   * harmonics stay), the low-pass does it *)
  let laser2 = Synth.render (sound "laser") and laser3 = third "laser" in
  hz "the laser's brightness at the start, recipe" 6568. (brightness laser2 0.);
  hz "at the end" 4150. (brightness laser2 0.15);
  hz "sfxr's, at the start" 3803. (brightness laser3 0.);
  hz "at the end" 1066. (brightness laser3 0.15);
  let explosion2 = Synth.render (sound "explosion") and explosion3 = third "explosion" in
  hz "the explosion's brightness at the start, recipe" 4027. (brightness explosion2 0.);
  hz "at the end" 3078. (brightness explosion2 0.65);
  hz "sfxr's, at the start" 1196. (brightness explosion3 0.);
  hz "at the end" 109. (brightness explosion3 0.65);
  hz "the hit's, recipe" 4725. (brightness (Synth.render (sound "hit")) 0.);
  hz "sfxr's" 1755. (brightness (third "hit") 0.);
  (* the coin's two notes: two voices one after the other, each with its
   * 5 ms ramps, dipped to a ninth of the level where they meet; one
   * voice with a jump doesn't *)
  let coin1 = Synth.render (sound "coin") and coin3 = third "coin" in
  Alcotest.(check (float 0.001)) "the recipe's coin, held" 0.391 (rms coin1 0.05 0.06);
  Alcotest.(check (float 0.001)) "where its notes meet" 0.045 (rms coin1 0.069 0.071);
  Alcotest.(check (float 0.001)) "sfxr's, held" 0.391 (rms coin3 0.05 0.06);
  Alcotest.(check (float 0.001)) "where its notes meet" 0.389 (rms coin3 0.069 0.071)

(* Schroeder's reverb on an impulse: the level of its tail at 0.2 s,
 * and T / 2 later (30 dB down if the combs were set right), and how
 * dense its echoes are *)
let test_reverb () =
  let seconds = 1.5 in
  let impulse = Array.init 10 (fun i -> if i = 0 then 1. else 0.) in
  let y = Synth.reverb ~seconds ~mix:1. impulse in
  let rms from until =
    let i = Signal.samples from and j = Signal.samples until in
    sqrt (Array.fold_left (fun e v -> e +. (v *. v)) 0. (Array.sub y i (j - i)) /. float_of_int (j - i))
  in
  let drop = 20. *. log10 (rms (0.2 +. (seconds /. 2.)) (0.3 +. (seconds /. 2.)) /. rms 0.2 0.3) in
  let dense = ref 0 in
  Array.iteri (fun i v -> if i >= Signal.samples 0.2 && i < Signal.samples 0.3 && Float.abs v > 1e-4 then incr dense) y;
  Alcotest.(check int) "T longer" (10 + Signal.samples seconds) (Array.length y);
  Alcotest.(check (float 0.5)) "half of T later: 30 dB down (dB)" (-30.) drop;
  (* one comb would echo once in 30 to 45 ms, 3 samples in 0.1 s; the
   * four and the all-passes, 1552: a wash, not echoes *)
  Alcotest.(check int) "its echoes in 0.1 s (samples)" 1552 !dense

(* sfxr's buttons: over 200 seeds, what makes each category itself
 * always holds; the same seed, the same sound *)
let test_random () =
  let all name check = for seed = 1 to 200 do check (Sfx.random name ~seed) done in
  let expect what b = if not b then Alcotest.fail what in
  all "laser" (fun s -> expect "a laser slides down" (s.slide < s.frequency));
  all "jump" (fun s -> expect "a jump slides up" (s.slide > s.frequency));
  all "powerup" (fun s -> expect "a powerup rises" (s.slide > s.frequency));
  all "explosion" (fun s ->
      expect "an explosion is noise, getting duller" (s.wave = Noise && s.low_pass_to < s.low_pass && s.slide < s.frequency));
  all "coin" (fun s -> expect "a coin jumps up an interval" (List.mem s.jump [ 5.; 7.; 12. ]));
  all "hit" (fun s -> expect "a hit is short and falls" (s.sustain = 0. && s.decay <= 0.2 && s.slide < s.frequency));
  all "blip" (fun s -> expect "a blip holds its note" (s.slide = s.frequency));
  Alcotest.(check bool) "the same seed: the same" true (Sfx.random "laser" ~seed:3 = Sfx.random "laser" ~seed:3);
  Alcotest.(check bool) "another: another" true (Sfx.random "laser" ~seed:3 <> Sfx.random "laser" ~seed:4);
  Alcotest.(check bool) "another category, the same seed: another" true
    (Sfx.random "laser" ~seed:3 <> Sfx.random "hit" ~seed:3)

let tests =
  Testo.categorize "Pitch_effect, echo, reverb and Sfx"
    [
      t "the pitch effects: vibrato, jump, arpeggio" test_pitch;
      t "the echo: an impulse's echoes, the tail" test_echo;
      t "the presets measured, vary" test_sfx;
      t "the ready-made sounds, three generations" test_generations;
      t "Schroeder's reverb: the decay, the density" test_reverb;
      t "sfxr's buttons: each category itself, 200 times" test_random;
    ]
