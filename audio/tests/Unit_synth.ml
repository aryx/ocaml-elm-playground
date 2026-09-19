(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Music, Synth, Mixer: the notes, sounds as values, the mixer's
 * one-shots and continuous voices *)

let t = Testo.create

let test_notes () =
  Alcotest.(check (option int)) "A4: 69" (Some 69) (Music.midi_number "A4");
  Alcotest.(check (option int)) "middle C: 60" (Some 60) (Music.midi_number "C4");
  Alcotest.(check (option int)) "F#5: 78" (Some 78) (Music.midi_number "F#5");
  Alcotest.(check (option int)) "Bb3: 58" (Some 58) (Music.midi_number "Bb3");
  Alcotest.(check (option int)) "not a note" None (Music.midi_number "H2");
  Alcotest.(check (float 0.01)) "C4: 261.63 Hz" 261.63 (Music.frequency "C4");
  Alcotest.(check (float 0.01)) "E5: 659.26 Hz" 659.26 (Music.frequency "E5");
  Alcotest.(check (float 1e-9)) "A5: 880, an octave" 880. (Music.frequency "A5")

let beep = Synth.voice (Wave Sine) 440. |> Synth.lasting 0.1

let test_synth () =
  Alcotest.(check (float 1e-9)) "after: the sum" 0.3 (Synth.duration (After [ beep; Synth.lasting 0.2 beep ]));
  Alcotest.(check (float 1e-9)) "together: the longest" 0.2 (Synth.duration (Together [ beep; Synth.lasting 0.2 beep ]));
  let s = Synth.render beep in
  Alcotest.(check int) "0.1 s: 4410 samples" 4410 (Array.length s);
  (* never a click: 5 ms ramps in and out *)
  if Float.abs s.(0) > 1e-9 || Float.abs s.(Array.length s - 1) > 0.01 then Alcotest.failf "a click: %g ... %g" s.(0) s.(Array.length s - 1);
  let peak = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. s in
  Alcotest.(check (float 0.01)) "at volume 0.5" 0.5 peak;
  (* a slide from 440 to 880 over a second: the second half has more
   * periods than the first, 330 to 660 on average *)
  let sweep = Synth.render (Synth.voice (Wave Sine) 440. |> Synth.sliding 880. |> Synth.lasting 1.) in
  let downs lo hi =
    let c = ref 0 in
    for i = lo + 1 to hi - 1 do
      if sweep.(i - 1) >= 0. && sweep.(i) < 0. then incr c
    done;
    !c
  in
  Alcotest.(check int) "the first half: 275 periods (440 to 660 Hz)" 275 (downs 0 22050);
  Alcotest.(check int) "the second: 385 (660 to 880)" 385 (downs 22050 44100)

let test_mixer () =
  let m = Mixer.create () in
  Mixer.play m (Synth.render beep);
  Alcotest.(check (pair int int)) "a one-shot playing" (1, 0) (Mixer.playing m);
  ignore (Mixer.pull m 4000);
  ignore (Mixer.pull m 1000);
  Alcotest.(check (pair int int)) "played out after 4410 samples" (0, 0) (Mixer.playing m);
  let v = { Synth.source = Wave Sine; frequency = 220.; slide = None; seconds = 0.; volume = 0.5; fade = false } in
  Mixer.keep m "hum" v;
  let first = Mixer.pull m 735 in
  Alcotest.(check (pair int int)) "a continuous voice" (0, 1) (Mixer.playing m);
  Alcotest.(check bool) "its volume rising from 0: no click" true (Float.abs first.(0) < 0.01);
  let last = Mixer.pull m 735 in
  Alcotest.(check (pair int int)) "not kept: gone after that pull" (0, 0) (Mixer.playing m);
  Alcotest.(check bool) "... having faded out" true (Float.abs last.(734) < 0.01);
  (* a loop: asked again, not restarted; stopped, gone after a pull *)
  Mixer.loop m "music" (Synth.render beep);
  ignore (Mixer.pull m 1000);
  Mixer.loop m "music" (Synth.render beep);
  Alcotest.(check (list string)) "a loop playing" [ "music" ] (Mixer.looping m);
  let wrapped = Mixer.pull m 4000 in
  (* 1000 + 4000 > 4410: it came around, the beep's start again at 3410 *)
  Alcotest.(check bool) "going around" true (Float.abs wrapped.(3410) < 1e-9 && Float.abs wrapped.(3500) > 0.1);
  Mixer.stop m "music";
  ignore (Mixer.pull m 735);
  Alcotest.(check (list string)) "stopped" [] (Mixer.looping m);
  for _ = 1 to 40 do Mixer.play m (Synth.render beep) done;
  Alcotest.(check int) "at most 32 one-shots" Mixer.max_playing (fst (Mixer.playing m))

let tests =
  Testo.categorize "Synth and Mixer"
    [ t "Music: notes and frequencies" test_notes; t "Synth: durations, no clicks, slides" test_synth; t "Mixer: one-shots, continuous voices" test_mixer ]
