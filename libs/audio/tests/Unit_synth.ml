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
  Mixer.play m (Synth.render_stereo beep);
  Alcotest.(check (pair int int)) "a one-shot playing" (1, 0) (Mixer.playing m);
  ignore (Mixer.pull m 4000);
  ignore (Mixer.pull m 1000);
  Alcotest.(check (pair int int)) "played out after 4410 samples" (0, 0) (Mixer.playing m);
  let v = { Synth.source = Wave Sine; frequency = 220.; slide = None; seconds = 0.; volume = 0.5; fade = false; effects = []; envelope = None } in
  Mixer.keep m "hum" v;
  let first = Mixer.pull m 735 in
  Alcotest.(check (pair int int)) "a continuous voice" (0, 1) (Mixer.playing m);
  Alcotest.(check bool) "its volume rising from 0: no click" true (Float.abs first.left.(0) < 0.01);
  let last = Mixer.pull m 735 in
  Alcotest.(check (pair int int)) "not kept: gone after that pull" (0, 0) (Mixer.playing m);
  Alcotest.(check bool) "... having faded out" true (Float.abs last.left.(734) < 0.01);
  (* a loop: asked again, not restarted; stopped, gone after a pull *)
  Mixer.loop m "music" (Synth.render_stereo beep);
  ignore (Mixer.pull m 1000);
  Mixer.loop m "music" (Synth.render_stereo beep);
  Alcotest.(check (list string)) "a loop playing" [ "music" ] (Mixer.looping m);
  let wrapped = Mixer.pull m 4000 in
  (* 1000 + 4000 > 4410: it came around, the beep's start again at 3410 *)
  Alcotest.(check bool) "going around" true (Float.abs wrapped.left.(3410) < 1e-9 && Float.abs wrapped.left.(3500) > 0.1);
  Mixer.stop m "music";
  ignore (Mixer.pull m 735);
  Alcotest.(check (list string)) "stopped" [] (Mixer.looping m);
  for _ = 1 to 40 do Mixer.play m (Synth.render_stereo beep) done;
  Alcotest.(check int) "at most 32 one-shots" Mixer.max_playing (fst (Mixer.playing m))

(* A loop's clock: the samples of it that have gone out, which keeps
 * counting when the loop comes round -- its read position goes back to
 * 0, and a rhythm game timing its steps by that would lose a whole song
 * every time round. *)
let test_loop_clock () =
  let m = Mixer.create () in
  Alcotest.(check (option int)) "nothing playing, no clock" None (Mixer.played m "song");
  Mixer.loop m "song" (Synth.render_stereo beep);
  Alcotest.(check (option int)) "started, nothing out yet" (Some 0) (Mixer.played m "song");
  for _ = 1 to 10 do ignore (Mixer.pull m 735) done;
  Alcotest.(check (option int)) "ten frames of it" (Some 7350) (Mixer.played m "song");
  (* the beep is 4410 samples long: 7350 is its second time round, and
   * the clock did not go back *)
  Alcotest.(check bool) "past the end, still counting" true (Option.get (Mixer.played m "song") > 4410);
  Mixer.stop m "song";
  ignore (Mixer.pull m 735);
  Alcotest.(check (option int)) "stopped: no clock" None (Mixer.played m "song")

(* phase 6's sources and filters in the tree: [naive] turns only the
 * waveforms naive; a fading FM voice darker as it dies (its index
 * following the envelope: the spectrum's centroid falls); a filter
 * keeps the sound's length *)
let centroid (x : Signal.t) : float =
  let m = Spectrum.of_signal x in
  let n = 2 * (Array.length m - 1) in
  let sum = ref 0. and weighted = ref 0. in
  Array.iteri (fun k v -> sum := !sum +. v; weighted := !weighted +. (v *. Spectrum.bin_frequency ~n k)) m;
  !weighted /. !sum

let test_sources () =
  let s = Synth.Together [ Synth.voice (Wave Square) 440.; Synth.voice Noise 1000. ] |> Synth.naive in
  (match s with
  | Together [ Voice { source = Naive Square; _ }; Voice { source = Noise; _ } ] -> ()
  | _ -> Alcotest.fail "naive: the square naive, the noise unchanged");
  let bell = Synth.render (Synth.voice (Fm { ratio = 1.4; index = 5. }) 440. |> Synth.lasting 2. |> Synth.fading) in
  let early = centroid (Array.sub bell (Signal.samples 0.1) 4096) and late = centroid (Array.sub bell (Signal.samples 1.5) 4096) in
  (* the spectrum's centre of mass: 1948 Hz at 0.1 s, 687 at 1.5 s *)
  Alcotest.(check (float 1.)) "the bell's brightness at 0.1 s (Hz)" 1948. early;
  Alcotest.(check (float 1.)) "at 1.5 s, darker (Hz)" 687. late;
  let filtered = Synth.Filtered ({ kind = Low_pass; cutoff = 300.; cutoff_to = 300.; q = 0.707 }, Synth.voice Noise 3000.) in
  Alcotest.(check (float 1e-9)) "a filter keeps the length" 0.3 (Synth.duration filtered);
  Alcotest.(check int) "its samples too" (Signal.samples 0.3) (Array.length (Synth.render filtered));
  (* a continuous voice filtered as the mixer pulls it, three frames of
   * 735 samples, is the same voice filtered in one go: the filter's
   * memory carried from pull to pull, no seam (atanh undoes the
   * mixer's tanh) *)
  let v = { Synth.source = Noise; frequency = 3000.; slide = None; seconds = 0.; volume = 0.5; fade = false; effects = []; envelope = None } in
  let f = { Synth.kind = Low_pass; cutoff = 300.; cutoff_to = 300.; q = 0.707 } in
  let frames (m : Mixer.t) ?filter () =
    Array.concat (List.init 3 (fun _ -> Mixer.keep ?filter m "engine" v; Array.map Float.atanh (Mixer.pull m 735).left))
  in
  let filtered = frames (Mixer.create ()) ~filter:f () and plain = frames (Mixer.create ()) () in
  let expected = Filter.run (Filter.biquad Low_pass ~cutoff:300. ~q:0.707) plain in
  Array.iteri (fun i x -> Alcotest.(check (float 1e-9)) (Printf.sprintf "a filtered continuous voice, sample %d" i) expected.(i) x) filtered

(* a tune twice as fast: half as long, the same notes; changed while
 * looping, it goes on from the same point of the tune, its clock too *)
let test_tempo () =
  let tune = Synth.After [ beep; Synth.voice (Wave Sine) 880. |> Synth.lasting 0.3 ] in
  Alcotest.(check (float 1e-9)) "twice as fast: half as long" 0.2 (Synth.duration (Synth.faster 2. tune));
  let fast = Synth.render (Synth.faster 2. tune) in
  let downs lo hi =
    let c = ref 0 in
    for i = Signal.samples lo + 1 to Signal.samples hi - 1 do
      if fast.(i - 1) >= 0. && fast.(i) < 0. then incr c
    done;
    !c
  in
  Alcotest.(check int) "the same first note: 440 Hz, 22 periods in its 0.05 s" 22 (downs 0. 0.05);
  Alcotest.(check int) "the same second: 880 Hz, 132 in its 0.15 s" 132 (downs 0.05 0.2);
  let m = Mixer.create () in
  let slow = Synth.render tune in
  Mixer.loop m "music" (Signal.both slow);
  ignore (Mixer.pull m (Array.length slow / 4));
  Mixer.change m "music" (Signal.both fast);
  Alcotest.(check (option int)) "the clock goes on" (Some (Array.length slow / 4)) (Mixer.played m "music");
  (* a quarter through the slow tune, a quarter through the fast one:
   * the next sample the fast tune's at its quarter *)
  let next = Mixer.pull m 1 in
  Alcotest.(check (float 1e-6)) "from the same point" (Float.tanh fast.(Array.length fast / 4)) next.left.(0)

(* a processor over a rendered sound: here one that halves the left side
 * and counts how often it was made -- once per rendering *)
let test_processed () =
  let made = ref 0 in
  let halve () =
    incr made;
    fun (st : Signal.stereo) -> Array.iteri (fun i x -> st.left.(i) <- x /. 2.) st.left
  in
  let s = Synth.Processed ({ make = halve; tail = 0.05 }, beep) in
  Alcotest.(check (float 1e-9)) "the tail added to its duration" 0.15 (Synth.duration s);
  let plain = Synth.render beep and st = Synth.render_stereo s in
  Alcotest.(check int) "a tail of silence after it" (Array.length plain + Signal.samples 0.05) (Array.length st.left);
  Alcotest.(check (float 1e-9)) "the left side halved" (plain.(1000) /. 2.) st.left.(1000);
  Alcotest.(check (float 1e-9)) "the right one not: a stereo processor" plain.(1000) st.right.(1000);
  Alcotest.(check (float 1e-9)) "mono: the two mixed" (0.75 *. plain.(1000)) (Synth.render s).(1000);
  Alcotest.(check int) "a processor made for each rendering" 2 !made

let tests =
  Testo.categorize "Synth and Mixer"
    [ t "Music: notes and frequencies" test_notes; t "Synth: durations, no clicks, slides" test_synth; t "Mixer: one-shots, continuous voices" test_mixer;
      t "Mixer: a loop's own clock" test_loop_clock; t "Synth: naive, FM darkening, filters" test_sources;
      t "tempo: faster, and a loop changed while playing" test_tempo; t "Synth: a sound through a processor" test_processed ]
