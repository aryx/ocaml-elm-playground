(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Instrument, and the Mixer playing one: notes at the frames
 * they were played, the gate's and the knob's ramps *)

let t = Testo.create

(* a golden run's pulls: 735 samples a frame, [events f] before frame f's *)
let run ~frames (events : int -> unit) (m : Mixer.t) : Signal.t =
  List.init frames (fun f ->
      events f;
      (Mixer.pull m 735).left)
  |> Array.concat

let peak (s : Signal.t) lo hi =
  let p = ref 0. in
  for i = lo to hi - 1 do
    p := Float.max !p (Float.abs s.(i))
  done;
  !p

(* the frequency from the downward zero crossings, first to last *)
let pitch (s : Signal.t) lo hi =
  let downs = ref [] in
  for i = lo + 1 to hi - 1 do
    if s.(i - 1) >= 0. && s.(i) < 0. then downs := i :: !downs
  done;
  match (!downs, List.rev !downs) with
  | last :: _, first :: _ when last > first ->
      float_of_int (List.length !downs - 1) *. float_of_int Signal.rate /. float_of_int (last - first)
  | _ -> 0.

let test_frames () =
  let m = Mixer.create () in
  let keys = Instrument.sine () in
  Mixer.instrument m "keys" keys;
  (* A4 pressed at frame 10, let go at frame 40 *)
  let s =
    run ~frames:60
      (fun f ->
        if f = 10 then keys.note_on 69 1.;
        if f = 40 then keys.note_off 69)
      m
  in
  Alcotest.(check (float 0.)) "silent before frame 10's first sample, 7350" 0. (peak s 0 7350);
  if peak s 7350 (7350 + 735) < 0.1 then Alcotest.fail "not sounding in frame 10";
  (* the gate's 5 ms: 221 samples to full, the volume 0.5 through the
   * mixer's tanh *)
  for i = 0 to 220 do
    let under = Float.tanh (0.5 *. Float.min 1. (float_of_int (i + 1) /. 220.5)) in
    if Float.abs s.(7350 + i) > under +. 1e-12 then Alcotest.failf "sample %d of the attack above the ramp: %g > %g" i s.(7350 + i) under
  done;
  Alcotest.(check (float 1e-3)) "then full, tanh 0.5" (Float.tanh 0.5) (peak s (15 * 735) (35 * 735));
  Alcotest.(check (float 1.)) "A4, 440 Hz" 440. (pitch s (15 * 735) (35 * 735));
  if peak s (40 * 735) ((40 * 735) + 100) < 0.1 then Alcotest.fail "cut at once instead of ramped down";
  Alcotest.(check (float 0.)) "silent 221 samples after frame 40" 0. (peak s ((40 * 735) + 221) (60 * 735))

let test_ramp () =
  (* the .mli's worked example: the volume from 0.2 to 0.8 over a block;
   * a low note, so the wave moves little from sample to sample and the
   * volume's step shows *)
  let keys = Instrument.sine () in
  let block () : Signal.stereo = { left = Array.make 735 0.; right = Array.make 735 0. } in
  keys.note_on 24 1.;
  (* C1, 32.7 Hz: at most 2 pi 32.7 / 44,100 = 0.0047 a sample *)
  keys.set "volume" 0.2;
  let b = block () in
  keys.fill b;
  keys.fill b;
  keys.set "volume" 0.8;
  let before = b.left.(734) in
  keys.fill b;
  let jump = ref (Float.abs (b.left.(0) -. before)) in
  for i = 1 to 734 do
    jump := Float.max !jump (Float.abs (b.left.(i) -. b.left.(i - 1)))
  done;
  if !jump > 0.0047 +. 0.00082 then Alcotest.failf "a step of %g: the knob jumped" !jump;
  (* arrived: C1's periods are 1349 samples, so three blocks see its
   * peak at the new volume; an unknown knob changes nothing *)
  keys.set "cutoff" 0.1;
  let most = ref 0. in
  for _ = 1 to 3 do
    keys.fill b;
    most := Float.max !most (peak b.left 0 735)
  done;
  Alcotest.(check (float 1e-3)) "at 0.8" 0.8 !most

let test_legato () =
  let keys = Instrument.sine () in
  let b : Signal.stereo = { left = Array.make 22050 0.; right = Array.make 22050 0. } in
  keys.note_on 69 1.;
  keys.note_on 81 1.;
  (* A4 let go while A5 sounds: ignored *)
  keys.note_off 69;
  keys.fill b;
  Alcotest.(check (float 2.)) "A5 sounding, 880 Hz" 880. (pitch b.left 0 22050);
  keys.note_off 81;
  keys.fill b;
  Alcotest.(check (float 0.)) "then silent" 0. (peak b.left 221 22050)

let test_stop () =
  let m = Mixer.create () in
  let keys = Instrument.sine () in
  Mixer.instrument m "keys" keys;
  keys.note_on 60 1.;
  ignore (Mixer.pull m 735);
  (* asked again: the same one keeps playing *)
  Mixer.instrument m "keys" (Instrument.sine ());
  if peak (Mixer.pull m 735).left 0 735 < 0.1 then Alcotest.fail "asking again replaced it";
  Alcotest.(check (list string)) "playing" [ "keys" ] (Mixer.instruments m);
  Mixer.stop m "keys";
  Alcotest.(check (list string)) "stopping: not listed" [] (Mixer.instruments m);
  let last = (Mixer.pull m 735).left in
  if Float.abs last.(734) > 0.01 then Alcotest.failf "not faded out: %g" last.(734);
  Alcotest.(check (float 0.)) "gone" 0. (peak (Mixer.pull m 735).left 0 735)

let tests =
  Testo.categorize "Instrument"
    [
      t "a note at the frames it was played" test_frames;
      t "a knob ramped over a block" test_ramp;
      t "legato: another key's release ignored" test_legato;
      t "asked again, then stopped" test_stop;
    ]
