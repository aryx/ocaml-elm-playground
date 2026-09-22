(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Audio's instruments: kept by name, played by note names, made afresh
 * after a stop *)

let t = Testo.create

let loudest (s : Signal.stereo) = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. s.left

let test_instrument () =
  let made = ref 0 in
  let make () =
    incr made;
    Instrument.sine ()
  in
  let keys = Audio.instrument "test-keys" make in
  (* asked for at every frame: the same one *)
  let again = Audio.instrument "test-keys" make in
  Alcotest.(check int) "made once" 1 !made;
  Audio.note_on again "A4";
  Audio.note_on keys "H9";
  if loudest (Audio.pull 735) < 0.1 then Alcotest.fail "A4 not sounding";
  Audio.note_off keys "A4";
  ignore (Audio.pull 735);
  Alcotest.(check (float 0.)) "let go: silent" 0. (loudest (Audio.pull 735));
  Audio.stop "test-keys";
  ignore (Audio.pull 735);
  ignore (Audio.instrument "test-keys" make);
  Alcotest.(check int) "stopped, then asked for: made afresh" 2 !made;
  Audio.stop "test-keys";
  ignore (Audio.pull 735)

let tests = Testo.categorize "Audio" [ t "an instrument, by name" test_instrument ]
