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

(* a module of ours, a looped square on C-2 in the first row: heard,
 * then stopped; a file that isn't one, nothing *)
let test_module () =
  let square = Mod.data_of_floats (Array.init 32 (fun i -> if i < 16 then 0.5 else -0.5)) in
  let blank : Mod.instrument = { name = ""; finetune = 0; volume = 0; loop_start = 0; loop_length = 0; data = "" } in
  let song : Mod.song =
    {
      title = "";
      instruments = Array.init 31 (fun k -> if k = 0 then { blank with volume = 64; loop_length = 32; data = square } else blank);
      restart = 127;
      positions = [| 0 |];
      patterns = [| Array.init 64 (fun r -> Array.init 4 (fun c -> if r = 0 && c = 0 then { Mod.empty_cell with instrument = 1; period = 428 } else Mod.empty_cell)) |];
      tag = "M.K.";
    }
  in
  Audio.play_module "test-module" (Mod.to_string song);
  if loudest (Audio.pull 735) < 0.1 then Alcotest.fail "the module isn't heard";
  Audio.stop "test-module";
  ignore (Audio.pull 735);
  Alcotest.(check (float 0.)) "stopped" 0. (loudest (Audio.pull 735));
  Audio.play_module "test-not-a-module" "not a module";
  Alcotest.(check (float 0.)) "not a module: nothing" 0. (loudest (Audio.pull 735))

let tests = Testo.categorize "Audio" [ t "an instrument, by name" test_instrument; t "a module, by name" test_module ]
