(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_dx7.mli *)

let t = Testo.create
let op = Dx7_voice.initial.operators.(0)
let cents (a : float) (b : float) : float = 1200. *. Float.log2 (a /. b)

(* the frequencies: ratios, fine, detune, fixed *)
let test_frequencies () =
  Alcotest.(check (float 1e-9)) "INIT VOICE at A4" 440. (Dx7_voice.frequency op 69);
  Alcotest.(check (float 1e-9)) "coarse 0: an octave down" 220. (Dx7_voice.frequency { op with coarse = 0 } 69);
  Alcotest.(check (float 1e-9)) "coarse 3 fine 50: 4.5 times" 1980. (Dx7_voice.frequency { op with coarse = 3; fine = 50 } 69);
  Alcotest.(check (float 0.01)) "detune +7 at A4 (cents)" 6.80 (cents (Dx7_voice.frequency { op with detune = 14 } 69) 440.);
  Alcotest.(check (float 0.01)) "detune +7 at A1: more cents" 14.69 (cents (Dx7_voice.frequency { op with detune = 14 } 33) 55.);
  Alcotest.(check (float 1e-9)) "fixed, coarse 2: 100 Hz" 100. (Dx7_voice.frequency { op with fixed = true; coarse = 2 } 69);
  Alcotest.(check (float 1e-9)) "at every key" 100. (Dx7_voice.frequency { op with fixed = true; coarse = 2 } 30)

(* the levels: the keyboard's scaling, the velocity's, the rates' *)
let test_scaling () =
  let lin = { op with break_point = 39; left_depth = 99; left_curve = 0; right_depth = 99; right_curve = 2 } in
  Alcotest.(check int) "-lin 99, 3 octaves below the break point" (-95) (Dx7_voice.level_scaling lin (39 + 17 - 36));
  (* an exponential curve starts gently: 3 steps an octave away, 15 at
   * three octaves where the linear one is at 95 *)
  Alcotest.(check int) "+exp 99, an octave above" 3 (Dx7_voice.level_scaling lin (39 + 17 + 12));
  Alcotest.(check int) "+exp 99, 3 octaves above" 15 (Dx7_voice.level_scaling lin (39 + 17 + 36));
  Alcotest.(check int) "at the break point: none" 0 (Dx7_voice.level_scaling lin (39 + 17));
  (* the velocity's steps: Dexed's table, 239 its middle *)
  let v = { op with velocity = 7 } in
  Alcotest.(check int) "velocity 127, sensitivity 7: 5.3 dB up" 224 (Dx7_voice.output_level v ~note:60 ~velocity:127 - (127 lsl 5));
  Alcotest.(check int) "velocity 64, sensitivity 7: 10.5 dB down" (-448) (Dx7_voice.output_level v ~note:60 ~velocity:64 - (127 lsl 5));
  Alcotest.(check int) "sensitivity 0: velocity ignored" (127 lsl 5) (Dx7_voice.output_level op ~note:60 ~velocity:1);
  Alcotest.(check int) "rate scaling 7 at C7: 21 more qrate" 21 (Dx7_voice.rate_scaling { op with rate_scaling = 7 } 96)

(* the bytes: every preset's 128 back to it; a cartridge *)
let test_bytes () =
  List.iter
    (fun (name, (p : Dx7_voice.patch)) ->
      let back = Dx7_voice.of_packed (Dx7_voice.to_packed p) in
      Alcotest.(check bool) (name ^ ": its 128 bytes read back") true ({ back with name = p.name } = p);
      Alcotest.(check string) (name ^ ": the name, padded") (Printf.sprintf "%-10s" p.name) back.name)
    (("initial", Dx7_voice.initial) :: Dx7_voice.presets);
  let voices = Array.of_list (List.map snd Dx7_voice.presets) in
  let syx = Dx7_voice.to_cartridge voices in
  Alcotest.(check int) "a cartridge: 4104 bytes" 4104 (String.length syx);
  (match Dx7_voice.cartridge syx with
  | Ok v ->
      Alcotest.(check int) "32 voices" 32 (Array.length v);
      Alcotest.(check string) "the first, ours" "E.PIANO   " v.(0).name;
      Alcotest.(check string) "the rest, INIT VOICE" "INIT VOICE" v.(31).name
  | Error e -> Alcotest.fail e);
  let broken = Bytes.of_string syx in
  Bytes.set broken 100 'x';
  Alcotest.(check bool) "a byte changed: the checksum says so" true (Dx7_voice.cartridge (Bytes.to_string broken) = Error "the checksum is wrong");
  Alcotest.(check bool) "too short" true (Result.is_error (Dx7_voice.cartridge "\xf0\x43"))

(* the patches as text, and back *)
let test_text () =
  List.iter
    (fun (name, p) ->
      match Dx7_voice.of_string (Dx7_voice.to_string p) with
      | Ok q -> Alcotest.(check bool) (name ^ ": read back") true (q = p)
      | Error e -> Alcotest.failf "%s: %s" name e)
    Dx7_voice.presets;
  let names = List.map (fun (k : Dx7_voice.knob) -> k.name) Dx7_voice.knobs in
  Alcotest.(check int) "the names unique" (List.length names) (List.length (List.sort_uniq compare names));
  Alcotest.(check int) "the DX7's 145 parameters (the name aside)" 145 (List.length names)

(* [play ?patch ~velocity keys seconds]: the keys pressed at 0, let go
 * at [release], the left side *)
let play ?(release = 1.) ?(velocity = 1.) (p : Dx7_voice.patch) (keys : int list) (seconds : float) : Signal.t * Dx7_voice.t =
  let v = Dx7_voice.create p in
  let i = Dx7_voice.instrument v in
  List.iter (fun k -> i.note_on k velocity) keys;
  let n = Signal.samples seconds in
  let out = Array.make n 0. in
  let at = ref 0 in
  while !at < n do
    if !at >= Signal.samples release && !at < Signal.samples release + 735 then List.iter i.note_off keys;
    let m = min 735 (n - !at) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    i.fill b;
    Array.blit b.left 0 out !at m;
    at := !at + m
  done;
  (out, v)

let peak (s : Signal.t) : float = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. s

(* INIT VOICE: a sine at the key, as loud as a carrier at full is heard;
 * freed after its release *)
let test_voice () =
  let s, v = play Dx7_voice.initial [ 69 ] 1.5 in
  let m = Spectrum.magnitudes (Spectrum.fft (Array.sub s 4096 4096)) in
  Alcotest.(check int) "A4: its peak near bin 41 (440 Hz)" 41 (Spectrum.peak m);
  Alcotest.(check (float 1e-3)) "its level: 2 cycles, an eighth heard, at volume 0.7" 0.0875 (peak (Array.sub s 4096 4096));
  Alcotest.(check int) "released: freed" 0 (Dx7_voice.voices v);
  let _, v = play ~release:10. Dx7_voice.initial (List.init 20 (fun k -> 40 + k)) 0.1 in
  Alcotest.(check int) "20 keys: 16 voices" 16 (Dx7_voice.voices v)

(* each preset's peak over the riff, to keep them under clipping *)
let riff (p : Dx7_voice.patch) : Signal.t =
  let v = Dx7_voice.create p in
  let i = Dx7_voice.instrument v in
  let chords = [ (0, [ 48; 60; 64; 67 ], 1.); (30, [ 53; 65; 69; 72 ], 0.5); (60, [ 55; 67; 71; 74 ], 0.8) ] in
  Array.concat
    (List.init 108 (fun frame ->
         List.iter
           (fun (at, keys, velocity) ->
             if frame = at then List.iter (fun k -> i.note_on k velocity) keys;
             if frame = at + 28 then List.iter i.note_off keys)
           chords;
         let b = { Signal.left = Array.make 735 0.; right = Array.make 735 0. } in
         i.fill b;
         b.left))

let test_peaks () =
  Alcotest.(check (list (pair string (float 0.01)))) "the presets' peaks"
    [ ("e.piano", 0.63); ("brass", 0.69); ("bass", 0.35); ("bells", 0.53); ("marimba", 0.28); ("organ", 0.55) ]
    (List.map (fun (name, p) -> (name, peak (riff p))) Dx7_voice.presets)

let tests =
  Testo.categorize "DX7"
    (List.map
       (fun (name, p) -> t ("golden WAV: " ^ name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" ("dx7_" ^ name) (riff p)))
       Dx7_voice.presets
    @ [
        t "the frequencies: ratios, fine, detune, fixed" test_frequencies;
        t "the scalings: keyboard, velocity, rates" test_scaling;
        t "the bytes: a voice, a cartridge" test_bytes;
        t "the patches as text" test_text;
        t "a voice: INIT VOICE, 16 voices" test_voice;
        t "the presets' peaks" test_peaks;
      ])
