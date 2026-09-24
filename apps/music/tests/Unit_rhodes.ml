(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_rhodes.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate
let preset name = List.assoc name Voice_rhodes.presets

(* [play p keys ~velocity seconds]: the keys pressed at 0, let go at
 * [release]; both sides *)
let play ?(release = 10.) ?(velocity = 1.) (p : Voice_rhodes.patch) (keys : int list) (seconds : float) : Signal.stereo * Voice_rhodes.t =
  let v = Voice_rhodes.create p in
  let i = Voice_rhodes.instrument v in
  List.iter (fun k -> i.note_on k velocity) keys;
  let n = Signal.samples seconds in
  let left = Array.make n 0. and right = Array.make n 0. in
  let at = ref 0 in
  while !at < n do
    if !at >= Signal.samples release && !at < Signal.samples release + 735 then List.iter i.note_off keys;
    let m = min 735 (n - !at) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    i.fill b;
    Array.blit b.left 0 left !at m;
    Array.blit b.right 0 right !at m;
    at := !at + m
  done;
  ({ Signal.left; right }, v)

(* the amplitude of [f] in [x] from sample [a] for [n], Hann-windowed *)
let amplitude (x : Signal.t) (f : float) (a : int) (n : int) : float =
  let re = ref 0. and im = ref 0. and sum = ref 0. in
  for i = a to a + n - 1 do
    let hann = 0.5 -. (0.5 *. cos (2. *. Float.pi *. float_of_int (i - a) /. float_of_int n)) in
    let w = 2. *. Float.pi *. f *. float_of_int i /. rate in
    re := !re +. (hann *. x.(i) *. cos w);
    im := !im +. (hann *. x.(i) *. sin w);
    sum := !sum +. hann
  done;
  2. *. sqrt ((!re *. !re) +. (!im *. !im)) /. !sum

(* the second harmonic against the first, in dB, 0.2 s into C4 *)
let c4 = 440. *. Float.pow 2. (-9. /. 12.)

let second ?(velocity = 1.) (p : Voice_rhodes.patch) : float =
  let s, _ = play ~velocity p [ 60 ] 0.4 in
  let a = Signal.samples 0.2 and n = 4096 in
  20. *. log10 (amplitude s.left (2. *. c4) a n /. amplitude s.left c4 a n)

(* the harmonics 2 to 5 against the first, summed, in dB *)
let richness ?(velocity = 1.) (p : Voice_rhodes.patch) : float =
  let s, _ = play ~velocity p [ 60 ] 0.4 in
  let a = Signal.samples 0.2 and n = 4096 in
  let h k = amplitude s.left (float_of_int k *. c4) a n in
  let upper = List.fold_left (fun acc k -> acc +. (h k *. h k)) 0. [ 2; 3; 4; 5 ] in
  10. *. log10 (upper /. (h 1 *. h 1))

let test_bark () =
  let p = preset "mark I" in
  Alcotest.(check (list (float 0.1))) "Rhodes: the 2nd harmonic at velocity 0.2 and 1 (dB)" [ -24.8; 1.2 ] [ second ~velocity:0.2 p; second p ];
  (* near the centre, the swing crosses the bell's top: twice the
   * frequency; off it, the fundamental *)
  Alcotest.(check (list (float 0.1))) "voicing 0 and 1, velocity 0.5 (dB)" [ 10.5; -14.3 ]
    [ second ~velocity:0.5 { p with voicing = 0. }; second ~velocity:0.5 { p with voicing = 1. } ];
  let w = preset "wurlitzer" in
  Alcotest.(check (list (float 0.1))) "Wurlitzer: harmonics 2-5 at velocity 0.2 and 1 (dB)" [ -25.8; -11.5 ] [ richness ~velocity:0.2 w; richness w ];
  Alcotest.(check (list (float 0.1))) "Rhodes: harmonics 2-5 at velocity 0.2 and 1 (dB)" [ -23.0; 4.3 ] [ richness ~velocity:0.2 p; richness p ]

let test_release () =
  List.iter
    (fun name ->
      let _, v = play ~release:0.5 (preset name) [ 48; 60; 64 ] 2. in
      Alcotest.(check int) (name ^ ": let go, freed") 0 (Voice_rhodes.voices v))
    [ "mark I"; "wurlitzer"; "clavinet" ]

(* the Suitcase: left loud when right is quiet *)
let test_tremolo () =
  let s, _ = play (preset "suitcase") [ 60 ] 1. in
  let rms (x : Signal.t) a = sqrt (Array.fold_left (fun acc y -> acc +. (y *. y)) 0. (Array.sub x a 441) /. 441.) in
  let ratios = List.init 8 (fun k -> rms s.left (k * 2205) /. rms s.right (k * 2205)) in
  (* depth 0.8: at the extremes, a side at 1, the other at 0.2 *)
  Alcotest.(check (float 0.01)) "left over right, loudest" 3.34 (List.fold_left Float.max 0. ratios);
  Alcotest.(check (float 0.01)) "and quietest" 0.20 (List.fold_left Float.min 10. ratios)

let peak (s : Signal.t) : float = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. s

(* each preset on a phrase: a chord, then a line, a velocity each *)
let riff (p : Voice_rhodes.patch) : Signal.t =
  let v = Voice_rhodes.create p in
  let i = Voice_rhodes.instrument v in
  let notes = [ (0, [ 48; 55; 60; 64 ], 0.7); (30, [ 67 ], 0.4); (40, [ 69 ], 0.6); (50, [ 72 ], 1.); (60, [ 53; 60; 65; 69 ], 1.) ] in
  Array.concat
    (List.init 120 (fun frame ->
         List.iter
           (fun (at, keys, velocity) ->
             if frame = at then List.iter (fun k -> i.note_on k velocity) keys;
             if frame = at + 25 then List.iter i.note_off keys)
           notes;
         let b = { Signal.left = Array.make 735 0.; right = Array.make 735 0. } in
         i.fill b;
         b.left))

let test_peaks () =
  Alcotest.(check (list (pair string (float 0.01)))) "the presets' peaks"
    [ ("mark I", 0.63); ("bark", 0.82); ("suitcase", 0.28); ("wurlitzer", 0.54); ("clavinet", 0.90) ]
    (List.map (fun (name, p) -> (name, peak (riff p))) Voice_rhodes.presets)

let test_text () =
  List.iter
    (fun (name, p) ->
      match Voice_rhodes.of_string (Voice_rhodes.to_string p) with
      | Ok q -> Alcotest.(check bool) (name ^ ": read back") true (q = p)
      | Error e -> Alcotest.failf "%s: %s" name e)
    Voice_rhodes.presets

let tests =
  Testo.categorize "Rhodes"
    (List.map
       (fun (name, p) ->
         let file = "rhodes_" ^ String.map (fun c -> if c = ' ' then '_' else c) name in
         t ("golden WAV: " ^ name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" file (riff p)))
       Voice_rhodes.presets
    @ [
        t "the bark: harmonics growing with velocity, the voicing" test_bark;
        t "released notes freed" test_release;
        t "the Suitcase's stereo tremolo" test_tremolo;
        t "the presets' peaks" test_peaks;
        t "the patches as text" test_text;
      ])
