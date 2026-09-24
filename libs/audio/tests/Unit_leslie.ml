(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_leslie.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate

(* [x] through the cabinet at [fast], in blocks of 735 *)
let through (l : Leslie.t) ~(fast : bool) (x : Signal.t) : Signal.stereo =
  let n = Array.length x in
  let s = { Signal.left = Array.copy x; right = Array.copy x } and k = ref 0 in
  while !k < n do
    let m = min 735 (n - !k) in
    let b = { Signal.left = Array.sub s.left !k m; right = Array.sub s.right !k m } in
    Leslie.process l ~fast b;
    Array.blit b.left 0 s.left !k m;
    Array.blit b.right 0 s.right !k m;
    k := !k + m
  done;
  s

let test_speeds () =
  let l = Leslie.create () in
  Alcotest.(check (float 1e-9)) "chorale at first" Leslie.horn_slow (Leslie.horn l);
  ignore (through l ~fast:true (Array.make (Signal.samples 0.5) 0.));
  Alcotest.(check (float 0.01)) "the horn after 0.5 s: 63% of the way" 4.59 (Leslie.horn l);
  ignore (through l ~fast:true (Array.make (Signal.samples 0.7) 0.));
  Alcotest.(check (float 0.01)) "the drum after 1.2 s: 63% too" 3.80 (Leslie.drum l);
  ignore (through l ~fast:true (Array.make (Signal.samples 5.) 0.));
  Alcotest.(check (float 0.01)) "the horn at tremolo" Leslie.horn_fast (Leslie.horn l);
  ignore (through l ~fast:false (Array.make (Signal.samples 0.5) 0.));
  Alcotest.(check (float 0.01)) "slowing: 63% of the way back" 3.01 (Leslie.horn l)

(* a 5 kHz sine (the horn's alone: the drum's low-pass lets it through
 * at -32 dB) at tremolo, settled: its frequency in 5 ms windows, the
 * highest and the lowest over a turn, and its level likewise *)
let test_doppler () =
  let l = Leslie.create () in
  ignore (through l ~fast:true (Array.make (Signal.samples 4.) 0.));
  let n = Signal.samples 1. in
  let x = Array.init n (fun i -> sin (2. *. Float.pi *. 5000. *. float_of_int i /. rate)) in
  let y = (through l ~fast:true x).left in
  let w = Signal.samples 0.005 in
  let frequencies = ref [] and levels = ref [] in
  for k = 2 to (n / w) - 1 do
    let a = k * w in
    let ups = ref [] and peak = ref 0. in
    for i = a + 1 to a + w - 1 do
      peak := Float.max !peak (Float.abs y.(i));
      if y.(i - 1) < 0. && y.(i) >= 0. then ups := (float_of_int (i - 1) +. (-.y.(i - 1) /. (y.(i) -. y.(i - 1)))) :: !ups
    done;
    (match (!ups, List.rev !ups) with
    | last :: _, first :: _ -> frequencies := (float_of_int (List.length !ups - 1) *. rate /. (last -. first)) :: !frequencies
    | _ -> ());
    levels := !peak :: !levels
  done;
  let top = List.fold_left Float.max 0. and bottom = List.fold_left Float.min Float.infinity in
  Alcotest.(check (float 3.)) "the horn coming: up" 5093. (top !frequencies);
  Alcotest.(check (float 3.)) "going away: down" 4907. (bottom !frequencies);
  (* the horn's 1, and the drum's copy, 32 dB under, adding *)
  Alcotest.(check (float 0.01)) "loudest, facing the microphone" 1.017 (top !levels);
  Alcotest.(check (float 0.02)) "quietest, turned away" 0.20 (bottom !levels)

(* the two microphones a quarter turn apart: the same sound, not the
 * same samples *)
let test_stereo () =
  let x = Array.init (Signal.samples 0.5) (fun i -> sin (2. *. Float.pi *. 3000. *. float_of_int i /. rate)) in
  let s = through (Leslie.create ()) ~fast:true x in
  let differ = ref 0. in
  Array.iteri (fun i l -> differ := Float.max !differ (Float.abs (l -. s.right.(i)))) s.left;
  Alcotest.(check bool) "left and right differ" true (!differ > 0.1)

let tests =
  Testo.categorize "Leslie"
    [
      t "the rotors: speeding up, slowing down" test_speeds;
      t "the horn: its Doppler swing and its tremolo" test_doppler;
      t "two microphones" test_stereo;
    ]
