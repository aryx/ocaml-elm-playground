(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_rack.mli *)

let t = Testo.create

(* a fifth of a second of two notes, then silence *)
let chord n =
  Array.init n (fun i ->
      let t = float_of_int i /. float_of_int Signal.rate in
      if t > 0.2 then 0. else 0.3 *. (sin (2. *. Float.pi *. 220. *. t) +. sin (2. *. Float.pi *. 277.18 *. t)))

let through (f : Signal.stereo -> unit) : Signal.stereo =
  let x = chord 22050 in
  let s = { Signal.left = Array.copy x; right = Array.copy x } in
  f s;
  s

let largest_difference (a : Signal.t) (b : Signal.t) : float = Array.fold_left Float.max 0. (Array.mapi (fun i v -> Float.abs (v -. b.(i))) a)

(* a rack, and the knobs named turned *)
let rack (knobs : (string * float) list) : Rack.t =
  let r = Rack.standard () in
  List.iter (fun (k, x) -> Rack.set r k x) knobs;
  r

let change (knobs : (string * float) list) : float =
  let x = chord 22050 in
  largest_difference (through (Rack.process (rack knobs))).left x

let test_bypass () =
  Alcotest.(check (float 0.)) "all off: untouched" 0. (change []);
  Alcotest.(check (float 1e-9)) "the EQ flat: untouched" 0. (change [ ("eq.on", 1.) ]);
  Alcotest.(check bool) "the drive on: changed" true (change [ ("drive.on", 1.) ] > 0.1);
  Alcotest.(check bool) "unknown names ignored" true (change [ ("flanger.on", 1.); ("delay.speed", 3.) ] = 0.)

let test_knobs () =
  let names l = List.map (fun (k : Effect.knob) -> k.name) l in
  Alcotest.(check (list string)) "the standard rack's knobs, known before one is made" (names Rack.standard_knobs) (names (Rack.knobs (Rack.standard ())));
  Alcotest.(check (list string)) "the delay's"
    [ "delay.on"; "delay.time"; "delay.feedback"; "delay.tone"; "delay.pingpong"; "delay.mix" ]
    (List.filter (fun n -> String.starts_with ~prefix:"delay." n) (names Rack.standard_knobs))

(* an effect's knobs turned by name = its typed settings *)
let test_wrappers () =
  let e = Delay.effect () in
  e.set "time" 0.1;
  e.set "pingpong" 1.;
  let typed = { Delay.time = 0.1; feedback = 0.4; tone = 3000.; ping_pong = true; mix = 0.3 } in
  Alcotest.(check (float 0.)) "Delay" 0. (largest_difference (through e.process).right (through (Delay.process (Delay.create ()) typed)).right);
  let e = Reverb.effect () in
  e.set "kind" 1.;
  e.set "time" 1.;
  let typed = { Reverb.kind = Freeverb; seconds = 1.; damping = 0.3; mix = 0.25 } in
  Alcotest.(check (float 0.)) "Reverb" 0. (largest_difference (through e.process).left (through (Reverb.process (Reverb.create ()) typed)).left)

(* the drive after the reverb: its tail distorted too, a different
 * sound; the order put back, the same as a rack made in it *)
let test_order () =
  let on = [ ("drive.on", 1.); ("drive.gain", 24.); ("reverb.on", 1.) ] in
  let usual = through (Rack.process (rack on)) in
  let r = rack on in
  Rack.reorder r [ "reverb" ];
  Alcotest.(check (list string)) "the reverb first" [ "reverb"; "drive"; "eq"; "delay" ] (Rack.order r);
  let mud = through (Rack.process r) in
  Alcotest.(check bool) "another sound" true (largest_difference usual.left mud.left > 0.1);
  let r = rack on in
  Rack.reorder r [ "reverb" ];
  Rack.reorder r [ "drive"; "eq"; "delay"; "reverb" ];
  Alcotest.(check (float 0.)) "put back" 0. (largest_difference usual.left (through (Rack.process r)).left)

let tests =
  Testo.categorize "Rack"
    [
      t "bypassed stages, a flat EQ" test_bypass;
      t "the knobs' names" test_knobs;
      t "the effects' knobs = their typed settings" test_wrappers;
      t "the order: the reverb before the drive" test_order;
    ]
