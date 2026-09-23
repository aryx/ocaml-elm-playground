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
  Alcotest.(check (list string)) "the reverb first" [ "reverb"; "drive"; "eq"; "modulation"; "delay"; "dynamics" ] (Rack.order r);
  let mud = through (Rack.process r) in
  Alcotest.(check bool) "another sound" true (largest_difference usual.left mud.left > 0.1);
  let r = rack on in
  Rack.reorder r [ "reverb" ];
  Rack.reorder r [ "drive"; "eq"; "modulation"; "delay"; "reverb"; "dynamics" ];
  Alcotest.(check (float 0.)) "put back" 0. (largest_difference usual.left (through (Rack.process r)).left)

(* a knob turned between two blocks, a gain 16 times bigger at once: a
 * quiet 100 Hz sine (0.0005: the drive's curve still straight, under
 * the compressor's threshold) moves by at most 0.000007 a sample,
 * 16 times that after, well under the bounds; unramped, the block's
 * edge would be a step of up to 0.0075 *)
let test_ramps () =
  let largest_step knob on value =
    let r = rack [ (on, 1.) ] in
    let x = Array.map (fun v -> 0.0005 *. v) (Oscillator.render Sine ~frequency:100. 0.1) in
    let block k =
      let b = Array.sub x (k * 735) 735 in
      let s = { Signal.left = b; right = Array.copy b } in
      Rack.process r s;
      s.left
    in
    let first = block 0 in
    Rack.set r knob value;
    let y = Array.append first (Array.concat (List.init 4 (fun k -> block (k + 1)))) in
    let step = ref 0. in
    for i = 1 to Array.length y - 1 do
      step := Float.max !step (Float.abs (y.(i) -. y.(i - 1)))
    done;
    !step
  in
  Alcotest.(check bool) "the drive from 12 to 36 dB: no step" true (largest_step "drive.gain" "drive.on" 36. < 0.001);
  Alcotest.(check bool) "the compressor's makeup from 0 to 24 dB: no step" true
    (largest_step "dynamics.makeup" "dynamics.on" 24. < 0.0005)

(* the compressor's needle: its meter through the rack *)
let test_meter () =
  let r = rack [ ("dynamics.on", 1.) ] in
  Alcotest.(check (float 0.)) "no sound, no reduction" 0. (Rack.meter r "dynamics.reduction");
  let x = Array.sub (chord 22050) 0 (Signal.samples 0.2) in
  Rack.process r { left = Array.copy x; right = x };
  Alcotest.(check bool) "while a chord sounds: turned down" true (Rack.meter r "dynamics.reduction" > 3.);
  Alcotest.(check (float 0.)) "no such meter" 0. (Rack.meter r "delay.reduction")

let tests =
  Testo.categorize "Rack"
    [
      t "bypassed stages, a flat EQ" test_bypass;
      t "the knobs' names" test_knobs;
      t "the effects' knobs = their typed settings" test_wrappers;
      t "the order: the reverb before the drive" test_order;
      t "the knobs ramped: no step at a block's edge" test_ramps;
      t "the meters" test_meter;
    ]
