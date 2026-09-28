(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_dx_envelope.mli *)

let t = Testo.create

(* [until env stage]: the samples until [env] reaches [stage], its
 * levels on the way *)
let until (env : Dx_envelope.t) (stage : int) : int * float list =
  let rec go n acc = if Dx_envelope.stage env >= stage || n > 10_000_000 then (n, List.rev acc) else go (n + 1) (Dx_envelope.next env :: acc) in
  go 0 []

let strike ?(l1 = 99) () : Dx_envelope.t = Dx_envelope.create ~rates:[| 99; 50; 99; 99 |] ~levels:[| l1; 0; 0; 0 |] ()

let test_levels () =
  Alcotest.(check (list int)) "0-99 onto 0-127" [ 0; 20; 46; 48; 78; 127 ] (List.map Dx_envelope.scale_output_level [ 0; 5; 19; 20; 50; 99 ]);
  Alcotest.(check (float 1e-9)) "full: 2 cycles, 4 pi radians" 2. (Dx_envelope.gain 3840.);
  Alcotest.(check (float 1e-9)) "256 steps a doubling: 6.02 dB" (-6.0206) (Float.round (Dx_envelope.decibels 3584. *. 1e4) /. 1e4)

(* the worked example: the attack's jump and curve, the decay's line *)
let test_strike () =
  let env = strike () in
  let attack, levels = until env 1 in
  (* from silence to 1716, then 10 doublings short of 17 times R1 99's
   * 14 steps *)
  Alcotest.(check (float 0.)) "the first sample: jumped, then up" (1716. +. (10. *. 14.)) (List.hd levels);
  Alcotest.(check int) "the attack at R1 99: samples" 33 attack;
  let decay, levels = until env 2 in
  Alcotest.(check int) "the decay at R2 50, 99 to 0: samples" 61184 decay;
  (* a straight line in dB: a second in, 44,100 / 16 steps lost *)
  let at_one_second = List.nth levels 44099 in
  Alcotest.(check (float 0.01)) "dB a second in" (-64.82) (Dx_envelope.decibels at_one_second);
  (* a rate is a speed: from L1 50, the same slope, a shorter time *)
  let env = strike ~l1:50 () in
  ignore (until env 1);
  let decay, _ = until env 2 in
  Alcotest.(check int) "the decay from 50: samples" 36608 decay

(* held at L3 while the key is, then to L4 *)
let test_hold_release () =
  let env = Dx_envelope.create ~rates:[| 99; 99; 99; 60 |] ~levels:[| 99; 90; 80; 0 |] () in
  ignore (until env 3);
  let held = Dx_envelope.next env in
  for _ = 1 to 44100 do
    ignore (Dx_envelope.next env)
  done;
  Alcotest.(check (float 0.)) "L3 held a second" held (Dx_envelope.next env);
  Dx_envelope.key_up env;
  let release, _ = until env 4 in
  (* L3 80 is 3264 steps; to 16 at R4 60's 6 x 2^11 / 65536 a sample *)
  Alcotest.(check int) "released at R4 60: samples" 17323 release

let tests =
  Testo.categorize "Dx_envelope"
    [ t "levels and gains" test_levels; t "struck: the jump, the curve, the line" test_strike; t "held, then released" test_hold_release ]
