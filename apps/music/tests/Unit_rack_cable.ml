(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_rack_cable.mli *)

let t = Testo.create
let lowest (c : Rack_cable.t) : float = List.fold_left (fun m (_, y) -> Float.min m y) infinity (Rack_cable.points c)

(* the worked example: ends 300 apart, 384 long, a sag of 97; at rest
 * it hangs at 107, lower: the rope stretches under its weight, 15
 * relaxations a step not quite stiff enough (Particles.mli) *)
let test_sag () =
  Alcotest.(check (float 1e-9)) "the rest length" 384. (Rack_cable.rest_length 300.);
  Alcotest.(check (float 0.1)) "the parabola's sag" 97.2 (Rack_cable.sag ~d:300. ~length:384.);
  let c = Rack_cable.make (0., 0.) (300., 0.) in
  Printf.printf "sag at rest: %.1f\n" (-.lowest c);
  Alcotest.(check (float 1.)) "hanging at 107" 107. (-.lowest c)

let test_ends () =
  let c = Rack_cable.step (Rack_cable.make (0., 0.) (300., 0.)) (10., 20.) (250., -30.) in
  match (Rack_cable.points c, List.rev (Rack_cable.points c)) with
  | a :: _, b :: _ ->
      Alcotest.(check (pair (float 0.) (float 0.))) "one end at its jack" (10., 20.) a;
      Alcotest.(check (pair (float 0.) (float 0.))) "the other at its" (250., -30.) b
  | _ -> Alcotest.fail "no points"

(* jerked: an end moved 100 pixels, then still: the swing dies away,
 * and the cable falls asleep *)
let test_swing () =
  let c = ref (Rack_cable.make (0., 0.) (300., 0.)) in
  c := Rack_cable.step !c (0., 0.) (200., 0.);
  let e0 = ref (Rack_cable.energy !c) and frames = ref 0 in
  for _ = 1 to 30 do
    c := Rack_cable.step !c (0., 0.) (200., 0.)
  done;
  Printf.printf "energy after the jerk %.3f, 30 frames later %.3f\n" !e0 (Rack_cable.energy !c);
  Alcotest.(check bool) "halved in 30 frames" true (Rack_cable.energy !c < !e0 /. 2.);
  e0 := Rack_cable.energy !c;
  while (not (Rack_cable.asleep !c)) && !frames < 1000 do
    c := Rack_cable.step !c (0., 0.) (200., 0.);
    incr frames
  done;
  Printf.printf "asleep after %d more frames\n" !frames;
  Alcotest.(check bool) "asleep within 1000 frames" true (Rack_cable.asleep !c)

let tests =
  Testo.categorize "Rack_cable" [ t "the sag at rest" test_sag; t "the ends at their jacks" test_ends; t "a swing dying away" test_swing ]
