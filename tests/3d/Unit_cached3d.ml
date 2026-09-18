(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Playground3d.cached3d's promise: it draws exactly like group3d, on
 * every backend. Checked here on what the software rasterizer gets
 * (Shape3d_render_software.faces); the GPU backends' side, a cached
 * frame byte-identical to an uncached one, is checked by hand with
 * -dump-frame (see docs/claude_notes/plan_opengl_perf.md), since GPU
 * pixels depend on the machine. *)

open Playground3d

let t = Testo.create

let shapes : shape3d list =
  [ cube Playground.red 1.; cube Playground.blue 2. |> move3d 3. 0. 0.; sphere Playground.green 1. ]

let same_faces msg (a : shape3d) (b : shape3d) =
  Alcotest.(check bool) msg true (Shape3d_render_software.faces a = Shape3d_render_software.faces b)

let test_same_faces () = same_faces "cached3d = group3d" (cached3d shapes) (group3d shapes)

(* the transformed result is a plain group, but the same geometry *)
let test_transformed () =
  same_faces "move3d" (cached3d shapes |> move3d 1. 2. 3.) (group3d shapes |> move3d 1. 2. 3.);
  same_faces "rotate3d" (cached3d shapes |> rotate3d 10. 20. 30.) (group3d shapes |> rotate3d 10. 20. 30.)

(* found once, when cached3d is called, see Playground3d.collect_hud_shapes *)
let test_huds () =
  let hud_shape = hud (Playground.words Playground.black "Score: 3") in
  let with_hud = hud_shape :: shapes in
  Alcotest.(check int)
    "the HUD shape inside is found" 1
    (List.length (collect_hud_shapes (cached3d with_hud)));
  Alcotest.(check bool)
    "the same as group3d's" true
    (collect_hud_shapes (cached3d with_hud) = collect_hud_shapes (group3d with_hud))

(* the id is the identity the GPU backends cache by: a new one per call,
 * even for the same shapes *)
let test_fresh_ids () =
  match ((cached3d shapes).form, (cached3d shapes).form) with
  | Cached3d a, Cached3d b -> Alcotest.(check bool) "different ids" true (a.id <> b.id)
  | _ -> Alcotest.fail "cached3d should build a Cached3d"

let tests =
  Testo.categorize "cached3d"
    [
      t "draws like group3d" test_same_faces;
      t "move3d/rotate3d on it" test_transformed;
      t "its HUD shapes" test_huds;
      t "a fresh id per call" test_fresh_ids;
    ]
