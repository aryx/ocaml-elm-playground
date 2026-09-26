(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/sketch: Sketchpad's drawing -- Relax.mli's worked example,
 * a rough hexagon made regular by two kinds of constraint, a linkage
 * keeping its lengths while a point is dragged, what cannot hold
 * staying an error, points shared and merged, the pen's aim, and a
 * drawing refused inside itself. *)

let t = Testo.create
let near = Alcotest.float 1e-3
let pos = Alcotest.(pair (float 1e-3) (float 1e-3))

let points s ps = List.fold_left (fun (s, ids) p -> let s, id = Sketch.add_point p s in (s, ids @ [ id ])) (s, []) ps
let line a b s = Sketch.add_item (Sketch.Line (a, b)) s
let length s l = match Sketch.ends s l with Some (a, b) -> let (ax, ay), (bx, by) = (Sketch.pos s a, Sketch.pos s b) in Float.hypot (bx -. ax) (by -. ay) | None -> 0.

let test_worked_example () =
  let s, ids = points Sketch.empty [ (0., 0.); (100., 10.) ] in
  let a = List.nth ids 0 and b = List.nth ids 1 in
  let s, l = line a b s in
  let s = Sketch.constrain (Sketch.Horizontal l) (Sketch.toggle_fixed a s) in
  Alcotest.check near "the error: the far end 10 above" 10. (Relax.error_of s (Sketch.Horizontal l));
  let s = Relax.sweep s in
  Alcotest.check pos "one sweep: the free end straight down" (100., 0.) (Sketch.pos s b);
  Alcotest.check pos "the fixed one where it was" (0., 0.) (Sketch.pos s a)

(* six points roughly round a circle, joined; the circle's center and
   rim point fixed *)
let rough_hexagon () =
  let s, c = Sketch.add_point (0., 0.) Sketch.empty in
  let s, r = Sketch.add_point (100., 0.) s in
  let s = Sketch.toggle_fixed r (Sketch.toggle_fixed c s) in
  let s, k = Sketch.add_item (Sketch.Circle (c, r)) s in
  let s, ps = points s [ (70., 60.); (-30., 110.); (-120., 10.); (-40., -70.); (60., -100.) ] in
  let corners = r :: ps in
  let s, sides =
    List.fold_left
      (fun (s, ls) i -> let s, l = line (List.nth corners i) (List.nth corners ((i + 1) mod 6)) s in (s, ls @ [ l ]))
      (s, []) [ 0; 1; 2; 3; 4; 5 ]
  in
  (s, k, corners, sides)

let test_hexagon () =
  let s, k, corners, sides = rough_hexagon () in
  let s = List.fold_left (fun s p -> Sketch.constrain (Sketch.On_circle (p, k)) s) s corners in
  let s = List.fold_left (fun s l -> Sketch.constrain (Sketch.Equal (List.hd sides, l)) s) s (List.tl sides) in
  Alcotest.(check bool) "rough: an error" true (Relax.error s > 10.);
  let s = Relax.solve ~sweeps:500 s in
  Alcotest.(check bool) "relaxed: none" true (Relax.error s < 1e-2);
  List.iter (fun l -> Alcotest.check (Alcotest.float 0.05) "a regular hexagon's side is its radius" 100. (length s l)) sides;
  Alcotest.check (Alcotest.pair (Alcotest.float 0.05) (Alcotest.float 0.05)) "its second corner at 60 degrees"
    (50., 50. *. Float.sqrt 3.) (Sketch.pos s (List.nth corners 1))

(* a four-bar linkage with no numbers: two cranks' ends on circles
   round fixed pivots, the rod between them as long as a fixed reference
   line; drag one end round its circle, the other follows on its own *)
let test_linkage () =
  let s, ids = points Sketch.empty [ (0., 0.); (60., 0.); (200., 0.); (200., 100.); (0., 200.); (200., 200.); (60., 0.); (200., 100.) ] in
  let id i = List.nth ids i in
  let s = List.fold_left (fun s i -> Sketch.toggle_fixed (id i) s) s [ 0; 1; 2; 3; 4; 5 ] in
  let s, c1 = Sketch.add_item (Sketch.Circle (id 0, id 1)) s in
  let s, c2 = Sketch.add_item (Sketch.Circle (id 2, id 3)) s in
  let s, reference = line (id 4) (id 5) s in
  let a = id 6 and b = id 7 in
  let s, rod = line a b s in
  let s = List.fold_left (fun s c -> Sketch.constrain c s) s [ Sketch.On_circle (a, c1); Sketch.On_circle (b, c2); Sketch.Equal (rod, reference) ] in
  let s = Relax.solve ~sweeps:200 s in
  Alcotest.check (Alcotest.float 0.01) "the rod: the reference's length" 200. (length s rod);
  let s = Sketch.set_pos a (0., 60.) s in
  let s = Relax.solve ~held:[ a ] ~sweeps:200 s in
  Alcotest.check pos "the dragged end stays under the pen" (0., 60.) (Sketch.pos s a);
  Alcotest.check (Alcotest.float 0.01) "the rod keeps its length" 200. (length s rod);
  Alcotest.check (Alcotest.float 0.01) "its other end on its circle" 0. (Relax.error_of s (Sketch.On_circle (b, c2)))

let test_what_cannot_hold () =
  let s, ids = points Sketch.empty [ (0., 0.); (100., 10.) ] in
  let s = List.fold_left (fun s i -> Sketch.toggle_fixed i s) s ids in
  let s, l = line (List.nth ids 0) (List.nth ids 1) s in
  let s = Relax.solve ~sweeps:10 (Sketch.constrain (Sketch.Horizontal l) s) in
  Alcotest.check near "both ends fixed: the error stays" 10. (Relax.error s)

let test_shared_points () =
  let s, ids = points Sketch.empty [ (0., 0.); (100., 0.); (0., 100.); (0., 102.) ] in
  let id i = List.nth ids i in
  let s, top = line (id 0) (id 1) s in
  let s, side = line (id 2) (id 3) s in
  let s = Sketch.set_pos (id 0) (5., 5.) s in
  Alcotest.check pos "moving a point moves the line on it" (5., 5.) (Sketch.pos s (fst (Option.get (Sketch.ends s top))));
  let s = Sketch.merge ~drop:(id 2) ~onto:(id 0) s in
  Alcotest.(check (option (pair int int))) "dropped on another, a point becomes it" (Some (id 0, id 3)) (Sketch.ends s side);
  let s = Sketch.merge ~drop:(id 3) ~onto:(id 0) s in
  Alcotest.(check (option (pair int int))) "a line down to one point is gone" None (Sketch.ends s side);
  let s = Sketch.delete (id 0) s in
  Alcotest.(check (option (pair int int))) "a point deleted takes its lines" None (Sketch.ends s top)

let test_aim () =
  let s, ids = points Sketch.empty [ (0., 0.); (100., 0.) ] in
  let s, l = line (List.nth ids 0) (List.nth ids 1) s in
  let doc = [ s ] in
  let aim p = Sketch.aim doc 0 ~tolerance:5. p in
  Alcotest.(check bool) "near an end: the point, not the line" true (aim (98., 3.) = Sketch.At_point (List.nth ids 1));
  Alcotest.(check bool) "near the middle: the nearest place on the line" true (aim (50., 3.) = Sketch.On_item (l, (50., 0.)));
  Alcotest.(check bool) "far: nothing" true (aim (50., 30.) = Sketch.Nothing)

let test_instances () =
  let s, ids = points Sketch.empty [ (0., 0.); (10., 0.) ] in
  let b, _ = line (List.nth ids 0) (List.nth ids 1) s in
  let doc = [ Sketch.empty; b ] in
  let inst = { Sketch.master = 1; at = (100., 100.); size = 2.; angle = 90. } in
  let doc = Option.get (Sketch.place 0 inst doc) in
  (match Sketch.strokes doc 0 with
  | [ Sketch.Seg (a, z) ] ->
      Alcotest.check pos "the master's origin at the instance's place" (100., 100.) a;
      Alcotest.check pos "its line twice as long, turned up" (100., 120.) z
  | _ -> Alcotest.fail "one stroke");
  Alcotest.(check bool) "B inside A, so A inside B: refused" true (Sketch.place 1 { inst with master = 0 } doc = None);
  Alcotest.(check bool) "a sheet inside itself: refused" true (Sketch.place 1 inst doc = None)

let tests =
  [
    t "sketch: Relax.mli's worked example" test_worked_example;
    t "sketch: a rough hexagon made regular" test_hexagon;
    t "sketch: a linkage keeps its lengths" test_linkage;
    t "sketch: what cannot hold stays an error" test_what_cannot_hold;
    t "sketch: points shared, merged, deleted" test_shared_points;
    t "sketch: the pen's aim" test_aim;
    t "sketch: instances, and no drawing inside itself" test_instances;
  ]
