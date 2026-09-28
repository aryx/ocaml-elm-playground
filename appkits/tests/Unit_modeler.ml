(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/modeler: a modeller's scene -- Modeler.mli's worked example
 * (scale, then turn, then move), Blender's z up turned into the ray
 * tracer's y up, Blender's names, a cutter forgotten with its object,
 * the views' projections and Modeler_view.mli's worked example, the
 * camera's projection agreeing with the ray tracer's rays, and a click
 * finding the object whose wire it is on. *)

module M = Modeler
module V = Modeler_view

let t = Testo.create
let v3 = Alcotest.(triple (float 1e-9) (float 1e-9) (float 1e-9))

let test_transform () =
  let o = { (List.hd M.default) with scale = (2., 1., 1.); rotation = (0., 0., 90.); location = (0., 0., 3.) } in
  Alcotest.check v3 "the corner (1, 1, 1)" (-1., 2., 4.) (M.transform o (1., 1., 1.));
  Alcotest.check v3 "z up becomes y up" (1., 3., -2.) (M.to_world (1., 2., 3.));
  let o = M.turn 2 90. (M.translate (1., 0., 0.) (List.hd M.default)) in
  Alcotest.check v3 "G then R: moved, then turned in place" (1., 1., 0.) (M.transform o (1., 0., 0.))

let test_names () =
  let t, a = M.add (M.Mesh (M.Cube, M.grey)) "Cube" M.default in
  let t, b = M.add (M.Mesh (M.Cube, M.grey)) "Cube" t in
  Alcotest.(check (list string)) "Blender's names" [ "Cube.001"; "Cube.002" ] [ a; b ];
  let t, copies = M.duplicate [ "Cube.002" ] t in
  Alcotest.(check (list string)) "a copy of Cube.002 is Cube.003" [ "Cube.003" ] copies;
  let t = M.update "Cube" (fun o -> { o with modifier = Some (M.Difference, "Cube.001") }) t in
  Alcotest.(check bool) "a cutter" true (M.is_cutter t "Cube.001");
  let t = M.remove [ "Cube.001" ] t in
  Alcotest.(check bool) "the cutter gone, the modifier with it" true ((Option.get (M.find t "Cube")).modifier = None)

let test_views () =
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "from the right" (2., 3.) (V.project V.Right (5., 2., 3.));
  Alcotest.check v3 "dragged right: along y" (0., 0.5, 0.) (V.unproject V.Right (10. /. 20., 0.));
  Alcotest.check v3 "from the top, up the screen: along y" (0., 1., 0.) (V.unproject V.Top (0., 1.));
  Alcotest.check v3 "from the front, up the screen: along z" (0., 0., 1.) (V.unproject V.Front (0., 1.))

let test_camera () =
  let cam = V.camera M.default ~target:(0., 0., 0.) in
  (match V.perspective cam ~aspect:1. (0., 0., 0.) with
  | Some (x, y) -> Alcotest.(check (pair (float 1e-9) (float 1e-9))) "the target at the picture's middle" (0., 0.) (x, y)
  | None -> Alcotest.fail "in front");
  (* the ray through the pixel where a point is drawn goes through it *)
  let p = (1., 1., 1.) in
  match V.perspective cam ~aspect:1. p with
  | None -> Alcotest.fail "in front"
  | Some (nx, ny) ->
      let px = 50. +. (nx *. 50.) and py = 50. -. (ny *. 50.) in
      let ray, _, _ = Raytrace.camera_ray_through cam ~width:100 ~height:100 px py in
      let w = M.to_world p in
      let to_p = Vec3.normalize (Vec3.sub w ray.origin) in
      Alcotest.(check (float 1e-9)) "the ray tracer's ray goes through it" 1. (Vec3.dot to_p (Vec3.normalize ray.direction))

let test_pick () =
  let to_screen p = Some (V.project V.Top p) in
  Alcotest.(check (option string)) "on the cube's edge" (Some "Cube") (V.pick ~to_screen ~tolerance:0.1 M.default (1.02, 0.3));
  Alcotest.(check (option string)) "inside a wireframe: nothing" None (V.pick ~to_screen ~tolerance:0.1 M.default (0.5, 0.3))

let tests =
  [
    t "modeler: scale, turn, move (Modeler.mli's worked example)" test_transform;
    t "modeler: names, copies, cutters" test_names;
    t "modeler: the orthographic views" test_views;
    t "modeler: the camera's projection is the ray tracer's" test_camera;
    t "modeler: a click on a wire" test_pick;
  ]
