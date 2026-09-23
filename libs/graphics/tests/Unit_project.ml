(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Project *)

let t = Testo.create
let camera : Camera.t = { eye = (0., 0., 10.); target = (0., 0., 0.); up = (0., 1., 0.); fov = 90.; ortho = 0.; near = 1.; far = 100. }
let project p = Project.vertex camera ~width:400 ~height:400 (p, (0.25, 0.5), (0., 0., 1.))

(* Project.mli's example: (0, 5, 0) half way up from the center *)
let test_vertex () =
  match project (0., 5., 0.) with
  | None -> Alcotest.fail "in view, not None"
  | Some v ->
      Alcotest.(check (float 1e-9)) "x: the center" 200. v.vx;
      Alcotest.(check (float 1e-9)) "y: half way up" 100. v.vy;
      Alcotest.(check (float 1e-9)) "z: 10 in front" 10. v.z;
      Alcotest.(check (float 1e-9)) "1/z" 0.1 v.inv_z;
      Alcotest.(check (float 1e-9)) "u/z" 0.025 v.u_over_z;
      Alcotest.(check (float 1e-9)) "v/z" 0.05 v.v_over_z

let test_behind () = Alcotest.(check bool) "behind the camera" true (project (0., 0., 20.) = None)

let tests = Testo.categorize "Project" [ t "the worked example" test_vertex; t "behind the camera" test_behind ]
