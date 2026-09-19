(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/geometry/Camera *)

let t = Testo.create

let vec =
  Alcotest.testable
    (fun ppf (x, y, z) -> Format.fprintf ppf "(%g, %g, %g)" x y z)
    (fun (a, b, c) (x, y, z) ->
      let eq u v = Float.abs (u -. v) < 1e-9 in
      eq a x && eq b y && eq c z)

(* looking down -z from (0, 0, 10), like an eye in front of the screen *)
let camera : Camera.t = { eye = (0., 0., 10.); target = (0., 0., 0.); up = (0., 1., 0.); fov = 90.; near = 1.; far = 100. }

let test_basis () =
  let right, up, forward = Camera.basis ~eye:camera.eye ~target:camera.target () in
  Alcotest.check vec "forward, towards the target" (0., 0., -1.) forward;
  Alcotest.check vec "right" (1., 0., 0.) right;
  Alcotest.check vec "up" (0., 1., 0.) up

let test_view () =
  Alcotest.check vec "the eye is the origin" (0., 0., 0.) (Camera.view camera camera.eye);
  Alcotest.check vec "the target, straight ahead" (0., 0., 10.) (Camera.view camera camera.target);
  Alcotest.check vec "5 up, 10 ahead" (0., 5., 10.) (Camera.view camera (0., 5., 0.))

(* Camera.mli's worked example: fov = 90 degrees, so f = 1, and a
 * point 10 in front of the camera and 5 up is half way to the top *)
let test_ndc () =
  Alcotest.(check (float 1e-9)) "f = 1 / tan 45" 1. (Camera.focal camera);
  let ndc p = Camera.ndc camera ~aspect:2. p in
  Alcotest.(check (option (pair (float 1e-9) (float 1e-9)))) "half way to the top" (Some (0., 0.5)) (ndc (0., 5., 10.));
  Alcotest.(check (option (pair (float 1e-9) (float 1e-9))))
    "x squeezed by the aspect ratio" (Some (0.25, 0.)) (ndc (5., 0., 10.));
  Alcotest.(check bool) "behind the near plane" true (ndc (0., 0., 0.5) = None);
  Alcotest.(check bool) "beyond the far plane" true (ndc (0., 0., 100.) = None)

let tests =
  Testo.categorize "Camera"
    [ t "basis" test_basis; t "view" test_view; t "ndc, the worked example" test_ndc ]
