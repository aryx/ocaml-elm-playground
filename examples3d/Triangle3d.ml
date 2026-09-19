(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The "hello triangle" of 3D: a single triangle, facing the camera,
 * the smallest scene that goes through the whole pipeline (camera
 * matrices, one draw call or one rasterized triangle, lighting); the
 * first test of each new backend. Its orange is darker
 * than orange: facing +z, it gets only part of the "sun" (see
 * Lighting), 0.25 + 0.75 * 0.34, about half its color. *)
open Playground
open Playground3d

(* in the z = 0 plane, counter-clockwise seen from the camera (on +z),
 * so its normal points to the camera and backface culling keeps it *)
let triangle = polygon3d orange [ (-1., -1., 0.); (1., -1., 0.); (0., 1., 0.) ]

let view (_computer : Playground.computer) () =
  let cam = camera ~eye:(0., 0., 5.) ~target:(0., 0., 0.) () in
  (cam, [ triangle ])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
