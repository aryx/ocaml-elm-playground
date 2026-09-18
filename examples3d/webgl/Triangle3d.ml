(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The "hello triangle" of the WebGL backend: a single triangle, facing
 * the camera, the smallest scene that goes through the whole pipeline
 * (camera matrices, one draw call, lighting).
 *
 * Until docs/claude_notes/plan_webgl.md's Phase 3, the backend ignores
 * the scene and draws its own hardcoded red/green/blue triangle
 * instead: this page is then just a check that the canvas, the WebGL
 * context, the shaders and the vertex buffer work. *)
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
