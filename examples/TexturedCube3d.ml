(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A spinning cube textured with checker.png (a 4-quadrant test image:
 * red top-left, green top-right, blue bottom-left, yellow bottom-right)
 * -- validates textured_cube's UV mapping end to end on the software,
 * OpenGL and WebGL backends (see Playground3d.textured_quad's doc
 * comment for why the SVG backend only shows a flat placeholder color
 * for now). *)
open Playground
open Playground3d

let view (computer : Playground.computer) () =
  let angle = spin 8. computer.time in
  let scene = textured_cube "examples/checker.png" 1.5 |> rotate3d 0. angle 0. in
  let cam = camera ~eye:(3., 2., 5.) ~target:(0., 0., 0.) () in
  (cam, [ scene ])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
