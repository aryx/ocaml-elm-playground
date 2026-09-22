(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A spinning cube: the "hello world" of the 3D playground, meant to
 * validate the render pipeline (projection, backface culling, and --
 * once there is more than one face color visible at once -- the
 * painter's algorithm / z-buffer depth ordering) end to end. *)
open Playground
open Playground3d

let view (computer : Playground.computer) () =
  let angle = spin 6. computer.time in
  let scene = cube purple 1.5 |> rotate3d 0. angle 0. in
  let cam = camera ~eye:(3., 2., 5.) ~target:(0., 0., 0.) () in
  (cam, [ scene ])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
