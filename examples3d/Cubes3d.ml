(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A small grid of differently-colored, overlapping-from-this-angle
 * cubes, orbited by the camera. Meant to validate (visually) that the
 * native rasterizer's z-buffer depth test and backface culling are
 * actually correct (not just "looks like a cube in silhouette", as
 * Cube3d.ml alone can't tell you, since it is a single flat color), and
 * to get a real FPS reading with more than a handful of triangles on
 * screen -- see docs/claude_notes/plan_playground3d.md's Phase 4. *)
open Playground
open Playground3d

let grid_size = 5

let colors = [| red; orange; yellow; green; blue; purple |]

let scene =
  let cubes = ref [] in
  for ix = 0 to grid_size - 1 do
    for iz = 0 to grid_size - 1 do
      let color = colors.((ix + iz) mod Array.length colors) in
      let x = float_of_int (ix - (grid_size / 2)) *. 1.3 in
      let z = float_of_int (iz - (grid_size / 2)) *. 1.3 in
      cubes := (cube color 1. |> move3d x 0. z) :: !cubes
    done
  done;
  group3d !cubes

let view (computer : Playground.computer) () =
  let angle = spin 20. computer.time in
  let radius = 8. in
  let radians = angle *. Float.pi /. 180. in
  let eye = (radius *. cos radians, 4., radius *. sin radians) in
  let cam = camera ~eye ~target:(0., 0., 0.) () in
  (cam, [ scene ])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
