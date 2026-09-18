(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Playground3d.cached3d in action: a big static scene (a grid of 1600
 * cubes, ~19000 triangles) built once, orbited by the camera, around a
 * small cube spinning in the middle.
 *
 *  - [grid] never changes, so it's built once, as a cached3d, outside
 *    view: the GPU backends (OpenGL, WebGL) upload it to the GPU on the
 *    first frame, and on the next ones only ask the GPU to draw it
 *    again. The moving camera doesn't change that: the camera is one
 *    matrix, the grid's vertices stay the same (see
 *    docs/claude_notes/notes_opengl.md, section 6).
 *  - [spinner] changes every frame (rotate3d computes new points), so
 *    it's an ordinary shape, rebuilt by view every frame, next to the
 *    cached grid.
 *
 * Run examples3d/opengl/CachedGrid3d.exe with -debug to see the
 * "vertices uploaded" per frame, and with -debug-keys press "o" to turn
 * the cache off: the grid is then rebuilt and uploaded every frame
 * too, like a group3d (watch the fps). The software and web (SVG)
 * backends draw the same picture, just without caching anything. *)
open Playground
open Playground3d

let grid_size = 40

let colors = [| red; orange; yellow; green; blue; purple |]

let grid : shape3d =
  let cubes = ref [] in
  for ix = 0 to grid_size - 1 do
    for iz = 0 to grid_size - 1 do
      let color = colors.((ix + iz) mod Array.length colors) in
      let x = float_of_int (ix - (grid_size / 2)) *. 1.5 in
      let z = float_of_int (iz - (grid_size / 2)) *. 1.5 in
      (* a gentle wave, so the grid isn't a flat checkerboard *)
      let y = sin (float_of_int ix *. 0.4) +. cos (float_of_int iz *. 0.3) in
      cubes := (cube color 1. |> move3d x y z) :: !cubes
    done
  done;
  cached3d !cubes

let spinner (computer : Playground.computer) : shape3d =
  cube white 3. |> rotate3d 0. (spin 3. computer.time) 20. |> move3d 0. 6. 0.

let view (computer : Playground.computer) () =
  let radians = spin 30. computer.time *. Float.pi /. 180. in
  let eye = (40. *. cos radians, 25., 40. *. sin radians) in
  (camera ~eye ~target:(0., 0., 0.) (), [ grid; spinner computer ])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
