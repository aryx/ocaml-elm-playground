(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A cube you can move with the arrow keys and mouse, either floating
 * freely or dropped to the floor, with its own spin you can freeze --
 * the interactive *idea* (not the code) is adapted from lucamug's
 * elm-playground-3d Example3Game.elm:
 * https://github.com/lucamug/elm-playground-3d/blob/master/examples/Example3Game.elm
 * (BSD-3-Clause license, (c) 2019-present Luca Mugnaini). This is a
 * fresh OCaml implementation of that interactive idea, not a
 * line-by-line translation. Notable adaptations:
 *  - a single-color cube (this library's cube has no per-face colors);
 *  - no on-screen instructions text -- game3d's view has no channel
 *    for a 2D HUD overlay on top of the 3D scene yet (see
 *    docs/claude_notes/playground3d_plan.md);
 *  - the whole scene (not just the cube) gets a mouse-driven turntable
 *    rotation around Y, this library's "up" axis (lucamug's world, and
 *    elm-playground-3d in general, is Z-up).
 *
 * Controls: arrow keys move the cube horizontally; mouse Y adjusts its
 * height; hold "d" to drop it to the floor instead; hold "s" to freeze
 * its spin; mouse X orbits the whole scene. *)
open Basics (* elm-core: float +, -, *, /, clamp *)
open Playground
open Playground3d

let view (computer : Playground.computer) ((ax, az) : number * number) : camera * shape3d list =
  let dropped = Set_.mem "d" computer.keyboard.keys in
  let frozen = Set_.mem "s" computer.keyboard.keys in
  let floor = plane gray 8. 8. |> move_y3d (-1.) in
  let back_wall =
    polygon3d darkGray
      [ (-4., -1., -4.); (4., -1., -4.); (4., 3., -4.); (-4., 3., -4.) ]
  in
  (* claude: real (thin) boxes, not flat polygon3d quads -- a flat
   * polygon only has a front face, so it vanishes (correctly
   * backface-culled) once the mouse-driven turntable rotation below
   * turns it edge-on or past that towards the camera; a box always has
   * some face towards the camera. See Playground3d.box's doc comment. *)
  let x_axis = box red 8. 0.1 0.1 |> move_y3d (-0.95) in
  let z_axis = box blue 0.1 0.1 8. |> move_y3d (-0.95) in
  let y_axis = box green 0.1 4. 0.1 |> move_y3d 1. in
  let (cx, cy, cz) =
    if dropped then (clamp (-3.) 3. ax, -0.5, clamp (-3.) 3. az)
    else (ax, 0.5 + (computer.mouse.my / 200.), az)
  in
  let cube_shape =
    cube purple 1.
    |> (if frozen then Fun.id
        else rotate3d (spin 6. computer.time) (spin 8. computer.time) (spin 10. computer.time))
    |> move3d cx cy cz
  in
  let scene =
    group3d [ floor; back_wall; x_axis; z_axis; y_axis; cube_shape ]
    |> rotate3d 0. (computer.mouse.mx / 3.) 0.
  in
  let cam = camera ~eye:(0., 4., 9.) ~target:(0., 0.5, 0.) () in
  (cam, [ scene ])

let update (computer : Playground.computer) ((ax, az) : number * number) : number * number =
  let (dx, dz) = to_xy computer.keyboard in
  (ax + dx, az + dz)

let app = game3d view update (0., 0.)
let main = Playground3d_platform.run_app3d app
