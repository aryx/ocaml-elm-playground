(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Boxes tumbling, balls rolling: rotation (docs/claude_notes/
 * notes_2d_physics.md section 11, physics/2d/Resolve.mli).
 *
 * A collision pushes at the point where the bodies touch; off their
 * center, the push also spins them, the more the farther (the lever
 * arm), the less the harder they are to spin (their moment of inertia,
 * from their shape). So a box landing on a corner tips over and falls
 * flat, and a ball on a rough ramp rolls down instead of sliding: the
 * friction at its bottom point spins it.
 *
 * Boxes on a rough ramp are the scene of Box2D Lite's "Varying
 * Friction Coefficients" and Box2D's testbed "Friction" (boxes of
 * frictions 0.75 to 0 sliding down ramps; here, one friction, and
 * rotation on or off),
 *   https://github.com/erincatto/box2d-lite/blob/master/samples/main.cpp
 *   https://github.com/erincatto/box2d/blob/v2.4.1/testbed/tests/friction.cpp
 *
 * Space drops them again; u makes every body upright (collisions never
 * turn them), to see the difference: the boxes land on their corners
 * and stay balanced on them, the balls slide down the ramp.
 *
 * (A pile of boxes needs more: resolving the contacts one at a time,
 * each fix undoes a bit of another, and a pile jitters and sinks. The
 * plan's next phase, stacking.)
 *)
open Playground
open Basics (* float arithmetics *)

let wall (w : number) (h : number) (x : number) (y : number) (angle : number) : Physics.body =
  Physics.body (rectangle (rgb 100 100 110) w h)
  |> Physics.at x y |> Physics.pointing angle |> Physics.immovable |> Physics.rough 0.8

(* the floor's top at -400, the sides at -480 and 480, and a ramp going
 * down to the right, on the left *)
let walls = [ wall 1000. 100. 0. (-450.) 0.; wall 40. 1000. (-500.) 0. 0.; wall 40. 1000. 500. 0. 0.; wall 500. 20. (-230.) (-160.) (-25.) ]

(* boxes of mass their area / 2500 (a 50 x 50 box: 1), a bit rough and
 * bouncy *)
let box (w : number) (h : number) (color : color) (x : number) (y : number) (angle : number) : Physics.body =
  Physics.body (rectangle color w h)
  |> Physics.at x y |> Physics.pointing angle |> Physics.heavy (w * h / 2500.) |> Physics.rough 0.6 |> Physics.bouncy 0.2

(* a ball, with a spoke to see it turn *)
let ball (r : number) (color : color) (x : number) (y : number) : Physics.body =
  Physics.body (group [ circle color r; rectangle white (r * 1.6) 3. ])
  |> Physics.at x y |> Physics.heavy (r * r * 3.14 / 2500.) |> Physics.rough 0.6

let bodies : Physics.body list =
  [ (* on the ramp *)
    box 60. 60. red (-400.) 150. 10.;
    ball 25. blue (-300.) 200.;
    box 40. 90. orange (-160.) 120. 70.;
    (* on the floor, landing on a corner, or an edge *)
    box 80. 40. green 80. 100. 30.;
    box 50. 50. purple 250. 200. 45.;
    box 120. 30. brown 380. 300. (-20.);
    ball 20. (rgb 30 60 140) 150. 300.;
  ]

type world = { bodies : Physics.body list; upright : bool }

(* Scene2d for its [pressed] keys *)
type model = world Scene2d.t

let start (upright : bool) : world = { bodies = (if upright then List.map Physics.upright bodies else bodies); upright }
let initial_model : model = Scene2d.start (start false)

let fall_and_bounce (b : Physics.body) : Physics.body =
  List.fold_left (fun b w -> Physics.bounce_off w b) (b |> Physics.fall 800. |> Physics.step) walls

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let w = scenes.scene in
  if Scene2d.pressed (fun k -> k.kspace) scenes then Scene2d.go (start w.upright) scenes
  else if Scene2d.pressed (fun k -> Set_.mem "u" k.keys) scenes then Scene2d.go (start (not w.upright)) scenes
  else { scenes with scene = { w with bodies = List.map fall_and_bounce w.bodies |> Physics.bounce_all } }

let text (s : string) : shape = words black s |> scale 2.

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  (rectangle (rgb 235 235 225) screen.width screen.height :: List.map Physics.draw walls)
  @ List.map Physics.draw model.scene.bodies
  @ [ text (if model.scene.upright then "rotation off: every body upright" else "rotation on") |> move_y 450.;
      text "(space: again, u: rotation on/off)" |> move_y 415. ]

let app = game view update initial_model
let main = Playground_platform.run_app app
