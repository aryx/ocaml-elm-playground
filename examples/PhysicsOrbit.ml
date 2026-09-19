(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A planet around a star, and computational physics' first lesson: how
 * you step through time matters as much as the laws you step. Space
 * switches the integrator and restarts the orbit:
 *
 *  - explicit Euler (the obvious method): the planet gains energy at
 *    every step, spirals out, and leaves in about ten seconds;
 *  - semi-implicit Euler (what game engines use), Verlet, RK4: the
 *    planet stays on its ellipse, orbit after orbit.
 *
 * The line at the top shows the planet's total energy compared to its
 * start, E/E0, which physics says is 1 forever: watch it drift with
 * explicit Euler, and stay put with the others (see
 * docs/claude_notes/notes_2d_physics.md section 5).
 *
 * Unlike the games, this example reaches under the Physics layer (which
 * always uses semi-implicit Euler) into the engine itself, physics/2d/:
 * comparing its four integrators (Integrate.mli) is the point.
 *
 * The numbers: G M = 10,000,000 (pixels^3 / s^2), the planet starting
 * 250 pixels from the star at 160 pixels per second, 80% of the circular
 * orbit's speed: an ellipse coming down to 118 pixels, one orbit every
 * 4.95 seconds. Two steps of 1/60 s per frame, so time runs twice as
 * fast as real time.
 *)
open Playground
open Basics (* float arithmetics *)

let gm = 1e7
let star = (0., 0.)
let gravity = Force.gravitation ~gm ~center:star
let start = Body.make ~vel:(0., 160.) (250., 0.)
let dt = 1. / 60.
let steps_per_frame = 2

type model = {
  planet : Body.t;
  integrator : Integrate.method_;
  trail : (number * number) list; (* the last positions, newest first *)
  space_was_down : bool;
}

let initial_model = { planet = start; integrator = Integrate.Explicit_euler; trail = []; space_was_down = false }
let energy (b : Body.t) = Energy.kinetic b + Energy.gravitation ~gm ~center:star b

(* the method after [m], in Integrate.methods' order, around *)
let next (m : Integrate.method_) : Integrate.method_ =
  let rec after = function x :: (y :: _ as rest) -> if x = m then y else after rest | _ -> List.hd Integrate.methods in
  after Integrate.methods

let update (computer : computer) (model : model) : model =
  let space = computer.keyboard.kspace in
  if space && not model.space_was_down then
    { initial_model with integrator = next model.integrator; space_was_down = true }
  else
    let rec go n b = if n = 0 then b else go (n -.. 1) (Integrate.step model.integrator ~force:gravity ~dt b) in
    let planet = go steps_per_frame model.planet in
    let trail = List.filteri (fun i _ -> i < 300) (planet.pos :: model.trail) in
    { model with planet; trail; space_was_down = space }

let text (color : color) (s : string) : shape = words color s |> scale 2.

let view (computer : computer) (model : model) : shape list =
  let (x, y) = model.planet.pos in
  let screen = computer.screen in
  [ rectangle black screen.width screen.height ]
  @ List.map (fun (x, y) -> circle (rgb 90 90 90) 1.5 |> move x y) model.trail
  @ [ circle yellow 20.;
      circle (rgb 120 180 255) 8. |> move x y;
      text white (Integrate.name model.integrator ^ "   (space: the next method)") |> move_y 450.;
      text white
        (Printf.sprintf "E/E0 = %.3f   distance %.0f" (energy model.planet / energy start) (Vec2.length model.planet.pos))
      |> move_y 410. ]

let app = game view update initial_model
let main = Playground_platform.run_app app
