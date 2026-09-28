(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A ragdoll down a staircase: Ragdoll3d, ten boxes and
 * nine joints (physics/3d/Joint3d.mli), shoved off the top landing.
 *
 *   space   again
 *   l       the joints' limits, on and off
 *   j       the joints' anchors (two small cubes each, together when
 *           the joint holds)
 *
 * Nothing in this file says how a body falls down stairs. It tumbles,
 * catches a heel on a step, folds at the hips and slides the rest of
 * the way because it is built like a body -- which is the whole trick of
 * the ragdoll, and why every game after Half-Life 2 has them. Press "l"
 * to see what the limits are for: the same fall, and the knees bend
 * backwards, the head goes round, the arms wind about the shoulders.
 *)
open Playground
open Playground3d

(* six steps of 20 cm, 35 cm deep, going down along +x from a landing
 * 1.2 m up *)
let stairs : Physics3d.body list =
  let step i =
    let top = 1.2 -. (0.2 *. float_of_int i) in
    Physics3d.body (box (rgb 150 140 130) 0.35 top 2.)
    |> Physics3d.at ((0.35 *. float_of_int i) +. 0.175) (top /. 2.) 0.
    |> Physics3d.immovable |> Physics3d.rough 0.7
  in
  (Physics3d.body (box (rgb 120 150 110) 12. 0.2 6.) |> Physics3d.at 2. (-0.1) 0. |> Physics3d.immovable |> Physics3d.rough 0.7)
  :: (Physics3d.body (box (rgb 150 140 130) 1.2 1.2 2.) |> Physics3d.at (-0.6) 0.6 0. |> Physics3d.immovable |> Physics3d.rough 0.7)
  :: List.init 6 step

let first = List.length stairs

type model = { world : Physics3d.world; limits : bool; keys_down : string list }

(* standing on the landing, a step from its edge, and shoved *)
let fresh (limits : bool) : model =
  let doll = Ragdoll3d.bodies (-0.3, 1.2, 0.) in
  let world = Physics3d.world (stairs @ doll) |> Ragdoll3d.join ~limits first in
  let bodies =
    List.mapi (fun i (b : Physics3d.body) -> if i = first then Physics3d.moving 2. 0. 0.3 b else b) world.bodies
  in
  { world = { world with bodies }; limits; keys_down = [] }

let initial_model = fresh true

let update (computer : computer) (m : model) : model =
  let down key = Set_.mem key computer.keyboard.keys in
  let pressed key = down key && not (List.mem key m.keys_down) in
  let keys_down = List.filter down [ "l" ] @ if computer.keyboard.kspace then [ "space" ] else [] in
  let space = computer.keyboard.kspace && not (List.mem "space" m.keys_down) in
  if pressed "l" then { (fresh (not m.limits)) with keys_down }
  else if space then { (fresh m.limits) with keys_down }
  else { m with world = Physics3d.simulate ~gravity:9.8 m.world; keys_down }

let text color size str = words color str |> scale size
let cam = Camera3d.from_far ~fov:40. ~offset:(1.5, 3., 8.) (1.4, 0.6, 0.)

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let joints = if Set_.mem "j" computer.keyboard.keys then Physics3d.debug_joints m.world else [] in
  ( cam,
    List.map Physics3d.draw m.world.bodies @ joints
    @ List.map hud
        [ text black 2.2 (if m.limits then "the joints' limits: on" else "the joints' limits: OFF -- knees bend backwards")
          |> move_y (screen.top -. 45.);
          text darkGray 2. "space: again    l: the limits    j: the joints" |> move_y (screen.bottom +. 25.) ] )

let app = game3d view update initial_model

let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
