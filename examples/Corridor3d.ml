(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Walking down a corridor, first-person: the scene that shows why a
 * renderer needs near-plane clipping. The floor stripes and the wall
 * segments around the camera reach *behind* it; a renderer that drops
 * every triangle with a vertex behind the camera (instead of clipping
 * it to the part in front) leaves holes in the floor and walls right in
 * front of you. Toggle "c" in the software backend to compare (see
 * graphics/3d/Clip.mli).
 *
 * Controls: up/down arrows walk forward/backward. *)
open Basics (* elm-core: float +, -, *, /, clamp *)
open Playground
open Playground3d

let length = 80.
let width = 6.
let height = 3.

(* stripes and segments along z, every [step] units, alternating colors *)
let step = 4.

let along (f : int -> number -> shape3d) : shape3d list =
  List.init (truncate (length / step)) (fun i -> f i ((-.length / 2.) + (step / 2.) + (float i * step)))

let floor =
  along (fun i z -> plane (if i mod 2 = 0 then rgb 40 120 40 else green) width step |> move_z3d z)

(* thin boxes, their inner faces facing the corridor *)
let walls =
  along (fun i z ->
      let color = if i mod 2 = 0 then rgb 200 200 200 else gray in
      group3d
        [
          box color 0.2 height step |> move3d (-.width / 2.) (height / 2.) z;
          box color 0.2 height step |> move3d (width / 2.) (height / 2.) z;
        ])

(* something to walk towards *)
let pillars =
  [ cube red 1. |> move3d (-1.5) 0.5 (-10.); cube blue 1. |> move3d 1.5 0.5 (-20.); cube orange 1. |> move3d 0. 0.5 (-30.) ]

(* the model: how far down the corridor we are, z of the eye *)
let view (_computer : Playground.computer) (z : number) =
  let cam = camera ~eye:(0., 1.5, z) ~target:(0., 1.5, z - 10.) () in
  (cam, floor @ walls @ pillars)

let update (computer : Playground.computer) (z : number) =
  let dz = (if computer.keyboard.kdown then 0.2 else 0.) - if computer.keyboard.kup then 0.2 else 0. in
  clamp (-.length / 2.) (length / 2.) (z + dz)

let app = game3d view update 20.
let main = Playground3d_platform.run_app3d app
