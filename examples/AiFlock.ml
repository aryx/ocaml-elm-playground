(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A school of fish from Craig Reynolds's three rules (1987), and no
 * leader: each fish looks only at the ones within a radius of it, and
 *
 *   separation   swims away from the ones too close
 *   alignment    goes the way they go
 *   cohesion     goes towards where they are
 *
 * Each rule has its weight on a slider (0 turns it off), and a key to
 * turn it off and on again: s, a and c. Turn them off one at a time and
 * see what each is for -- separation alone is a gas, the fish spreading
 * out and never agreeing on anything; cohesion alone a blob, clumping
 * and jostling; alignment alone a current, streams going the same way
 * without keeping together -- and all three are a school: it forms,
 * turns, splits round nothing and joins up again. Nothing in the program
 * knows what a school is.
 *
 * The fish in white is one like the others, with its radius drawn and
 * the fish it can see joined to it: its whole world.
 *
 * What it uses: playground/Ai (flocking, facing) over ai/Flock, Physics
 * for the bodies, playground/Gui for the sliders. *)
open Playground

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

type model = {
  fish : Physics.body list;
  separation : number;
  alignment : number;
  cohesion : number;
  radius : number;
  keys_before : string Set_.t; (* last frame's: a toggle is a key newly down *)
}

let speed = 150.
let count = 60

let fish_shape (color : color) = polygon color [ (10., 0.); (-6., 6.); (-3., 0.); (-6., -6.) ]

(* laid out on a spiral, the golden angle apart, and headed every way:
 * a crowd, not yet a school *)
let initial_model =
  { fish =
      List.init count (fun i ->
          let a = float_of_int i *. 137.508 *. Float.pi /. 180. and r = 30. *. Float.sqrt (float_of_int i) in
          Physics.body (fish_shape (if i = 0 then white else rgb 40 110 170))
          |> Physics.at (r *. cos a) (r *. sin a)
          |> Physics.moving (speed *. cos (a *. 3.)) (speed *. sin (a *. 3.))
          |> Ai.facing);
    separation = 1.5;
    alignment = 1.;
    cohesion = 1.;
    radius = 100.;
    keys_before = Set_.empty }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* a weight's key, pressed this frame: off, or back to its default *)
let toggle (computer : computer) (m : model) (key : string) (default : number) (w : number) : number =
  if Set_.mem key computer.keyboard.keys && not (Set_.mem key m.keys_before) then (if w = 0. then default else 0.) else w

let slider_x = -300.

let update (computer : computer) (m : model) : model =
  let bottom = computer.screen.bottom in
  let slider y v from to_ = Gui.slider computer ~at:(slider_x, bottom +. y) ~from ~to_ v in
  let m =
    { m with
      separation = slider 130. m.separation 0. 3.;
      alignment = slider 95. m.alignment 0. 3.;
      cohesion = slider 60. m.cohesion 0. 3.;
      radius = slider 25. m.radius 20. 200. }
  in
  let m =
    { m with
      separation = toggle computer m "s" 1.5 m.separation;
      alignment = toggle computer m "a" 1. m.alignment;
      cohesion = toggle computer m "c" 1. m.cohesion;
      keys_before = computer.keyboard.keys }
  in
  let fish =
    List.map
      (fun f ->
        f
        |> Ai.flocking ~speed ~force:200. ~radius:m.radius ~separation:m.separation ~alignment:m.alignment ~cohesion:m.cohesion m.fish
        |> Physics.step |> Ai.facing |> Physics.wrap computer.screen)
      m.fish
  in
  { m with fish }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  let first = List.hd m.fish in
  let near = List.filter (fun f -> f != first && Physics.distance f first < m.radius) m.fish in
  let line (a : Physics.body) (b : Physics.body) =
    let dx = b.x -. a.x and dy = b.y -. a.y in
    rectangle (rgb 250 250 250) (Float.hypot dx dy) 1.5
    |> rotate (Float.atan2 dy dx *. 180. /. Float.pi)
    |> move ((a.x +. b.x) /. 2.) ((a.y +. b.y) /. 2.)
    |> fade 0.6
  in
  (* centred, like every shape: clear of the slider's right end *)
  let label y s = text (rgb 230 240 250) 1.6 s |> move (slider_x +. 250.) (screen.bottom +. y) in
  let on w = if w = 0. then "off" else Printf.sprintf "%.2f" w in
  [ rectangle (rgb 20 60 100) screen.width screen.height; circle (rgb 40 90 140) m.radius |> fade 0.5 |> move first.x first.y ]
  @ List.map (line first) near
  @ List.map Physics.draw m.fish
  @ [ label 130. ("separation (s): " ^ on m.separation);
      label 95. ("alignment (a): " ^ on m.alignment);
      label 60. ("cohesion (c): " ^ on m.cohesion);
      label 25. (Printf.sprintf "radius: %.0f" m.radius);
      text (rgb 230 240 250) 2. "three rules, no leader: turn them off one at a time" |> move_y (screen.top -. 30.) ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
