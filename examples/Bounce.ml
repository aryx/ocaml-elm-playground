(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Balls falling and bouncing: collision response, with the Physics
 * layer's bounce (docs/claude_notes/notes_2d_physics.md section 10).
 *
 * On the left, five balls dropped from the same height, from clay
 * (bounciness 0: stops dead) to a superball (1: bounces back as high,
 * forever). On the right, balls of all sizes bouncing off each other
 * too (bounce_all): the big ones are heavier (their mass is their
 * area), and push the small ones around more than they're pushed.
 * Space drops them all again.
 *
 * Every ball, at every tick:
 *
 *   ball |> fall 800. |> step |> bounce_off floor |> bounce_off wall ...
 *
 * The floor and the walls are bodies too, immovable ones (an infinite
 * mass, see Physics.immovable).
 *)
open Playground
open Basics (* float arithmetics *)

let wall (w : number) (h : number) (x : number) (y : number) : Physics.body =
  Physics.body (rectangle (rgb 100 100 110) w h) |> Physics.at x y |> Physics.immovable

(* the floor's top at -400, the sides at -500 and 500, and a divider
 * between the two experiments *)
let walls =
  [ wall 1000. 100. 0. (-450.); wall 40. 1000. (-520.) 0.; wall 40. 1000. 520. 0.; wall 10. 900. 0. 50. ]

let bounciness = [ 0.; 0.25; 0.5; 0.75; 1. ]

let row : Physics.body list =
  List.mapi
    (fun i e ->
      let red = 80 +.. (40 *.. i) in
      Physics.body (circle (rgb red 80 (255 -.. red)) 25.) |> Physics.at (-400. + (85. * float_of_int i)) 300. |> Physics.bouncy e)
    bounciness

(* 16 balls on a jittered grid, radii from 15 to 34, made from their
 * index (no Random: the same every time) *)
let pile : Physics.body list =
  List.init 16 (fun i ->
      let r = float_of_int (15 +.. (i *.. 7 mod 20)) in
      let x = 100. + (100. * float_of_int (i mod 4)) + float_of_int (i *.. 37 mod 30) in
      let y = 50. + (110. * float_of_int (i /.. 4)) in
      Physics.body (circle (rgb (100 +.. (i *.. 40 mod 155)) (200 -.. (i *.. 11 mod 100)) 90) r)
      |> Physics.at x y |> Physics.bouncy 0.8
      |> Physics.heavy (r * r / 400.))

type model = { row : Physics.body list; pile : Physics.body list; space_was_down : bool }

let initial_model = { row; pile; space_was_down = false }

let fall_and_bounce (b : Physics.body) : Physics.body =
  List.fold_left (fun b w -> Physics.bounce_off w b) (b |> Physics.fall 800. |> Physics.step) walls

let update (computer : computer) (model : model) : model =
  let space = computer.keyboard.kspace in
  if space && not model.space_was_down then { initial_model with space_was_down = true }
  else
    {
      row = List.map fall_and_bounce model.row;
      pile = List.map fall_and_bounce model.pile |> Physics.bounce_all;
      space_was_down = space;
    }

let text (s : string) : shape = words black s |> scale 2.

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  (rectangle (rgb 235 235 225) screen.width screen.height :: List.map Physics.draw walls)
  @ [ text "bounciness: 0, 0.25, 0.5, 0.75, 1" |> move (-250.) 450.;
      text "bouncing off each other" |> move 250. 450.;
      text "(space: again)" |> move 250. 415. ]
  @ List.map Physics.draw (model.row @ model.pile)

let app = game view update initial_model
let main = Playground_platform.run_app app
