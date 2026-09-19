(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* 300 marbles in a box, all bouncing off each other: the broad phase
 * (physics/2d/Broadphase.mli, docs/claude_notes/notes_2d_physics.md
 * section 9).
 *
 * 300 marbles make 44,850 pairs, but each touches only a few others.
 * Which pairs are worth testing? Space switches the method, and the
 * line at the top counts the box tests it made this frame:
 *
 *  - all pairs: 44,850, every frame;
 *  - a grid (drawn): only the marbles sharing a cell, a few hundred;
 *  - sort and sweep: only the marbles whose x ranges overlap.
 *
 * A stress scene like Chipmunk's "Plink" (Scott Lembcke: 300
 * pentagons falling through a grid of triangles),
 *   https://github.com/slembcke/Chipmunk2D/blob/master/demo/Plink.c
 * (the idea, written from scratch here).
 *
 * The three find the same pairs, so the marbles move exactly the same:
 * only the work differs (and the frame rate, with -uncapped, on the
 * software backend's title bar).
 *)
open Playground
open Basics (* float arithmetics *)

let wall (w : number) (h : number) (x : number) (y : number) : Physics.body =
  Physics.body (rectangle (rgb 100 100 110) w h) |> Physics.at x y |> Physics.immovable

(* a box from -480 to 480, the floor at -400 *)
let walls = [ wall 1000. 100. 0. (-450.); wall 40. 1000. (-500.) 0.; wall 40. 1000. 500. 0. ]

(* 20 x 15 marbles, radii 6 to 12, thrown in all directions, from their
 * index (no Random) *)
let marbles : Physics.body list =
  List.init 300 (fun i ->
      let r = float_of_int (6 +.. (i *.. 5 mod 7)) in
      let x = -380. + (40. * float_of_int (i mod 20)) and y = -300. + (40. * float_of_int (i /.. 20)) in
      Physics.body (circle (rgb (80 +.. (i *.. 53 mod 176)) (80 +.. (i *.. 97 mod 176)) (200 -.. (i *.. 31 mod 120))) r)
      |> Physics.at x y
      |> Physics.moving (float_of_int (i *.. 37 mod 300) - 150.) (float_of_int (i *.. 71 mod 300) - 150.)
      |> Physics.bouncy 0.9)

type model = { marbles : Physics.body list; method_ : Broadphase.method_; space_was_down : bool }

let initial_model = { marbles; method_ = Broadphase.All_pairs; space_was_down = false }

(* the method after [m], around *)
let next (m : Broadphase.method_) : Broadphase.method_ =
  let rec after = function x :: (y :: _ as rest) -> if x = m then y else after rest | _ -> List.hd Broadphase.methods in
  after Broadphase.methods

let update (computer : computer) (model : model) : model =
  let space = computer.keyboard.kspace in
  let method_ = if space && not model.space_was_down then next model.method_ else model.method_ in
  let marbles =
    model.marbles
    |> List.map (fun m -> List.fold_left (fun m w -> Physics.bounce_off w m) (m |> Physics.fall 300. |> Physics.step) walls)
    |> Physics.bounce_all ~broad_phase:method_
  in
  { marbles; method_; space_was_down = space }

let text (s : string) : shape = words black s |> scale 2.

(* the grid's cells: as big as the biggest marble, 24 pixels, their
 * edges on the multiples of 24 (Broadphase.grid) *)
let grid_lines : shape list =
  let lines = List.init 41 (fun i -> -480. + (24. * float_of_int i)) in
  List.map (fun x -> rectangle (rgb 200 200 230) 1. 960. |> move_x x) lines
  @ List.map (fun y -> rectangle (rgb 200 200 230) 960. 1. |> move_y y) lines

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let r = Physics.broad_phase model.method_ model.marbles in
  (rectangle (rgb 240 240 235) screen.width screen.height :: (if model.method_ = Broadphase.Grid then grid_lines else []))
  @ List.map Physics.draw walls
  @ List.map Physics.draw model.marbles
  @ [ text (Broadphase.name model.method_ ^ "   (space: the next method)") |> move_y 460.;
      text (Printf.sprintf "%d box tests, %d pairs touching (all pairs: 44850)" r.tests (List.length r.pairs)) |> move_y 425. ]

let app = game view update initial_model
let main = Playground_platform.run_app app
