(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Logo.mli *)

open Playground

type number = Playground.number

type command =
  | Forward of number
  (* degrees, clockwise *)
  | Turn of number
  | Home
  | Set_xy of number * number
  | Set_heading of number
  | Pen of bool
  | Pen_color of color
  | Pen_size of number
  | Filled of color * command list
  | Label of string
  | Show of bool
  | Repeat of int * command list

let forward d = Forward d
let back d = Forward (-.d)
let left a = Turn (-.a)
let right a = Turn a
let home = Home
let set_xy x y = Set_xy (x, y)
let set_heading h = Set_heading h
let pen_up = Pen false
let pen_down = Pen true
let pen_color c = Pen_color c
let pen_size s = Pen_size s
let filled c program = Filled (c, program)
let label s = Label s
let hide_turtle = Show false
let show_turtle = Show true
let repeat n program = Repeat (n, program)
let block program = Repeat (1, program)
let stop = Repeat (0, [])
let fd = forward
let bk = back
let lt = left
let rt = right
let pu = pen_up
let pd = pen_down

(*****************************************************************************)
(* The turtle, running a program *)
(*****************************************************************************)

type turtle = {
  x : number;
  y : number;
  (* degrees, clockwise, 0 up *)
  heading : number;
  pen : bool;
  color : color;
  size : number;
  visible : bool;
}

let start = { x = 0.; y = 0.; heading = 0.; pen = true; color = black; size = 2.; visible = true }

type run = {
  turtle : turtle;
  (* the work left to do, and done so far (see Logo.mli's [work]) *)
  budget : number;
  spent : number;
  (* true once a command was cut short: the rest isn't done *)
  out : bool;
  (* the shapes drawn, and the points walked through (for [filled]),
   * the last first *)
  drawn : shape list;
  path : (number * number) list;
}

(* a degree turned is worth a quarter of a pixel walked *)
let turn_cost = 0.25

(* a line, as a thin rectangle turned; [size] longer than the segment,
 * so that consecutive segments overlap at the corners instead of
 * leaving a notch *)
let segment (color : color) (size : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  rectangle color (Float.hypot (x2 -. x1) (y2 -. y1) +. size) size
  |> rotate (Float.atan2 (y2 -. y1) (x2 -. x1) *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

(* the fraction of a command of [cost] the budget allows *)
let affordable (r : run) (cost : number) : number = if cost <= r.budget then 1. else r.budget /. cost

let spend (r : run) (cost : number) : run =
  if cost <= r.budget then { r with budget = r.budget -. cost; spent = r.spent +. cost }
  else { r with budget = 0.; spent = r.spent +. r.budget; out = true }

(* to (x, y), in a straight line, drawing if the pen is down; only as
 * far as the budget allows *)
let walk (r : run) ((x, y) : number * number) : run =
  let t = r.turtle in
  let dist = Float.hypot (x -. t.x) (y -. t.y) in
  let f = affordable r dist in
  let x = t.x +. (f *. (x -. t.x)) and y = t.y +. (f *. (y -. t.y)) in
  let drawn = if t.pen && dist > 0. then segment t.color t.size (t.x, t.y) (x, y) :: r.drawn else r.drawn in
  { (spend r dist) with turtle = { t with x; y }; drawn; path = (x, y) :: r.path }

let rec run_command (r : run) (c : command) : run =
  let t = r.turtle in
  if r.out then r
  else
    match c with
    | Forward d ->
        (* the heading's direction: clockwise from up *)
        let h = t.heading *. Float.pi /. 180. in
        walk r (t.x +. (d *. sin h), t.y +. (d *. cos h))
    | Turn a ->
        let cost = Float.abs a *. turn_cost in
        let f = affordable r cost in
        { (spend r cost) with turtle = { t with heading = t.heading +. (f *. a) } }
    | Home -> { r with turtle = { t with x = 0.; y = 0.; heading = 0. }; path = (0., 0.) :: r.path }
    | Set_xy (x, y) -> walk r (x, y)
    | Set_heading h -> { r with turtle = { t with heading = h } }
    | Pen b -> { r with turtle = { t with pen = b } }
    | Pen_color color -> { r with turtle = { t with color } }
    | Pen_size size -> { r with turtle = { t with size } }
    | Show visible -> { r with turtle = { t with visible } }
    | Label s -> { r with drawn = (words t.color s |> move t.x t.y) :: r.drawn }
    | Filled (color, program) ->
        (* the program's own lines, and its path from here *)
        let sub = run_program { r with drawn = []; path = [ (t.x, t.y) ] } program in
        let fill = if sub.out then [] else [ polygon color (List.rev sub.path) ] in
        { sub with drawn = sub.drawn @ fill @ r.drawn; path = sub.path @ r.path }
    | Repeat (n, program) ->
        let r = ref r in
        for _ = 1 to n do
          r := run_program !r program
        done;
        !r

and run_program (r : run) (program : command list) : run = List.fold_left run_command r program

let run (budget : number) (program : command list) : run =
  run_program { turtle = start; budget; spent = 0.; out = false; drawn = []; path = [ (0., 0.) ] } program

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

(* the turtle: a triangle pointing where it heads *)
let turtle_shape (t : turtle) : shape =
  group [ polygon (rgb 40 160 70) [ (0., 14.); (-9., -9.); (9., -9.) ]; polygon (rgb 20 90 40) [ (0., 6.); (-4., -4.); (4., -4.) ] ]
  |> rotate (-.t.heading) |> move t.x t.y

let draw_upto (budget : number) (program : command list) : shape list =
  let r = run budget program in
  List.rev r.drawn @ if r.turtle.visible then [ turtle_shape r.turtle ] else []

let draw (program : command list) : shape list = draw_upto infinity program
let work (program : command list) : number = (run infinity program).spent

(*****************************************************************************)
(* Applications *)
(*****************************************************************************)

let picture (program : command list) = Playground.picture (draw program)

let animation ?(speed = 300.) (program : command list) =
  let view (_ : computer) (s : unit Scene2d.t) = draw_upto (s.elapsed *. speed) program in
  (* space: draw it again *)
  let update (computer : computer) (s : unit Scene2d.t) =
    let s = Scene2d.update computer s in
    if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go () s else s
  in
  game view update (Scene2d.start ())
