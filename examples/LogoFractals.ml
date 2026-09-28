(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The classic fractals, each a few lines of Logo (Logo.mli),
 * drawn by the turtle as you watch:
 *
 *   left/right  another fractal       up/down  its level of recursion
 *   space       draw it again         a        all at once
 *
 * Each one is a recursive procedure: at level 0 a straight line (or a
 * triangle), at level n the same figure made of level n-1 copies.
 * Going down to levels 0, 1 and 2 shows the rule; up to 5 or 10, the
 * fractal. E.g. the Koch curve, whose level 1 is  _/\_ :
 *
 *   let rec koch n len =
 *     if n = 0 then forward len
 *     else block [ koch (n - 1) (len / 3.); left 60.; koch (n - 1) (len / 3.); right 120.;
 *                  koch (n - 1) (len / 3.); left 60.; koch (n - 1) (len / 3.) ]
 *
 * the same as in Logo:
 *
 *   to koch :n :len
 *     if :n = 0 [forward :len stop]
 *     koch :n - 1 :len / 3 left 60 koch :n - 1 :len / 3 right 120
 *     koch :n - 1 :len / 3 left 60 koch :n - 1 :len / 3
 *   end
 *
 * Each is drawn in 10 seconds whatever its level (the turtle's speed is
 * its work divided by 10, Logo.work), so the high levels go fast.
 *
 * The fractals, and where the Logo versions come from: Helge von Koch,
 * 1904 (the snowflake); Waclaw Sierpinski, 1915 (the triangle); the
 * binary tree; the dragon curve, John Heighway, Bruce Banks and William
 * Harter at NASA, 1966, made famous by Martin Gardner's column
 * (Scientific American, 1967); David Hilbert's curve, 1891, filling a
 * square. All in Abelson and diSessa's "Turtle Geometry" (MIT Press,
 * 1981) and in Brian Harvey's "Computer Science Logo Style", vol. 1,
 * https://people.eecs.berkeley.edu/~bh/v1-toc2.html; and Benoit
 * Mandelbrot, "The Fractal Geometry of Nature", 1982, for the word.
 *
 * What it uses: the Playground, Logo, and Scene2d for the keys.
 *)
open Playground
open Basics (* float arithmetics *)
open Logo

(*****************************************************************************)
(* The fractals *)
(*****************************************************************************)

(* Koch's snowflake: three Koch curves, around a triangle, filled *)
let rec koch n len =
  if n = 0 then forward len
  else
    let k = koch (n -.. 1) (len / 3.) in
    block [ k; left 60.; k; right 120.; k; left 60.; k ]

let snowflake n = [ pen_up; set_xy (-270.) 170.; set_heading 90.; pen_down; pen_color (rgb 30 80 160);
                    filled (rgb 180 215 245) [ repeat 3 [ koch n 540.; right 120. ] ] ]

(* Sierpinski's triangle: three half-size triangles, at its corners; the
 * turtle walks between them pen up, their sides are its own *)
let rec sierpinski n len =
  if n = 0 then filled (rgb 240 150 60) [ repeat 3 [ forward len; right 120. ] ]
  else repeat 3 [ sierpinski (n -.. 1) (len / 2.); pen_up; forward len; right 120.; pen_down ]

let triangle n = [ pen_up; set_xy (-300.) (-260.); set_heading 30.; pen_down; pen_color (rgb 120 50 10); pen_size 1.; sierpinski n 600. ]

(* the tree: a trunk, and two smaller trees at its top, 25 degrees
 * apart; thinner and greener higher up. Back down pen up: the branches
 * changed the pen *)
let rec tree n size =
  if n = 0 then stop
  else
    block
      [ pen_size (max 1. (size / 10.));
        pen_color (if n > 2 then rgb 110 70 30 else rgb 50 160 50);
        forward size;
        left 25.; tree (n -.. 1) (size * 0.75);
        right 50.; tree (n -.. 1) (size * 0.75);
        left 25.;
        pen_up; back size; pen_down ]

let binary_tree n = [ pen_up; set_xy 0. (-420.); pen_down; tree n 190. ]

(* the dragon: fold a strip of paper in two n times, open it with right
 * angles at the folds; a dragon of level n is a dragon of level n-1,
 * a turn, and one of level n-1 turning the other way *)
let rec dragon n turn len =
  if n = 0 then forward len else block [ dragon (n -.. 1) 90. len; right turn; dragon (n -.. 1) (-90.) len ]

(* from its start to its end, a dragon of level n is its level n-1's
 * two, at right angles: sqrt 2 longer, turned 45 degrees clockwise. So
 * its segments are sqrt 2^n shorter to keep it the same size (480
 * pixels from start to end), and it starts turned 45 n degrees
 * counterclockwise to stay put, from level to level *)
let dragon_curve n =
  let len = 480. / (2. ** (float_of_int n / 2.)) in
  [ pen_up; set_xy (-200.) (-80.); set_heading (90. - (45. * float_of_int n)); pen_down; pen_color (rgb 170 30 60); dragon n 90. len ]

(* Hilbert's curve, level n: four level n-1 ones, the first and last
 * turned, joined by three steps; [parity] 1 or -1, the way it turns *)
let rec hilbert n parity len =
  if n = 0 then stop
  else
    let h p = hilbert (n -.. 1) p len in
    block
      [ left (90. * parity); h (-.parity); forward len; right (90. * parity); h parity; forward len;
        h parity; right (90. * parity); forward len; h (-.parity); left (90. * parity) ]

let hilbert_curve n =
  let len = 600. / ((2. ** float_of_int n) - 1.) in
  [ pen_up; set_xy 300. (-300.); set_heading 0.; pen_down; pen_color (rgb 90 40 150); hilbert n 1. len ]

type fractal = { name : string; program : int -> command list; initial : int; max_level : int }

let fractals =
  [| { name = "Koch's snowflake (1904)"; program = snowflake; initial = 4; max_level = 6 };
     { name = "Sierpinski's triangle (1915)"; program = triangle; initial = 5; max_level = 7 };
     { name = "a binary tree"; program = binary_tree; initial = 9; max_level = 12 };
     { name = "the dragon curve (1966)"; program = dragon_curve; initial = 12; max_level = 16 };
     { name = "Hilbert's curve (1891)"; program = hilbert_curve; initial = 4; max_level = 7 } |]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type state = {
  index : int;
  level : int;
  program : command list;
  (* the turtle's speed, to draw it in 10 seconds *)
  speed : number;
  all_at_once : bool;
}

type model = state Scene2d.t

let state (index : int) (level : int) : state =
  let program = fractals.(index).program level in
  { index; level; program; speed = max 1. (work program / 10.); all_at_once = false }

let initial_model : model = Scene2d.start (state 0 fractals.(0).initial)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (model : model) : model =
  let model = Scene2d.update computer model in
  let s = model.scene in
  let pressed key = Scene2d.pressed key model in
  let n = Array.length fractals in
  let another i = Scene2d.go (state i fractals.(i).initial) model in
  if pressed (fun k -> k.kright) then another ((s.index +.. 1) mod n)
  else if pressed (fun k -> k.kleft) then another ((s.index +.. n -.. 1) mod n)
  else if pressed (fun k -> k.kup) && s.level < fractals.(s.index).max_level then Scene2d.go (state s.index (s.level +.. 1)) model
  else if pressed (fun k -> k.kdown) && s.level > 0 then Scene2d.go (state s.index (s.level -.. 1)) model
  else if pressed (fun k -> k.kspace) then Scene2d.go { s with all_at_once = false } model
  else if pressed (fun k -> Set_.mem "a" k.keys) then { model with scene = { s with all_at_once = true } }
  else model

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view (computer : computer) (model : model) : shape list =
  let s = model.scene in
  let screen = computer.screen in
  let text size color str = words color str |> scale size in
  (rectangle (rgb 250 248 240) screen.width screen.height
   :: draw_upto (if s.all_at_once then infinity else model.elapsed * s.speed) s.program)
  @ [ text 2.5 black (Printf.sprintf "%s, level %d" fractals.(s.index).name s.level) |> move_y (screen.top - 30.);
      text 1.5 (rgb 100 100 100) "left/right: another fractal   up/down: its level   space: again   a: all at once"
      |> move_y (screen.bottom + 20.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
