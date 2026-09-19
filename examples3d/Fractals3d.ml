(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* examples/Fractals.ml, flying: fractals drawn by the 3D turtle
 * (playground3d/Logo3d.mli), which can also pitch up and down and roll,
 * the camera turning around the drawing:
 *
 *   left/right  another fractal       up/down  its level of recursion
 *   space       draw it again         a        all at once
 *
 * - A tree: a trunk and three smaller trees at its top, rolled 120
 *   degrees apart around it, each pitched 35 degrees down from it; a
 *   leaf (Logo3d.dot) at the end of every twig. The 2D tree's program,
 *   with a roll: [repeat 3 [ ...; roll_right 120. ]].
 *
 * - Hilbert's curve in 3D: the cube's 8^n points, each visited once,
 *   in unit steps. Written as an L-system (Aristid Lindenmayer, 1968:
 *   a string rewritten n times by rules, all its letters at once), then
 *   read by the turtle, a letter a command (Przemyslaw Prusinkiewicz,
 *   1986): F forward, + and - left and right, & and ^ down and up, \
 *   and / the rolls, | a U-turn; A to D only matter to the rewriting.
 *   The rules are those of "The Algorithmic Beauty of Plants"
 *   (Prusinkiewicz and Lindenmayer, Springer, 1990, figure 1.24),
 *   http://algorithmicbotany.org/papers/#abop; colored along the way,
 *   red to blue, to follow it.
 *
 * - Koch's snowflake, crumpled: the 2D Koch curve (examples/Fractals.ml)
 *   has its bumps turned left and right; here every other level pitches
 *   them up and down instead, out of the plane. Its ends stay where
 *   the flat one's are (the bump's pitches, up 60, down 120, up 60, add
 *   up to nothing, like the turns did): the same snowflake seen from
 *   the front, a crumpled one from the side.
 *
 * A finished drawing is kept as a [cached3d] (built once, when the
 * fractal or its level is chosen): the GPU backends then keep it in
 * their buffers, and only the one being drawn is rebuilt every frame.
 *
 * What it uses: Playground3d (cached3d, hud), Logo3d, Scene2d for the
 * keys.
 *)
open Playground
open Playground3d
open Basics (* float arithmetics *)
open Logo3d

(*****************************************************************************)
(* The fractals *)
(*****************************************************************************)

let brown = rgb 110 70 30
let leaf = rgb 60 170 60

let rec tree n size =
  if n = 0 then block [ pen_color leaf; dot 24. ]
  else
    block
      [ pen_size (max 1. (size / 8.)); pen_color brown; forward size;
        repeat 3 [ down 35.; tree (n -.. 1) (size * 0.7); up 35.; roll_right 120. ];
        pen_up; back size; pen_down ]

let tree3d n = [ tree n 150. ]

(* an L-system: [rules] rewrite each letter, [n] times *)
let rec expand (rules : (char * string) list) (n : int) (s : string) : string =
  if n = 0 then s
  else
    expand rules (n -.. 1)
      (String.concat "" (List.map (fun c -> Option.value (List.assoc_opt c rules) ~default:(String.make 1 c)) (List.of_seq (String.to_seq s))))

let hilbert_rules =
  [ ('A', "B-F+CFC+F-D&F^D-F+&&CFC+F+B//");
    ('B', "A&F^CFB^F^D^^-F-D^|F^B|FC^F^A//");
    ('C', "|D^|F^B-F+C^F^A&&FA&F^C+F+B^F^D//");
    ('D', "|CFB-F+B|FA&F^A&&FB-F+B|FC//") ]

(* the turtle reading it, [step] a forward; the color from red to blue
 * along the steps, [steps] in all *)
let read (step : number) (steps : int) (s : string) : command list =
  let i = ref 0 in
  List.of_seq (String.to_seq s)
  |> List.map (function
       | 'F' ->
           let t = float_of_int !i / float_of_int (max 1 steps) in
           incr i;
           block [ pen_color (rgb (truncate (20. + (230. * (1. - t)))) 60 (truncate (20. + (230. * t)))); forward step ]
       | '+' -> left 90.
       | '-' -> right 90.
       | '&' -> down 90.
       | '^' -> up 90.
       | '\\' -> roll_right 90.
       | '/' -> roll_left 90.
       | '|' -> left 180.
       | _ -> stop)

let hilbert3d n =
  (* 2^n - 1 steps along each side, 300 in all *)
  let step = 300. / ((2. ** float_of_int n) - 1.) in
  [ pen_size (step * 0.3); block (read step ((1 lsl (3 *.. n)) -.. 1) (expand hilbert_rules n "A")) ]

(* the Koch curve, its bumps in the turtle's plane at the even levels,
 * pitched out of it at the odd ones *)
let rec koch n len =
  if n = 0 then forward len
  else
    let k = koch (n -.. 1) (len / 3.) in
    if n mod 2 = 0 then block [ k; left 60.; k; right 120.; k; left 60.; k ]
    else block [ k; up 60.; k; down 120.; k; up 60.; k ]

let snowflake3d n = [ pen_color (rgb 30 80 160); pen_size 3.; right 90.; repeat 3 [ koch n 400.; right 120. ] ]

type fractal = { name : string; program : int -> command list; initial : int; min_level : int; max_level : int }

let fractals =
  [| { name = "a tree"; program = tree3d; initial = 5; min_level = 0; max_level = 7 };
     { name = "Hilbert's curve, in 3D"; program = hilbert3d; initial = 2; min_level = 1; max_level = 3 };
     { name = "Koch's snowflake, crumpled"; program = snowflake3d; initial = 3; min_level = 0; max_level = 5 } |]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type state = {
  index : int;
  level : int;
  program : command list;
  (* to draw it in 10 seconds *)
  speed : number;
  work : number;
  (* the finished drawing, and the camera for it *)
  whole : shape3d;
  camera : number -> camera;
  all_at_once : bool;
}

type model = state Scene2d.t

let state (index : int) (level : int) : state =
  let program = fractals.(index).program level in
  let work = work program in
  { index; level; program; speed = max 1. (work / 10.); work; whole = cached3d (draw program);
    camera = camera_around program; all_at_once = false }

let initial_model : model = Scene2d.start (state 0 fractals.(0).initial)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (model : model) : model =
  let model = Scene2d.update computer model in
  let s = model.scene in
  let f = fractals.(s.index) in
  let pressed key = Scene2d.pressed key model in
  let n = Array.length fractals in
  let another i = Scene2d.go (state i fractals.(i).initial) model in
  if pressed (fun k -> k.kright) then another ((s.index +.. 1) mod n)
  else if pressed (fun k -> k.kleft) then another ((s.index +.. n -.. 1) mod n)
  else if pressed (fun k -> k.kup) && s.level < f.max_level then Scene2d.go (state s.index (s.level +.. 1)) model
  else if pressed (fun k -> k.kdown) && s.level > f.min_level then Scene2d.go (state s.index (s.level -.. 1)) model
  else if pressed (fun k -> k.kspace) then Scene2d.go { s with all_at_once = false } model
  else if pressed (fun k -> Set_.mem "a" k.keys) then { model with scene = { s with all_at_once = true } }
  else model

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view (computer : computer) (model : model) : camera * shape3d list =
  let s = model.scene in
  let screen = computer.screen in
  let text size color y str = hud (words color str |> scale size |> move_y y) in
  let progress = model.elapsed * s.speed in
  let drawing = if s.all_at_once || progress >= s.work then [ s.whole ] else draw_upto progress s.program in
  ( s.camera (spin 30. computer.time),
    drawing
    @ [ text 2.5 black (screen.top - 30.) (Printf.sprintf "%s, level %d" fractals.(s.index).name s.level);
        text 1.5 (rgb 100 100 100) (screen.bottom + 20.)
          "left/right: another fractal   up/down: its level   space: again   a: all at once" ] )

let app = game3d view update initial_model
let main = Playground3d_platform.run_app3d app
