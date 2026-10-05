(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Breakout_env.mli *)
open Playground

type t = {
  model : TinyBreakout.model;
  frame : int; (* since the start, a 60th of a second each *)
}

let actions = 3
let start () : t = { model = TinyBreakout.initial_model; frame = 0 }

(* the computer at a frame, with the keys held *)
let computer (frame : int) (keyboard : keyboard) : computer =
  { initial_computer with keyboard; time = Time (float_of_int frame /. 60.); screen = to_screen 1000. 1000. }

let game (e : t) : TinyBreakout.game option =
  match e.model.scenes.scene with TinyBreakout.Playing g -> Some g | Title | Game_over _ -> None

let score (e : t) : int = match game e with Some g -> g.score | None -> 0
let over (e : t) : bool = match e.model.scenes.scene with TinyBreakout.Game_over _ -> true | Title | Playing _ -> false
let balls (e : t) : int = match game e with Some g -> g.balls | None -> 0

(* one frame: the action's key, and space every other frame while no
 * ball is in play (a press is a key down after a key up) *)
let frame (e : t) (action : int) : t =
  let waiting = match game e with Some g -> g.ball = None | None -> true in
  let keyboard =
    { initial_computer.keyboard with kleft = action = 0; kright = action = 2; kspace = waiting && e.frame mod 2 = 0 }
  in
  { model = TinyBreakout.update (computer e.frame keyboard) e.model; frame = e.frame + 1 }

let step (e : t) (action : int) : t * int * bool =
  let before = score e and had = balls e in
  let e = frame (frame (frame (frame e action) action) action) action in
  (* a new game starts its score again: no points lost for that *)
  (e, max 0 (score e - before), balls e < had)

let numbers (e : t) : float array =
  match game e with
  | None -> [| 0.; 0.; 0.; 0.; 0.; 0. |]
  | Some g -> (
      match g.ball with
      | None -> [| g.paddle_x /. 350.; 0.; 0.; 0.; 0.; 0. |]
      | Some b -> [| g.paddle_x /. 350.; b.x /. 350.; b.y /. 500.; b.vx /. 600.; b.vy /. 600.; 1. |])

let view (e : t) : shape list = TinyBreakout.view (computer e.frame initial_computer.keyboard) e.model

(*****************************************************************************)
(* The screen *)
(*****************************************************************************)

(* half the paper's 84: a quarter of the pixels, and of the time *)
let side = 42

(* any picture brought down to [side] by [side] greys, a byte each:
 * every pixel of the small one the *average* of the square of the
 * large one it covers, not one pixel picked out of it. A ball of a
 * pixel and a half then leaves its share of grey wherever it is, and
 * the same scene drawn at another size, or by another renderer with
 * its own way of smoothing edges, comes out nearly the same *)
let shrink (fb : Framebuffer.t) : Bytes.t =
  let out = Bytes.create (side * side) in
  for y = 0 to side - 1 do
    let y0 = y * fb.height / side and y1 = max ((y * fb.height / side) + 1) ((y + 1) * fb.height / side) in
    for x = 0 to side - 1 do
      let x0 = x * fb.width / side and x1 = max ((x * fb.width / side) + 1) ((x + 1) * fb.width / side) in
      let sum = ref 0 in
      for sy = y0 to y1 - 1 do
        for sx = x0 to x1 - 1 do
          let rgb = Framebuffer.get_rgb fb ~x:sx ~y:sy in
          (* how bright the eye finds it: green most, blue least *)
          sum := !sum + ((299 * ((rgb lsr 16) land 255)) + (587 * ((rgb lsr 8) land 255)) + (114 * (rgb land 255)))
        done
      done;
      Bytes.set_uint8 out ((y * side) + x) (!sum / (1000 * (y1 - y0) * (x1 - x0)))
    done
  done;
  out

(* drawn six times as fine as it will be read, then shrunk *)
let fine = 6

let screen ?(zoom = 1.) (e : t) : Bytes.t =
  let n = side * fine in
  let fb = Framebuffer.create ~width:n ~height:n in
  Framebuffer.clear fb ~rgb:0;
  Shape_render_software.render ~scale:(zoom *. float_of_int n /. 1000.) fb (view e);
  shrink fb
