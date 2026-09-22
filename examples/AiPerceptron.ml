(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The first learning machine, learning while you watch (ai/Neuron.mli,
 * notes_ai_learning.md section 1). Click to drop a blue point, hold
 * shift (or right-click) for a red one, and the line walks into place:
 * one example at a time, and it only ever moves when it is wrong.
 *
 * What there is to see:
 *
 *  - **It learns from mistakes alone.** The point it is being shown is
 *    ringed; when the ring is on a point it already gets right, the
 *    line does not move at all. Rosenblatt's rule, 1958:
 *
 *        w <- w + rate * (target - answer) * x
 *
 *  - **The weights are the line.** The arrow from the middle is [w],
 *    the normal to the line, and the line is everywhere the weighted
 *    sum is zero. Learning is not "fitting a curve", it is turning
 *    that arrow.
 *
 *  - **It stops.** When a line exists, the mistakes reach zero and
 *    every later example leaves the weights exactly where they are --
 *    the perceptron convergence theorem, watchable in a second.
 *
 *  - **And then XOR.** Press "x" for the four corners of exclusive-or.
 *    No line separates them, so the rule never stops: the line swings
 *    between two bad answers for ever, and the mistake counter never
 *    reaches zero. Worse -- and this is the part worth watching -- it
 *    does not even settle on the best line it could draw, which gets
 *    three of the four. A rule that cannot converge does not politely
 *    stop at the best approximation; it wanders.
 *
 * That failure cost the field a decade (Minsky and Papert,
 * *Perceptrons*, 1969), and the way out is a stack of these neurons
 * and a way to train a stack: examples/AiNeuralNet.ml, when Net and
 * Backprop are written.
 *
 * Keys: click blue, shift-click (or right-click) red, "x" XOR, "a" AND,
 * "c" clear, "space" one example at a time or running, "r" new weights.
 *
 * What it uses: ai/Neuron (the whole of the learning), Scene2d (the
 * keys). Not playground/Ai: this is the algorithm itself, not a game
 * asking for an opponent. *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

(* the points live in -1..1 both ways, and are drawn 300 pixels to the
 * unit: a neuron's weights mean nothing without knowing the scale its
 * inputs are on *)
let unit_ = 300.
let screen_of ((x, y) : number * number) : number * number = (x * unit_, y * unit_)
let world_of ((x, y) : number * number) : number * number = (x / unit_, y / unit_)

type model = {
  points : Neuron.example list; (* newest first *)
  neuron : Neuron.t;
  showing : int; (* which example it is being shown, an index from the end *)
  running : bool;
  every : int; (* frames between two examples *)
  tick : int;
  seed : int;
  said : string;
}

let fresh (seed : int) (points : Neuron.example list) (said : string) : model =
  { points; neuron = Neuron.make ~inputs:2 ~seed; showing = 0; running = true; every = 6; tick = 0; seed; said }

(* the four corners, drawn where they can be seen *)
let corners (f : float -> float -> float) : Neuron.example list =
  List.map (fun (a, b) -> ([| (a * 1.2) - 0.6; (b * 1.2) - 0.6 |], f a b)) [ (0., 0.); (0., 1.); (1., 0.); (1., 1.) ]

let initial_model : model = fresh 1 [] "click to drop points"

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* one example shown: the rule, and nothing else *)
let show_one (m : model) : model =
  match m.points with
  | [] -> m
  | _ ->
      let n = List.length m.points in
      let i = m.showing mod n in
      let example = List.nth m.points (n -.. 1 -.. i) in
      { m with neuron = Neuron.learn ~rate:0.1 m.neuron example; showing = i +.. 1; tick = 0 }

let update (computer : computer) (s : model Scene2d.t) : model Scene2d.t =
  let scenes = Scene2d.update computer s in
  let m = scenes.scene in
  let key k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  let mouse = computer.mouse in
  let m =
    if mouse.mclick then
      let (x, y) = world_of (mouse.mx, mouse.my) in
      if Float.abs x <= 1. && Float.abs y <= 1. then
        let label = if computer.keyboard.kshift || mouse.mrdown then 0. else 1. in
        { m with points = ([| x; y |], label) :: m.points; said = "" }
      else m
    else m
  in
  let m = if key "x" then fresh m.seed (corners (fun a b -> if a <> b then 1. else 0.)) "XOR: no line does this" else m in
  let m = if key "a" then fresh m.seed (corners (fun a b -> if a = 1. && b = 1. then 1. else 0.)) "AND: one line does" else m in
  let m = if key "c" then fresh m.seed [] "click to drop points" else m in
  let m = if key "r" then { (fresh (m.seed +.. 1) m.points m.said) with running = m.running } else m in
  let m = if Scene2d.pressed (fun k -> k.kspace) scenes then { m with running = not m.running } else m in
  (* running, one example every [every] frames; stopped, one per press
     of the right arrow: the rule is easier to believe step by step *)
  let m =
    if Scene2d.pressed (fun k -> k.kright) scenes then show_one m
    else if m.running && m.tick >= m.every then show_one m
    else { m with tick = m.tick +.. 1 }
  in
  { scenes with scene = m }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let blue = rgb 90 160 240
let red = rgb 230 110 100
let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let colour_of (label : number) : color = if label = 1. then blue else red

(* the line where the weighted sum is zero, w0 x + w1 y + b = 0, cut
 * to the board: where it crosses the four edges, and nothing at all
 * when it misses the board entirely (early on it often does -- a small
 * weight against a large bias is a line far away) *)
let line_shapes (n : Neuron.t) : shape list =
  let (a, b, c) = (n.weights.(0), n.weights.(1), n.bias) in
  let inside v = v >= -1.001 && v <= 1.001 in
  let crossings =
    (* against the two vertical edges, then the two horizontal ones *)
    (if Float.abs b > 1e-9 then
       List.filter_map
         (fun x -> let y = -.(c + (a * x)) / b in if inside y then Some (x, y) else None)
         [ -1.; 1. ]
     else [])
    @
    if Float.abs a > 1e-9 then
      List.filter_map (fun y -> let x = -.(c + (b * y)) / a in if inside x then Some (x, y) else None) [ -1.; 1. ]
    else []
  in
  match List.sort_uniq compare crossings with
  | p :: rest when rest <> [] ->
      let q = List.nth rest (List.length rest -.. 1) in
      let (x1, y1) = screen_of p and (x2, y2) = screen_of q in
      [ rectangle (rgb 240 230 150) (Float.hypot (x2 - x1) (y2 - y1)) 3.
        |> rotate (Float.atan2 (y2 - y1) (x2 - x1) * 180. / Float.pi)
        |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.) ]
  | _ -> []

(* the weights as an arrow from the middle: the line's normal *)
let weights_arrow (n : Neuron.t) : shape list =
  let (a, b) = (n.weights.(0), n.weights.(1)) in
  let len = Float.hypot a b in
  if len < 1e-9 then []
  else
    let k = 120. / len in
    let (x, y) = (a * k, b * k) in
    [ rectangle (rgb 150 220 170) (Float.hypot x y) 2.5 |> rotate (Float.atan2 y x * 180. / Float.pi) |> move (x / 2.) (y / 2.);
      triangle (rgb 150 220 170) 8. |> rotate ((Float.atan2 y x * 180. / Float.pi) - 90.) |> move x y ]

let view (computer : computer) (s : model Scene2d.t) : shape list =
  let m = s.scene and screen = computer.screen in
  let n = List.length m.points in
  let dots =
    List.mapi
      (fun i ((x, label) : Neuron.example) ->
        let (sx, sy) = screen_of (x.(0), x.(1)) in
        let wrong = Neuron.answer m.neuron x <> label in
        group
          ([ circle (colour_of label) 9. ]
          (* what it gets wrong, ringed in its own colour: the mistakes
             are the only thing it learns from *)
          @ (if wrong then [ circle white 14. |> fade 0.25 ] else [])
          @ if n > 0 && i = n -.. 1 -.. ((m.showing +.. n -.. 1) mod n) then [ circle white 18. |> fade 0.5 ] else [])
        |> move sx sy)
      m.points
  in
  let mistakes = Neuron.mistakes m.neuron m.points in
  [ rectangle (rgb 18 20 30) screen.width screen.height;
    rectangle (rgb 30 34 50) (2. * unit_) (2. * unit_);
    rectangle (rgb 60 66 90) (2. * unit_) 1.;
    rectangle (rgb 60 66 90) 1. (2. * unit_) ]
  @ line_shapes m.neuron @ weights_arrow m.neuron @ dots
  @ [ text white 2.2 "ONE NEURON" |> move_y 420.;
      text (rgb 150 155 175) 1.4 "click blue, shift-click red;  a: AND  x: XOR  c: clear  r: new weights" |> move_y (-400.);
      text (rgb 150 155 175) 1.4
        (Printf.sprintf "space: %s   right arrow: one example" (if m.running then "pause" else "run"))
      |> move_y (-430.);
      text
        (if mistakes = 0 then rgb 140 220 150 else rgb 230 190 120)
        1.6
        (if m.points = [] then "" else Printf.sprintf "%d of %d wrong" mistakes n)
      |> move_y 370.;
      text (rgb 200 205 220) 1.5 m.said |> move_y (-360.) ]

let app = game view update (Scene2d.start initial_model)
let main = Playground_platform.run_app app
