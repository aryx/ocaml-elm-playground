(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Net.mli *)

type activation = Sigmoid | Tanh | Relu | Linear

let squash (f : activation) (z : float) : float =
  match f with
  | Sigmoid -> 1. /. (1. +. exp (-.z))
  | Tanh -> tanh z
  | Relu -> if z > 0. then z else 0.
  | Linear -> z

(* the derivatives, taken from the output where that is cheaper: the
 * sigmoid's is a (1 - a) and the tanh's 1 - a^2, which is why the
 * forward pass keeps what it computed *)
let slope (f : activation) ~(z : float) ~(a : float) : float =
  match f with
  | Sigmoid -> a *. (1. -. a)
  | Tanh -> 1. -. (a *. a)
  | Relu -> if z > 0. then 1. else 0.
  | Linear -> 1.

type layer = { w : Matrix.t; b : Matrix.t; f : activation }
type t = layer list

let make ~(seed : int) ?(hidden = Tanh) ?(last = Sigmoid) (sizes : int list) : t =
  let rec go seed sizes =
    match sizes with
    | inputs :: outputs :: rest ->
        (* Glorot: the spread that keeps the signal's size about the
         * same on the way through *)
        let spread = sqrt (6. /. float_of_int (inputs + outputs)) in
        let f = if rest = [] then last else hidden in
        { w = Matrix.random ~seed ~spread outputs inputs; b = Matrix.create outputs 1; f }
        :: go (seed + 1) (outputs :: rest)
    | _ -> []
  in
  go seed sizes

type pass = { input : Matrix.t; steps : (Matrix.t * Matrix.t) list }

let forward_pass (net : t) (x : float array) : pass =
  let input = Matrix.vector x in
  let (_, steps) =
    List.fold_left
      (fun (a', steps) (l : layer) ->
        let z = Matrix.add (Matrix.mul l.w a') l.b in
        let a = Matrix.map (squash l.f) z in
        (a, (z, a) :: steps))
      (input, []) net
  in
  { input; steps = List.rev steps }

let output (p : pass) : float array =
  match List.rev p.steps with (_, a) :: _ -> Matrix.to_vector a | [] -> Matrix.to_vector p.input

let forward (net : t) (x : float array) : float array = output (forward_pass net x)

let sizes (net : t) : int list =
  match net with [] -> [] | first :: _ -> first.w.cols :: List.map (fun (l : layer) -> l.w.rows) net

let weights (net : t) : int =
  List.fold_left (fun n (l : layer) -> n + Array.length l.w.data + Array.length l.b.data) 0 net
