(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Neuron.mli *)

type t = { weights : float array; bias : float }
type example = float array * float

let make ~(inputs : int) ~(seed : int) : t =
  let st = Random.State.make [| seed |] in
  { weights = Array.init inputs (fun _ -> Random.State.float st 0.2 -. 0.1); bias = 0. }

let sum (n : t) (x : float array) : float =
  if Array.length x <> Array.length n.weights then invalid_arg "Neuron: wrong number of inputs";
  let s = ref n.bias in
  Array.iteri (fun i v -> s := !s +. (n.weights.(i) *. v)) x;
  !s

(* the step: the oldest activation there is, and the reason this neuron
 * cannot be trained by gradients (a step has no slope to walk down) *)
let answer (n : t) (x : float array) : float = if sum n x > 0. then 1. else 0.

let learn ?(rate = 0.1) (n : t) ((x, target) : example) : t =
  let wrong = target -. answer n x in
  (* a right answer leaves everything exactly as it was *)
  if wrong = 0. then n
  else
    { weights = Array.mapi (fun i w -> w +. (rate *. wrong *. x.(i))) n.weights; bias = n.bias +. (rate *. wrong) }

let epoch ?rate (n : t) (examples : example list) : t = List.fold_left (fun n e -> learn ?rate n e) n examples
let mistakes (n : t) (examples : example list) : int =
  List.length (List.filter (fun ((x, target) : example) -> answer n x <> target) examples)

let train ?(epochs = 100) ?rate (n : t) (examples : example list) : t =
  let rec go n left = if left = 0 || mistakes n examples = 0 then n else go (epoch ?rate n examples) (left - 1) in
  go n epochs

let learns ?epochs ?(seed = 1) (examples : example list) : float =
  match examples with
  | [] -> 1.
  | (x, _) :: _ ->
      let n = train ?epochs (make ~inputs:(Array.length x) ~seed) examples in
      let got = List.length examples - mistakes n examples in
      float_of_int got /. float_of_int (List.length examples)

let problem (f : float -> float -> float) : example list =
  List.map (fun (a, b) -> ([| a; b |], f a b)) [ (0., 0.); (0., 1.); (1., 0.); (1., 1.) ]

let and_ : example list = problem (fun a b -> if a = 1. && b = 1. then 1. else 0.)
let or_ : example list = problem (fun a b -> if a = 1. || b = 1. then 1. else 0.)
let xor : example list = problem (fun a b -> if a <> b then 1. else 0.)
