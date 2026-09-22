(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Train.mli *)

type history = { epoch : int; training : float; held_out : float }

(* every k-th example, not a random fifth: a set sorted by class would
 * otherwise put whole classes on one side of the split *)
let split ?(part = 0.2) (examples : Backprop.example list) : Backprop.example list * Backprop.example list =
  let k = int_of_float (Float.round (1. /. Float.max 0.01 (Float.min 0.9 part))) in
  let keep i = i mod k <> 0 in
  (List.filteri (fun i _ -> keep i) examples, List.filteri (fun i _ -> not (keep i)) examples)

(* a shuffle that repeats: Fisher-Yates from a seed *)
let shuffled (seed : int) (examples : Backprop.example list) : Backprop.example array =
  let a = Array.of_list examples in
  let st = Random.State.make [| seed |] in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int st (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  a

let epoch ?(seed = 0) ?(rate = 0.5) ?(batch = 32) (net : Net.t) (examples : Backprop.example list) : Net.t =
  let a = shuffled seed examples in
  let n = Array.length a in
  let net = ref net in
  let i = ref 0 in
  while !i < n do
    let size = min batch (n - !i) in
    let b = Array.to_list (Array.sub a !i size) in
    net := Backprop.step ~rate !net (Backprop.over !net b);
    i := !i + size
  done;
  !net

let run ?(epochs = 20) ?(rate = 0.5) ?(decay = 1.) ?(batch = 32) ?(seed = 0) ?(held = []) (net : Net.t)
    (examples : Backprop.example list) : Net.t * history list =
  let rec go net rate e acc =
    if e > epochs then (net, List.rev acc)
    else
      let net = epoch ~seed:(seed + e) ~rate ~batch net examples in
      let entry =
        { epoch = e; training = Backprop.loss net examples; held_out = (if held = [] then 0. else Backprop.loss net held) }
      in
      go net (rate *. decay) (e + 1) (entry :: acc)
  in
  go net rate 1 []

let best (out : float array) : int =
  let best = ref 0 in
  Array.iteri (fun i v -> if v > out.(!best) then best := i) out;
  !best

let one_hot (n : int) (i : int) : float array = Array.init n (fun k -> if k = i then 1. else 0.)

let accuracy (net : Net.t) (examples : Backprop.example list) ~(answer : float array -> int) : float =
  match examples with
  | [] -> 1.
  | _ ->
      let right =
        List.length (List.filter (fun ((x, y) : Backprop.example) -> answer (Net.forward net x) = answer y) examples)
      in
      float_of_int right /. float_of_int (List.length examples)
