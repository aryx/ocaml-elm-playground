(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Ngram_mlp.mli *)

type t = {
  context : int;
  embedding : Matrix.t; (* a row per token: where it is *)
  hidden_w : Matrix.t; (* a row per hidden neuron, over the context's coordinates *)
  hidden_b : Matrix.t;
  out_w : Matrix.t; (* a row per token, over the hidden neurons *)
  out_b : Matrix.t;
  adam : Adam.t;
}

type example = int array * int

(*****************************************************************************)
(* Making one *)
(*****************************************************************************)

let matrices (m : t) : Matrix.t list = [ m.embedding; m.hidden_w; m.hidden_b; m.out_w; m.out_b ]
let parameters (m : t) : int = List.fold_left (fun n (x : Matrix.t) -> n + Array.length x.data) 0 (matrices m)

let make ~(seed : int) ?(context = 3) ?(dim = 2) ?(hidden = 100) ?(rate = 0.01) (vocabulary : int) : t =
  let inputs = context * dim in
  let m =
    {
      context;
      embedding = Matrix.random ~seed vocabulary dim;
      (* small enough that tanh starts in its middle, where it has a
       * slope (Backprop.mli's vanishing gradient, avoided at birth) *)
      hidden_w = Matrix.random ~seed:(seed + 1) ~spread:(1. /. sqrt (float_of_int inputs)) hidden inputs;
      hidden_b = Matrix.create hidden 1;
      (* nearly zero: the first answer is "every token alike", and the
       * first loss log 27, not the price of a confident wrong guess *)
      out_w = Matrix.random ~seed:(seed + 2) ~spread:0.01 vocabulary hidden;
      out_b = Matrix.create vocabulary 1;
      adam = Adam.make 0;
    }
  in
  { m with adam = Adam.make ~rate (parameters m) }

(*****************************************************************************)
(* The examples *)
(*****************************************************************************)

let examples (t : Tokenizer.t) ~(context : int) (words : string list) : example array =
  let out = ref [] in
  List.iter
    (fun word ->
      (* the window starts full of boundaries and slides a token at a
       * time, up to the boundary that ends the word *)
      let window = ref (Array.make context Tokenizer.boundary) in
      List.iter
        (fun next ->
          out := (!window, next) :: !out;
          window := Array.append (Array.sub !window 1 (context - 1)) [| next |])
        (Tokenizer.encode t word @ [ Tokenizer.boundary ]))
    words;
  Array.of_list (List.rev !out)

let batch (state : Lehmer.state) (all : example array) (size : int) : example array =
  Array.init size (fun _ -> all.(Lehmer.int state (Array.length all)))

(*****************************************************************************)
(* Forward, on plain numbers *)
(*****************************************************************************)

let scores (m : t) (context : int array) : float array =
  let x = Array.concat (Array.to_list (Array.map (fun token -> Matrix.row m.embedding token) context)) in
  let weighted (w : Matrix.t) (b : Matrix.t) (input : float array) (r : int) : float =
    let sum = ref b.data.(r) in
    Array.iteri (fun c v -> sum := !sum +. (w.data.((r * w.cols) + c) *. v)) input;
    !sum
  in
  let h = Array.init m.hidden_w.rows (fun r -> tanh (weighted m.hidden_w m.hidden_b x r)) in
  Array.init m.out_w.rows (fun r -> weighted m.out_w m.out_b h r)

let softmax (scores : float array) : float array =
  let top = Array.fold_left Float.max neg_infinity scores in
  let es = Array.map (fun s -> exp (s -. top)) scores in
  let total = Array.fold_left ( +. ) 0. es in
  Array.map (fun e -> e /. total) es

let probabilities (m : t) (context : int array) : float array = softmax (scores m context)

let loss (m : t) (examples : example array) : float =
  let surprise = ref 0. in
  Array.iter (fun (context, next) -> surprise := !surprise -. log (probabilities m context).(next)) examples;
  !surprise /. float_of_int (Array.length examples)

let sample ?(longest = 40) ?(temperature = 1.) (state : Lehmer.state) (t : Tokenizer.t) (m : t) : string =
  let rec go (window : int array) (written : int list) : int list =
    let next = Sampling.draw state (Sampling.temper temperature (probabilities m window)) in
    if next = Tokenizer.boundary || List.length written >= longest then List.rev written
    else go (Array.append (Array.sub window 1 (m.context - 1)) [| next |]) (next :: written)
  in
  Tokenizer.decode t (go (Array.make m.context Tokenizer.boundary) [])

(*****************************************************************************)
(* One step, on a graph *)
(*****************************************************************************)

(* a neuron's weighted sum as one node ([Grad.dot]), or as the 2n
 * nodes of its products and additions: the same numbers, and the
 * switch is there to time the two (Unit_ngram_mlp) *)
let fused = ref true

let weighted (w : Grad.t array) (cols : int) (r : int) (input : Grad.t array) (b : Grad.t) : Grad.t =
  let row = Array.sub w (r * cols) cols in
  if !fused then Grad.( +: ) (Grad.dot row input) b
  else Grad.( +: ) (Grad.sum (Array.to_list (Array.map2 Grad.( *: ) row input))) b

(* the same computation as [scores], on Grad values, down to the mean
 * surprise over the batch *)
let graph (m : t) (ps : Grad.t array list) (examples : example array) : Grad.t =
  match ps with
  | [ embedding; hidden_w; hidden_b; out_w; out_b ] ->
      let dim = m.embedding.cols in
      let one ((context, next) : example) : Grad.t =
        let x = Array.concat (Array.to_list (Array.map (fun token -> Array.sub embedding (token * dim) dim) context)) in
        let h =
          Array.init m.hidden_w.rows (fun r -> Grad.tanh_ (weighted hidden_w m.hidden_w.cols r x hidden_b.(r)))
        in
        let scores = List.init m.out_w.rows (fun r -> weighted out_w m.out_w.cols r h out_b.(r)) in
        Grad.cross_entropy scores next
      in
      Grad.( /: )
        (Grad.sum (Array.to_list (Array.map one examples)))
        (Grad.value (float_of_int (Array.length examples)))
  | _ -> invalid_arg "Ngram_mlp.graph"

let step ?rate (m : t) (examples : example array) : t =
  let ps = List.map (fun (x : Matrix.t) -> Array.map Grad.value x.data) (matrices m) in
  Grad.backward (graph m ps examples);
  let weights = Array.concat (List.map (fun (x : Matrix.t) -> x.data) (matrices m)) in
  let slopes = Array.concat (List.map (Array.map Grad.slope) ps) in
  let (adam, weights) = Adam.step ?rate m.adam weights slopes in
  (* the one array cut back into the five matrices *)
  let at = ref 0 in
  let cut (x : Matrix.t) : Matrix.t =
    let n = Array.length x.data in
    let data = Array.sub weights !at n in
    at := !at + n;
    { x with data }
  in
  let embedding = cut m.embedding in
  let hidden_w = cut m.hidden_w in
  let hidden_b = cut m.hidden_b in
  let out_w = cut m.out_w in
  let out_b = cut m.out_b in
  { m with embedding; hidden_w; hidden_b; out_w; out_b; adam }
