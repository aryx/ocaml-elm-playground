(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Policy_value.mli *)

type t = {
  inputs : int;
  moves : int;
  (* "body1.w", "body1.b", "body2.w", "body2.b", "policy.w",
   * "policy.b", "value.w", "value.b" *)
  matrices : (string * Matrix.t) list;
  adam : Adam.t;
}

type lesson = {
  input : float array; (* the position, as the network reads it *)
  policy : float array; (* the share of the search's visits each move got *)
  value : float; (* how it ended for whoever was to play: 1, 0 or -1 *)
}

(*****************************************************************************)
(* Making one *)
(*****************************************************************************)

let parameters (n : t) : int = List.fold_left (fun k (_, (x : Matrix.t)) -> k + Array.length x.data) 0 n.matrices

let make ~(seed : int) ?(hidden = 64) ?(rate = 0.003) ~(inputs : int) ~(moves : int) () : t =
  (* a layer: weights small and different, biases zero *)
  let layer (k : int) (name : string) (outs : int) (ins : int) : (string * Matrix.t) list =
    [ (name ^ ".w", Matrix.random ~seed:(seed + k) ~spread:(1. /. sqrt (float_of_int ins)) outs ins);
      (name ^ ".b", Matrix.create 1 outs) ]
  in
  let matrices =
    layer 0 "body1" hidden inputs @ layer 1 "body2" hidden hidden @ layer 2 "policy" moves hidden
    @ layer 3 "value" 1 hidden
  in
  let n = { inputs; moves; matrices; adam = Adam.make 0 } in
  { n with adam = Adam.make ~rate (parameters n) }

(*****************************************************************************)
(* The network, on Tensor *)
(*****************************************************************************)

type graph = (string * Tensor.t) list

let graph_of (n : t) : graph = List.map (fun (name, x) -> (name, Tensor.value x)) n.matrices

(* a row per position in, and out: a row of scores per move, a column
 * of values between -1 and 1 *)
let heads (g : graph) (x : Tensor.t) : Tensor.t * Tensor.t =
  let layer name x = Tensor.add_row (Tensor.mul_t x (List.assoc (name ^ ".w") g)) (List.assoc (name ^ ".b") g) in
  let body = Tensor.relu (layer "body2" (Tensor.relu (layer "body1" x))) in
  (layer "policy" body, Tensor.tanh_ (layer "value" body))

let rows_of (arrays : float array array) : Matrix.t =
  Matrix.init (Array.length arrays) (Array.length arrays.(0)) (fun r c -> arrays.(r).(c))

(* the two losses added: how far the policy is from the search's
 * visits, and the square of how far the value is from the result *)
let loss_of (g : graph) (lessons : lesson array) : Tensor.t =
  let x = Tensor.value (rows_of (Array.map (fun l -> l.input) lessons)) in
  let (scores, values) = heads g x in
  let policy = Tensor.cross_entropy_to scores (rows_of (Array.map (fun l -> l.policy) lessons)) in
  let off = Tensor.sub values (Tensor.value (Matrix.vector (Array.map (fun l -> l.value) lessons))) in
  let value = Tensor.scale (1. /. float_of_int (Array.length lessons)) (Tensor.sum (Tensor.times off off)) in
  Tensor.add policy value

(*****************************************************************************)
(* Using and training *)
(*****************************************************************************)

let softmax (scores : float array) : float array =
  let top = Array.fold_left Float.max neg_infinity scores in
  let es = Array.map (fun s -> exp (s -. top)) scores in
  let total = Array.fold_left ( +. ) 0. es in
  Array.map (fun e -> e /. total) es

(* the same network on plain numbers, one position, no graph: what the
 * search calls a hundred times a move, where a graph's slopes would
 * be so much memory cleared for nothing. The first version went
 * through [heads], and a search of a hundred playouts of Connect 4
 * took 19 ms; this way, and the policy asked once a node (Mcts), 8 (Unit_selfplay checks the two give the same opinion) *)
let opinion (n : t) (input : float array) : float array * float =
  let layer (name : string) (x : float array) : float array =
    let (w : Matrix.t) = List.assoc (name ^ ".w") n.matrices and (b : Matrix.t) = List.assoc (name ^ ".b") n.matrices in
    (* the input as a matrix of one row, against each row of weights *)
    let out = Matrix.mul_t { rows = 1; cols = Array.length x; data = x } w in
    Array.mapi (fun r v -> v +. b.data.(r)) out.data
  in
  let relu = Array.map (fun v -> if v > 0. then v else 0.) in
  let body = relu (layer "body2" (relu (layer "body1" input))) in
  (softmax (layer "policy" body), tanh (layer "value" body).(0))

(* the opinion through the graph, as [step] computes it *)
let opinion_by_graph (n : t) (input : float array) : float array * float =
  let (scores, values) = heads (graph_of n) (Tensor.value (rows_of [| input |])) in
  (softmax (Tensor.of_ scores).data, Tensor.number values)

let loss (n : t) (lessons : lesson array) : float = Tensor.number (loss_of (graph_of n) lessons)

let step ?rate (n : t) (lessons : lesson array) : t * float =
  let g = graph_of n in
  let l = loss_of g lessons in
  Tensor.backward l;
  let weights = Array.concat (List.map (fun (_, (x : Matrix.t)) -> x.data) n.matrices) in
  let slopes = Array.concat (List.map (fun (_, x) -> (Tensor.slope x).data) g) in
  let (adam, weights) = Adam.step ?rate n.adam weights slopes in
  let at = ref 0 in
  let matrices =
    List.map
      (fun (name, (x : Matrix.t)) ->
        let k = Array.length x.data in
        let data = Array.sub weights !at k in
        at := !at + k;
        (name, { x with data }))
      n.matrices
  in
  ({ n with matrices; adam }, Tensor.number l)

(*****************************************************************************)
(* As a file *)
(*****************************************************************************)

let to_weights ?(notes = []) (n : t) : Weights.t = { notes; matrices = n.matrices }

let of_weights (w : Weights.t) : (t, string) result =
  let names = [ "body1.w"; "body1.b"; "body2.w"; "body2.b"; "policy.w"; "policy.b"; "value.w"; "value.b" ] in
  match List.map (Weights.matrix w) names with
  | [ Some b1; Some b1b; Some b2; Some b2b; Some p; Some pb; Some v; Some vb ] ->
      if b2.cols <> b1.rows || p.cols <> b2.rows || v.cols <> b2.rows || v.rows <> 1 || b1b.cols <> b1.rows
         || b2b.cols <> b2.rows || pb.cols <> p.rows || vb.cols <> 1
      then Error "the matrices' sizes do not fit one another"
      else
        let matrices = List.combine names [ b1; b1b; b2; b2b; p; pb; v; vb ] in
        let n = { inputs = b1.cols; moves = p.rows; matrices; adam = Adam.make 0 } in
        Ok { n with adam = Adam.make (parameters n) }
  | _ -> Error "not a policy and value network's weights: a matrix is missing"
