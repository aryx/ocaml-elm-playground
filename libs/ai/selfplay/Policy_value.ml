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

type board = {
  planes : int; (* the kinds of thing a square can hold, in the input *)
  height : int;
  width : int;
  channels : int; (* what each layer makes of a square *)
  layers : int; (* convolutions, one after the other *)
}

type shape =
  | Flat (* the position as so many unrelated numbers *)
  | Board of board (* the position as a board: convolutions *)

type t = {
  inputs : int;
  moves : int;
  shape : shape;
  (* Flat: "body1.w", "body1.b", "body2.w", "body2.b", "policy.w",
   * "policy.b", "value.w", "value.b".
   * Board: "conv0.w", "conv0.b", ..., then "policy.conv.w", ".b",
   * "policy.w", ".b", "value.conv.w", ".b", "value.hidden.w", ".b",
   * "value.w", ".b" *)
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

(* a layer: weights small and different, biases zero *)
let layer_of (seed : int) (name : string) (outs : int) (ins : int) : (string * Matrix.t) list =
  [ (name ^ ".w", Matrix.random ~seed ~spread:(1. /. sqrt (float_of_int ins)) outs ins); (name ^ ".b", Matrix.create 1 outs) ]

(* the policy head's and the value head's own small widths, on a board *)
let policy_channels = 2
let value_hidden = 64

let make ~(seed : int) ?(hidden = 64) ?(rate = 0.003) ?board ~(inputs : int) ~(moves : int) () : t =
  let (shape, matrices) =
    match board with
    | None ->
        ( Flat,
          layer_of seed "body1" hidden inputs @ layer_of (seed + 1) "body2" hidden hidden
          @ layer_of (seed + 2) "policy" moves hidden @ layer_of (seed + 3) "value" 1 hidden )
    | Some (b : board) ->
        let squares = b.height * b.width in
        if inputs <> b.planes * squares then invalid_arg "Policy_value.make: the board's planes and squares are not the inputs";
        ( Board b,
          List.concat
            (List.init b.layers (fun l ->
                 layer_of (seed + l) (Printf.sprintf "conv%d" l) b.channels (9 * if l = 0 then b.planes else b.channels)))
          @ layer_of (seed + 100) "policy.conv" policy_channels b.channels
          @ layer_of (seed + 101) "policy" moves (policy_channels * squares)
          @ layer_of (seed + 102) "value.conv" 1 b.channels
          @ layer_of (seed + 103) "value.hidden" value_hidden squares
          @ layer_of (seed + 104) "value" 1 value_hidden )
  in
  let n = { inputs; moves; shape; matrices; adam = Adam.make 0 } in
  { n with adam = Adam.make ~rate (parameters n) }

(* a position's numbers, a plane after the other, as a board: a row
 * per square, a column per plane *)
let squares_of (b : board) (input : float array) : Matrix.t =
  let squares = b.height * b.width in
  Matrix.init squares b.planes (fun s p -> input.((p * squares) + s))

(*****************************************************************************)
(* The network, on Tensor *)
(*****************************************************************************)

type graph = (string * Tensor.t) list

let graph_of (n : t) : graph = List.map (fun (name, x) -> (name, Tensor.value x)) n.matrices

let rows_of (arrays : float array array) : Matrix.t =
  Matrix.init (Array.length arrays) (Array.length arrays.(0)) (fun r c -> arrays.(r).(c))

(* a layer on every row of [x] *)
let layer (g : graph) (name : string) (x : Tensor.t) : Tensor.t =
  Tensor.add_row (Tensor.mul_t x (List.assoc (name ^ ".w") g)) (List.assoc (name ^ ".b") g)

(* Flat: a row per position in, and out: a row of scores per move, a
 * column of values between -1 and 1 *)
let flat_heads (g : graph) (x : Tensor.t) : Tensor.t * Tensor.t =
  let body = Tensor.relu (layer g "body2" (Tensor.relu (layer g "body1" x))) in
  (layer g "policy" body, Tensor.tanh_ (layer g "value" body))

(* Board: one position, a row per square; out, a row of scores per
 * move and a value *)
let board_heads (b : board) (g : graph) (x : Tensor.t) : Tensor.t * Tensor.t =
  let squares = b.height * b.width in
  (* the convolutions: each a layer on every square's neighbourhood.
   * From the second on, what a layer finds is *added* to what it was
   * given (a residual, as in Gpt): each learns a correction *)
  let rec body (l : int) (x : Tensor.t) : Tensor.t =
    if l = b.layers then x
    else
      let found = Tensor.relu (layer g (Printf.sprintf "conv%d" l) (Tensor.patches x ~height:b.height ~width:b.width)) in
      body (l + 1) (if l = 0 then found else Tensor.add found x)
  in
  let x = body 0 x in
  (* each head first squeezes the channels of a square into a couple
   * of numbers (a layer on each square alone), then reads the whole
   * board laid end to end *)
  let policy = Tensor.reshape (Tensor.relu (layer g "policy.conv" x)) 1 (policy_channels * squares) in
  let value = Tensor.reshape (Tensor.relu (layer g "value.conv" x)) 1 squares in
  (layer g "policy" policy, Tensor.tanh_ (layer g "value" (Tensor.relu (layer g "value.hidden" value))))

(* the two losses added: how far the policy is from the search's
 * visits, and the square of how far the value is from the result *)
let loss_of (n : t) (g : graph) (lessons : lesson array) : Tensor.t =
  let count = float_of_int (Array.length lessons) in
  match n.shape with
  | Flat ->
      let x = Tensor.value (rows_of (Array.map (fun l -> l.input) lessons)) in
      let (scores, values) = flat_heads g x in
      let policy = Tensor.cross_entropy_to scores (rows_of (Array.map (fun l -> l.policy) lessons)) in
      let off = Tensor.sub values (Tensor.value (Matrix.vector (Array.map (fun l -> l.value) lessons))) in
      Tensor.add policy (Tensor.scale (1. /. count) (Tensor.sum (Tensor.times off off)))
  | Board b ->
      (* a position at a time: a matrix is a board here, not a batch *)
      let one (l : lesson) : Tensor.t =
        let (scores, value) = board_heads b g (Tensor.value (squares_of b l.input)) in
        let off = Tensor.shift (-.l.value) value in
        Tensor.add (Tensor.cross_entropy_to scores (rows_of [| l.policy |])) (Tensor.times off off)
      in
      let losses = Array.map one lessons in
      Tensor.scale (1. /. count) (Array.fold_left Tensor.add losses.(0) (Array.sub losses 1 (Array.length losses - 1)))

(*****************************************************************************)
(* Using and training *)
(*****************************************************************************)

let softmax (scores : float array) : float array =
  let top = Array.fold_left Float.max neg_infinity scores in
  let es = Array.map (fun s -> exp (s -. top)) scores in
  let total = Array.fold_left ( +. ) 0. es in
  Array.map (fun e -> e /. total) es

(* the opinion through the graph, as [step] computes it *)
let opinion_by_graph (n : t) (input : float array) : float array * float =
  let g = graph_of n in
  let (scores, values) =
    match n.shape with
    | Flat -> flat_heads g (Tensor.value (rows_of [| input |]))
    | Board b -> board_heads b g (Tensor.value (squares_of b input))
  in
  (softmax (Tensor.of_ scores).data, Tensor.number values)

(* the same network on plain matrices, one position, no graph: what
 * the search calls a hundred times a move, where a graph's slopes
 * would be so much memory cleared for nothing. The first version went
 * through the graph, and a search of a hundred playouts of Connect 4
 * took 19 ms; this way, and the policy asked once a node (Mcts), 8
 * (Unit_selfplay checks the two give the same opinion) *)
let opinion (n : t) (input : float array) : float array * float =
  let layer (name : string) (x : Matrix.t) : Matrix.t =
    let (w : Matrix.t) = List.assoc (name ^ ".w") n.matrices and (b : Matrix.t) = List.assoc (name ^ ".b") n.matrices in
    let out = Matrix.mul_t x w in
    for i = 0 to Array.length out.data - 1 do
      out.data.(i) <- out.data.(i) +. b.data.(i mod out.cols)
    done;
    out
  in
  let relu (x : Matrix.t) : Matrix.t = Matrix.map (fun v -> if v > 0. then v else 0.) x in
  let row (x : Matrix.t) : Matrix.t = { rows = 1; cols = Array.length x.data; data = x.data } in
  match n.shape with
  | Flat ->
      let body = relu (layer "body2" (relu (layer "body1" (row (Matrix.vector input))))) in
      (softmax (layer "policy" body).data, tanh (layer "value" body).data.(0))
  | Board b ->
      let rec body (l : int) (x : Matrix.t) : Matrix.t =
        if l = b.layers then x
        else
          let found = relu (layer (Printf.sprintf "conv%d" l) (Matrix.patches x ~height:b.height ~width:b.width)) in
          body (l + 1) (if l = 0 then found else Matrix.add found x)
      in
      let x = body 0 (squares_of b input) in
      let policy = row (relu (layer "policy.conv" x)) and value = row (relu (layer "value.conv" x)) in
      (softmax (layer "policy" policy).data, tanh (layer "value" (relu (layer "value.hidden" value))).data.(0))

let loss (n : t) (lessons : lesson array) : float = Tensor.number (loss_of n (graph_of n) lessons)

let gradient (n : t) (lessons : lesson array) : float array * float =
  let g = graph_of n in
  let l = loss_of n g lessons in
  Tensor.backward l;
  (Array.concat (List.map (fun (_, x) -> (Tensor.slope x).data) g), Tensor.number l)

let apply ?rate (n : t) (slopes : float array) : t =
  let weights = Array.concat (List.map (fun (_, (x : Matrix.t)) -> x.data) n.matrices) in
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
  { n with matrices; adam }

let step ?rate (n : t) (lessons : lesson array) : t * float =
  let (slopes, l) = gradient n lessons in
  (apply ?rate n slopes, l)

(*****************************************************************************)
(* As a file *)
(*****************************************************************************)

let to_weights ?(notes = []) (n : t) : Weights.t =
  let shape =
    match n.shape with
    | Flat -> [ ("shape", "flat") ]
    | Board b -> [ ("shape", Printf.sprintf "board %d %d %d %d %d" b.planes b.height b.width b.channels b.layers) ]
  in
  { notes = shape @ notes; matrices = n.matrices }

let of_weights (w : Weights.t) : (t, string) result =
  let found names = List.for_all (fun name -> Weights.matrix w name <> None) names in
  let finish (inputs : int) (moves : int) (shape : shape) : (t, string) result =
    let n = { inputs; moves; shape; matrices = w.matrices; adam = Adam.make 0 } in
    (* an opinion asked of it now: sizes that do not chain raise here,
     * not later in a game *)
    match opinion n (Array.make inputs 0.) with
    | exception Invalid_argument _ -> Error "the matrices' sizes do not fit one another"
    | (p, _) when Array.length p <> moves -> Error "the matrices' sizes do not fit one another"
    | _ -> Ok { n with adam = Adam.make (parameters n) }
  in
  match Option.map (String.split_on_char ' ') (Weights.note w "shape") with
  | Some ("board" :: sizes) -> (
      match List.map int_of_string_opt sizes with
      | [ Some planes; Some height; Some width; Some channels; Some layers ] ->
          let b = { planes; height; width; channels; layers } in
          let names =
            List.concat (List.init layers (fun l -> [ Printf.sprintf "conv%d.w" l; Printf.sprintf "conv%d.b" l ]))
            @ [ "policy.conv.w"; "policy.conv.b"; "policy.w"; "policy.b"; "value.conv.w"; "value.conv.b";
                "value.hidden.w"; "value.hidden.b"; "value.w"; "value.b" ]
          in
          if not (found names) then Error "not a board network's weights: a matrix is missing"
          else finish (planes * height * width) (Option.get (Weights.matrix w "policy.w")).rows (Board b)
      | _ -> Error "a board network's sizes are not five numbers")
  (* a file that does not say is flat *)
  | Some [ "flat" ] | None ->
      let names = [ "body1.w"; "body1.b"; "body2.w"; "body2.b"; "policy.w"; "policy.b"; "value.w"; "value.b" ] in
      if not (found names) then Error "not a policy and value network's weights: a matrix is missing"
      else finish (Option.get (Weights.matrix w "body1.w")).cols (Option.get (Weights.matrix w "policy.w")).rows Flat
  | Some _ -> Error "a shape that is neither flat nor a board"
