(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Gpt.mli *)

type config = {
  vocabulary : int;
  width : int; (* the numbers a token is, all the way through *)
  heads : int;
  layers : int;
  block : int; (* the longest text it reads *)
  positions : bool; (* false: it is not told where a token stands *)
  attention : bool; (* false: no token looks at another *)
}

let config ?(width = 16) ?(heads = 4) ?(layers = 1) ?(block = 16) ?(positions = true) ?(attention = true)
    (vocabulary : int) : config =
  { vocabulary; width; heads; layers; block; positions; attention }

type t = {
  config : config;
  (* every matrix by its name: "wte", "wpe", "head", and per layer
   * "0.q", "0.k", "0.v", "0.o", "0.fc1", "0.fc2" *)
  matrices : (string * Matrix.t) list;
  adam : Adam.t;
}

(*****************************************************************************)
(* Making one *)
(*****************************************************************************)

let shapes (c : config) : (string * int * int) list =
  [ ("wte", c.vocabulary, c.width); ("wpe", c.block, c.width); ("head", c.vocabulary, c.width) ]
  @ List.concat
      (List.init c.layers (fun l ->
           let name s = Printf.sprintf "%d.%s" l s in
           [ (name "q", c.width, c.width); (name "k", c.width, c.width); (name "v", c.width, c.width);
             (name "o", c.width, c.width); (name "fc1", 4 * c.width, c.width); (name "fc2", c.width, 4 * c.width) ]))

let parameters (m : t) : int = List.fold_left (fun n (_, (x : Matrix.t)) -> n + Array.length x.data) 0 m.matrices

(* a number from a bell curve, by Box and Muller's two uniform ones *)
let gauss (state : Lehmer.state) : float =
  let u = 1. -. Lehmer.float state 1. and v = Lehmer.float state 1. in
  sqrt (-2. *. log u) *. cos (2. *. Float.pi *. v)

let make ~(seed : int) ?(rate = 0.01) (c : config) : t =
  if c.width mod c.heads <> 0 then invalid_arg "Gpt.make: the width is not a multiple of the heads";
  let state = Lehmer.make seed in
  let matrices =
    List.map (fun (name, rows, cols) -> (name, Matrix.init rows cols (fun _ _ -> 0.08 *. gauss state))) (shapes c)
  in
  let m = { config = c; matrices; adam = Adam.make 0 } in
  (* microgpt's Adam: shorter memories than the paper's *)
  { m with adam = Adam.make ~rate ~b1:0.85 ~b2:0.99 (parameters m) }

(*****************************************************************************)
(* The model, on Grad values *)
(*****************************************************************************)

(* a matrix as its rows, each an array of graph values *)
type rows = Grad.t array array

let rows_of (x : Matrix.t) : rows = Array.init x.rows (fun r -> Array.init x.cols (fun c -> Grad.value x.data.((r * x.cols) + c)))

(* a layer without bias: each output a row's sum of products, one
 * node each ([Grad.dot]) *)
let linear (w : rows) (x : Grad.t array) : Grad.t array = Array.map (fun row -> Grad.dot row x) w
let add (a : Grad.t array) (b : Grad.t array) : Grad.t array = Array.map2 Grad.( +: ) a b

(* the vector brought to length sqrt n, whatever it was: divided by
 * the root of the mean of its squares *)
let rmsnorm (x : Grad.t array) : Grad.t array =
  let mean = Grad.( /: ) (Grad.dot x x) (Grad.value (float_of_int (Array.length x))) in
  let scale = Grad.pow (Grad.( +: ) mean (Grad.value 1e-5)) (-0.5) in
  Array.map (fun v -> Grad.( *: ) v scale) x

(* what a layer has kept of the tokens read so far: each one's key
 * and value, the newest first *)
type memory = { keys : Grad.t array list; values : Grad.t array list }

(* one head, for the token being read: its query against every key so
 * far, the shares that gives, and the values mixed in those shares.
 * Also the shares as plain numbers, oldest token first, for whoever
 * draws them *)
let head (c : config) (h : int) (q : Grad.t array) (mem : memory) : Grad.t array * float array =
  let size = c.width / c.heads in
  let part (x : Grad.t array) = Array.sub x (h * size) size in
  let q = part q in
  let scores =
    List.map (fun k -> Grad.( /: ) (Grad.dot q (part k)) (Grad.value (sqrt (float_of_int size)))) mem.keys
  in
  let shares = Array.of_list (Grad.softmax scores) in
  let values = Array.of_list (List.map part mem.values) in
  let mixed = Array.init size (fun d -> Grad.dot shares (Array.map (fun v -> v.(d)) values)) in
  (mixed, Array.of_list (List.rev_map Grad.of_ (Array.to_list shares)))

(* the model's matrices as graph values, found by name *)
type graph = (string * rows) list

let graph_of (m : t) : graph = List.map (fun (name, x) -> (name, rows_of x)) m.matrices

(* one token read at one position: the scores for what comes next,
 * each layer's memory with this token added, and each head's shares
 * (layer by layer, head by head) *)
let read (c : config) (g : graph) (token : int) (position : int) (memories : memory list) :
    Grad.t array * memory list * float array list =
  let w name = List.assoc name g in
  let x = (w "wte").(token) in
  let x = if c.positions then add x (w "wpe").(position) else x in
  let x = ref (rmsnorm x) and looked = ref [] in
  let memories =
    List.mapi
      (fun l (mem : memory) ->
        let name s = Printf.sprintf "%d.%s" l s in
        (* attention: this token asks, every token so far answers *)
        let before = !x in
        let normed = rmsnorm before in
        let mem = { keys = linear (w (name "k")) normed :: mem.keys; values = linear (w (name "v")) normed :: mem.values } in
        (if c.attention then
           let q = linear (w (name "q")) normed in
           let heads = List.init c.heads (fun h -> head c h q mem) in
           looked := !looked @ List.map snd heads;
           x := add (linear (w (name "o")) (Array.concat (List.map fst heads))) before);
        (* then each token thinks by itself: a layer four times as
           wide, and back *)
        let before = !x in
        let hidden = Array.map Grad.relu (linear (w (name "fc1")) (rmsnorm before)) in
        x := add (linear (w (name "fc2")) hidden) before;
        mem)
      memories
  in
  (linear (w "head") !x, memories, !looked)

let empty (c : config) : memory list = List.init c.layers (fun _ -> { keys = []; values = [] })

(* a text's loss as a graph: each token read in turn, its surprise at
 * the token that follows, averaged. At most [block] tokens are read *)
let text_loss (c : config) (g : graph) (tokens : int list) : Grad.t =
  let rec go (tokens : int list) (position : int) (memories : memory list) (losses : Grad.t list) : Grad.t list =
    match tokens with
    | token :: (next :: _ as rest) when position < c.block ->
        let (scores, memories, _) = read c g token position memories in
        go rest (position + 1) memories (Grad.cross_entropy (Array.to_list scores) next :: losses)
    | _ -> losses
  in
  let losses = go tokens 0 (empty c) [] in
  Grad.( /: ) (Grad.sum losses) (Grad.value (float_of_int (max 1 (List.length losses))))

(*****************************************************************************)
(* The same model, on whole arrays *)
(*****************************************************************************)
(* [read] above takes one token and keeps a memory of those before.
 * Here the whole text goes through at once, a row per token: the
 * memory is the rows above, and "only those before" is the softmax
 * stopping at the diagonal ([Tensor.softmax_rows ~causal]). The same
 * numbers come out (Unit_gpt checks the loss and every slope), from
 * about forty nodes instead of thirty thousand. *)

type arrays = (string * Tensor.t) list

let arrays_of (m : t) : arrays = List.map (fun (name, x) -> (name, Tensor.value x)) m.matrices

let text_loss_arrays (c : config) (g : arrays) (tokens : int list) : Tensor.t =
  let w name = List.assoc name g in
  (* [linear] on every row at once: X W^T *)
  let linear name x = Tensor.mul x (Tensor.transpose (w name)) in
  let rmsnorm x = Tensor.scale_rows x (Tensor.pow (Tensor.shift 1e-5 (Tensor.row_mean (Tensor.times x x))) (-0.5)) in
  let tokens = Array.of_list tokens in
  let n = min c.block (Array.length tokens - 1) in
  if n <= 0 then Tensor.value (Matrix.create 1 1)
  else
    (* each token but the last is read; what follows it is its answer *)
    let read = Array.sub tokens 0 n and answers = Array.sub tokens 1 n in
    let x = Tensor.rows (w "wte") read in
    let x = if c.positions then Tensor.add x (Tensor.rows (w "wpe") (Array.init n (fun i -> i))) else x in
    let x = ref (rmsnorm x) in
    for l = 0 to c.layers - 1 do
      let name s = Printf.sprintf "%d.%s" l s in
      (if c.attention then
         let before = !x in
         let normed = rmsnorm before in
         let q = linear (name "q") normed and k = linear (name "k") normed and v = linear (name "v") normed in
         let size = c.width / c.heads in
         let heads =
           List.init c.heads (fun h ->
               let part m = Tensor.cols m (h * size) size in
               (* every query against every key: a square of scores,
                  of which each row keeps its part up to the diagonal *)
               let scores = Tensor.scale (1. /. sqrt (float_of_int size)) (Tensor.mul (part q) (Tensor.transpose (part k))) in
               Tensor.mul (Tensor.softmax_rows ~causal:true scores) (part v))
         in
         x := Tensor.add (linear (name "o") (Tensor.join_cols heads)) before);
      let before = !x in
      x := Tensor.add (linear (name "fc2") (Tensor.relu (linear (name "fc1") (rmsnorm before)))) before
    done;
    Tensor.cross_entropy (linear "head" !x) answers

(*****************************************************************************)
(* Using and training *)
(*****************************************************************************)

(* on whole arrays (the default), or a node per number: the same
 * losses and the same slopes, to time one against the other *)
let on_arrays = ref true

let loss (m : t) (texts : int list list) : float =
  let one =
    if !on_arrays then
      let g = arrays_of m in
      fun tokens -> Tensor.number (text_loss_arrays m.config g tokens)
    else
      let g = graph_of m in
      fun tokens -> Grad.of_ (text_loss m.config g tokens)
  in
  List.fold_left (fun sum tokens -> sum +. one tokens) 0. texts /. float_of_int (max 1 (List.length texts))

let gradient (m : t) (tokens : int list) : float array * float =
  if !on_arrays then (
    let g = arrays_of m in
    let l = text_loss_arrays m.config g tokens in
    Tensor.backward l;
    (Array.concat (List.map (fun (_, x) -> Array.copy (Tensor.slope x).data) g), Tensor.number l))
  else
    let g = graph_of m in
    let l = text_loss m.config g tokens in
    Grad.backward l;
    (Array.concat (List.map (fun (_, rows) -> Array.map Grad.slope (Array.concat (Array.to_list rows))) g), Grad.of_ l)

let step ?rate (m : t) (tokens : int list) : t * float =
  let (slopes, l) = gradient m tokens in
  let weights = Array.concat (List.map (fun (_, (x : Matrix.t)) -> x.data) m.matrices) in
  let (adam, weights) = Adam.step ?rate m.adam weights slopes in
  (* the one array cut back into the matrices *)
  let at = ref 0 in
  let matrices =
    List.map
      (fun (name, (x : Matrix.t)) ->
        let n = Array.length x.data in
        let data = Array.sub weights !at n in
        at := !at + n;
        (name, { x with data }))
      m.matrices
  in
  ({ m with matrices; adam }, l)

(* the tokens read one after the other: the last one's scores as
 * probabilities, and what each head looked at when reading it *)
let next (m : t) (tokens : int list) : float array * float array list =
  let g = graph_of m in
  let rec go (tokens : int list) (position : int) (memories : memory list) =
    match tokens with
    | [] -> invalid_arg "Gpt.next: no token"
    | token :: rest ->
        let (scores, memories, looked) = read m.config g token position memories in
        if rest = [] || position + 1 >= m.config.block then
          (Array.of_list (List.map Grad.of_ (Grad.softmax (Array.to_list scores))), looked)
        else go rest (position + 1) memories
  in
  go tokens 0 (empty m.config)

let sample ?(temperature = 1.) (state : Lehmer.state) (t : Tokenizer.t) (m : t) : string =
  let rec go (tokens : int list) : int list =
    if List.length tokens >= m.config.block then tokens
    else
      let (p, _) = next m (List.rev tokens) in
      let token = Sampling.draw state (Sampling.temper temperature p) in
      if token = Tokenizer.boundary then tokens else go (token :: tokens)
  in
  (* the boundary it starts from is not part of the word *)
  Tokenizer.decode t (List.tl (List.rev (go [ Tokenizer.boundary ])))

(*****************************************************************************)
(* As a file *)
(*****************************************************************************)

let to_weights ?(notes = []) (m : t) : Weights.t =
  let c = m.config in
  {
    notes =
      [ ("width", string_of_int c.width); ("heads", string_of_int c.heads); ("layers", string_of_int c.layers);
        ("block", string_of_int c.block); ("positions", string_of_bool c.positions);
        ("attention", string_of_bool c.attention) ]
      @ notes;
    matrices = m.matrices;
  }

let of_weights (w : Weights.t) : (t, string) result =
  let number name = Option.bind (Weights.note w name) int_of_string_opt in
  let flag name = Option.bind (Weights.note w name) bool_of_string_opt in
  match (number "width", number "heads", number "layers", number "block", flag "positions", flag "attention", Weights.matrix w "wte") with
  | (Some width, Some heads, Some layers, Some block, Some positions, Some attention, Some wte) ->
      let c = { vocabulary = wte.rows; width; heads; layers; block; positions; attention } in
      (* every matrix the shape says, of the size it says *)
      let fits =
        List.for_all
          (fun (name, rows, cols) ->
            match Weights.matrix w name with Some (x : Matrix.t) -> x.rows = rows && x.cols = cols | None -> false)
          (shapes c)
      in
      if not fits || width mod heads <> 0 then Error "the matrices are not those its notes describe"
      else
        let matrices = List.map (fun (name, _, _) -> (name, Option.get (Weights.matrix w name))) (shapes c) in
        let m = { config = c; matrices; adam = Adam.make 0 } in
        Ok { m with adam = Adam.make ~b1:0.85 ~b2:0.99 (parameters m) }
  | _ -> Error "not a Gpt's weights: a note or the matrix wte is missing"
