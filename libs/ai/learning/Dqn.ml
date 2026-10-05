(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Dqn.mli *)

type screen = {
  width : int;
  height : int;
  frames : int; (* the last ones, stacked: a column each *)
  first : int * int * int; (* the first convolution: window, stride, channels *)
  second : int * int * int;
  hidden : int;
}

type shape =
  | Numbers of int (* two layers of so many neurons *)
  | Screen of screen

type t = {
  inputs : int;
  actions : int;
  shape : shape;
  (* Numbers: "body1", "body2", "out"; Screen: "conv1", "conv2",
   * "hidden", "out"; each a ".w" and a ".b" *)
  matrices : (string * Matrix.t) list;
  adam : Adam.t;
}

(*****************************************************************************)
(* Making one *)
(*****************************************************************************)

let parameters (n : t) : int = List.fold_left (fun k (_, (x : Matrix.t)) -> k + Array.length x.data) 0 n.matrices

let layer_of (seed : int) (name : string) (outs : int) (ins : int) : (string * Matrix.t) list =
  [ (name ^ ".w", Matrix.random ~seed ~spread:(1. /. sqrt (float_of_int ins)) outs ins); (name ^ ".b", Matrix.create 1 outs) ]

(* how many windows fit along a side *)
let fit (side : int) ((size, stride, _) : int * int * int) : int = ((side - size) / stride) + 1

let make ~(seed : int) ?(rate = 0.001) ?(shape = Numbers 64) ~(inputs : int) ~(actions : int) () : t =
  let matrices =
    match shape with
    | Numbers hidden ->
        layer_of seed "body1" hidden inputs @ layer_of (seed + 1) "body2" hidden hidden
        @ layer_of (seed + 2) "out" actions hidden
    | Screen s ->
        if inputs <> s.width * s.height * s.frames then invalid_arg "Dqn.make: the screen's pixels and frames are not the inputs";
        let (size1, _, channels1) = s.first and (size2, _, channels2) = s.second in
        let seen = fit (fit s.width s.first) s.second * fit (fit s.height s.first) s.second in
        layer_of seed "conv1" channels1 (size1 * size1 * s.frames)
        @ layer_of (seed + 1) "conv2" channels2 (size2 * size2 * channels1)
        @ layer_of (seed + 2) "hidden" s.hidden (seen * channels2)
        @ layer_of (seed + 3) "out" actions s.hidden
  in
  let n = { inputs; actions; shape; matrices; adam = Adam.make 0 } in
  { n with adam = Adam.make ~rate (parameters n) }

(* the frames, one after the other in the input, as a picture: a row
 * per pixel, a column per frame *)
let picture_of (s : screen) (input : float array) : Matrix.t =
  let pixels = s.width * s.height in
  Matrix.init pixels s.frames (fun p f -> input.((f * pixels) + p))

(*****************************************************************************)
(* The network, on Tensor *)
(*****************************************************************************)

type graph = (string * Tensor.t) list

let graph_of (n : t) : graph = List.map (fun (name, x) -> (name, Tensor.value x)) n.matrices

let layer (g : graph) (name : string) (x : Tensor.t) : Tensor.t =
  Tensor.add_row (Tensor.mul_t x (List.assoc (name ^ ".w") g)) (List.assoc (name ^ ".b") g)

(* a row of inputs per state in, a row of values per action out *)
let numbers (g : graph) (x : Tensor.t) : Tensor.t =
  layer g "out" (Tensor.relu (layer g "body2" (Tensor.relu (layer g "body1" x))))

(* one state, a row per pixel: two convolutions that step, each
 * shrinking the picture, then every number left read at once *)
let screen (s : screen) (g : graph) (x : Tensor.t) : Tensor.t =
  let (size1, stride1, _) = s.first and (size2, stride2, _) = s.second in
  let x = Tensor.relu (layer g "conv1" (Tensor.windows x ~height:s.height ~width:s.width ~size:size1 ~stride:stride1)) in
  let (h, w) = (fit s.height s.first, fit s.width s.first) in
  let x = Tensor.relu (layer g "conv2" (Tensor.windows x ~height:h ~width:w ~size:size2 ~stride:stride2)) in
  let all = Array.length (Tensor.of_ x).data in
  layer g "out" (Tensor.relu (layer g "hidden" (Tensor.reshape x 1 all)))

(*****************************************************************************)
(* Using it *)
(*****************************************************************************)

(* the same on plain matrices, no graph: what playing asks, every
 * frame *)
let values (n : t) (input : float array) : float array =
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
  | Numbers _ -> (layer "out" (relu (layer "body2" (relu (layer "body1" (row (Matrix.vector input))))))).data
  | Screen s ->
      let (size1, stride1, _) = s.first and (size2, stride2, _) = s.second in
      let x = relu (layer "conv1" (Matrix.windows (picture_of s input) ~height:s.height ~width:s.width ~size:size1 ~stride:stride1)) in
      let x = relu (layer "conv2" (Matrix.windows x ~height:(fit s.height s.first) ~width:(fit s.width s.first) ~size:size2 ~stride:stride2)) in
      (layer "out" (relu (layer "hidden" (row x)))).data

let values_by_graph (n : t) (input : float array) : float array =
  let g = graph_of n in
  let out =
    match n.shape with
    | Numbers _ -> numbers g (Tensor.value { rows = 1; cols = Array.length input; data = input })
    | Screen s -> screen s g (Tensor.value (picture_of s input))
  in
  (Tensor.of_ out).data

let best (n : t) (input : float array) : int =
  let q = values n input in
  let at = ref 0 in
  Array.iteri (fun i v -> if v > q.(!at) then at := i) q;
  !at

(*****************************************************************************)
(* Learning *)
(*****************************************************************************)

type lived = {
  state : float array;
  action : int;
  reward : float;
  next : float array option; (* None: it ended there *)
}

(* what each step lived should have been worth: the reward, and what
 * the best action after it is worth according to [target] *)
let worth ~(discount : float) (target : t) (l : lived) : float =
  match l.next with
  | None -> l.reward
  | Some next -> l.reward +. (discount *. Array.fold_left Float.max neg_infinity (values target next))

(* the loss: the square of how far the value given to the action
 * taken is from what it should have been, averaged. The other
 * actions' values are not judged: nothing was learned about them *)
let loss_of ~(discount : float) ~(target : t) (n : t) (g : graph) (lived : lived array) : Tensor.t =
  let count = Array.length lived in
  (* what each step should have been worth, asked of the target once *)
  let due = Array.map (worth ~discount target) lived in
  (* a row per step: 1 at the action taken, and its worth there. The
   * value given, times the first, less the second, is how far off the
   * action taken was, and nothing for the others *)
  let taken (r : int) = Matrix.init 1 n.actions (fun _ a -> if a = lived.(r).action then 1. else 0.) in
  let worth_there (r : int) = Matrix.init 1 n.actions (fun _ a -> if a = lived.(r).action then due.(r) else 0.) in
  let square (off : Tensor.t) : Tensor.t = Tensor.sum (Tensor.times off off) in
  let total =
    match n.shape with
    | Numbers _ ->
        let x = Tensor.value (Matrix.init count n.inputs (fun r c -> lived.(r).state.(c))) in
        let all f = Matrix.init count n.actions (fun r a -> (f r : Matrix.t).data.(a)) in
        let rows_taken = Array.init count taken and rows_worth = Array.init count worth_there in
        square
          (Tensor.sub
             (Tensor.times (numbers g x) (Tensor.value (all (fun r -> rows_taken.(r)))))
             (Tensor.value (all (fun r -> rows_worth.(r)))))
    | Screen s ->
        let one (r : int) : Tensor.t =
          let q = screen s g (Tensor.value (picture_of s lived.(r).state)) in
          square (Tensor.sub (Tensor.times q (Tensor.value (taken r))) (Tensor.value (worth_there r)))
        in
        let losses = Array.init count one in
        Array.fold_left Tensor.add losses.(0) (Array.sub losses 1 (count - 1))
  in
  Tensor.scale (1. /. float_of_int count) total

let loss ?(discount = 0.99) ~(target : t) (n : t) (lived : lived array) : float =
  Tensor.number (loss_of ~discount ~target n (graph_of n) lived)

let gradient ?(discount = 0.99) ~(target : t) (n : t) (lived : lived array) : float array * float =
  let g = graph_of n in
  let l = loss_of ~discount ~target n g lived in
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

let step ?discount ?rate ~(target : t) (n : t) (lived : lived array) : t * float =
  let (slopes, l) = gradient ?discount ~target n lived in
  (apply ?rate n slopes, l)

(*****************************************************************************)
(* What it has lived *)
(*****************************************************************************)

type memory = {
  kept : lived option array; (* a ring: the oldest is written over *)
  mutable next : int;
  mutable count : int;
}

let memory (size : int) : memory = { kept = Array.make size None; next = 0; count = 0 }
let remembered (m : memory) : int = m.count

let remember (m : memory) (l : lived) : unit =
  m.kept.(m.next) <- Some l;
  m.next <- (m.next + 1) mod Array.length m.kept;
  m.count <- min (m.count + 1) (Array.length m.kept)

let recall (state : Lehmer.state) (m : memory) (n : int) : lived array =
  Array.init n (fun _ -> Option.get m.kept.(Lehmer.int state m.count))

(*****************************************************************************)
(* As a file *)
(*****************************************************************************)

let to_weights ?(notes = []) (n : t) : Weights.t =
  let shape =
    match n.shape with
    | Numbers hidden -> Printf.sprintf "numbers %d" hidden
    | Screen s ->
        let (a, b, c) = s.first and (d, e, f) = s.second in
        Printf.sprintf "screen %d %d %d %d %d %d %d %d %d %d" s.width s.height s.frames a b c d e f s.hidden
  in
  { notes = [ ("shape", shape); ("actions", string_of_int n.actions) ] @ notes; matrices = n.matrices }

let of_weights (w : Weights.t) : (t, string) result =
  let finish (inputs : int) (shape : shape) : (t, string) result =
    match Option.bind (Weights.note w "actions") int_of_string_opt with
    | None -> Error "no note saying how many actions"
    | Some actions -> (
        let n = { inputs; actions; shape; matrices = w.matrices; adam = Adam.make 0 } in
        (* asked once now: sizes that do not chain, or a matrix that is
         * not there, fail here and not in a game *)
        match values n (Array.make inputs 0.) with
        | exception (Invalid_argument _ | Not_found) -> Error "the matrices are not those its notes describe"
        | q when Array.length q <> actions -> Error "the matrices are not those its notes describe"
        | _ -> Ok { n with adam = Adam.make (parameters n) })
  in
  match Option.map (String.split_on_char ' ') (Weights.note w "shape") with
  | Some [ "numbers"; hidden ] -> (
      match (int_of_string_opt hidden, Weights.matrix w "body1.w") with
      | (Some hidden, Some body1) -> finish body1.cols (Numbers hidden)
      | _ -> Error "not a Dqn's weights")
  | Some ("screen" :: sizes) -> (
      match List.map int_of_string_opt sizes with
      | [ Some width; Some height; Some frames; Some a; Some b; Some c; Some d; Some e; Some f; Some hidden ] ->
          finish (width * height * frames) (Screen { width; height; frames; first = (a, b, c); second = (d, e, f); hidden })
      | _ -> Error "a screen's sizes are not ten numbers")
  | _ -> Error "not a Dqn's weights: no note saying its shape"
