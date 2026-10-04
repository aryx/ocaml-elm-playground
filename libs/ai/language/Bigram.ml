(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Bigram.mli *)

(*****************************************************************************)
(* Counting *)
(*****************************************************************************)

let counts (t : Tokenizer.t) (words : string list) : Matrix.t =
  let n = Tokenizer.size t in
  let m = Matrix.create n n in
  List.iter
    (fun word ->
      let rec pairs (tokens : int list) =
        match tokens with
        | before :: (after :: _ as rest) ->
            Matrix.set m before after (Matrix.get m before after +. 1.);
            pairs rest
        | _ -> ()
      in
      pairs (Tokenizer.bounded t word))
    words;
  m

let probabilities ?(smoothing = 0.) (counts : Matrix.t) : Matrix.t =
  Matrix.init counts.rows counts.cols (fun r c ->
      let total = Array.fold_left ( +. ) 0. (Matrix.row counts r) +. (smoothing *. float_of_int counts.cols) in
      (* a token nothing ever followed: no opinion, every token alike *)
      if total = 0. then 1. /. float_of_int counts.cols else (Matrix.get counts r c +. smoothing) /. total)

(*****************************************************************************)
(* How good *)
(*****************************************************************************)

let loss (p : Matrix.t) (counts : Matrix.t) : float =
  let total = Matrix.sum counts in
  let surprise = ref 0. in
  Array.iteri (fun i n -> if n > 0. then surprise := !surprise -. (n *. log p.data.(i))) counts.data;
  !surprise /. total

let bits (nats : float) : float = nats /. log 2.

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

let sample ?(longest = 40) (state : Lehmer.state) (t : Tokenizer.t) (p : Matrix.t) : string =
  let rec go (before : int) (written : int list) : int list =
    let next = Sampling.draw state (Matrix.row p before) in
    if next = Tokenizer.boundary || List.length written >= longest then List.rev written else go next (next :: written)
  in
  Tokenizer.decode t (go Tokenizer.boundary [])

(*****************************************************************************)
(* Learning the same table *)
(*****************************************************************************)

type learned = {
  scores : Matrix.t; (* a row per token before, a score per token after *)
  adam : Adam.t;
}

(* all scores zero: every token as likely as any other after any
 * token, which is knowing nothing *)
let start ?(rate = 0.5) (size : int) : learned =
  { scores = Matrix.create size size; adam = Adam.make ~rate (size * size) }

let learned_probabilities (l : learned) : Matrix.t =
  let p = Matrix.create l.scores.rows l.scores.cols in
  for r = 0 to l.scores.rows - 1 do
    let row = Matrix.row l.scores r in
    let top = Array.fold_left Float.max neg_infinity row in
    let es = Array.map (fun s -> exp (s -. top)) row in
    let total = Array.fold_left ( +. ) 0. es in
    Array.iteri (fun c e -> Matrix.set p r c (e /. total)) es
  done;
  p

(* the loss as a graph over the scores: per token before, the softmax
 * of its row, and each pair's surprise weighted by how often the text
 * has it. The text enters only through the counts -- 228,146 pairs
 * are 729 numbers -- so the graph is a few thousand nodes whatever
 * the text's length *)
let graph (counts : Matrix.t) (scores : Grad.t array) : Grad.t =
  let n = counts.rows in
  let total = Matrix.sum counts in
  let terms = ref [] in
  for before = 0 to n - 1 do
    if Array.exists (fun c -> c > 0.) (Matrix.row counts before) then
      let p = Grad.softmax (List.init n (fun after -> scores.((before * n) + after))) in
      List.iteri
        (fun after p ->
          let seen = Matrix.get counts before after in
          if seen > 0. then terms := Grad.( *: ) (Grad.value (seen /. total)) (Grad.neg (Grad.log_ p)) :: !terms)
        p
  done;
  Grad.sum !terms

let step (counts : Matrix.t) (l : learned) : learned =
  let scores = Array.map Grad.value l.scores.data in
  Grad.backward (graph counts scores);
  let (adam, data) = Adam.step l.adam l.scores.data (Array.map Grad.slope scores) in
  { scores = { l.scores with data }; adam }
