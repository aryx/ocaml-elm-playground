(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Grad.mli *)

(* a node: its value, the slope of the final answer with respect to
 * it, what it was made from, and how to send a slope back to those --
 * which is the only thing an operation has to know about itself *)
type t = {
  mutable v : float;
  mutable d : float; (* the slope, filled in by [backward] *)
  from : t list;
  send_back : t -> unit; (* takes the node, pushes its d onto [from] *)
  mutable walk : int; (* the last walk that came through, see [order] *)
}

let nothing (_ : t) : unit = ()
let value (v : float) : t = { v; d = 0.; from = []; send_back = nothing; walk = 0 }
let of_ (n : t) : float = n.v
let slope (n : t) : float = n.d
let set (n : t) (v : float) : unit = n.v <- v

let make (v : float) (from : t list) (send_back : t -> unit) : t = { v; d = 0.; from; send_back; walk = 0 }

(* every operation is its value and where its slope goes. The sums are
 * the chain rule: a node's slope is added to each input's, multiplied
 * by that input's local derivative *)
let ( +: ) (a : t) (b : t) : t =
  make (a.v +. b.v) [ a; b ] (fun n ->
      a.d <- a.d +. n.d;
      b.d <- b.d +. n.d)

let ( -: ) (a : t) (b : t) : t =
  make (a.v -. b.v) [ a; b ] (fun n ->
      a.d <- a.d +. n.d;
      b.d <- b.d -. n.d)

let ( *: ) (a : t) (b : t) : t =
  make (a.v *. b.v) [ a; b ] (fun n ->
      (* each input's slope is the other input: d(ab)/da = b *)
      a.d <- a.d +. (n.d *. b.v);
      b.d <- b.d +. (n.d *. a.v))

let ( /: ) (a : t) (b : t) : t =
  make (a.v /. b.v) [ a; b ] (fun n ->
      a.d <- a.d +. (n.d /. b.v);
      b.d <- b.d -. (n.d *. a.v /. (b.v *. b.v)))

let neg (a : t) : t = make (-.a.v) [ a ] (fun n -> a.d <- a.d -. n.d)
let exp_ (a : t) : t = make (exp a.v) [ a ] (fun n -> a.d <- a.d +. (n.d *. n.v))
let log_ (a : t) : t = make (log a.v) [ a ] (fun n -> a.d <- a.d +. (n.d /. a.v))

(* the squashes, with the derivatives Net.slope also uses: taken from
 * the output, which the node already holds *)
let tanh_ (a : t) : t =
  make (tanh a.v) [ a ] (fun n -> a.d <- a.d +. (n.d *. (1. -. (n.v *. n.v))))

let sigmoid (a : t) : t =
  make (1. /. (1. +. exp (-.a.v))) [ a ] (fun n -> a.d <- a.d +. (n.d *. n.v *. (1. -. n.v)))

let relu (a : t) : t = make (if a.v > 0. then a.v else 0.) [ a ] (fun n -> a.d <- a.d +. (if a.v > 0. then n.d else 0.))
let square (a : t) : t = make (a.v *. a.v) [ a ] (fun n -> a.d <- a.d +. (n.d *. 2. *. a.v))

(* a to a constant power: d(a^k)/da = k a^(k-1) *)
let pow (a : t) (k : float) : t = make (a.v ** k) [ a ] (fun n -> a.d <- a.d +. (n.d *. k *. (a.v ** (k -. 1.))))

let sum (l : t list) : t = List.fold_left ( +: ) (value 0.) l

(* a whole sum of products as one node, sum_i a_i b_i: what [*:] and
 * [+:] would build with 2n nodes. Each a_i's slope is its b_i, as for
 * one product *)
let dot (a : t array) (b : t array) : t =
  let v = ref 0. in
  Array.iteri (fun i (x : t) -> v := !v +. (x.v *. b.(i).v)) a;
  make !v
    (Array.to_list a @ Array.to_list b)
    (fun n ->
      Array.iteri
        (fun i (x : t) ->
          let y = b.(i) in
          x.d <- x.d +. (n.d *. y.v);
          y.d <- y.d +. (n.d *. x.v))
        a)

(*****************************************************************************)
(* Choosing among several *)
(*****************************************************************************)

(* out of the operations above, nothing new: the derivative the .mli
 * gives (p - 1 at the answer, p elsewhere) is what the walk finds by
 * itself. The largest score is taken off first as a plain number, not
 * a node: it changes no probability and no slope, and keeps exp from
 * overflowing *)
let softmax (scores : t list) : t list =
  let top = List.fold_left (fun m (s : t) -> Float.max m s.v) neg_infinity scores in
  let es = List.map (fun s -> exp_ (s -: value top)) scores in
  let total = sum es in
  List.map (fun e -> e /: total) es

let cross_entropy (scores : t list) (answer : int) : t = neg (log_ (List.nth (softmax scores) answer))

(*****************************************************************************)
(* Going backwards *)
(*****************************************************************************)

(* the nodes, deepest first: a node may only send its slope back once
 * everything it feeds has sent to it, so they are ordered by what
 * depends on what.
 *
 * A node is listed when the walk leaves it, after all it was made
 * from. Each walk has a number, written on the nodes it meets, so
 * "have I been here" is one comparison; and the way back is kept in a
 * list of our own (each node with its inputs still to visit) rather
 * than in OCaml's stack, which a browser's is too short for: a sum of
 * ten thousand terms is a chain ten thousand deep.
 *
 * The first version asked that question of a list of the nodes seen,
 * which is a walk of the list per node, quadratic:
 *
 *     let order (root : t) : t list =
 *       let seen = ref [] and out = ref [] in
 *       let rec go (n : t) =
 *         if not (List.memq n !seen) then (
 *           seen := n :: !seen;
 *           List.iter go n.from;
 *           out := n :: !out)
 *       in
 *       go root;
 *       !out
 *
 * [backward] on a chain of products (Unit_grad):
 *
 *     nodes      the list    the mark
 *      1000       1.9 ms      0.2 ms
 *      4000      25.6 ms      0.4 ms
 *     16000     313.7 ms      3.4 ms
 *
 * A network of 105 weights never noticed; a GPT's loss is tens of
 * thousands of nodes, every step. *)
let walks = ref 0

let order (root : t) : t list =
  incr walks;
  let walk = !walks in
  let out = ref [] in
  (* the nodes entered and not yet left, the innermost first *)
  let rec go (path : (t * t list) list) : unit =
    match path with
    | [] -> ()
    | (n, []) :: back ->
        out := n :: !out;
        go back
    | (n, input :: rest) :: back ->
        if input.walk = walk then go ((n, rest) :: back)
        else (
          input.walk <- walk;
          go ((input, input.from) :: (n, rest) :: back))
  in
  root.walk <- walk;
  go [ (root, root.from) ];
  !out

let backward (root : t) : unit =
  let nodes = order root in
  List.iter (fun n -> n.d <- 0.) nodes;
  root.d <- 1.;
  List.iter (fun n -> n.send_back n) nodes

let zero (root : t) : unit = List.iter (fun n -> n.d <- 0.) (order root)
let nodes (root : t) : int = List.length (order root)
