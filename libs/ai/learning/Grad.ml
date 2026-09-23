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
}

let nothing (_ : t) : unit = ()
let value (v : float) : t = { v; d = 0.; from = []; send_back = nothing }
let of_ (n : t) : float = n.v
let slope (n : t) : float = n.d

let make (v : float) (from : t list) (send_back : t -> unit) : t = { v; d = 0.; from; send_back }

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
let sum (l : t list) : t = List.fold_left ( +: ) (value 0.) l

(* the nodes, deepest first: a node may only send its slope back once
 * everything it feeds has sent to it, so they are ordered by what
 * depends on what *)
let order (root : t) : t list =
  let seen = ref [] and out = ref [] in
  let rec go (n : t) =
    if not (List.memq n !seen) then (
      seen := n :: !seen;
      List.iter go n.from;
      out := n :: !out)
  in
  go root;
  !out

let backward (root : t) : unit =
  let nodes = order root in
  List.iter (fun n -> n.d <- 0.) nodes;
  root.d <- 1.;
  List.iter (fun n -> n.send_back n) nodes

let zero (root : t) : unit = List.iter (fun n -> n.d <- 0.) (order root)
let nodes (root : t) : int = List.length (order root)
