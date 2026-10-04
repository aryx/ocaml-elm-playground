(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Sampling.mli *)

let draw (state : Lehmer.state) (p : float array) : int =
  let x = Lehmer.float state 1. in
  (* the first index whose share, added to those before, passes x; the
   * last one if rounding left the sum a hair under 1 *)
  let rec go (i : int) (sum : float) : int =
    if i >= Array.length p - 1 then Array.length p - 1
    else
      let sum = sum +. p.(i) in
      if x < sum then i else go (i + 1) sum
  in
  go 0 0.

let temper (t : float) (p : float array) : float array =
  let raised = Array.map (fun x -> x ** (1. /. t)) p in
  let total = Array.fold_left ( +. ) 0. raised in
  Array.map (fun x -> x /. total) raised

let best (p : float array) : int =
  let at = ref 0 in
  Array.iteri (fun i x -> if x > p.(!at) then at := i) p;
  !at
