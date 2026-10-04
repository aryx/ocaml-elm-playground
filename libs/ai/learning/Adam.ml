(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Adam.mli *)

type t = {
  rate : float;
  b1 : float;
  b2 : float;
  epsilon : float;
  m : float array; (* the slopes' running average, per weight *)
  v : float array; (* their squares' *)
  t : int; (* the steps taken *)
}

let make ?(rate = 0.001) ?(b1 = 0.9) ?(b2 = 0.999) ?(epsilon = 1e-8) (n : int) : t =
  { rate; b1; b2; epsilon; m = Array.make n 0.; v = Array.make n 0.; t = 0 }

let steps (a : t) : int = a.t

let step ?rate (a : t) (weights : float array) (slopes : float array) : t * float array =
  let rate = match rate with Some r -> r | None -> a.rate in
  let t = a.t + 1 in
  let m = Array.mapi (fun i m -> (a.b1 *. m) +. ((1. -. a.b1) *. slopes.(i))) a.m in
  let v = Array.mapi (fun i v -> (a.b2 *. v) +. ((1. -. a.b2) *. slopes.(i) *. slopes.(i))) a.v in
  (* the averages started at zero: after t steps they hold only
   * 1 - b^t of what they should *)
  let full1 = 1. -. (a.b1 ** float_of_int t) and full2 = 1. -. (a.b2 ** float_of_int t) in
  let weights =
    Array.mapi (fun i w -> w -. (rate *. (m.(i) /. full1) /. (sqrt (v.(i) /. full2) +. a.epsilon))) weights
  in
  ({ a with m; v; t }, weights)
