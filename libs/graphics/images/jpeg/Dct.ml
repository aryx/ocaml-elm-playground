(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Dct.mli *)

let pi = 4.0 *. atan 1.0

(* cos((2i + 1) k pi / 16), for i and k from 0 to 7 *)
let cosines : float array =
  Array.init 64 (fun n ->
      let i = n / 8 and k = n mod 8 in
      cos (float ((2 * i) + 1) *. float k *. pi /. 16.))

let c (k : int) : float = if k = 0 then 1. /. sqrt 2. else 1.

(*****************************************************************************)
(* The formula *)
(*****************************************************************************)

let fdct (f : float array) : float array =
  Array.init 64 (fun n ->
      let u = n mod 8 and v = n / 8 in
      let sum = ref 0. in
      for y = 0 to 7 do
        for x = 0 to 7 do
          sum := !sum +. (f.((y * 8) + x) *. cosines.((x * 8) + u) *. cosines.((y * 8) + v))
        done
      done;
      0.25 *. c u *. c v *. !sum)

let idct (big_f : float array) : float array =
  Array.init 64 (fun n ->
      let x = n mod 8 and y = n / 8 in
      let sum = ref 0. in
      for v = 0 to 7 do
        for u = 0 to 7 do
          sum := !sum +. (c u *. c v *. big_f.((v * 8) + u) *. cosines.((x * 8) + u) *. cosines.((y * 8) + v))
        done
      done;
      0.25 *. !sum)

(*****************************************************************************)
(* Arai, Agui, Nakajima *)
(*****************************************************************************)

(* the scale factors AAN leaves out of the 1D transform: s(0) = 1, s(k) =
 * cos(k pi / 16) * sqrt 2 *)
let aan_scale : float array =
  Array.init 8 (fun k -> if k = 0 then 1. else cos (float k *. pi /. 16.) *. sqrt 2.)

(* one 1D inverse transform, of the 8 values of [a] at [off], [off +
 * step], ..., in place: the flow graph of jidctflt.c, the even part
 * (inputs 0, 2, 4, 6) and the odd part (1, 3, 5, 7), then their sums
 * and differences *)
let idct_1d (a : float array) ~(off : int) ~(step : int) : unit =
  let get k = a.(off + (k * step)) and set k v = a.(off + (k * step)) <- v in
  (* even part *)
  let tmp0 = get 0 and tmp1 = get 2 and tmp2 = get 4 and tmp3 = get 6 in
  let tmp10 = tmp0 +. tmp2 and tmp11 = tmp0 -. tmp2 in
  let tmp13 = tmp1 +. tmp3 in
  let tmp12 = ((tmp1 -. tmp3) *. 1.414213562) -. tmp13 in
  let tmp0 = tmp10 +. tmp13 and tmp3 = tmp10 -. tmp13 in
  let tmp1 = tmp11 +. tmp12 and tmp2 = tmp11 -. tmp12 in
  (* odd part *)
  let tmp4 = get 1 and tmp5 = get 3 and tmp6 = get 5 and tmp7 = get 7 in
  let z13 = tmp6 +. tmp5 and z10 = tmp6 -. tmp5 in
  let z11 = tmp4 +. tmp7 and z12 = tmp4 -. tmp7 in
  let tmp7 = z11 +. z13 in
  let tmp11 = (z11 -. z13) *. 1.414213562 in
  let z5 = (z10 +. z12) *. 1.847759065 in
  let tmp10 = (1.082392200 *. z12) -. z5 in
  let tmp12 = (-2.613125930 *. z10) +. z5 in
  let tmp6 = tmp12 -. tmp7 in
  let tmp5 = tmp11 -. tmp6 in
  let tmp4 = tmp10 +. tmp5 in
  set 0 (tmp0 +. tmp7);
  set 7 (tmp0 -. tmp7);
  set 1 (tmp1 +. tmp6);
  set 6 (tmp1 -. tmp6);
  set 2 (tmp2 +. tmp5);
  set 5 (tmp2 -. tmp5);
  set 4 (tmp3 +. tmp4);
  set 3 (tmp3 -. tmp4)

let idct_aan (big_f : float array) : float array =
  (* the scale factors, and the 1/8 the two passes leave *)
  let a = Array.init 64 (fun n -> big_f.(n) *. aan_scale.(n mod 8) *. aan_scale.(n / 8) /. 8.) in
  for u = 0 to 7 do
    idct_1d a ~off:u ~step:8 (* the columns *)
  done;
  for y = 0 to 7 do
    idct_1d a ~off:(y * 8) ~step:1 (* the rows *)
  done;
  a
