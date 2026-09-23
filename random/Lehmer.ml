(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Lehmer.mli *)

type t = int

let m = 2147483647 (* 2^31 - 1, a prime *)
let a = 16807 (* 7^5 *)
let q = m / a (* 127773 *)
let r = m mod a (* 2836 *)

let of_int (n : int) : t =
  let s = abs (n mod m) in
  if s = 0 then 1 else s

(* MurmurHash3's fmix32, on Int32 (wrapping at 2^32 natively and in a
 * browser alike) *)
let fmix32 (h : int32) : int32 =
  let xorshift k h = Int32.logxor h (Int32.shift_right_logical h k) in
  h |> xorshift 16 |> Int32.mul 0x85ebca6bl |> xorshift 13 |> Int32.mul 0xc2b2ae35l |> xorshift 16

let scramble (n : int) : t =
  (* the low 31 bits of the hash, a non-negative int on both platforms *)
  of_int (Int32.to_int (Int32.logand (fmix32 (Int32.of_int n)) 0x7fffffffl))

let next (s : t) : t =
  let t = (a * (s mod q)) - (r * (s / q)) in
  if t > 0 then t else t + m

let to_unit (s : t) : float = float_of_int (s - 1) /. float_of_int (m - 1)
