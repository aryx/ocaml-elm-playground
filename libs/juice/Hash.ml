(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Hash.mli *)

(* Park and Miller's minimal standard, one step: 16807 x mod (2^31 - 1)
 * by Schrage's method, every product below 2^31 (127773 = m / 16807,
 * 2836 = m mod 16807) *)
let m = 2147483647

let park_miller (x : int) : int =
  let hi = x / 127773 and lo = x mod 127773 in
  let t = (16807 * lo) - (2836 * hi) in
  if t > 0 then t else t + m

(* the high bits folded into the low ones: breaks the linearity that
 * leaves neighbouring points correlated (see Hash.mli) *)
let mix (x : int) : int =
  let x = x lxor (x lsr 13) in
  if x = 0 then 1 else x

let hash ~(seed : int) (i : int) : float =
  (* 20 bits of point, 10 of seed: below 2^30, positive *)
  let x = ((i land 0xfffff) * 1024) + (seed land 0x3ff) + 1 in
  let x = park_miller (mix (park_miller (mix (park_miller x)))) in
  (2. *. float_of_int x /. float_of_int m) -. 1.

let unit ~(seed : int) (i : int) : float = (hash ~seed i +. 1.) /. 2.
