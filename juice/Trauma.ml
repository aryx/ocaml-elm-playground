(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Trauma.mli *)

let add (amount : float) (trauma : float) : float = Float.min 1. (trauma +. amount)

let decay ?(per_second = 1.) ~(dt : float) (trauma : float) : float = Float.max 0. (trauma -. (per_second *. dt))

let shake (trauma : float) : float = trauma *. trauma

(*****************************************************************************)
(* The random values *)
(*****************************************************************************)

(* Park and Miller's minimal standard, one step: 16807 x mod (2^31 - 1)
 * by Schrage's method, every product below 2^31 (127773 = m / 16807,
 * 2836 = m mod 16807) *)
let m = 2147483647

let park_miller (x : int) : int =
  let hi = x / 127773 and lo = x mod 127773 in
  let t = (16807 * lo) - (2836 * hi) in
  if t > 0 then t else t + m

(* the high bits folded into the low ones: breaks the linearity that
 * leaves neighbouring points correlated (see Trauma.mli) *)
let mix (x : int) : int =
  let x = x lxor (x lsr 13) in
  if x = 0 then 1 else x

let hash ~(seed : int) (i : int) : float =
  (* 20 bits of point, 10 of seed: below 2^30, positive *)
  let x = ((i land 0xfffff) * 1024) + (seed land 0x3ff) + 1 in
  let x = park_miller (mix (park_miller (mix (park_miller x)))) in
  (2. *. float_of_int x /. float_of_int m) -. 1.

(*****************************************************************************)
(* Smooth or not *)
(*****************************************************************************)

let noise ~(seed : int) (x : float) : float =
  let i = Float.to_int (Float.floor x) in
  Tween.lerp (hash ~seed i) (hash ~seed (i + 1)) (Ease.smoothstep (x -. Float.floor x))

let jitter ~(seed : int) (x : float) : float = hash ~seed (Float.to_int (Float.floor x))

type offset = { dx : float; dy : float; angle : float }

let offset ?(smooth = true) ?(max_offset = 40.) ?(max_angle = 5.) ~(seed : int) ~(trauma : float) (time : float) :
    offset =
  let s = shake trauma in
  let read k = if smooth then noise ~seed:(seed + k) (time *. 25.) else jitter ~seed:(seed + k) (time *. 60.) in
  { dx = max_offset *. s *. read 0; dy = max_offset *. s *. read 1; angle = max_angle *. s *. read 2 }
