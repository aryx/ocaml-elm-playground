(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Timing.mli *)

type t = { x1 : float; y1 : float; x2 : float; y2 : float }

let clamp01 v = Float.max 0. (Float.min 1. v)
let bezier x1 y1 x2 y2 = { x1 = clamp01 x1; y1; x2 = clamp01 x2; y2 }
let linear = bezier 0. 0. 1. 1.
let ease_in = bezier 0.42 0. 1. 1.
let ease_out = bezier 0. 0. 0.58 1.
let ease_in_out = bezier 0.42 0. 0.58 1.
let default = bezier 0.25 0.1 0.25 1.

(* one coordinate of the curve at s, its ends 0 and 1: its polynomial's
 * coefficients, Horner's way *)
let coord (p1 : float) (p2 : float) (s : float) : float =
  let c = 3. *. p1 in
  let b = (3. *. (p2 -. p1)) -. c in
  let a = 1. -. c -. b in
  ((((a *. s) +. b) *. s) +. c) *. s

let slope (p1 : float) (p2 : float) (s : float) : float =
  let c = 3. *. p1 in
  let b = (3. *. (p2 -. p1)) -. c in
  let a = 1. -. c -. b in
  (((3. *. a *. s) +. (2. *. b)) *. s) +. c

(* the s where x(s) = t *)
let solve (c : t) (t : float) : float =
  let rec newton s k =
    if k = 0 then None
    else
      let e = coord c.x1 c.x2 s -. t in
      if Float.abs e < 1e-7 then Some s
      else
        let d = slope c.x1 c.x2 s in
        if Float.abs d < 1e-6 then None else newton (s -. (e /. d)) (k - 1)
  in
  match newton t 8 with
  | Some s when s >= 0. && s <= 1. -> s
  | _ ->
      let rec bisect lo hi k =
        let s = (lo +. hi) /. 2. in
        if k = 0 then s else if coord c.x1 c.x2 s < t then bisect s hi (k - 1) else bisect lo s (k - 1)
      in
      bisect 0. 1. 40

let at (c : t) (t : float) : float =
  let t = clamp01 t in
  if t = 0. || t = 1. then t else coord c.y1 c.y2 (solve c t)
