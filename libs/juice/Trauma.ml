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
(* Smooth or not *)
(*****************************************************************************)

let noise ~(seed : int) (x : float) : float =
  let i = Float.to_int (Float.floor x) in
  Tween.lerp (Hash.hash ~seed i) (Hash.hash ~seed (i + 1)) (Ease.smoothstep (x -. Float.floor x))

let jitter ~(seed : int) (x : float) : float = Hash.hash ~seed (Float.to_int (Float.floor x))

type offset = { dx : float; dy : float; angle : float }

let offset ?(smooth = true) ?(max_offset = 40.) ?(max_angle = 5.) ~(seed : int) ~(trauma : float) (time : float) :
    offset =
  let s = shake trauma in
  let read k = if smooth then noise ~seed:(seed + k) (time *. 25.) else jitter ~seed:(seed + k) (time *. 60.) in
  { dx = max_offset *. s *. read 0; dy = max_offset *. s *. read 1; angle = max_angle *. s *. read 2 }
