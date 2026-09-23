(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Follow.mli *)

type t = { value : float; velocity : float }

let at (x : float) : t = { value = x; velocity = 0. }

(* the velocity first, then the value with the new velocity *)
let chase ?(frequency = 2.) ?(damping = 1.) ~(dt : float) (target : float) (t : t) : t =
  let w = 2. *. Float.pi *. frequency in
  let acceleration = (w *. w *. (target -. t.value)) -. (2. *. damping *. w *. t.velocity) in
  let velocity = t.velocity +. (acceleration *. dt) in
  { value = t.value +. (velocity *. dt); velocity }

let smooth ~(rate : float) ~(dt : float) (target : float) (x : float) : float = x +. ((target -. x) *. (1. -. exp (-.rate *. dt)))
