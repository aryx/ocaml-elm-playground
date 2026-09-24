(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Modal.mli *)

let rate = float_of_int Signal.rate

type t = {
  w : float;
  mutable r : float;
  mutable a1 : float; (* 2 r cos w *)
  mutable a2 : float; (* r^2 *)
  mutable y1 : float;
  mutable y2 : float;
  mutable pending : float; (* the next strike's impulse *)
  mutable level : float; (* the amplitude's bound, decayed each sample *)
}

let coefficients (t : t) (t60 : float) : unit =
  t.r <- Float.pow 10. (-3. /. (Float.max 1e-4 t60 *. rate));
  t.a1 <- 2. *. t.r *. cos t.w;
  t.a2 <- t.r *. t.r

let create ~(frequency : float) ~(t60 : float) : t =
  let w = 2. *. Float.pi *. Float.min frequency (0.49 *. rate) /. rate in
  let t = { w; r = 0.; a1 = 0.; a2 = 0.; y1 = 0.; y2 = 0.; pending = 0.; level = 0. } in
  coefficients t t60;
  t

let strike (t : t) (a : float) : unit =
  t.pending <- t.pending +. (a *. sin t.w);
  t.level <- t.level +. Float.abs a

let damp (t : t) ~(t60 : float) : unit = coefficients t t60

let next (t : t) : float =
  let y = (t.a1 *. t.y1) -. (t.a2 *. t.y2) +. t.pending in
  t.pending <- 0.;
  t.y2 <- t.y1;
  t.y1 <- y;
  t.level <- t.level *. t.r;
  y

let level (t : t) : float = t.level
