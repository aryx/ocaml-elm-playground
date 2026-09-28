(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Drift.mli *)

let step_samples = 64

(* the walk's parameters (sigma, tau); where it is; the random state; the
 * samples since the last step *)
type t = { sigma : float; tau : float; mutable x : float; mutable random : int; mutable since : int }

(* a random number of variance 1: uniform on -1..1 has 1/3 *)
let draw (t : t) : float =
  t.random <- Noise.lcg t.random;
  sqrt 3. *. Noise.uniform t.random

let create ?(cents = 3.) ?(seconds = 2.) ~(seed : int) () : t =
  let t = { sigma = cents *. sqrt (2. /. seconds); tau = seconds; x = 0.; random = seed; since = 0 } in
  (* from its settled spread, not from 0: two oscillators out of tune
   * from the start *)
  t.x <- cents *. draw t;
  t

let advance (t : t) (n : int) : unit =
  let h = float_of_int step_samples /. float_of_int Signal.rate in
  t.since <- t.since + n;
  while t.since >= step_samples do
    t.x <- (t.x *. (1. -. (h /. t.tau))) +. (t.sigma *. sqrt h *. draw t);
    t.since <- t.since - step_samples
  done

let cents (t : t) : float = t.x
let factor (t : t) : float = Float.pow 2. (t.x /. 1200.)
