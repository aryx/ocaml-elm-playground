(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Svf.mli *)

type model = Chamberlin | Zero_delay
type mode = Low_pass | Band_pass | High_pass | Notch

let modes = [ Low_pass; Band_pass; High_pass; Notch ]

(* the two integrators' memories: for Chamberlin's, the low-pass and
 * band-pass outputs; for the zero-delay one, the trapezoids' states *)
type t = { mutable s1 : float; mutable s2 : float }

let create () : t = { s1 = 0.; s2 = 0. }
let clamp (cutoff : float) : float = Float.min 20000. (Float.max 10. cutoff)

let pick (mode : mode) ~low ~band ~high : float =
  match mode with Low_pass -> low | Band_pass -> band | High_pass -> high | Notch -> low +. high

(* two integrators in a loop, each fed the last one's output of this
 * sample or the last: low += f band, high = x - low - band / q, band +=
 * f high *)
let chamberlin (t : t) (mode : mode) ~(cutoff : float) ~(q : float) (x : float) : float =
  let f = 2. *. sin (Float.pi *. clamp cutoff /. float_of_int Signal.rate) in
  let low = t.s2 +. (f *. t.s1) in
  let high = x -. low -. (t.s1 /. q) in
  let band = t.s1 +. (f *. high) in
  t.s1 <- band;
  t.s2 <- low;
  pick mode ~low ~band ~high

(* the same loop solved: the high-pass first, from the two states, then
 * the two trapezoidal integrators *)
let zero_delay (t : t) (mode : mode) ~(cutoff : float) ~(q : float) (x : float) : float =
  let g = tan (Float.pi *. clamp cutoff /. float_of_int Signal.rate) in
  let r2 = 1. /. q in
  let high = (x -. ((r2 +. g) *. t.s1) -. t.s2) /. (1. +. (r2 *. g) +. (g *. g)) in
  let v1 = g *. high in
  let band = v1 +. t.s1 in
  t.s1 <- band +. v1;
  let v2 = g *. band in
  let low = v2 +. t.s2 in
  t.s2 <- low +. v2;
  pick mode ~low ~band ~high

let process (t : t) (model : model) (mode : mode) ~(cutoff : Signal.t) ~(q : float) (samples : Signal.t) : unit =
  for i = 0 to Array.length samples - 1 do
    let cutoff = cutoff.(i) in
    samples.(i) <-
      (match model with
      | Chamberlin -> chamberlin t mode ~cutoff ~q samples.(i)
      | Zero_delay -> zero_delay t mode ~cutoff ~q samples.(i))
  done
