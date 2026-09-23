(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Moog_ladder.mli *)

type model = Naive | Zero_delay | Nonlinear

let models = [ Naive; Zero_delay; Nonlinear ]
let name = function Naive -> "naive" | Zero_delay -> "zero-delay feedback" | Nonlinear -> "nonlinear"

(* the four poles' memories: the naive ones' outputs, the zero-delay
 * ones' integrator states *)
type t = { s : float array }

let create () : t = { s = Array.make 4 0. }
let reset (t : t) : unit = Array.fill t.s 0 4 0.
let clamp (cutoff : float) : float = Float.min 20000. (Float.max 10. cutoff)

(* one pole after the other, the fourth's output fed back from the last
 * sample: the loop can't be computed within the sample, so it is
 * delayed by one *)
let naive (t : t) ~(k : float) ~(c : float) ~(cutoff : float) (x : float) : float =
  let g = 1. -. exp (-2. *. Float.pi *. clamp cutoff /. float_of_int Signal.rate) in
  let input = ref ((x *. (1. +. (c *. k))) -. (k *. t.s.(3))) in
  for i = 0 to 3 do
    t.s.(i) <- t.s.(i) +. (g *. (!input -. t.s.(i)));
    input := t.s.(i)
  done;
  t.s.(3)

(* the zero-delay loop: each pole a trapezoidal integrator, whose output
 * is G (its input) + (its state) / (1 + g); four in a row, the output
 * G^4 u + sigma, sigma from the states; with u = x - k y, solved:
 * y = (G^4 x + sigma) / (1 + k G^4) *)
let zero_delay (t : t) ~(nonlinear : bool) ~(k : float) ~(c : float) ~(cutoff : float) (x : float) : float =
  let g = tan (Float.pi *. clamp cutoff /. float_of_int Signal.rate) in
  let big_g = g /. (1. +. g) in
  let s = t.s in
  let sigma = ((big_g *. big_g *. big_g *. s.(0)) +. (big_g *. big_g *. s.(1)) +. (big_g *. s.(2)) +. s.(3)) /. (1. +. g) in
  let g4 = big_g *. big_g *. big_g *. big_g in
  let x = x *. (1. +. (c *. k)) in
  let y = ((g4 *. x) +. sigma) /. (1. +. (k *. g4)) in
  (* the loop's input, now known; saturating, it can't run away *)
  let u = x -. (k *. y) in
  let input = ref (if nonlinear then tanh u else u) in
  for i = 0 to 3 do
    (* the transistors' saturation at each pole's input *)
    let v = big_g *. ((if nonlinear && i > 0 then tanh !input else !input) -. s.(i)) in
    let out = v +. s.(i) in
    s.(i) <- out +. v;
    input := out
  done;
  !input

let process ?(compensation = 0.) (t : t) (model : model) ~(cutoff : Signal.t) ~(resonance : float) (samples : Signal.t) : unit =
  let k = resonance and c = compensation in
  for i = 0 to Array.length samples - 1 do
    let cutoff = cutoff.(i) in
    samples.(i) <-
      (match model with
      | Naive -> naive t ~k ~c ~cutoff samples.(i)
      | Zero_delay -> zero_delay t ~nonlinear:false ~k ~c ~cutoff samples.(i)
      | Nonlinear -> zero_delay t ~nonlinear:true ~k ~c ~cutoff samples.(i))
  done
