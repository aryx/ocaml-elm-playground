(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mix.mli *)

let add (sounds : Signal.t list) : Signal.t =
  let n = List.fold_left (fun m s -> max m (Array.length s)) 0 sounds in
  let out = Array.make n 0. in
  List.iter (fun s -> Array.iteri (fun i x -> out.(i) <- out.(i) +. x) s) sounds;
  out

let gain (g : float) (s : Signal.t) : Signal.t = Array.map (fun x -> g *. x) s
let delay (seconds : float) (s : Signal.t) : Signal.t = Array.append (Array.make (Signal.samples seconds) 0.) s
let then_ (a : Signal.t) (b : Signal.t) : Signal.t = Array.append a b
let decibels (ratio : float) : float = 20. *. log10 ratio
let of_decibels (db : float) : float = 10. ** (db /. 20.)
let clip (x : float) : float = Float.max (-1.) (Float.min 1. x)
(* OPTIMIZATION (rational): tanh as a ratio of two polynomials (its
 * continued fraction cut at x^7, a Pade approximant): within 1e-4 of
 * libc's, exactly 1 past 4.97 where it reaches it; libc's is two
 * exp, about 130 instructions on arm, and every mixed sample of
 * every channel takes one (Mixer). old: tanh x, exact_tanh *)
let exact_tanh = ref false

let soft_clip (x : float) : float =
  if !exact_tanh then tanh x
  else if x >= 4.97 then 1.
  else if x <= -4.97 then -1.
  else
    let x2 = x *. x in
    x *. (135135. +. (x2 *. (17325. +. (x2 *. (378. +. x2)))))
    /. (135135. +. (x2 *. (62370. +. (x2 *. (3150. +. (28. *. x2))))))
let limit ?(soft = false) (s : Signal.t) : Signal.t = Array.map (if soft then soft_clip else clip) s
