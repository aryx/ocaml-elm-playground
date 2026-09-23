(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ease.mli *)

type t = float -> float

let linear (t : float) : float = t

(*****************************************************************************)
(* The families *)
(*****************************************************************************)

let quad (t : float) : float = t *. t
let cubic (t : float) : float = t *. t *. t
let sine (t : float) : float = 1. -. cos (t *. Float.pi /. 2.)

(* Penner's constant: 10% of overshoot (see Ease.mli) *)
let s = 1.70158
let back (t : float) : float = ((s +. 1.) *. t *. t *. t) -. (s *. t *. t)

(* a sine of period 0.3, its amplitude growing from 2^-10 to 1; ends at
 * exactly 0 and 1 *)
let elastic (t : float) : float =
  if t <= 0. then 0.
  else if t >= 1. then 1.
  else -.(2. ** ((10. *. t) -. 10.)) *. sin (((10. *. t) -. 10.75) *. (2. *. Float.pi /. 3.))

(* Penner writes the bounce the way it falls (its out curve): four
 * parabolas, each touching 1, of heights 1/4, 1/16 and 1/64 below it
 * -- a ball losing half its speed each time *)
let bounce_out (t : float) : float =
  let n = 7.5625 and d = 2.75 in
  if t < 1. /. d then n *. t *. t
  else if t < 2. /. d then
    let t = t -. (1.5 /. d) in
    (n *. t *. t) +. 0.75
  else if t < 2.5 /. d then
    let t = t -. (2.25 /. d) in
    (n *. t *. t) +. 0.9375
  else
    let t = t -. (2.625 /. d) in
    (n *. t *. t) +. 0.984375

let smoothstep (t : float) : float = t *. t *. (3. -. (2. *. t))

(*****************************************************************************)
(* Out and in-out, from any curve *)
(*****************************************************************************)

let out (f : t) : t = fun t -> 1. -. f (1. -. t)

let in_out (f : t) : t = fun t -> if t < 0.5 then f (2. *. t) /. 2. else 1. -. (f (2. -. (2. *. t)) /. 2.)

(* its in curve is its out one run backwards *)
let bounce : t = out bounce_out

let all : (string * t) list =
  [ ("linear", linear) ]
  @ List.concat_map
      (fun (name, f) -> [ ("in_" ^ name, f); ("out_" ^ name, out f); ("in_out_" ^ name, in_out f) ])
      [ ("quad", quad); ("cubic", cubic); ("sine", sine); ("back", back); ("elastic", elastic); ("bounce", bounce) ]
  @ [ ("smoothstep", smoothstep) ]
