(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Filter.mli *)

let rate = float_of_int Signal.rate

(*****************************************************************************)
(* The one-pole *)
(*****************************************************************************)

let one_pole_coefficient (cutoff : float) : float = 1. -. exp (-2. *. Float.pi *. cutoff /. rate)

let low_pass ~(cutoff : float) (s : Signal.t) : Signal.t =
  let a = one_pole_coefficient cutoff and y = ref 0. in
  Array.map
    (fun x ->
      y := !y +. (a *. (x -. !y));
      !y)
    s

let high_pass ~(cutoff : float) (s : Signal.t) : Signal.t = Array.map2 ( -. ) s (low_pass ~cutoff s)

(*****************************************************************************)
(* The biquad *)
(*****************************************************************************)

type kind = Low_pass | High_pass | Band_pass
type biquad = { b0 : float; b1 : float; b2 : float; a1 : float; a2 : float }

let biquad ?(rate = rate) (kind : kind) ~(cutoff : float) ~(q : float) : biquad =
  let w = 2. *. Float.pi *. cutoff /. rate in
  let c = cos w and alpha = sin w /. (2. *. q) in
  let a0 = 1. +. alpha in
  let (b0, b1, b2) =
    match kind with
    | Low_pass -> ((1. -. c) /. 2., 1. -. c, (1. -. c) /. 2.)
    | High_pass -> ((1. +. c) /. 2., -.(1. +. c), (1. +. c) /. 2.)
    | Band_pass -> (alpha, 0., -.alpha)
  in
  { b0 = b0 /. a0; b1 = b1 /. a0; b2 = b2 /. a0; a1 = -2. *. c /. a0; a2 = (1. -. alpha) /. a0 }

(* the cookbook's EQ: A the amplitude's square root (the gain in dB over
 * 40), so that the bell's top and the shelf's far side are A^2 *)
let peaking ~(frequency : float) ~(q : float) ~(gain : float) : biquad =
  let a = 10. ** (gain /. 40.) and w = 2. *. Float.pi *. frequency /. rate in
  let c = cos w and alpha = sin w /. (2. *. q) in
  let a0 = 1. +. (alpha /. a) in
  { b0 = (1. +. (alpha *. a)) /. a0; b1 = -2. *. c /. a0; b2 = (1. -. (alpha *. a)) /. a0; a1 = -2. *. c /. a0; a2 = (1. -. (alpha /. a)) /. a0 }

(* the shelves, with the cookbook's slope S = 1 (the steepest without a
 * bump): alpha = sin w / sqrt 2; [high] the mirror image, cos w's sign
 * flipped *)
let shelf ~(high : bool) ~(frequency : float) ~(gain : float) : biquad =
  let a = 10. ** (gain /. 40.) and w = 2. *. Float.pi *. frequency /. rate in
  let c = if high then -.cos w else cos w and alpha = sin w /. sqrt 2. in
  let r = 2. *. sqrt a *. alpha and s = if high then -1. else 1. in
  let a0 = a +. 1. +. ((a -. 1.) *. c) +. r in
  {
    b0 = a *. (a +. 1. -. ((a -. 1.) *. c) +. r) /. a0;
    b1 = s *. 2. *. a *. (a -. 1. -. ((a +. 1.) *. c)) /. a0;
    b2 = a *. (a +. 1. -. ((a -. 1.) *. c) -. r) /. a0;
    a1 = s *. -2. *. (a -. 1. +. ((a +. 1.) *. c)) /. a0;
    a2 = (a +. 1. +. ((a -. 1.) *. c) -. r) /. a0;
  }

let low_shelf ~(frequency : float) ~(gain : float) : biquad = shelf ~high:false ~frequency ~gain
let high_shelf ~(frequency : float) ~(gain : float) : biquad = shelf ~high:true ~frequency ~gain

(* H(z) = (b0 + b1 z^-1 + b2 z^-2) / (1 + a1 z^-1 + a2 z^-2) at z =
 * e^iw, its size: each polynomial's real and imaginary parts, with
 * z^-k = cos kw - i sin kw *)
let response ?(rate = rate) (f : biquad) (frequency : float) : float =
  let w = 2. *. Float.pi *. frequency /. rate in
  let size c0 c1 c2 =
    let re = c0 +. (c1 *. cos w) +. (c2 *. cos (2. *. w)) and im = -.((c1 *. sin w) +. (c2 *. sin (2. *. w))) in
    Float.hypot re im
  in
  size f.b0 f.b1 f.b2 /. size 1. f.a1 f.a2

(* the four samples of memory: the last two inputs, the last two
 * outputs *)
type memory = { mutable x1 : float; mutable x2 : float; mutable y1 : float; mutable y2 : float }

let step (f : biquad) (m : memory) (x : float) : float =
  let y = (f.b0 *. x) +. (f.b1 *. m.x1) +. (f.b2 *. m.x2) -. (f.a1 *. m.y1) -. (f.a2 *. m.y2) in
  m.x2 <- m.x1;
  m.x1 <- x;
  m.y2 <- m.y1;
  m.y1 <- y;
  y

let silence () : memory = { x1 = 0.; x2 = 0.; y1 = 0.; y2 = 0. }

let process (f : biquad) (m : memory) (s : Signal.t) : unit = Array.iteri (fun i x -> s.(i) <- step f m x) s

let run (f : biquad) (s : Signal.t) : Signal.t =
  let m = silence () in
  Array.map (step f m) s

let sweep (kind : kind) ~(q : float) ~(from : float) ~(to_ : float) (s : Signal.t) : Signal.t =
  let m = silence () and n = float_of_int (max 1 (Array.length s)) in
  Array.mapi (fun i x -> step (biquad kind ~cutoff:(from *. ((to_ /. from) ** (float_of_int i /. n))) ~q) m x) s
