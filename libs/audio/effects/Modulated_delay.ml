(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Modulated_delay.mli *)

type settings = { center : float; depth : float; rate : float; feedback : float; mix : float }

let chorus = { center = 0.015; depth = 0.003; rate = 0.5; feedback = 0.; mix = 0.5 }
let flanger = { center = 0.0025; depth = 0.002; rate = 0.2; feedback = 0.5; mix = 0.7 }
let longest = 0.05
let rate = float_of_int Signal.rate

(* the last [size] samples written, [at] the next to write *)
type line = { samples : float array; mutable at : int; mutable last : float (* the last copy, fed back *) }

let size = Signal.samples longest + 4
let line () = { samples = Array.make size 0.; at = 0; last = 0. }

(* [d] samples ago (d >= 1), between two samples: linear *)
let read (l : line) (d : float) : float =
  let back = Float.to_int d in
  let frac = d -. float_of_int back in
  let at k = l.samples.((l.at - k + (2 * size)) mod size) in
  ((1. -. frac) *. at back) +. (frac *. at (back + 1))

let write (l : line) (x : float) : unit =
  l.samples.(l.at) <- x;
  l.at <- (l.at + 1) mod size

type t = {
  left : line;
  right : line;
  mutable phase : float; (* the LFO's, in turns *)
  (* the last block's, ramped from (nan: none yet) *)
  mutable last_mix : float;
  mutable last_feedback : float;
}

let create () : t = { left = line (); right = line (); phase = 0.; last_mix = Float.nan; last_feedback = Float.nan }

let process (t : t) (s : settings) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  let feedback = Float.min 0.95 (Float.max (-0.95) s.feedback) in
  let from_mix = if Float.is_nan t.last_mix then s.mix else t.last_mix
  and from_feedback = if Float.is_nan t.last_feedback then feedback else t.last_feedback in
  (* the delay in samples at an LFO phase, kept within the line *)
  let delay phase =
    let d = (s.center +. (s.depth *. sin (2. *. Float.pi *. phase))) *. rate in
    Float.max 1. (Float.min (float_of_int (size - 3)) d)
  in
  let side (l : line) (x : float) (phase : float) (mix : float) (feedback : float) : float =
    let copy = read l (delay phase) in
    write l (x +. (feedback *. l.last));
    l.last <- copy;
    x +. (mix *. copy)
  in
  for i = 0 to n - 1 do
    let mix = Effect.ramp from_mix s.mix i n and feedback = Effect.ramp from_feedback feedback i n in
    out.left.(i) <- side t.left out.left.(i) t.phase mix feedback;
    (* the right side's LFO a quarter turn ahead *)
    out.right.(i) <- side t.right out.right.(i) (t.phase +. 0.25) mix feedback;
    t.phase <- t.phase +. (s.rate /. rate);
    if t.phase >= 1. then t.phase <- t.phase -. 1.
  done;
  t.last_mix <- s.mix;
  t.last_feedback <- feedback
