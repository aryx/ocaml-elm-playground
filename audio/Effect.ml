(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Effect.mli *)

type pitch =
  | Vibrato of { rate : float; depth : float }
  | Jump of { semitones : float; at : float }
  | Arpeggio of { semitones : float list; step : float }

let semitones (k : float) : float = 2. ** (k /. 12.)

let factor (p : pitch) (t : float) : float =
  match p with
  | Vibrato { rate; depth } -> semitones (depth *. sin (2. *. Float.pi *. rate *. t))
  | Jump { semitones = k; at } -> if t < at then 1. else semitones k
  | Arpeggio { semitones = []; _ } -> 1.
  | Arpeggio { semitones = l; step } -> semitones (List.nth l (int_of_float (t /. step) mod List.length l))

let tail ~(delay : float) ~(feedback : float) : float =
  if feedback <= 0. then delay else delay *. Float.ceil (log 0.001 /. log feedback)

let echo ~(delay : float) ~(feedback : float) (s : Signal.t) : Signal.t =
  let d = max 1 (Signal.samples delay) in
  let n = Array.length s + Signal.samples (tail ~delay ~feedback) in
  (* the delay line: the last d outputs, the oldest at [pos] *)
  let line = Array.make d 0. and pos = ref 0 in
  Array.init n (fun i ->
      let x = if i < Array.length s then s.(i) else 0. in
      let y = x +. (feedback *. line.(!pos)) in
      line.(!pos) <- y;
      pos := (!pos + 1) mod d;
      y)
