(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pitch_effect.mli *)

type t =
  | Vibrato of { rate : float; depth : float }
  | Jump of { semitones : float; at : float }
  | Arpeggio of { semitones : float list; step : float }

let semitones (k : float) : float = 2. ** (k /. 12.)

let factor (p : t) (t : float) : float =
  match p with
  | Vibrato { rate; depth } -> semitones (depth *. sin (2. *. Float.pi *. rate *. t))
  | Jump { semitones = k; at } -> if t < at then 1. else semitones k
  | Arpeggio { semitones = []; _ } -> 1.
  | Arpeggio { semitones = l; step } -> semitones (List.nth l (int_of_float (t /. step) mod List.length l))
