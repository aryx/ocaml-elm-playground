(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Noise.mli *)

type mode = Long | Short

let step (mode : mode) (r : int) : int =
  let tap = match mode with Long -> 1 | Short -> 6 in
  let feedback = (r land 1) lxor ((r lsr tap) land 1) in
  (r lsr 1) lor (feedback lsl 14)

let period (mode : mode) : int =
  let rec go n r = if r = 1 && n > 0 then n else go (n + 1) (step mode r) in
  go 0 1

let render ?(mode = Long) ~(rate : float) (seconds : float) : Signal.t =
  let r = ref 1 and clock = ref 0. in
  Array.init (Signal.samples seconds) (fun _ ->
      let out = if !r land 1 = 1 then 1. else -1. in
      (* rate / 44,100 steps per sample: a step each time the clock
       * passes a whole number *)
      clock := !clock +. (rate /. float_of_int Signal.rate);
      while !clock >= 1. do
        r := step mode !r;
        clock := !clock -. 1.
      done;
      out)
