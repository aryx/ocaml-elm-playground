(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Signal.mli *)

type t = float array

let rate = 44100
let nyquist = float_of_int rate /. 2.
let samples (seconds : float) : int = int_of_float (seconds *. float_of_int rate)
let period_in_samples (frequency : float) : float = float_of_int rate /. frequency

let alias (frequency : float) : float =
  (* the spectrum repeats every [rate], and mirrors around each
   * multiple of it *)
  let f = Float.rem (Float.abs frequency) (float_of_int rate) in
  if f <= nyquist then f else float_of_int rate -. f

let of_function (seconds : float) (f : float -> float) : t =
  Array.init (samples seconds) (fun i -> f (float_of_int i /. float_of_int rate))

let to_int16 (x : float) : int = max (-32768) (min 32767 (int_of_float (Float.round (x *. 32767.))))
