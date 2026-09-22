(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pluck.mli *)

let period (frequency : float) : int =
  max 2 (int_of_float (Float.round ((float_of_int Signal.rate /. frequency) -. 0.5)))

let render ?(decay = 0.996) ~(frequency : float) (seconds : float) : Signal.t =
  let p = period frequency in
  (* the pluck: the line filled with noise, -1 or 1, from 5000 steps into
   * the LFSR's sequence: from 1, its first bits are mostly 0s (the first
   * 200 average -0.49, 5000 steps in -0.03) *)
  let register = ref 1 in
  for _ = 1 to 5000 do
    register := Noise.step Long !register
  done;
  let noise =
    Array.init p (fun _ ->
        register := Noise.step Long !register;
        if !register land 1 = 1 then 1. else -1.)
  in
  (* less its average: the averaging keeps a constant (0 Hz passes a
   * low-pass), and the string would ring around an offset *)
  let mean = Array.fold_left ( +. ) 0. noise /. float_of_int p in
  let line = Array.map (fun x -> x -. mean) noise in
  let pos = ref 0 in
  Array.init (Signal.samples seconds) (fun _ ->
      let x = line.(!pos) and next = line.((!pos + 1) mod p) in
      line.(!pos) <- decay *. 0.5 *. (x +. next);
      pos := (!pos + 1) mod p;
      x)
