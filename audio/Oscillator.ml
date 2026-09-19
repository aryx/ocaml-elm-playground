(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Oscillator.mli *)

type waveform = Sine | Square | Triangle | Sawtooth

let waveforms = [ Sine; Square; Triangle; Sawtooth ]
let name = function Sine -> "sine" | Square -> "square" | Triangle -> "triangle" | Sawtooth -> "sawtooth"

let wave (w : waveform) (phase : float) : float =
  match w with
  | Sine -> sin (2. *. Float.pi *. phase)
  | Square -> if phase < 0.5 then 1. else -1.
  (* 0 up to 1 at a quarter, down to -1 at three quarters, back to 0:
   * in step with the sine *)
  | Triangle -> if phase < 0.25 then 4. *. phase else if phase < 0.75 then 2. -. (4. *. phase) else (4. *. phase) -. 4.
  | Sawtooth -> (2. *. phase) -. 1.

type t = { waveform : waveform; frequency : float; phase : float }

let make (waveform : waveform) (frequency : float) : t = { waveform; frequency; phase = 0. }

let next (o : t) : float * t =
  let phase = o.phase +. (o.frequency /. float_of_int Signal.rate) in
  (wave o.waveform o.phase, { o with phase = phase -. Float.floor phase })

let render (w : waveform) ~(frequency : float) (seconds : float) : Signal.t =
  let o = ref (make w frequency) in
  Array.init (Signal.samples seconds) (fun _ ->
      let (x, o') = next !o in
      o := o';
      x)
