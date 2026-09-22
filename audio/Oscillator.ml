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

let polyblep ~(dt : float) (t : float) : float =
  if t < dt then
    let u = t /. dt in
    (2. *. u) -. (u *. u) -. 1.
  else if t > 1. -. dt then
    let u = (t -. 1.) /. dt in
    (u *. u) +. (2. *. u) +. 1.
  else 0.

let wave_band_limited (w : waveform) ~(dt : float) (phase : float) : float =
  match w with
  | Sine | Triangle -> wave w phase
  (* up at 0, down at 0.5 *)
  | Square ->
      let down = phase +. 0.5 in
      wave w phase +. polyblep ~dt phase -. polyblep ~dt (down -. Float.floor down)
  (* down at 0 *)
  | Sawtooth -> wave w phase -. polyblep ~dt phase

type t = { waveform : waveform; frequency : float; phase : float }

let make (waveform : waveform) (frequency : float) : t = { waveform; frequency; phase = 0. }

let next ?(band_limited = false) (o : t) : float * t =
  let dt = o.frequency /. float_of_int Signal.rate in
  let phase = o.phase +. dt in
  let x = if band_limited then wave_band_limited o.waveform ~dt o.phase else wave o.waveform o.phase in
  (x, { o with phase = phase -. Float.floor phase })

let render ?band_limited (w : waveform) ~(frequency : float) (seconds : float) : Signal.t =
  let o = ref (make w frequency) in
  Array.init (Signal.samples seconds) (fun _ ->
      let (x, o') = next ?band_limited !o in
      o := o';
      x)
