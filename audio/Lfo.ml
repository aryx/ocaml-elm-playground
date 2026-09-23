(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Lfo.mli *)

type shape = Sine | Triangle | Square | Saw_up | Saw_down | Sample_and_hold

let shapes = [ Sine; Triangle; Square; Saw_up; Saw_down; Sample_and_hold ]

let name = function
  | Sine -> "sine"
  | Triangle -> "triangle"
  | Square -> "square"
  | Saw_up -> "saw up"
  | Saw_down -> "saw down"
  | Sample_and_hold -> "sample & hold"

let value (shape : shape) (phase : float) : float =
  match shape with
  | Sine -> Oscillator.wave Sine phase
  | Triangle -> Oscillator.wave Triangle phase
  | Square -> Oscillator.wave Square phase
  | Saw_up -> Oscillator.wave Sawtooth phase
  | Saw_down -> -.Oscillator.wave Sawtooth phase
  | Sample_and_hold -> 0.

(* the phase; the random generator's state, and the value it last drew,
 * held *)
type t = { mutable phase : float; mutable random : int; mutable held : float }

let create ?(seed = 0) () : t =
  let random = Noise.lcg seed in
  { phase = 0.; random; held = Noise.uniform random }

let fill (t : t) (shape : shape) ~(rate : float) (out : Signal.t) : unit =
  let dphase = rate /. float_of_int Signal.rate in
  for i = 0 to Array.length out - 1 do
    out.(i) <- (match shape with Sample_and_hold -> t.held | _ -> value shape t.phase);
    let next = t.phase +. dphase in
    (* a new period: sample and hold draws its next value *)
    if next >= 1. then (
      t.random <- Noise.lcg t.random;
      t.held <- Noise.uniform t.random);
    t.phase <- next -. Float.floor next
  done

let unipolar (x : float) : float = (x +. 1.) /. 2.
let of_tempo ~(bpm : float) ~(beats : float) : float = bpm /. 60. /. beats
