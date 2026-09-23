(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Instrument.mli *)

type t = {
  note_on : int -> float -> unit;
  note_off : int -> unit;
  set : string -> float -> unit;
  fill : Signal.stereo -> unit;
}

let gate_samples = 0.005 *. float_of_int Signal.rate

(* the sine's state: the key sounding, its pitch and velocity; the
 * oscillator's phase (0 to 1); the gate's level, ramping to the
 * velocity or to 0; the volume knob, and its value at the last block,
 * ramped from *)
type sine = {
  mutable key : int option;
  mutable frequency : float;
  mutable velocity : float;
  mutable phase : float;
  mutable gate : float;
  mutable volume : float;
  mutable last_volume : float;
}

let sine () : t =
  let s = { key = None; frequency = 440.; velocity = 1.; phase = 0.; gate = 0.; volume = 0.5; last_volume = 0.5 } in
  let note_on key velocity =
    s.key <- Some key;
    s.frequency <- Music.midi_frequency key;
    s.velocity <- velocity
  in
  let note_off key = if s.key = Some key then s.key <- None in
  let set name value = if name = "volume" then s.volume <- Float.min 1. (Float.max 0. value) in
  let fill (out : Signal.stereo) =
    let n = Array.length out.left in
    let target = if s.key = None then 0. else s.velocity in
    let step = 1. /. gate_samples in
    let dphase = s.frequency /. float_of_int Signal.rate in
    for i = 0 to n - 1 do
      (* the gate: towards its target, a 5 ms ramp's step at a time *)
      s.gate <- (if s.gate < target then Float.min target (s.gate +. step) else Float.max target (s.gate -. step));
      (* the knob: from the last block's value to this one's *)
      let volume = s.last_volume +. ((s.volume -. s.last_volume) *. float_of_int (i + 1) /. float_of_int n) in
      let x = volume *. s.gate *. sin (2. *. Float.pi *. s.phase) in
      out.left.(i) <- x;
      out.right.(i) <- x;
      s.phase <- Float.rem (s.phase +. dphase) 1.
    done;
    s.last_volume <- s.volume
  in
  { note_on; note_off; set; fill }
