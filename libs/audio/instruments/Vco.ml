(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Vco.mli *)

type shape = Sine | Triangle | Sawtooth | Pulse

let shapes = [ Sine; Triangle; Sawtooth; Pulse ]
let name = function Sine -> "sine" | Triangle -> "triangle" | Sawtooth -> "sawtooth" | Pulse -> "pulse"

(* the phase; for a master, where in each step of the last block it
 * wrapped (the fraction of the step, or -1); for a slave, the second
 * half of a restart's correction, due at the next sample (h, d) *)
type t = { mutable phase : float; mutable wraps : float array; mutable due : (float * float) option }

let create () : t = { phase = 0.; wraps = [||]; due = None }

let naive (shape : shape) ~(width : float) (phase : float) : float =
  match shape with
  | Sine -> Oscillator.wave Sine phase
  | Triangle -> Oscillator.wave Triangle phase
  | Sawtooth -> Oscillator.wave Sawtooth phase
  | Pulse -> Oscillator.pulse ~width phase

let band_limited_wave (shape : shape) ~(width : float) ~(dt : float) (phase : float) : float =
  match shape with
  | Sine -> Oscillator.wave Sine phase
  | Triangle -> Oscillator.wave_band_limited Triangle ~dt phase
  | Sawtooth -> Oscillator.wave_band_limited Sawtooth ~dt phase
  | Pulse -> Oscillator.pulse_band_limited ~width ~dt phase

(* the correction of the wave's own jump at phase 0 (the sawtooth's
 * down, the pulse's up), to leave out where a restart replaced it *)
let own_jump (shape : shape) ~(dt : float) (phase : float) : float =
  match shape with
  | Sawtooth -> -.Oscillator.polyblep ~dt phase
  | Pulse -> Oscillator.polyblep ~dt phase
  | Sine | Triangle -> 0.

let wrap (x : float) : float = x -. Float.floor x

type restart = Fraction | At_sample

let run ~(band_limited : bool) ~(restart : restart) ?width ?sync (t : t) (shape : shape) ~(frequency : Signal.t) (out : Signal.t) :
    unit =
  let n = Array.length out in
  if Array.length t.wraps <> n then t.wraps <- Array.make n (-1.);
  for i = 0 to n - 1 do
    let dt = frequency.(i) /. float_of_int Signal.rate in
    let width = match width with None -> 0.5 | Some w -> Float.min 0.99 (Float.max 0.01 w.(i)) in
    let p = t.phase in
    let x = ref (if band_limited then band_limited_wave shape ~width ~dt p else naive shape ~width p) in
    (* the second half of the last step's restart *)
    (match t.due with
    | Some (h, d) ->
        x := !x +. (h /. 2. *. ((2. *. d) -. (d *. d) -. 1.)) -. own_jump shape ~dt p;
        t.due <- None
    | None -> ());
    (* restarted before the next sample? at [f], a fraction of the step *)
    let restart_at = match sync with Some m when i < Array.length m.wraps -> m.wraps.(i) | _ -> -1. in
    if restart_at >= 0. then (
      let f = match restart with Fraction -> restart_at | At_sample -> 1. in
      let d = 1. -. f in
      let before = naive shape ~width (wrap (p +. (f *. dt))) and after = naive shape ~width 0. in
      let h = after -. before in
      if band_limited then (
        (* its own jump, if it was coming in this step, won't: the
         * restart comes first *)
        if p +. (f *. dt) < 1. then x := !x -. own_jump shape ~dt p;
        x := !x +. (h /. 2. *. d *. d);
        t.due <- Some (h, d));
      t.wraps.(i) <- f;
      t.phase <- d *. dt)
    else (
      t.wraps.(i) <- (if p +. dt >= 1. then (1. -. p) /. dt else -1.);
      t.phase <- wrap (p +. dt));
    out.(i) <- !x
  done

let fill ?(band_limited = true) ?width ?sync t shape ~frequency out =
  run ~band_limited ~restart:Fraction ?width ?sync t shape ~frequency out

let fill_sync_at_sample ?width ~sync t shape ~frequency out =
  run ~band_limited:true ~restart:At_sample ?width ~sync t shape ~frequency out
