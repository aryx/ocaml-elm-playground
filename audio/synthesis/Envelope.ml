(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Envelope.mli *)

type t = { attack : float; decay : float; sustain : float; release : float }

let percussive ~(attack : float) ~(decay : float) : t = { attack; decay; sustain = 0.; release = 0. }

(* the attack, decay, sustain part: as if the note were held forever *)
let pressed (e : t) (time : float) : float =
  if time < 0. then 0.
  else if time < e.attack then time /. e.attack
  else if time < e.attack +. e.decay then 1. -. ((1. -. e.sustain) *. (time -. e.attack) /. e.decay)
  else e.sustain

let level (e : t) ~(held : float) (time : float) : float =
  if time < held then pressed e time
  else
    (* the release, from wherever the volume was when let go *)
    let from = pressed e held in
    if e.release <= 0. || time >= held +. e.release then 0. else from *. (1. -. ((time -. held) /. e.release))

let duration (e : t) ~(held : float) : float = held +. e.release

let apply (e : t) ~(held : float) (samples : Signal.t) : Signal.t =
  Array.mapi (fun i x -> x *. level e ~held (float_of_int i /. float_of_int Signal.rate)) samples

type curve = Linear | Exponential
type stage = Idle | Attack | Decay | Sustain | Release

(* the stage and the level; for a straight release, its slope, from
 * where it started *)
type running = { mutable stage : stage; mutable level : float; mutable release_from : float }

let start () : running = { stage = Idle; level = 0.; release_from = 0. }
let gate_on (r : running) : unit = r.stage <- Attack

let gate_off (r : running) : unit =
  if r.stage <> Idle then (
    r.stage <- Release;
    r.release_from <- r.level)

let stage (r : running) : stage = r.stage
let current (r : running) : float = r.level

(* a time in samples; under one sample, at once *)
let samples (seconds : float) : float = Float.max 1. (seconds *. float_of_int Signal.rate)

(* a one-pole's step taking [seconds] / [per] to go 1 / e of the way *)
let coefficient ~(per : float) (seconds : float) : float = 1. -. exp (-.per /. samples seconds)

let silent = 0.00001

let fill (curve : curve) (e : t) (r : running) (out : Signal.t) : unit =
  let sustain = Float.min 1. (Float.max 0. e.sustain) in
  for i = 0 to Array.length out - 1 do
    (match (r.stage, curve) with
    | Idle, _ -> r.level <- 0.
    | Attack, Linear -> r.level <- r.level +. (1. /. samples e.attack)
    (* aiming at 1.5, arriving at 1 in the attack's time *)
    | Attack, Exponential -> r.level <- r.level +. ((1.5 -. r.level) *. coefficient ~per:(log 3.) e.attack)
    (* the decay and the sustain move to the sustain level, which may
     * change: a knob turned while the note is held *)
    | (Decay | Sustain), Linear ->
        let step = (1. -. sustain) /. samples e.decay in
        r.level <- (if r.level > sustain then Float.max sustain (r.level -. step) else Float.min sustain (r.level +. step))
    | (Decay | Sustain), Exponential -> r.level <- r.level +. ((sustain -. r.level) *. coefficient ~per:(log 1000.) e.decay)
    | Release, Linear -> r.level <- Float.max 0. (r.level -. (r.release_from /. samples e.release))
    | Release, Exponential -> r.level <- r.level *. (1. -. coefficient ~per:(log 1000.) e.release));
    (* the stage changes *)
    (match r.stage with
    | Attack when r.level >= 1. ->
        r.level <- 1.;
        r.stage <- Decay
    | Decay when Float.abs (r.level -. sustain) < silent -> r.stage <- Sustain
    | Release when r.level < silent ->
        r.level <- 0.;
        r.stage <- Idle
    | _ -> ());
    out.(i) <- r.level
  done
