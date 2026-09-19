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
