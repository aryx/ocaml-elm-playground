(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sequencer.mli *)

type step = { note : int option; accent : bool; slide : bool }

let rest = { note = None; accent = false; slide = false }
let note ?(accent = false) ?(slide = false) (n : int) : step = { note = Some n; accent; slide }

type event = Note_on of { note : int; accent : bool; glide : bool } | Note_off

let rate = float_of_int Signal.rate
let samples_per_step (bpm : float) : float = 60. /. (bpm *. 4.) *. rate

type t = {
  mutable pattern : step array;
  mutable bpm : float;
  mutable running : bool;
  mutable clock : int; (* samples since the start *)
  mutable next : int; (* the step to begin next *)
  mutable next_at : float; (* its exact time, in samples *)
  mutable off_at : float option; (* the gate's closing, exact *)
  mutable sliding : bool; (* the step sounding slides into the next *)
  mutable sounding : int; (* the step sounding, for a panel *)
}

let create ?(bpm = 120.) (pattern : step array) : t =
  { pattern; bpm; running = false; clock = 0; next = 0; next_at = 0.; off_at = None; sliding = false; sounding = 0 }

let set_pattern (t : t) (p : step array) : unit = t.pattern <- p
let set_bpm (t : t) (bpm : float) : unit = t.bpm <- Float.max 20. bpm
let running (t : t) : bool = t.running
let step (t : t) : int = t.sounding

let start (t : t) : unit =
  t.running <- true;
  t.next <- 0;
  t.next_at <- float_of_int t.clock;
  t.off_at <- None;
  t.sliding <- false

let stop (t : t) : unit = t.running <- false

(* the first sample at or after an exact time *)
let sample_of (time : float) : int = int_of_float (Float.ceil (time -. 1e-9))

let advance (t : t) (n : int) (f : int -> event -> unit) : unit =
  let block_end = t.clock + n in
  let continue = ref t.running in
  while !continue do
    let len = Array.length t.pattern in
    let start = sample_of t.next_at in
    let off = Option.map sample_of t.off_at in
    match off with
    (* the gate closes before the next step begins *)
    | Some o when o <= start && o < block_end ->
        f (o - t.clock) Note_off;
        t.off_at <- None
    | _ when start < block_end && len > 0 ->
        let k = t.next mod len in
        let s = t.pattern.(k) in
        let sps = samples_per_step t.bpm in
        (match s.note with
        | Some n ->
            f (start - t.clock) (Note_on { note = n; accent = s.accent; glide = t.sliding });
            let next_is_note = t.pattern.((k + 1) mod len).note <> None in
            t.sliding <- s.slide && next_is_note;
            t.off_at <- (if t.sliding then None else Some (t.next_at +. (sps /. 2.)))
        | None ->
            (* a rest after a slide: the held gate closes now *)
            if t.sliding then f (start - t.clock) Note_off;
            t.sliding <- false);
        t.sounding <- k;
        t.next <- t.next + 1;
        t.next_at <- t.next_at +. sps
    | _ -> continue := false
  done;
  t.clock <- block_end
