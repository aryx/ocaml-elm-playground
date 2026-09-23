(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Voicing.mli *)

type priority = Low | High | Last
type event = Begin of int | Change of int | End | Nothing

(* the keys held, the last pressed first *)
type t = { priority : priority; retrigger : bool; mutable held : int list }

let create ?(priority = Last) ?(retrigger = false) () : t = { priority; retrigger; held = [] }

let sounding (t : t) : int option =
  match (t.held, t.priority) with
  | [], _ -> None
  | last :: _, Last -> Some last
  | keys, Low -> Some (List.fold_left min max_int keys)
  | keys, High -> Some (List.fold_left max min_int keys)

(* from what sounded before to what sounds now *)
let event (t : t) (before : int option) : event =
  match (before, sounding t) with
  | _, None -> if before = None then Nothing else End
  | None, Some n -> Begin n
  | Some b, Some n when b = n -> Nothing
  | Some _, Some n -> if t.retrigger then Begin n else Change n

let press (t : t) (key : int) : event =
  let before = sounding t in
  t.held <- key :: List.filter (( <> ) key) t.held;
  event t before

let release (t : t) (key : int) : event =
  let before = sounding t in
  t.held <- List.filter (( <> ) key) t.held;
  event t before

type glide = { mutable pitch : float; mutable target : float }

let glide ?(note = 60.) () : glide = { pitch = note; target = note }
let glide_to (g : glide) (note : int) : unit = g.target <- float_of_int note
let pitch (g : glide) : float = g.pitch
let frequency (pitch : float) : float = 440. *. Float.pow 2. ((pitch -. 69.) /. 12.)

let fill_pitch (g : glide) ~(seconds : float) (out : Signal.t) : unit =
  let c = if seconds <= 0. then 1. else 1. -. exp (-1. /. (seconds *. float_of_int Signal.rate)) in
  for i = 0 to Array.length out - 1 do
    g.pitch <- g.pitch +. ((g.target -. g.pitch) *. c);
    out.(i) <- g.pitch
  done

let fill_frequency (g : glide) ~(seconds : float) (out : Signal.t) : unit =
  fill_pitch g ~seconds out;
  Array.iteri (fun i p -> out.(i) <- frequency p) out
