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

type step = { note : int option; accent : bool; slide : bool; locks : (string * float) list }

let rest = { note = None; accent = false; slide = false; locks = [] }
let note ?(accent = false) ?(slide = false) (n : int) : step = { note = Some n; accent; slide; locks = [] }
let lock (s : step) (name : string) (v : float) : step = { s with locks = (name, v) :: List.remove_assoc name s.locks }
let unlock (s : step) : step = { s with locks = [] }

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
  mutable block_start : int; (* the block last advanced, for [position] *)
}

let create ?(bpm = 120.) (pattern : step array) : t =
  { pattern; bpm; running = false; clock = 0; next = 0; next_at = 0.; off_at = None; sliding = false; sounding = 0; block_start = 0 }

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
  t.block_start <- t.clock;
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

(*****************************************************************************)
(* Parameter locks *)
(*****************************************************************************)

type locks = Per_step | Points of float

let lock_value (locks : locks) (pattern : step array) (name : string) (position : float) : float option =
  let len = Array.length pattern in
  if len = 0 then None
  else
    let p = Float.rem position (float_of_int len) in
    let p = if p < 0. then p +. float_of_int len else p in
    match locks with
    | Per_step -> List.assoc_opt name pattern.(min (len - 1) (Float.to_int p)).locks
    | Points smoothing -> (
        (* the lock points, in order: (step, value) *)
        let points = List.concat (List.init len (fun i -> match List.assoc_opt name pattern.(i).locks with Some v -> [ (float_of_int i, v) ] | None -> [])) in
        match points with
        | [] -> None
        | [ (_, v) ] -> Some v
        | first :: _ ->
            let last = List.nth points (List.length points - 1) in
            (* a, the point at or before p, b the one after: round the
             * pattern's end if need be *)
            let a = List.fold_left (fun a (i, v) -> if i <= p then (i, v) else a) (fst last -. float_of_int len, snd last) points in
            let b = match List.find_opt (fun (i, _) -> i > p) points with Some b -> b | None -> (fst first +. float_of_int len, snd first) in
            let (ai, av), (bi, bv) = (a, b) in
            (* held, then a line over the last [smoothing] of the way *)
            let start = bi -. (smoothing *. (bi -. ai)) in
            if p < start then Some av else Some (av +. ((bv -. av) *. (p -. start) /. (bi -. start))))

let position (t : t) (offset : int) : float option =
  if (not t.running) || t.next = 0 then None
  else
    (* the last step begun, [next - 1], began at [next_at - sps]; the
     * steps in a block all [sps] apart *)
    let sps = samples_per_step t.bpm in
    let s = float_of_int (t.block_start + offset) in
    let p = float_of_int (t.next - 1) -. ((t.next_at -. sps -. s) /. sps) in
    if p < 0. then None else Some p

let locked (t : t) (locks : locks) (name : string) (offset : int) : float option =
  Option.bind (position t offset) (lock_value locks t.pattern name)
