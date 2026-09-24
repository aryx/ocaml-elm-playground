(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tape.mli *)

type transport = Stopped | Playing | Recording of int

type t = {
  reel : Signal.t array; (* a track each *)
  levels : float array;
  mutable head : float;
  mutable speed : float;
  mutable transport : transport;
  mutable loop : (int * int) option;
  mutable memory : Signal.t; (* what was lifted *)
}

let create ?(seconds = 360.) ?(tracks = 4) () : t =
  let n = Signal.samples seconds in
  {
    reel = Array.init tracks (fun _ -> Array.make n 0.);
    levels = Array.make tracks 1.;
    head = 0.;
    speed = 1.;
    transport = Stopped;
    loop = None;
    memory = [||];
  }

let tracks (t : t) : int = Array.length t.reel
let length (t : t) : int = Array.length t.reel.(0)
let track (t : t) (k : int) : Signal.t = t.reel.(k)
let head (t : t) : float = t.head
let set_head (t : t) (h : float) : unit = t.head <- Float.max 0. (Float.min (float_of_int (length t - 1)) h)
let speed (t : t) : float = t.speed
let set_speed (t : t) (s : float) : unit = t.speed <- s
let play (t : t) : unit = t.transport <- Playing
let record (t : t) (k : int) : unit = if k >= 0 && k < tracks t then t.transport <- Recording k
let stop (t : t) : unit = t.transport <- Stopped
let moving (t : t) : bool = t.transport <> Stopped
let recording (t : t) : int option = match t.transport with Recording k -> Some k | _ -> None
let set_level (t : t) (k : int) (level : float) : unit = t.levels.(k) <- Float.max 0. (Float.min 1. level)
let set_loop (t : t) (l : (int * int) option) : unit = t.loop <- l

(* [x] added at a position between two samples, spread over both *)
let write (track : Signal.t) (position : float) (x : float) : unit =
  let i = Float.to_int (Float.floor position) in
  let frac = position -. float_of_int i in
  let n = Array.length track in
  if i >= 0 && i < n then track.(i) <- track.(i) +. ((1. -. frac) *. x);
  if i + 1 >= 0 && i + 1 < n && frac > 0. then track.(i + 1) <- track.(i + 1) +. (frac *. x)

let process (t : t) ~(input : Signal.t) (out : Signal.t) : unit =
  let last = float_of_int (length t - 1) in
  Array.iteri
    (fun i _ ->
      match t.transport with
      | Stopped -> out.(i) <- 0.
      | Playing | Recording _ ->
          (* the tracks under the head, mixed *)
          let x = ref 0. in
          Array.iteri (fun k track -> x := !x +. (t.levels.(k) *. Resample.read Linear track t.head)) t.reel;
          out.(i) <- !x;
          (match t.transport with Recording k when i < Array.length input -> write t.reel.(k) t.head input.(i) | _ -> ());
          (* the tape moves; a loop sends it back; its ends stop it *)
          t.head <- t.head +. t.speed;
          (match t.loop with
          | Some (a, b) when t.speed > 0. && t.head >= float_of_int b -> t.head <- t.head -. float_of_int (b - a)
          | Some (a, b) when t.speed < 0. && t.head < float_of_int a -> t.head <- t.head +. float_of_int (b - a)
          | _ -> ());
          if t.head < 0. || t.head > last then begin
            t.head <- Float.max 0. (Float.min last t.head);
            t.transport <- Stopped
          end)
    out

let lift (t : t) (k : int) ~(from : int) ~(until : int) : unit =
  let from = max 0 from and until = min (length t) until in
  if until > from then begin
    t.memory <- Array.sub t.reel.(k) from (until - from);
    Array.fill t.reel.(k) from (until - from) 0.
  end

let drop (t : t) (k : int) : unit =
  let at = Float.to_int (Float.round t.head) in
  Array.iteri (fun i x -> if at + i < length t then t.reel.(k).(at + i) <- t.reel.(k).(at + i) +. x) t.memory
