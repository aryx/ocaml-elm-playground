(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Polyphony.mli *)

type voice = { release : unit -> unit; fill : Signal.t -> unit; silent : unit -> bool }

(* a voice sounding: its key, whether the key is still held *)
type sounding = { key : int; voice : voice; mutable held : bool }

type t = {
  limit : int option;
  mutable sounding : sounding list; (* the oldest first *)
  mutable scratch : Signal.t; (* a voice's block, before it's added *)
}

let create ?voices () : t = { limit = voices; sounding = []; scratch = [||] }

(* the oldest released, else the oldest *)
let steal (t : t) : unit =
  let victim = match List.find_opt (fun s -> not s.held) t.sounding with Some s -> Some s | None -> List.nth_opt t.sounding 0 in
  match victim with Some v -> t.sounding <- List.filter (fun s -> s != v) t.sounding | None -> ()

let release (t : t) (key : int) : unit =
  List.iter
    (fun s ->
      if s.key = key && s.held then begin
        s.held <- false;
        s.voice.release ()
      end)
    t.sounding

(* a key already held (a keyboard's glitch, two presses without a
 * release) is let go first: one voice held per key *)
let press (t : t) (key : int) (voice : voice) : unit =
  release t key;
  (match t.limit with Some n when n > 0 && List.length t.sounding >= n -> steal t | _ -> ());
  t.sounding <- t.sounding @ [ { key; voice; held = true } ]

let fill (t : t) (out : Signal.t) : unit =
  let n = Array.length out in
  if Array.length t.scratch <> n then t.scratch <- Array.make n 0.;
  Array.fill out 0 n 0.;
  List.iter
    (fun s ->
      s.voice.fill t.scratch;
      for i = 0 to n - 1 do
        out.(i) <- out.(i) +. t.scratch.(i)
      done)
    t.sounding;
  (* the released voices whose sound is over *)
  t.sounding <- List.filter (fun s -> s.held || not (s.voice.silent ())) t.sounding

let voices (t : t) : int = List.length t.sounding
let held (t : t) : int list = List.sort compare (List.filter_map (fun s -> if s.held then Some s.key else None) t.sounding)
let rate = float_of_int Signal.rate

let sine ~(adsr : Envelope.t) (frequency : float) (velocity : float) : voice =
  let envelope = Envelope.start () and phase = ref 0. and levels = ref [||] in
  Envelope.gate_on envelope;
  let fill (out : Signal.t) =
    let n = Array.length out in
    if Array.length !levels <> n then levels := Array.make n 0.;
    Envelope.fill Exponential adsr envelope !levels;
    for i = 0 to n - 1 do
      out.(i) <- velocity *. !levels.(i) *. sin (2. *. Float.pi *. !phase);
      phase := !phase +. (frequency /. rate);
      if !phase >= 1. then phase := !phase -. 1.
    done
  in
  { release = (fun () -> Envelope.gate_off envelope); fill; silent = (fun () -> Envelope.stage envelope = Idle) }
