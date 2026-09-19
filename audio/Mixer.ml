(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mixer.mli *)

(* a one-shot: its samples, and how far it's been read *)
type shot = { samples : Signal.t; mutable at : int }

(* a continuous voice: what it should play, its state, and whether it
 * was kept since the last pull *)
type continuous = { mutable voice : Synth.voice; mutable running : Synth.running; mutable kept : bool }

(* a loop: its samples, how far read, and whether it's being stopped *)
type looped = { sound : Signal.t; mutable pos : int; mutable stopping : bool }

type t = { mutable shots : shot list; continuous : (string, continuous) Hashtbl.t; loops : (string, looped) Hashtbl.t }

let max_playing = 32
let create () : t = { shots = []; continuous = Hashtbl.create 8; loops = Hashtbl.create 2 }

let loop (m : t) (name : string) (sound : Signal.t) : unit =
  match Hashtbl.find_opt m.loops name with
  | Some l when not l.stopping -> ()
  | _ -> if Array.length sound > 0 then Hashtbl.replace m.loops name { sound; pos = 0; stopping = false }

let stop (m : t) (name : string) : unit = Option.iter (fun l -> l.stopping <- true) (Hashtbl.find_opt m.loops name)
let looping (m : t) : string list = Hashtbl.fold (fun name _ acc -> name :: acc) m.loops [] |> List.sort compare

let play (m : t) (samples : Signal.t) : unit =
  (* the newest first; past max_playing, the oldest dropped *)
  m.shots <- List.filteri (fun i _ -> i < max_playing) ({ samples; at = 0 } :: m.shots)

let keep (m : t) (name : string) (voice : Synth.voice) : unit =
  match Hashtbl.find_opt m.continuous name with
  | Some c ->
      c.voice <- voice;
      c.kept <- true
  | None -> Hashtbl.replace m.continuous name { voice; running = Synth.start (); kept = true }

let pull (m : t) (n : int) : Signal.t =
  let out = Array.make n 0. in
  (* the one-shots' next samples *)
  m.shots
  |> List.iter (fun s ->
         let k = min n (Array.length s.samples - s.at) in
         for i = 0 to k - 1 do
           out.(i) <- out.(i) +. s.samples.(s.at + i)
         done;
         s.at <- s.at + k);
  m.shots <- List.filter (fun s -> s.at < Array.length s.samples) m.shots;
  (* the continuous ones: going on, or fading out if not kept *)
  let gone = ref [] in
  m.continuous
  |> Hashtbl.iter (fun name c ->
         let samples =
           if c.kept then (
             let (samples, running) = Synth.continue c.running c.voice n in
             c.running <- running;
             samples)
           else (
             gone := name :: !gone;
             Synth.release c.running c.voice n)
         in
         Array.iteri (fun i x -> out.(i) <- out.(i) +. x) samples;
         c.kept <- false);
  List.iter (Hashtbl.remove m.continuous) !gone;
  (* the loops: read around and around; a stopped one fades out over
   * this pull, then is gone *)
  let stopped = ref [] in
  m.loops
  |> Hashtbl.iter (fun name l ->
         let len = Array.length l.sound in
         for i = 0 to n - 1 do
           let fade = if l.stopping then 1. -. (float_of_int i /. float_of_int n) else 1. in
           out.(i) <- out.(i) +. (fade *. l.sound.(l.pos));
           l.pos <- (l.pos + 1) mod len
         done;
         if l.stopping then stopped := name :: !stopped);
  List.iter (Hashtbl.remove m.loops) !stopped;
  Mix.limit ~soft:true out

let playing (m : t) : int * int = (List.length m.shots, Hashtbl.length m.continuous)
