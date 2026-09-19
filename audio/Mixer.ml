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

type t = { mutable shots : shot list; continuous : (string, continuous) Hashtbl.t }

let max_playing = 32
let create () : t = { shots = []; continuous = Hashtbl.create 8 }

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
  Mix.limit ~soft:true out

let playing (m : t) : int * int = (List.length m.shots, Hashtbl.length m.continuous)
