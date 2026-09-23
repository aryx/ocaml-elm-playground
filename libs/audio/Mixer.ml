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
type shot = { samples : Signal.stereo; mutable at : int }

(* a continuous voice: what it should play, its state, whether it was
 * kept since the last pull, its filter, and its pan (and the last
 * pull's, to glide from) *)
type continuous = {
  mutable voice : Synth.voice;
  mutable running : Synth.running;
  mutable kept : bool;
  mutable filter : Synth.filter option;
  memory : Filter.memory;
  mutable pan : float;
  mutable last_pan : float;
}

(* a loop: its samples, how far read (wrapping at the end), how many
 * samples of it have gone out in all (not wrapping: the loop's clock),
 * and whether it's being stopped *)
type looped = { sound : Signal.stereo; mutable pos : int; mutable played : int; mutable stopping : bool }

(* an instrument: what it is, and whether it's being stopped *)
type played_live = { live : Instrument.t; mutable ending : bool }

type t = {
  mutable shots : shot list;
  continuous : (string, continuous) Hashtbl.t;
  loops : (string, looped) Hashtbl.t;
  instruments : (string, played_live) Hashtbl.t;
}

let max_playing = 32
let stereo = ref true
let length (s : Signal.stereo) : int = Array.length s.left
let create () : t = { shots = []; continuous = Hashtbl.create 8; loops = Hashtbl.create 2; instruments = Hashtbl.create 2 }

let loop (m : t) (name : string) (sound : Signal.stereo) : unit =
  match Hashtbl.find_opt m.loops name with
  | Some l when not l.stopping -> ()
  | _ -> if length sound > 0 then Hashtbl.replace m.loops name { sound; pos = 0; played = 0; stopping = false }

let change (m : t) (name : string) (sound : Signal.stereo) : unit =
  match Hashtbl.find_opt m.loops name with
  | Some l when (not l.stopping) && length sound > 0 ->
      let pos = l.pos * length sound / length l.sound in
      Hashtbl.replace m.loops name { l with sound; pos = min pos (length sound - 1) }
  | _ -> loop m name sound

let stop (m : t) (name : string) : unit =
  Option.iter (fun l -> l.stopping <- true) (Hashtbl.find_opt m.loops name);
  Option.iter (fun i -> i.ending <- true) (Hashtbl.find_opt m.instruments name)

let looping (m : t) : string list = Hashtbl.fold (fun name _ acc -> name :: acc) m.loops [] |> List.sort compare

let instrument (m : t) (name : string) (live : Instrument.t) : unit =
  match Hashtbl.find_opt m.instruments name with
  | Some i when not i.ending -> ()
  | _ -> Hashtbl.replace m.instruments name { live; ending = false }

let instruments (m : t) : string list =
  Hashtbl.fold (fun name i acc -> if i.ending then acc else name :: acc) m.instruments [] |> List.sort compare

let play (m : t) (samples : Signal.stereo) : unit =
  (* the newest first; past max_playing, the oldest dropped *)
  m.shots <- List.filteri (fun i _ -> i < max_playing) ({ samples; at = 0 } :: m.shots)

let keep ?filter ?(pan = 0.) (m : t) (name : string) (voice : Synth.voice) : unit =
  match Hashtbl.find_opt m.continuous name with
  | Some c ->
      c.voice <- voice;
      c.filter <- filter;
      c.pan <- pan;
      c.kept <- true
  | None ->
      Hashtbl.replace m.continuous name
        { voice; running = Synth.start (); kept = true; filter; memory = Filter.silence (); pan; last_pan = pan }

let pull (m : t) (n : int) : Signal.stereo =
  let left = Array.make n 0. and right = Array.make n 0. in
  (* the one-shots' next samples *)
  m.shots
  |> List.iter (fun s ->
         let k = min n (length s.samples - s.at) in
         for i = 0 to k - 1 do
           left.(i) <- left.(i) +. s.samples.left.(s.at + i);
           right.(i) <- right.(i) +. s.samples.right.(s.at + i)
         done;
         s.at <- s.at + k);
  m.shots <- List.filter (fun s -> s.at < length s.samples) m.shots;
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
         let samples =
           match c.filter with
           | None -> samples
           | Some f ->
               let q = Filter.biquad f.kind ~cutoff:f.cutoff ~q:f.q in
               Array.map (Filter.step q c.memory) samples
         in
         (* the pan gliding from the last pull's to this one's, like the
          * volume (Synth.continue): no click when a source moves *)
         let (l0, r0) = Space.pan c.last_pan and (l1, r1) = Space.pan c.pan in
         Array.iteri
           (fun i x ->
             let a = float_of_int (i + 1) /. float_of_int n in
             left.(i) <- left.(i) +. (x *. (l0 +. ((l1 -. l0) *. a)));
             right.(i) <- right.(i) +. (x *. (r0 +. ((r1 -. r0) *. a))))
           samples;
         c.last_pan <- c.pan;
         c.kept <- false);
  List.iter (Hashtbl.remove m.continuous) !gone;
  (* the loops: read around and around; a stopped one fades out over
   * this pull, then is gone *)
  let stopped = ref [] in
  m.loops
  |> Hashtbl.iter (fun name l ->
         let len = length l.sound in
         for i = 0 to n - 1 do
           let fade = if l.stopping then 1. -. (float_of_int i /. float_of_int n) else 1. in
           left.(i) <- left.(i) +. (fade *. l.sound.left.(l.pos));
           right.(i) <- right.(i) +. (fade *. l.sound.right.(l.pos));
           l.pos <- (l.pos + 1) mod len
         done;
         l.played <- l.played + n;
         if l.stopping then stopped := name :: !stopped);
  List.iter (Hashtbl.remove m.loops) !stopped;
  (* the instruments: a block each; a stopped one fades out over this
   * pull, like a loop, then is gone *)
  let ended = ref [] in
  m.instruments
  |> Hashtbl.iter (fun name i ->
         let block : Signal.stereo = { left = Array.make n 0.; right = Array.make n 0. } in
         i.live.fill block;
         for k = 0 to n - 1 do
           let fade = if i.ending then 1. -. (float_of_int k /. float_of_int n) else 1. in
           left.(k) <- left.(k) +. (fade *. block.left.(k));
           right.(k) <- right.(k) +. (fade *. block.right.(k))
         done;
         if i.ending then ended := name :: !ended);
  List.iter (Hashtbl.remove m.instruments) !ended;
  let out : Signal.stereo = { left = Mix.limit ~soft:true left; right = Mix.limit ~soft:true right } in
  if !stereo then out else Signal.both (Signal.mono out)

let playing (m : t) : int * int = (List.length m.shots, Hashtbl.length m.continuous)
let played (m : t) (name : string) : int option = Option.map (fun l -> l.played) (Hashtbl.find_opt m.loops name)
