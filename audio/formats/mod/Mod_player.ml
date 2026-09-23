(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mod_player.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* a channel: its Paula, the row's cell, the note's period (before the
 * vibrato and the arpeggio bend it), the volume, and the effects'
 * memories (a 300 or a 400 goes on with the last speeds) *)
type channel = {
  paula : Paula.t;
  mutable cell : Mod.cell;
  mutable instrument : int; (* the last one given, 1-based; 0: none yet *)
  mutable finetune : int;
  mutable period : int;
  mutable volume : int;
  mutable target : int;
  mutable slide_speed : int;
  mutable vibrato_speed : int;
  mutable vibrato_depth : int;
  mutable vibrato_pos : int;
}

type t = {
  mutable song : Mod.song;
  mutable samples : Signal.t array; (* the instruments' samples, as numbers *)
  channels : channel array;
  loop : bool;
  mutable reading : Paula.reading;
  mutable separation : float;
  mutable position : int;
  mutable row : int;
  mutable tick : int;
  mutable speed : int;
  mutable tempo : int;
  mutable until_tick : float; (* samples left before the next tick *)
  (* a Bxx or a Dxy, done when the row ends: the position, the row *)
  mutable next : (int * int) option;
  mutable finished : bool;
}

let tick_samples (tempo : int) : float = float_of_int Signal.rate *. 2.5 /. float_of_int tempo

(* the instruments' samples as numbers, read once *)
let samples_of (song : Mod.song) : Signal.t array =
  Array.map (fun (i : Mod.instrument) -> Array.init (String.length i.data) (Mod.sample i)) song.instruments

let create ?(loop = true) (song : Mod.song) : t =
  let channel () =
    {
      paula = Paula.create ();
      cell = Mod.empty_cell;
      instrument = 0;
      finetune = 0;
      period = 0;
      volume = 0;
      target = 0;
      slide_speed = 0;
      vibrato_speed = 0;
      vibrato_depth = 0;
      vibrato_pos = 0;
    }
  in
  {
    song;
    samples = samples_of song;
    channels = Array.init (Mod.channels song) (fun _ -> channel ());
    loop;
    reading = Hold;
    separation = 1.;
    position = 0;
    row = 0;
    tick = 0;
    speed = 6;
    tempo = 125;
    until_tick = 0.;
    next = None;
    finished = Array.length song.positions = 0;
  }

let set_reading (p : t) (r : Paula.reading) : unit = p.reading <- r
let set_separation (p : t) (s : float) : unit = p.separation <- Float.max 0. (Float.min 1. s)
let position (p : t) : int * int = (p.position, p.row)
let song (p : t) : Mod.song = p.song

let seek (p : t) ~(position : int) ~(row : int) : unit =
  p.position <- max 0 (min (Array.length p.song.positions - 1) position);
  p.row <- max 0 (min 63 row);
  p.tick <- 0;
  p.until_tick <- 0.;
  p.next <- None;
  p.finished <- Array.length p.song.positions = 0

let set_song (p : t) (song : Mod.song) : unit =
  if song.instruments != p.song.instruments then p.samples <- samples_of song;
  p.song <- song;
  (* a position gone (the order list shortened): back to its last *)
  if p.position >= Array.length song.positions then p.position <- max 0 (Array.length song.positions - 1)
let finished (p : t) : bool = p.finished
let channel_period (p : t) (c : int) : int = p.channels.(c).period
let channel_volume (p : t) (c : int) : int = p.channels.(c).volume
let speed (p : t) : int = p.speed
let tempo (p : t) : int = p.tempo

(*****************************************************************************)
(* Periods *)
(*****************************************************************************)

(* the period a finetune (eighths of a semitone) and [semitones] up make
 * of [period] *)
let bend (period : int) ~(finetune : int) ~(semitones : int) : int =
  int_of_float (Float.round (float_of_int period /. Float.pow 2. ((float_of_int semitones +. (float_of_int finetune /. 8.)) /. 12.)))

let clamp_period (p : int) : int = max 113 (min 856 p)

(* ProTracker's vibrato: a quarter sine, 0 to 255 in 16 steps *)
let vibrato_table =
  [| 0; 24; 49; 74; 97; 120; 141; 161; 180; 197; 212; 224; 235; 244; 250; 253; 255; 253; 250; 244; 235; 224; 212; 197; 180; 161; 141; 120; 97; 74; 49; 24 |]

(*****************************************************************************)
(* A row's first tick: the cells *)
(*****************************************************************************)

let volume_slide (c : channel) (param : int) : unit =
  let up = param lsr 4 and down = param land 0x0F in
  c.volume <- max 0 (min 64 (if up > 0 then c.volume + up else c.volume - down))

let play_cell (p : t) (c : channel) (cell : Mod.cell) : unit =
  c.cell <- cell;
  if cell.instrument > 0 && cell.instrument <= Array.length p.song.instruments then (
    let i = p.song.instruments.(cell.instrument - 1) in
    c.instrument <- cell.instrument;
    c.volume <- i.volume;
    c.finetune <- i.finetune);
  (* a new note restarts the channel's instrument, unless it's where a
   * slide goes *)
  if cell.period > 0 then (
    let period = bend cell.period ~finetune:c.finetune ~semitones:0 in
    if cell.effect = 3 || cell.effect = 5 then c.target <- period
    else (
      c.period <- period;
      c.vibrato_pos <- 0;
      if c.instrument > 0 then (
        let k = c.instrument - 1 in
        let i = p.song.instruments.(k) in
        let offset = if cell.effect = 9 then cell.param * 256 else 0 in
        if Array.length p.samples.(k) > 0 then
          Paula.trigger c.paula p.samples.(k) ~loop_start:i.loop_start ~loop_length:i.loop_length ~offset)));
  (* the effects of the first tick *)
  let x = cell.param lsr 4 and y = cell.param land 0x0F in
  match cell.effect with
  | 3 -> if cell.param > 0 then c.slide_speed <- cell.param
  | 4 ->
      if x > 0 then c.vibrato_speed <- x;
      if y > 0 then c.vibrato_depth <- y
  | 0xB -> p.next <- Some (cell.param, 0)
  | 0xC -> c.volume <- min 64 cell.param
  | 0xD -> p.next <- Some ((match p.next with Some (pos, _) -> pos | None -> p.position + 1), min 63 ((x * 10) + y))
  | 0xE -> (
      match x with
      | 1 -> c.period <- clamp_period (c.period - y)
      | 2 -> c.period <- clamp_period (c.period + y)
      | 0xA -> c.volume <- min 64 (c.volume + y)
      | 0xB -> c.volume <- max 0 (c.volume - y)
      | 0xC -> if y = 0 then c.volume <- 0
      | _ -> ())
  | 0xF -> if cell.param = 0 then () else if cell.param < 32 then p.speed <- cell.param else p.tempo <- cell.param
  | _ -> ()

(*****************************************************************************)
(* The other ticks: the effects going on *)
(*****************************************************************************)

let slide_to (c : channel) : unit =
  if c.target > 0 then
    if c.period < c.target then c.period <- min c.target (c.period + c.slide_speed)
    else c.period <- max c.target (c.period - c.slide_speed)

let vibrato_delta (c : channel) : int =
  let d = vibrato_table.(c.vibrato_pos land 31) * c.vibrato_depth / 128 in
  if c.vibrato_pos land 32 <> 0 then -d else d

(* the period Paula plays this tick: the note's, bent by the arpeggio
 * or the vibrato *)
let heard_period (p : t) (c : channel) : int =
  let cell = c.cell in
  match cell.effect with
  | 0 when cell.param > 0 ->
      let s = match p.tick mod 3 with 0 -> 0 | 1 -> cell.param lsr 4 | _ -> cell.param land 0x0F in
      bend c.period ~finetune:0 ~semitones:s
  | 4 | 6 -> c.period + vibrato_delta c
  | _ -> c.period

let effect_tick (p : t) (c : channel) : unit =
  let cell = c.cell in
  let x = cell.param lsr 4 and y = cell.param land 0x0F in
  match cell.effect with
  | 1 -> c.period <- clamp_period (c.period - cell.param)
  | 2 -> c.period <- clamp_period (c.period + cell.param)
  | 3 -> slide_to c
  | 4 -> c.vibrato_pos <- (c.vibrato_pos + c.vibrato_speed) land 63
  | 5 ->
      slide_to c;
      volume_slide c cell.param
  | 6 ->
      c.vibrato_pos <- (c.vibrato_pos + c.vibrato_speed) land 63;
      volume_slide c cell.param
  | 0xA -> volume_slide c cell.param
  | 0xE when x = 0xC && p.tick = y -> c.volume <- 0
  | _ -> ()

(*****************************************************************************)
(* Ticks, rows, positions *)
(*****************************************************************************)

let tick (p : t) : unit =
  if not p.finished then (
    if p.tick = 0 then (
      let pattern = p.song.patterns.(p.song.positions.(p.position)) in
      Array.iteri (fun k c -> play_cell p c pattern.(p.row).(k)) p.channels)
    else Array.iter (effect_tick p) p.channels;
    Array.iter
      (fun c ->
        Paula.set_period c.paula (clamp_period (heard_period p c));
        Paula.set_volume c.paula c.volume)
      p.channels;
    p.tick <- p.tick + 1;
    (* the row over: the next, or where a jump or a break says *)
    if p.tick >= p.speed then (
      p.tick <- 0;
      let position, row = match p.next with Some (pos, r) -> (pos, r) | None -> if p.row = 63 then (p.position + 1, 0) else (p.position, p.row + 1) in
      p.next <- None;
      if position >= Array.length p.song.positions then
        if p.loop then (
          p.position <- 0;
          p.row <- row)
        else (
          p.finished <- true;
          Array.iter (fun c -> Paula.stop c.paula) p.channels)
      else (
        p.position <- position;
        p.row <- row)))

let fill (p : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  (* channels 1 and 4 left, 2 and 3 right: the Amiga's *)
  let near = (1. +. p.separation) /. 2. and far = (1. -. p.separation) /. 2. in
  for i = 0 to n - 1 do
    if p.until_tick <= 0. then (
      tick p;
      p.until_tick <- p.until_tick +. tick_samples p.tempo);
    p.until_tick <- p.until_tick -. 1.;
    let l = ref 0. and r = ref 0. in
    Array.iteri
      (fun k c ->
        let x = Paula.next p.reading c.paula in
        let left = k mod 4 = 0 || k mod 4 = 3 in
        l := !l +. (x *. if left then near else far);
        r := !r +. (x *. if left then far else near))
      p.channels;
    (* two channels a side at full volume: 2, halved *)
    out.left.(i) <- 0.5 *. !l;
    out.right.(i) <- 0.5 *. !r
  done
