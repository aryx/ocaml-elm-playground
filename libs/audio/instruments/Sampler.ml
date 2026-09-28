(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sampler.mli *)

(*****************************************************************************)
(* A recording, played *)
(*****************************************************************************)

type sample = { data : Signal.t; root : int }
type direction = Forward | Backward
type loop = Forever | Until_release | Off

type settings = {
  start : float;
  loop_start : float;
  loop_end : float;
  end_ : float;
  direction : direction;
  tune : float;
  crossfade : float;
  gain : float;
  loop : loop;
  release : float option;
}

let default =
  { start = 0.; loop_start = 0.; loop_end = 1.; end_ = 1.; direction = Forward; tune = 0.; crossfade = 0.; gain = 1.; loop = Off; release = Some 0.01 }

let rate = float_of_int Signal.rate
let clamp lo hi x = Float.max lo (Float.min hi x)
let speed (s : sample) (st : settings) (key : int) : float = Float.pow 2. ((float_of_int (key - s.root) +. st.tune) /. 12.)

(* the region in samples; [u] counts from the region's first sample
 * played (its end, backwards), the loop in the same count *)
type region = { len : float; index : float -> float; loop_from : float; loop_to : float }

let region (s : sample) (st : settings) : region =
  let n = float_of_int (Array.length s.data) in
  let a = clamp 0. n (st.start *. n) and b = clamp 0. n (st.end_ *. n) in
  let a, b = (Float.min a b, Float.max a b) in
  let ls = clamp a b (st.loop_start *. n) and le = clamp a b (st.loop_end *. n) in
  let ls, le = (Float.min ls le, Float.max ls le) in
  match st.direction with
  | Forward -> { len = b -. a; index = (fun u -> a +. u); loop_from = ls -. a; loop_to = le -. a }
  | Backward -> { len = b -. a; index = (fun u -> b -. 1. -. u); loop_from = b -. le; loop_to = b -. ls }

let seconds (s : sample) (st : settings) (key : int) : float = (region s st).len /. speed s st key /. rate

(* a note, and a way to fade it out (the kit's choke) *)
type player = { voice : Polyphony.voice; fade_out : float -> unit }

let player (s : sample) (st : settings) ~(key : int) ~(velocity : float) : player =
  let r = region s st in
  let k = speed s st key in
  let length = r.loop_to -. r.loop_from in
  (* the crossfade: no longer than the loop, nor than what precedes
   * the loop's start *)
  let xf = Float.min (st.crossfade *. length) r.loop_from in
  let read u = Resample.read Cubic s.data (r.index u) in
  let u = ref 0. and released = ref false and fade = ref 1. and fading = ref 0. in
  let looping () = length >= 1. && (st.loop = Forever || (st.loop = Until_release && not !released)) in
  let fade_out seconds = fading := Float.max !fading (1. /. Float.max 1. (seconds *. rate)) in
  let done_ () = !u >= r.len || !fade <= 0. in
  let fill out =
    Array.iteri
      (fun i _ ->
        if done_ () then out.(i) <- 0.
        else begin
          let y =
            if looping () && xf >= 1. && !u >= r.loop_to -. xf then
              let a = (!u -. (r.loop_to -. xf)) /. xf in
              ((1. -. a) *. read !u) +. (a *. read (!u -. length))
            else read !u
          in
          out.(i) <- st.gain *. velocity *. !fade *. y;
          fade := !fade -. !fading;
          u := !u +. k;
          if looping () && !u >= r.loop_to then u := !u -. length
        end)
      out
  in
  let release () =
    released := true;
    Option.iter fade_out st.release
  in
  { voice = { release; fill; silent = done_ }; fade_out }

let voice (s : sample) (st : settings) ~(key : int) ~(velocity : float) : Polyphony.voice = (player s st ~key ~velocity).voice

(*****************************************************************************)
(* The drum sampler *)
(*****************************************************************************)

type play = Key | Oneshot | Mute_group | Loop
type pad = { sample : sample; settings : settings; play : play; pan : float }

(* the play mode as the settings' loop and release: Key and Loop fade
 * when let go (10 ms, ours), the others play on *)
let pad ?(settings = default) ?(play = Oneshot) ?(pan = 0.) (sample : sample) : pad =
  let settings =
    match play with
    | Key -> { settings with loop = Off; release = Some 0.01 }
    | Oneshot | Mute_group -> { settings with loop = Off; release = None }
    | Loop -> { settings with loop = Forever; release = Some 0.01 }
  in
  { sample; settings; play; pan }

(* ours: a choked pad silent in 5 ms *)
let choke_seconds = 0.005

type sounding = { index : int; key : int; p : player }
type kit = { pads : pad array; base : int; mutable sounding : sounding list; mutable mono : Signal.t }

let kit ?(base = 36) (pads : pad array) : kit = { pads = Array.sub pads 0 (min 24 (Array.length pads)); base; sounding = []; mono = [||] }

let press (k : kit) (key : int) (velocity : float) : unit =
  let i = key - k.base in
  if i >= 0 && i < Array.length k.pads then begin
    let pad = k.pads.(i) in
    if pad.play = Mute_group then
      List.iter (fun s -> if k.pads.(s.index).play = Mute_group then s.p.fade_out choke_seconds) k.sounding;
    (* the pad at its root: its key is the pad's, not a pitch *)
    let p = player pad.sample pad.settings ~key:pad.sample.root ~velocity in
    k.sounding <- { index = i; key; p } :: k.sounding
  end

let release (k : kit) (key : int) : unit = List.iter (fun s -> if s.key = key then s.p.voice.release ()) k.sounding

let fill (k : kit) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  if Array.length k.mono <> n then k.mono <- Array.make n 0.;
  Array.fill out.left 0 n 0.;
  Array.fill out.right 0 n 0.;
  List.iter
    (fun s ->
      s.p.voice.fill k.mono;
      (* the pan, ours: the middle at full on both sides, a side turned
       * down as the pad goes to the other *)
      let pan = k.pads.(s.index).pan in
      let l = Float.min 1. (1. -. pan) and r = Float.min 1. (1. +. pan) in
      Array.iteri
        (fun j x ->
          out.left.(j) <- out.left.(j) +. (l *. x);
          out.right.(j) <- out.right.(j) +. (r *. x))
        k.mono)
    k.sounding;
  k.sounding <- List.filter (fun s -> not (s.p.voice.silent ())) k.sounding

let sounding (k : kit) : int = List.length k.sounding
