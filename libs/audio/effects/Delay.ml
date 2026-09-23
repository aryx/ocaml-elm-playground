(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Delay.mli *)

let beats ~(bpm : float) (b : float) : float = b *. 60. /. bpm
let longest = 2.

(* a line of the last [size] samples written, [at] the next to write *)
type line = { samples : float array; mutable at : int; mutable tone : float (* the low-pass's state *) }

type t = {
  left : line;
  right : line;
  mutable delay : float; (* in samples, gliding to the setting *)
  (* the last block's, ramped from (nan: none yet) *)
  mutable last_feedback : float;
  mutable last_mix : float;
}

type settings = { time : float; feedback : float; tone : float; ping_pong : bool; mix : float }

let size = Signal.samples longest + 2
let line () = { samples = Array.make size 0.; at = 0; tone = 0. }
let create () : t = { left = line (); right = line (); delay = -1.; last_feedback = Float.nan; last_mix = Float.nan }

(* [d] samples ago, between two samples: linear *)
let read (l : line) (d : float) : float =
  let back = Float.to_int d and frac = d -. Float.of_int (Float.to_int d) in
  let at k = l.samples.((l.at - k + (2 * size)) mod size) in
  ((1. -. frac) *. at back) +. (frac *. at (back + 1))

let write (l : line) (x : float) : unit =
  l.samples.(l.at) <- x;
  l.at <- (l.at + 1) mod size

(* the glide to a new time: 1/2205 of the way each sample, 63% in 50 ms *)
let glide = 1. /. 2205.

let process (t : t) (s : settings) (out : Signal.stereo) : unit =
  let target = Float.max 1. (Float.min (float_of_int (size - 3)) (s.time *. float_of_int Signal.rate)) in
  if t.delay < 0. then t.delay <- target;
  let a = Filter.one_pole_coefficient s.tone and feedback = Float.min 0.95 (Float.max 0. s.feedback) in
  let n = Array.length out.left in
  let from_feedback = if Float.is_nan t.last_feedback then feedback else t.last_feedback
  and from_mix = if Float.is_nan t.last_mix then s.mix else t.last_mix in
  let darker (l : line) (x : float) =
    l.tone <- l.tone +. (a *. (x -. l.tone));
    l.tone
  in
  Array.iteri
    (fun i x_left ->
      let x_right = out.right.(i) in
      let feedback = Effect.ramp from_feedback feedback i n and mix = Effect.ramp from_mix s.mix i n in
      t.delay <- t.delay +. (glide *. (target -. t.delay));
      let wet_left = read t.left t.delay and wet_right = read t.right t.delay in
      let back_left = darker t.left wet_left and back_right = darker t.right wet_right in
      if s.ping_pong then (
        write t.left (((x_left +. x_right) /. 2.) +. (feedback *. back_right));
        write t.right (feedback *. back_left))
      else (
        write t.left (x_left +. (feedback *. back_left));
        write t.right (x_right +. (feedback *. back_right)));
      out.left.(i) <- x_left +. (mix *. wet_left);
      out.right.(i) <- x_right +. (mix *. wet_right))
    out.left;
  t.last_feedback <- feedback;
  t.last_mix <- s.mix

let knobs : Effect.knob list =
  [
    { name = "time"; control = Knob (0.05, longest); initial = beats ~bpm:120. 0.75 };
    { name = "feedback"; control = Knob (0., 0.95); initial = 0.4 };
    { name = "tone"; control = Knob (300., 12000.); initial = 3000. };
    { name = "pingpong"; control = Switch; initial = 0. };
    { name = "mix"; control = Knob (0., 1.); initial = 0.3 };
  ]

let effect () : Effect.t =
  let t = create () and s = ref { time = 0.; feedback = 0.; tone = 0.; ping_pong = false; mix = 0. } in
  let set (knob : string) (x : float) =
    match knob with
    | "time" -> s := { !s with time = x }
    | "feedback" -> s := { !s with feedback = x }
    | "tone" -> s := { !s with tone = x }
    | "pingpong" -> s := { !s with ping_pong = Control.on x }
    | "mix" -> s := { !s with mix = x }
    | _ -> ()
  in
  List.iter (fun (k : Effect.knob) -> set k.name k.initial) knobs;
  { name = "delay"; knobs; set; process = (fun out -> process t !s out); meters = (fun () -> []) }
