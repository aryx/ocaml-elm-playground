(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Audio_debug.mli *)

open Playground

(* the last [kept] samples, oldest first *)
let kept = 2048
let last = Array.make kept 0.

let record (samples : float array) : unit =
  let n = Array.length samples in
  if n >= kept then Array.blit samples (n - kept) last 0 kept
  else (
    Array.blit last n last 0 (kept - n);
    Array.blit samples 0 last (kept - n) n)

type view = Off | Oscilloscope | Spectrum

let next = function Off -> Oscilloscope | Oscilloscope -> Spectrum | Spectrum -> Off
let name = function Off -> "off" | Oscilloscope -> "oscilloscope" | Spectrum -> "spectrum"

(* a segment, a thin rectangle turned *)
let segment (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  rectangle color (Float.hypot (x2 -. x1) (y2 -. y1) +. 1.) 2.
  |> rotate (Float.atan2 (y2 -. y1) (x2 -. x1) *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let label (s : string) : shape = words white s |> scale 1.4

(* the panel: 900 x 280 over the bottom of the screen *)
let width = 900.
let height = 280.

let oscilloscope (y0 : number) : shape list =
  (* the trigger: the first rising zero crossing in the first half *)
  let start = ref 0 in
  (try
     for i = 1 to (kept / 2) - 1 do
       if last.(i - 1) < 0. && last.(i) >= 0. then (
         start := i;
         raise Exit)
     done
   with Exit -> ());
  (* 1024 samples (23 ms), 256 segments *)
  let point j = let i = !start + (j * 4) in ((-.width /. 2.) +. (float_of_int j *. width /. 256.), y0 +. (last.(i) *. height *. 0.45)) in
  segment (rgb 60 60 80) (-.width /. 2., y0) (width /. 2., y0)
  :: List.init 256 (fun j -> segment (rgb 120 255 140) (point j) (point (j + 1)))
  @ [ label "oscilloscope: the last 23 ms (v)" |> move 0. (y0 +. (height /. 2.) -. 18.) ]

let spectrum (y0 : number) : shape list =
  let mags = Spectrum.of_signal last in
  let n = 2 * (Array.length mags - 1) in
  (* 90 bars, from 20 Hz to 20 kHz, each the loudest bin in its band *)
  let bars = 90 in
  let freq b = 20. *. (1000. ** (float_of_int b /. float_of_int bars)) in
  let bottom = y0 -. (height /. 2.) +. 20. in
  let bar b =
    let lo = freq b and hi = freq (b + 1) in
    let m = ref 0. in
    Array.iteri (fun k v -> let f = Spectrum.bin_frequency ~n k in if f >= lo && f < hi && v > !m then m := v) mags;
    let db = if !m <= 0. then -80. else Float.max (-80.) (20. *. log10 !m) in
    let h = (db +. 80.) /. 80. *. (height -. 60.) in
    let w = width /. float_of_int bars in
    rectangle (rgb 250 190 80) (w -. 2.) (Float.max 1. h)
    |> move ((-.width /. 2.) +. ((float_of_int b +. 0.5) *. w)) (bottom +. (h /. 2.))
  in
  let mark f text =
    let x = (-.width /. 2.) +. (width *. log (f /. 20.) /. log 1000.) in
    [ rectangle (rgb 90 90 110) 1. (height -. 40.) |> move x (y0 -. 10.); label text |> move x (bottom -. 10.) ]
  in
  mark 100. "100 Hz" @ mark 1000. "1 kHz" @ mark 10000. "10 kHz" @ List.init bars bar
  @ [ label "spectrum: 20 Hz to 20 kHz, -80 to 0 dB (v)" |> move 0. (y0 +. (height /. 2.) -. 18.) ]

let shapes (view : view) (screen : screen) : shape list =
  let y0 = screen.bottom +. (height /. 2.) +. 30. in
  let panel = rectangle black width height |> fade 0.8 |> move_y y0 in
  match view with Off -> [] | Oscilloscope -> panel :: oscilloscope y0 | Spectrum -> panel :: spectrum y0
