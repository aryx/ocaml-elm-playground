(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Paula.mli *)

type reading = Hold | Linear | Cubic

let readings = [ Hold; Linear; Cubic ]
let reading_name = function Hold -> "hold (Paula's)" | Linear -> "linear" | Cubic -> "cubic"

(* the sample, where in it (in bytes, fractional), the loop, the period,
 * the volume; silent when [sample] is empty *)
type t = {
  mutable sample : Signal.t;
  mutable pos : float;
  mutable loop_start : int;
  mutable loop_length : int;
  mutable period : int;
  mutable volume : int;
}

let create () : t = { sample = [||]; pos = 0.; loop_start = 0; loop_length = 0; period = 428; volume = 64 }

let trigger (c : t) (sample : Signal.t) ~(loop_start : int) ~(loop_length : int) ~(offset : int) : unit =
  c.sample <- sample;
  c.pos <- float_of_int offset;
  c.loop_start <- loop_start;
  c.loop_length <- loop_length

let set_period (c : t) (p : int) : unit = if p > 0 then c.period <- p
let set_volume (c : t) (v : int) : unit = c.volume <- max 0 (min 64 v)
let stop (c : t) : unit = c.sample <- [||]
let playing (c : t) : bool = Array.length c.sample > 0

let next (reading : reading) (c : t) : float =
  let n = Array.length c.sample in
  if n = 0 then 0.
  else
    (* past the loop's end, back by its length; past the sample's end
     * with no loop, silent *)
    let loop_end = c.loop_start + c.loop_length in
    if c.loop_length > 0 && c.pos >= float_of_int loop_end then c.pos <- c.pos -. float_of_int c.loop_length;
    if c.pos >= float_of_int n then (
      c.sample <- [||];
      0.)
    else
      let x =
        match reading with
        | Hold -> c.sample.(int_of_float c.pos)
        | Linear -> Resample.read Linear c.sample c.pos
        | Cubic -> Resample.read Cubic c.sample c.pos
      in
      c.pos <- c.pos +. (Mod.paula_clock /. float_of_int c.period /. float_of_int Signal.rate);
      x *. float_of_int c.volume /. 64.
