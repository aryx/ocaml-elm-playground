(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Dx_envelope.mli *)

let level_table = [| 0; 5; 9; 13; 17; 20; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 42; 43; 45; 46 |]
let scale_output_level (level : int) : int = if level >= 20 then 28 + level else level_table.(max 0 level)
let floor = 16.
let full = 3840.
let gain (steps : float) : float = Float.pow 2. ((steps /. 256.) -. 14.)
let decibels (steps : float) : float = 20. *. log10 (gain steps /. gain full)

type t = {
  rates : int array;
  levels : int array;
  output_level : int;
  rate_scaling : int;
  mutable stage : int;
  mutable level : float;
  mutable target : float;
  mutable rising : bool;
  mutable speed : float; (* steps a sample *)
  mutable down : bool;
}

(* the next stage: its target in steps, its speed from its rate *)
let advance (t : t) (stage : int) : unit =
  t.stage <- stage;
  if stage < 4 then begin
    let steps = ((scale_output_level t.levels.(stage) lsr 1) lsl 6) + t.output_level - 4256 in
    t.target <- float_of_int (max 16 steps);
    t.rising <- t.target > t.level;
    let qrate = min 63 (((t.rates.(stage) * 41) lsr 6) + t.rate_scaling) in
    (* Dexed's increment is for a block of 64 samples, in steps times
     * 65536: per sample, 2^6 less, and 2^16 *)
    t.speed <- float_of_int ((4 + (qrate land 3)) lsl (2 + (qrate lsr 2))) /. 65536.
  end

let create ~(rates : int array) ~(levels : int array) ?(output_level = 4064) ?(rate_scaling = 0) () : t =
  let t =
    { rates; levels; output_level; rate_scaling; stage = 0; level = 0.; target = 0.; rising = false; speed = 0.; down = true }
  in
  advance t 0;
  t

let key_up (t : t) : unit =
  if t.down then begin
    t.down <- false;
    advance t 3
  end

(* [samples] samples on in one step, the speed times as much *)
let step (t : t) (samples : float) : float =
  (* L3 held while the key is *)
  if t.stage < 3 || (t.stage = 3 && not t.down) then begin
    if t.rising then begin
      if t.level < 1716. then t.level <- 1716.;
      (* the whole doublings left below 17 *)
      t.level <- t.level +. (Float.of_int (Float.to_int ((4352. -. t.level) /. 256.)) *. t.speed *. samples);
      if t.level >= t.target then begin
        t.level <- t.target;
        advance t (t.stage + 1)
      end
    end
    else begin
      t.level <- t.level -. (t.speed *. samples);
      if t.level <= t.target then begin
        t.level <- t.target;
        advance t (t.stage + 1)
      end
    end
  end;
  t.level

let next (t : t) : float = step t 1.
let run (t : t) (samples : int) : float = step t (float_of_int samples)
let stage (t : t) : int = t.stage
