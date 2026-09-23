(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Phaser.mli *)

type settings = { low : float; high : float; rate : float; feedback : float; mix : float }

let initial = { low = 200.; high = 3000.; rate = 0.3; feedback = 0.3; mix = 1. }
let stages = 4
let rate = float_of_int Signal.rate

(* a first-order all-pass's memory: its last input and output *)
type stage = { mutable x1 : float; mutable y1 : float }

(* one side: its four stages, and the last output, fed back *)
type side = { stages : stage array; mutable last : float }

type t = {
  left : side;
  right : side;
  mutable phase : float; (* the LFO's, in turns *)
  (* the last block's, ramped from (nan: none yet) *)
  mutable last_mix : float;
}

let side () = { stages = Array.init stages (fun _ -> { x1 = 0.; y1 = 0. }); last = 0. }
let create () : t = { left = side (); right = side (); phase = 0.; last_mix = Float.nan }

let coefficient (fc : float) : float =
  let k = tan (Float.pi *. Float.min fc (0.49 *. rate) /. rate) in
  (k -. 1.) /. (k +. 1.)

let through (s : side) (a : float) (feedback : float) (x : float) : float =
  let v = ref (x +. (feedback *. s.last)) in
  Array.iter
    (fun st ->
      let y = (a *. !v) +. st.x1 -. (a *. st.y1) in
      st.x1 <- !v;
      st.y1 <- y;
      v := y)
    s.stages;
  s.last <- !v;
  !v

let process (t : t) (s : settings) (out : Signal.stereo) : unit =
  let n = Array.length out.left and feedback = Float.min 0.9 (Float.max (-0.9) s.feedback) in
  let from_mix = if Float.is_nan t.last_mix then s.mix else t.last_mix in
  (* the break frequency at an LFO phase: evenly in octaves *)
  let fc phase = s.low *. Float.pow (s.high /. s.low) (0.5 +. (0.5 *. sin (2. *. Float.pi *. phase))) in
  for i = 0 to n - 1 do
    let mix = Effect.ramp from_mix s.mix i n in
    let wet_left = through t.left (coefficient (fc t.phase)) feedback out.left.(i)
    and wet_right = through t.right (coefficient (fc (t.phase +. 0.25))) feedback out.right.(i) in
    out.left.(i) <- out.left.(i) +. (mix *. wet_left);
    out.right.(i) <- out.right.(i) +. (mix *. wet_right);
    t.phase <- t.phase +. (s.rate /. rate);
    if t.phase >= 1. then t.phase <- t.phase -. 1.
  done;
  t.last_mix <- s.mix
