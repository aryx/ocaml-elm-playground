(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Leslie.mli *)

let rate = float_of_int Signal.rate
let horn_slow = 0.8
let horn_fast = 6.8
let drum_slow = 0.7
let drum_fast = 5.6
let horn_seconds = 0.5
let drum_seconds = 1.2
let crossover = 800.
let sound_speed = 343. (* m/s *)

(* ours: the horn's mouth's circle, the drum opening's, their levels
 * facing and turned away *)
let horn_radius = 0.15
let drum_radius = 0.1

(* the microphones, a quarter turn left and right of the listener *)
let microphones = [| 0.25; -0.25 |]

(* a delay line: what a rotor emitted, [at] the next to write *)
type line = { samples : float array; mutable at : int }

type rotor = {
  line : line;
  radius : float;
  mutable angle : float; (* turns *)
  mutable speed : float; (* turns a second *)
  facing : float; (* its level turned towards a microphone, 1 - facing away (0.6 + 0.4 cos) *)
  way : float; (* 1 or -1: the drum turns the other way *)
}

type t = {
  horn_rotor : rotor;
  drum_rotor : rotor;
  low : Filter.biquad;
  high : Filter.biquad;
  low_memory : Filter.memory;
  high_memory : Filter.memory;
}

let rotor radius facing way speed =
  { line = { samples = Array.make 64 0.; at = 0 }; radius; angle = 0.; speed; facing; way }

let create () : t =
  {
    horn_rotor = rotor horn_radius 0.4 1. horn_slow;
    drum_rotor = rotor drum_radius 0.2 (-1.) drum_slow;
    low = Filter.biquad Low_pass ~cutoff:crossover ~q:0.707;
    high = Filter.biquad High_pass ~cutoff:crossover ~q:0.707;
    low_memory = Filter.silence ();
    high_memory = Filter.silence ();
  }

let horn (t : t) : float = t.horn_rotor.speed
let drum (t : t) : float = t.drum_rotor.speed

(* what microphone [m] hears of rotor [r] now: the sound emitted a
 * distance's time ago -- the mouth nearer, less long ago -- at the
 * mouth's level towards it *)
let heard (r : rotor) (m : float) : float =
  let a = 2. *. Float.pi *. (r.angle -. m) in
  let delay = ((r.radius *. (1. -. cos a)) /. sound_speed *. rate) +. 1. in
  let n = Array.length r.line.samples in
  let whole = Float.to_int delay in
  let frac = delay -. float_of_int whole in
  let at k = r.line.samples.((r.line.at - k + (2 * n)) mod n) in
  let x = ((1. -. frac) *. at whole) +. (frac *. at (whole + 1)) in
  (1. -. r.facing +. (r.facing *. cos a)) *. x

(* one sample: the rotor's speed nearer its target, its angle on *)
let turn (r : rotor) (target : float) (seconds : float) : unit =
  r.speed <- r.speed +. ((target -. r.speed) *. (1. -. exp (-1. /. (seconds *. rate))));
  r.angle <- r.angle +. (r.way *. r.speed /. rate);
  r.angle <- r.angle -. Float.of_int (Float.to_int r.angle)

let write (l : line) (x : float) : unit =
  l.samples.(l.at) <- x;
  l.at <- (l.at + 1) mod Array.length l.samples

let process_mix (t : t) ~(fast : bool) ~(from_mix : float) ~(mix : float) (s : Signal.stereo) : unit =
  let n = Array.length s.left in
  let h = t.horn_rotor and d = t.drum_rotor in
  for i = 0 to n - 1 do
    let x = (s.left.(i) +. s.right.(i)) /. 2. in
    write h.line (Filter.step t.high t.high_memory x);
    write d.line (Filter.step t.low t.low_memory x);
    turn h (if fast then horn_fast else horn_slow) horn_seconds;
    turn d (if fast then drum_fast else drum_slow) drum_seconds;
    let mix = Effect.ramp from_mix mix i n in
    let mic m = heard h m +. heard d m in
    s.left.(i) <- ((1. -. mix) *. x) +. (mix *. mic microphones.(0));
    s.right.(i) <- ((1. -. mix) *. x) +. (mix *. mic microphones.(1))
  done

let process (t : t) ~(fast : bool) (s : Signal.stereo) : unit = process_mix t ~fast ~from_mix:1. ~mix:1. s

let knobs : Effect.knob list =
  [ { name = "fast"; control = Switch; initial = 0. }; { name = "mix"; control = Knob (0., 1.); initial = 1. } ]

let effect () : Effect.t =
  let t = create () and fast = ref false and mix = ref 1. and last_mix = ref 1. in
  let set (knob : string) (x : float) =
    match knob with "fast" -> fast := Control.on x | "mix" -> mix := x | _ -> ()
  in
  let process s =
    process_mix t ~fast:!fast ~from_mix:!last_mix ~mix:!mix s;
    last_mix := !mix
  in
  { name = "leslie"; knobs; set; process; meters = (fun () -> [ ("horn", horn t); ("drum", drum t) ]) }
