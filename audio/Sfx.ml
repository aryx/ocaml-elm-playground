(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sfx.mli *)

type wave = Square | Sawtooth | Triangle | Sine | Noise

type t = {
  wave : wave;
  frequency : float;
  slide : float;
  attack : float;
  sustain : float;
  decay : float;
  vibrato_rate : float;
  vibrato_depth : float;
  jump : float;
  jump_at : float;
  low_pass : float;
  low_pass_to : float;
  resonance : float;
  high_pass : float;
  echo : float;
  volume : float;
}

let default : t =
  {
    wave = Square;
    frequency = 440.;
    slide = 440.;
    attack = 0.;
    sustain = 0.1;
    decay = 0.1;
    vibrato_rate = 0.;
    vibrato_depth = 0.;
    jump = 0.;
    jump_at = 0.;
    low_pass = 0.;
    low_pass_to = 0.;
    resonance = 0.707;
    high_pass = 0.;
    echo = 0.;
    volume = 0.5;
  }

(* the echoes, each 0.4 of the last *)
let feedback = 0.4

let to_sound (s : t) : Synth.t =
  let source : Synth.source =
    match s.wave with
    | Square -> Wave Square
    | Sawtooth -> Wave Sawtooth
    | Triangle -> Wave Triangle
    | Sine -> Wave Sine
    | Noise -> Noise
  in
  let attack = Float.max 0.005 s.attack in
  let voice : Synth.voice =
    {
      source;
      frequency = s.frequency;
      slide = (if s.slide = s.frequency then None else Some s.slide);
      seconds = attack +. s.sustain +. s.decay;
      volume = s.volume;
      fade = false;
      effects =
        (if s.vibrato_depth > 0. then [ Effect.Vibrato { rate = s.vibrato_rate; depth = s.vibrato_depth } ] else [])
        @ if s.jump <> 0. then [ Effect.Jump { semitones = s.jump; at = s.jump_at } ] else [];
      envelope = Some { attack; decay = 0.; sustain = 1.; release = s.decay };
    }
  in
  let sound = Synth.Voice voice in
  let sound =
    if s.low_pass > 0. then
      let cutoff_to = if s.low_pass_to > 0. then s.low_pass_to else s.low_pass in
      Synth.Filtered ({ kind = Low_pass; cutoff = s.low_pass; cutoff_to; q = s.resonance }, sound)
    else sound
  in
  let sound =
    if s.high_pass > 0. then Synth.Filtered ({ kind = High_pass; cutoff = s.high_pass; cutoff_to = s.high_pass; q = 0.707 }, sound)
    else sound
  in
  if s.echo > 0. then Synth.Echo ({ delay = s.echo; feedback }, sound) else sound

let duration (s : t) : float = Synth.duration (to_sound s)

(*****************************************************************************)
(* The presets *)
(*****************************************************************************)

let blip = { default with frequency = 880.; slide = 880.; sustain = 0.03; decay = 0.04 }

(* C6, then G6: a fifth, 7 semitones *)
let coin = { default with frequency = 1047.; slide = 1047.; sustain = 0.08; decay = 0.22; jump = 7.; jump_at = 0.07; volume = 0.4 }
let jump = { default with frequency = 300.; slide = 650.; sustain = 0.04; decay = 0.14; volume = 0.4 }

let laser =
  { default with wave = Sawtooth; frequency = 1200.; slide = 200.; sustain = 0.02; decay = 0.18; low_pass = 8000.; low_pass_to = 1000. }

let hit = { default with wave = Noise; frequency = 3000.; slide = 1500.; sustain = 0.01; decay = 0.09; low_pass = 5000. }

let explosion =
  {
    default with
    wave = Noise;
    frequency = 1500.;
    slide = 150.;
    sustain = 0.1;
    decay = 0.6;
    low_pass = 4000.;
    low_pass_to = 150.;
    volume = 0.75;
  }

let step = { default with wave = Triangle; frequency = 150.; slide = 90.; sustain = 0.; decay = 0.05; volume = 0.3 }

let powerup =
  { default with frequency = 330.; slide = 990.; sustain = 0.25; decay = 0.2; vibrato_rate = 14.; vibrato_depth = 0.5; volume = 0.35 }

let presets =
  [
    ("blip", blip);
    ("coin", coin);
    ("jump", jump);
    ("laser", laser);
    ("hit", hit);
    ("explosion", explosion);
    ("step", step);
    ("powerup", powerup);
  ]

(*****************************************************************************)
(* Mutate *)
(*****************************************************************************)

let vary ~(seed : int) (s : t) : t =
  if seed = 0 then s
  else
    let st = Random.State.make [| seed |] in
    (* times 0.7 to 1.4, evenly in ratio *)
    let scale x = x *. (0.7 *. (2. ** Random.State.float st 1.)) in
    (* the same number of semitones for both ends of a slide *)
    let pitch = 2. ** ((Random.State.float st 10. -. 5.) /. 12.) in
    {
      s with
      frequency = s.frequency *. pitch;
      slide = s.slide *. pitch;
      attack = scale s.attack;
      sustain = scale s.sustain;
      decay = scale s.decay;
      vibrato_rate = scale s.vibrato_rate;
      jump_at = scale s.jump_at;
      low_pass = scale s.low_pass;
      low_pass_to = scale s.low_pass_to;
      high_pass = scale s.high_pass;
    }
