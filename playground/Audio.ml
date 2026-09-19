(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Audio.mli *)

type sound = Synth.t

let tone f = Synth.voice (Wave Sine) f
let square f = Synth.voice (Wave Square) f
let triangle f = Synth.voice (Wave Triangle) f
let sawtooth f = Synth.voice (Wave Sawtooth) f
let noise roughness = Synth.voice Noise roughness
let note name = tone (Music.frequency name)
let lasting = Synth.lasting
let fading = Synth.fading
let louder = Synth.louder
let sliding = Synth.sliding
let together sounds = Synth.Together sounds
let after sounds = Synth.After sounds

(* the ready-made sounds: our own recipes, after sfxr's categories *)
let blip = square 880. |> lasting 0.06 |> fading
let coin = after [ square 1047. |> lasting 0.07; square 1568. |> lasting 0.25 |> fading ] |> louder 0.8
let jump = square 300. |> sliding 650. |> lasting 0.18 |> fading |> louder 0.8
let laser = sawtooth 1200. |> sliding 200. |> lasting 0.2 |> fading
let hit = noise 3000. |> lasting 0.1 |> fading
let explosion = noise 1500. |> sliding 150. |> lasting 0.7 |> fading |> louder 1.5
let step = triangle 150. |> sliding 90. |> lasting 0.05 |> fading |> louder 0.6

(* the mixer every sound goes to; the platform pulls its samples *)
let mixer = Mixer.create ()
let play (s : sound) : unit = Mixer.play mixer (Synth.render s)

(* a continuous sound's voices, each kept under its own name (after:
   only the first sound goes on) *)
let rec voices (s : sound) : Synth.voice list =
  match s with Voice v -> [ v ] | Together l -> List.concat_map voices l | After (s :: _) -> voices s | After [] -> []

let keep_playing (name : string) (s : sound) : unit =
  List.iteri (fun i v -> Mixer.keep mixer (Printf.sprintf "%s#%d" name i) v) (voices s)

let pull (n : int) : float array = Mixer.pull mixer n
