(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A piano on the computer's keyboard, the way music programs (and
 * trackers, from the Amiga's Ultimate Soundtracker, 1987) lay it out:
 * the letters a s d f g h j k are the white keys, C4 to C5, and w e
 * t y u the black ones above them:
 *
 *      w   e       t   y   u
 *    a   s   d   f   g   h   j   k
 *    C4  D4  E4  F4  G4  A4  B4  C5
 *
 * Space switches the waveform (sine, square, triangle, sawtooth): the
 * same notes, a flute, a clarinet-ish NES pulse, a soft bass, brass --
 * the timbre, the recipe of harmonics (audio/Oscillator.mli); and a
 * fifth, not a waveform at all: a plucked string (audio/Pluck.mli,
 * noise in a delay line, bright then mellow as it rings). Each note
 * is an equal-tempered frequency (audio/Music.mli: A4 = 440 Hz, a
 * semitone 2^(1/12) higher each) and fades like a plucked string.
 *
 * What it uses: the Playground, Scene2d (the keys pressed), Audio
 * (square, triangle, ... pluck, fading, play) and Music's frequencies.
 *)
open Playground
open Basics (* float arithmetics *)

(* the keys: the letter, the note, black or white, where on screen *)
let white_keys = [ ("a", "C4"); ("s", "D4"); ("d", "E4"); ("f", "F4"); ("g", "G4"); ("h", "A4"); ("j", "B4"); ("k", "C5") ]

(* each black key after the white key at that index *)
let black_keys = [ ("w", "C#4", 0); ("e", "D#4", 1); ("t", "F#4", 3); ("y", "G#4", 4); ("u", "A#4", 5) ]

(* the timbres: the four waveforms, then the plucked string *)
let timbres = List.map Oscillator.name Oscillator.waveforms @ [ "plucked string" ]

type state = { waveform : int (* an index in timbres *) }
type model = state Scene2d.t

let initial_model : model = Scene2d.start { waveform = 0 }

let sound_of (waveform : int) (name : string) : Audio.sound =
  let f = Music.frequency name in
  let s =
    match waveform with
    | 0 -> Audio.tone f
    | 1 -> Audio.square f
    | 2 -> Audio.triangle f
    | 3 -> Audio.sawtooth f
    | _ -> Audio.pluck f
  in
  (* the string dies away by itself, and rings longer *)
  if waveform = 4 then s |> Audio.lasting 1.5 else s |> Audio.lasting 0.8 |> Audio.fading

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let s = scenes.scene in
  let pressed l = Scene2d.pressed (fun k -> Set_.mem l k.keys) scenes in
  List.iter (fun (key, note) -> if pressed key then Audio.play (sound_of s.waveform note)) white_keys;
  List.iter (fun (key, note, _) -> if pressed key then Audio.play (sound_of s.waveform note)) black_keys;
  if Scene2d.pressed (fun k -> k.kspace) scenes then { scenes with scene = { waveform = (s.waveform +.. 1) mod List.length timbres } } else scenes

let key_width = 100.

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and keys = computer.keyboard.keys in
  let down l = Set_.mem l keys in
  let x_of i = (float_of_int i - 3.5) * key_width in
  let whites =
    List.mapi
      (fun i (key, note) ->
        group
          [ rectangle (if down key then rgb 250 220 120 else white) (key_width - 4.) 300.;
            words black key |> scale 2. |> move_y (-110.);
            words (rgb 120 120 120) note |> scale 1.5 |> move_y (-135.) ]
        |> move_x (x_of i))
      white_keys
  and blacks =
    List.map
      (fun (key, note, i) ->
        group
          [ rectangle (if down key then rgb 200 160 60 else rgb 30 30 30) (key_width * 0.6) 180.;
            words white key |> scale 1.8 |> move_y (-50.);
            words (rgb 170 170 170) note |> scale 1.2 |> move_y (-70.) ]
        |> move (x_of i + (key_width / 2.)) 60.)
      black_keys
  in
  (rectangle (rgb 60 50 45) screen.width screen.height :: whites)
  @ blacks
  @ [ words white (Printf.sprintf "%s (space: the next waveform)" (List.nth timbres model.scene.waveform))
      |> scale 2.5 |> move_y 300. ]

let help =
  {|Piano
  keys:  a s d f g h j k  the white keys, C4 to C5
         w e t y u        the black keys
         space            the next timbre (sine, square, triangle, sawtooth,
                          plucked string)
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
