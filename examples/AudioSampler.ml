(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A sampler: one recording, every note (audio/Resample.mli).
 *
 * The Fairlight CMI (1979) and the Emulator (1981) played real
 * recordings from a keyboard, and the Amiga's trackers (1987) did the
 * same with a few kilobytes of each instrument: one recording, made at
 * one note, and every other note that recording read faster or slower.
 * An octave up is read twice as fast -- and so lasts half as long: a
 * recording has no pitch to change on its own, pitch and time go
 * together, as on a tape. Play the highest keys and hear the note get
 * shorter as it gets higher.
 *
 * The recording here is made by the program (an electric piano's note,
 * FM, at middle C, frozen by Audio.recorded: from then on it is samples,
 * like a WAV file's, Audio.wav); the keys are AudioPiano's, a s d f g h
 * j k for C4 to C5 and w e t y u for the black ones, and each plays it
 * [2^(n/12)] times as fast.
 *
 * Reading faster lands between samples, and space switches how the
 * sampler guesses there: the nearest sample (the Amiga's way, gritty),
 * a straight line between the two neighbours, or a curve through four
 * (Catmull-Rom) -- the audio twin of an image's nearest and bilinear
 * filtering; on a sine a fifth up, their errors are -40, -79 and -112
 * dB (Unit_resample), and worse the higher the note.
 *
 * What it uses: the Playground, Scene2d (the keys pressed), Audio (fm,
 * fading, recorded, pitched, play) and audio/'s Resample directly (its
 * kind, switched).
 *)
open Playground
open Basics (* float arithmetics *)

(* the keys: the letter, the semitones above C4 *)
let keys =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

let names = [| "C4"; "C#4"; "D4"; "D#4"; "E4"; "F4"; "F#4"; "G4"; "G#4"; "A4"; "A#4"; "B4"; "C5" |]

(* the recording: an electric piano's note at C4, 1.2 s *)
let recording = Audio.fm 261.63 1. 3. |> Audio.lasting 1.2 |> Audio.fading |> Audio.recorded

type state = { last : int option (* the semitones of the last key *) }
type model = state Scene2d.t

let initial_model : model = Scene2d.start { last = None }
let ratio (semitones : int) : number = 2. ** (float_of_int semitones / 12.)

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed key = Scene2d.pressed (fun k -> Set_.mem key k.keys) scenes in
  if Scene2d.pressed (fun k -> k.kspace) scenes then begin
    let rec next = function a :: b :: _ when a = !Resample.kind -> b | _ :: l -> next l | [] -> List.hd Resample.kinds in
    Resample.kind := next Resample.kinds
  end;
  match List.find_opt (fun (key, _) -> pressed key) keys with
  | Some (_, n) ->
      Audio.play (recording |> Audio.pitched (ratio n));
      { scenes with scene = { last = Some n } }
  | None -> scenes

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and down key = Set_.mem key computer.keyboard.keys in
  let text size color s = words color s |> scale size in
  let rows =
    List.mapi
      (fun i (key, n) ->
        let lit = down key || model.scene.last = Some n in
        text 1.8 (if lit then rgb 250 200 80 else rgb 190 190 210)
          (Printf.sprintf "%s  %-4s  read %.3f times as fast, lasting %.2f s" key names.(n) (ratio n) (1.2 / ratio n))
        |> move_y (260. - (float_of_int i * 34.)))
      keys
  in
  (rectangle (rgb 25 25 40) screen.width screen.height :: rows)
  @ [ text 2.5 white "one recording (C4), every note" |> move_y 400.;
      text 2. (rgb 250 200 80) (Printf.sprintf "reading between samples: %s (space: the next)" (Resample.name !Resample.kind))
      |> move_y 340.;
      text 1.6 (rgb 150 150 170) "higher is faster, so shorter: pitch and time together, as on a tape" |> move_y (-240.) ]

let help =
  {|Sampler
  keys:  a s d f g h j k  C4 to C5, w e t y u the black keys: one
                          recording, read faster or slower
         space            how it reads between samples: nearest,
                          linear, cubic
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
