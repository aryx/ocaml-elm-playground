(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The game sounds, and the numbers they are made of: sfxr (Tomas
 * Pettersson, 2007), readable (audio/Sfx.mli).
 *
 * Keys 1 to 8 play the ready-made sounds every game here uses -- blip,
 * coin, jump, laser, hit, explosion, step, powerup -- and show what each
 * is: a waveform, a frequency sliding to another, an envelope (attack,
 * sustain, decay), maybe a vibrato, a jump, a filter, an echo. A dozen
 * numbers at most, and the sound drawn as it comes out (each column the
 * samples' highest and lowest: the envelope's shape, its attack, what's
 * held, its decay; an echo's copies).
 *
 * "r" is sfxr's "mutate" button: the same sound with every number
 * nudged (Sfx.vary, a new seed each time), a variation in the same
 * family -- what a game plays so that ten shots in a row don't sound
 * like a machine gun of identical samples. Space plays it again, "e"
 * adds an echo.
 *
 * What it uses: the Playground, Scene2d (the keys pressed), Audio (sfx,
 * play) and audio/'s Sfx and Synth directly (the samples drawn).
 *)
open Playground
open Basics (* float arithmetics *)

type state = { preset : int; seed : int; echo : bool }
type model = state Scene2d.t

let initial_model : model = Scene2d.start { preset = 5; seed = 0; echo = false }

(* the sound shown and played: the preset, varied, maybe echoed *)
let current (s : state) : Sfx.t =
  let p = Sfx.vary ~seed:s.seed (snd (List.nth Sfx.presets s.preset)) in
  if s.echo then { p with echo = 0.15 } else p

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let s = scenes.scene in
  let pressed key = Scene2d.pressed (fun k -> Set_.mem key k.keys) scenes in
  let chosen = List.find_opt (fun i -> pressed (string_of_int (i +.. 1))) (List.init (List.length Sfx.presets) Fun.id) in
  let s' =
    match chosen with
    | Some i -> Some { s with preset = i; seed = 0 }
    | None when pressed "r" -> Some { s with seed = s.seed +.. 1 }
    | None when pressed "e" -> Some { s with echo = not s.echo }
    | None when Scene2d.pressed (fun k -> k.kspace) scenes -> Some s
    | None -> None
  in
  match s' with
  | Some s' ->
      Audio.play (Audio.sfx (current s'));
      { scenes with scene = s' }
  | None -> scenes

(* the sound's numbers, those in use: a name, its value *)
let numbers (p : Sfx.t) : (string * string) list =
  let wave = match p.wave with Square -> "square" | Sawtooth -> "sawtooth" | Triangle -> "triangle" | Sine -> "sine" | Noise -> "noise" in
  let opt cond line = if cond then [ line ] else [] in
  [ ("wave", wave);
    ("frequency", Printf.sprintf "%.0f Hz%s" p.frequency (if p.slide <> p.frequency then Printf.sprintf ", sliding to %.0f" p.slide else ""));
    ("envelope", Printf.sprintf "attack %.3f, sustain %.3f, decay %.3f s" p.attack p.sustain p.decay);
    ("volume", Printf.sprintf "%.2f" p.volume) ]
  @ opt (p.vibrato_depth > 0.) ("vibrato", Printf.sprintf "%.1f semitones, %.1f times a second" p.vibrato_depth p.vibrato_rate)
  @ opt (p.jump <> 0.) ("jump", Printf.sprintf "%+.0f semitones at %.3f s" p.jump p.jump_at)
  @ opt (p.low_pass > 0.)
      ("low-pass", Printf.sprintf "%.0f Hz%s" p.low_pass (if p.low_pass_to > 0. then Printf.sprintf ", falling to %.0f" p.low_pass_to else ""))
  @ opt (p.high_pass > 0.) ("high-pass", Printf.sprintf "%.0f Hz" p.high_pass)
  @ opt (p.echo > 0.) ("echo", Printf.sprintf "every %.2f s, each 0.4 of the last" p.echo)

(* the samples, 400 columns, each its highest and lowest *)
let waveform (p : Sfx.t) : shape list =
  let x = Synth.render (Sfx.to_sound p) and columns = 400 and width = 800. and height = 240. in
  let n = Array.length x in
  List.init columns (fun c ->
      let lo = c *.. n /.. columns and hi = max ((c +.. 1) *.. n /.. columns) ((c *.. n /.. columns) +.. 1) in
      let top = ref (-1.) and bottom = ref 1. in
      for i = lo to min (n -.. 1) (hi -.. 1) do
        top := max !top x.(i);
        bottom := min !bottom x.(i)
      done;
      let h = max 1. ((!top - !bottom) * height / 2.) in
      rectangle (rgb 120 220 160) (width / float_of_int columns) h
      |> move ((-.width / 2.) + ((float_of_int c + 0.5) * width / float_of_int columns)) ((!top + !bottom) * height / 4.))
  @ [ words (rgb 150 150 170) (Printf.sprintf "%.2f s" (Sfx.duration p)) |> scale 1.4 |> move ((width / 2.) - 30.) (-.(height / 2.) - 15.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and s = model.scene in
  let p = current s in
  let menu =
    List.mapi
      (fun i (name, _) ->
        words (if i = s.preset then rgb 250 200 80 else rgb 150 150 170) (Printf.sprintf "%d %s" (i +.. 1) name)
        |> scale 1.8 |> move ((-.400.) + (float_of_int i * 110.)) 420.)
      Sfx.presets
  in
  let title = fst (List.nth Sfx.presets s.preset) ^ if s.seed > 0 then Printf.sprintf ", varied (seed %d)" s.seed else "" in
  (rectangle (rgb 20 22 30) screen.width screen.height :: menu)
  @ [ words white title |> scale 2.5 |> move_y 350. ]
  @ List.concat
      (List.mapi
         (fun i (name, value) ->
           let y = 290. - (float_of_int i * 30.) in
           [ words (rgb 150 150 170) name |> scale 1.6 |> move (-.300.) y; words (rgb 220 220 235) value |> scale 1.6 |> move 60. y ])
         (numbers p))
  @ (waveform p |> List.map (move_y (-.130.)))
  @ [ words (rgb 150 150 170) "1-8: a sound   r: a variation   e: echo   space: again" |> scale 1.6 |> move_y (-.440.) ]

let help =
  {|Sfx
  keys:  1 to 8  the ready-made sounds (blip, coin, jump, laser, hit,
                 explosion, step, powerup)
         r       a variation of it (sfxr's mutate)
         e       an echo, on or off
         space   again
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
