(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of the Minimoog Model D (Moog Music, 1970), the
 * synthesizer that took the modular synthesizer's sound out of the studio
 * and onto the stage: three oscillators, a mixer, the ladder filter, two
 * contours, glide and a modulation wheel, wired once and for all behind
 * a panel read left to right. The voice is Minimoog_voice.ml, over
 * audio/'s blocks; this is its panel and its keyboard.
 *
 * The computer's letters are the keys, as in AudioPiano: a s d f g h j
 * k the white keys from C, w e t y u the black ones; z and x an octave
 * down and up. The arrows up and down bend the pitch (the pitch wheel);
 * the mod wheel is a slider. Hold a key and press another: the lowest
 * sounds (low-note priority), and the contours go on (legato). Keys 1
 * to 4 flip the teaching switches: the ladder (naive, zero-delay,
 * nonlinear), the contours' curves (straight or exponential), the
 * oscillators' drift, and their band-limiting -- the simple and the
 * better versions of notes_synth.md, on the same patch.
 *
 * The panel here is plain -- a slider per knob, a box per switch, a menu
 * per selector, generated from Minimoog_voice.knobs, the same table the
 * patches are written with -- until the Model D's own (phase 5 of
 * plan_synth_teaching.md).
 *
 * Uses: Minimoog_voice (the voice), Audio's instruments (the voice
 * played live), Gui (the panel). Not: Scene2d, Sprite, the physics.
 *
 * Exercises: the reissue's additions (a separate LFO, a choice of note
 * priority, the filter contour as a modulation source); velocity on
 * the filter, from a MIDI keyboard; a second voice, the Minimoog made
 * duophonic like the ARP Odyssey.
 *)
open Playground
open Basics (* float arithmetics *)

(* the keys: the letter and the semitone above the octave's C *)
let keys =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

type model = {
  patch : Minimoog_voice.patch;
  preset : int; (* an index in Minimoog_voice.presets *)
  octave : int; (* the a key's C: C3 *)
  mod_wheel : float;
  held : string list; (* the keys held at the last frame *)
  options : Minimoog_voice.options;
}

let presets = Minimoog_voice.presets

let initial_model : model =
  { patch = snd (List.hd presets); preset = 0; octave = 3; mod_wheel = 0.; held = []; options = Minimoog_voice.analog }

(* the voice lives with the sound, not in the model: the mixer pulls
 * its blocks between frames (Instrument.mli) *)
let voice = Minimoog_voice.create initial_model.patch
let inst : Instrument.t = Minimoog_voice.instrument voice

let note (octave : int) (semitone : int) : int = (12 *.. (octave +.. 1)) +.. semitone

let next_ladder (l : Moog_ladder.model) : Moog_ladder.model =
  match l with Naive -> Zero_delay | Zero_delay -> Nonlinear | Nonlinear -> Naive

(* {1 The panel, from the knobs' table} *)

let columns = 3
let rows = 12
let column_x (c : int) : number = -345. + (325. * float_of_int c)
let row_y (r : int) : number = 390. - (50. * float_of_int r)

let control (computer : computer) (i : int) (k : Minimoog_voice.knob) (p : Minimoog_voice.patch) : Minimoog_voice.patch =
  let x = column_x (i /.. rows) and y = row_y (i mod rows) in
  Gui.label computer ~at:(x - 100., y) k.name;
  let v = k.get p in
  let v' =
    match k.control with
    | Knob (from, to_) -> Gui.slider computer ~at:(x + 80., y) ~from ~to_ v
    | Switch -> if Gui.checkbox computer ~at:(x + 20., y) "" (v >= 0.5) then 1. else 0.
    | Selector labels -> float_of_int (Gui.menu computer ~at:(x + 80., y) labels (int_of_float v))
  in
  if v' <> v then k.put p v' else p

let update (computer : computer) (m : model) : model =
  (* playing from the first frame, and kept playing *)
  ignore (Audio.instrument "minimoog" (fun () -> inst));
  (* the preset menu first: a new one replaces the whole panel *)
  let names = List.map fst presets in
  let preset = Gui.menu computer ~at:(330., 460.) names m.preset in
  let patch = if preset <> m.preset then snd (List.nth presets preset) else m.patch in
  let patch = List.fold_left (fun (p, i) k -> (control computer i k p, i +.. 1)) (patch, 0) Minimoog_voice.knobs |> fst in
  Gui.label computer ~at:(-330., -230.) "mod wheel";
  let mod_wheel = Gui.slider computer ~at:(-160., -230.) ~from:0. ~to_:1. m.mod_wheel in
  (* the keys: pressed and let go since the last frame *)
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) and released k = List.mem k m.held && not (List.mem k now) in
  let octave = if pressed "z" then max 1 (m.octave -.. 1) else if pressed "x" then min 6 (m.octave +.. 1) else m.octave in
  List.iter
    (fun (k, semitone) ->
      let n = note m.octave semitone in
      if pressed k then inst.note_on n 1.;
      if released k then inst.note_off n)
    keys;
  (* the teaching switches *)
  let o = m.options in
  let options =
    if pressed "1" then { o with ladder = next_ladder o.ladder }
    else if pressed "2" then { o with curve = (match o.curve with Linear -> Exponential | Exponential -> Linear) }
    else if pressed "3" then { o with drift = not o.drift }
    else if pressed "4" then { o with band_limited = not o.band_limited }
    else o
  in
  Minimoog_voice.set_patch voice patch;
  Minimoog_voice.set_options voice options;
  inst.set "mod_wheel" mod_wheel;
  inst.set "pitch_wheel" (if computer.keyboard.kup then 1. else if computer.keyboard.kdown then -1. else 0.);
  { patch; preset; octave; mod_wheel; held = now; options }

(* {1 The keyboard, drawn} *)

let key_width = 50.

let keyboard_view (computer : computer) (m : model) : shape list =
  let held k = Set_.mem k computer.keyboard.keys in
  let whites = List.filter (fun (_, s) -> not (List.mem (s mod 12) [ 1; 3; 6; 8; 10 ])) keys in
  let white_x i = (float_of_int i - 4.) * key_width in
  let white_shapes =
    List.mapi
      (fun i (k, _) ->
        group
          [
            rectangle (if held k then rgb 255 200 120 else white) (key_width - 4.) 150.;
            words black k |> move_y (-55.);
          ]
        |> move (white_x i) (-360.))
      whites
  in
  (* a black key between the white keys before and after its semitone *)
  let black_shapes =
    List.filter_map
      (fun (k, s) ->
        if List.mem (s mod 12) [ 1; 3; 6; 8; 10 ] then
          let before = List.length (List.filter (fun (_, w) -> w < s) whites) -.. 1 in
          Some
            (group [ rectangle (if held k then rgb 200 120 40 else black) (key_width * 0.6) 90.; words white k |> move_y (-25.) ]
            |> move (white_x before + (key_width / 2.)) (-330.))
        else None)
      keys
  in
  white_shapes @ black_shapes @ [ words black (Printf.sprintf "C%d" m.octave) |> move (white_x 0) (-455.) ]

let status (m : model) : string =
  let o = m.options in
  Printf.sprintf "1 ladder: %s   2 contours: %s   3 drift: %s   4 oscillators: %s" (Moog_ladder.name o.ladder)
    (match o.curve with Linear -> "straight" | Exponential -> "exponential")
    (if o.drift then "on" else "off")
    (if o.band_limited then "band-limited" else "naive")

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 235 230 220) computer.screen.width computer.screen.height ]
  @ [
      words black "TinyMinimoog" |> scale 1.6 |> move (-330.) 460.;
      words black "preset" |> move 190. 460.;
      words (rgb 90 90 90) (status m) |> move 0. (-270.);
      words (rgb 90 90 90) "keys a to k, w e t y u; z x octave; up down: the pitch wheel" |> move 0. (-480.);
    ]
  @ keyboard_view computer m
  @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
