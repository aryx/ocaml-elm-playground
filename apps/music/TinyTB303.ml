(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of the Roland TB-303 Bass Line (1981): a monophonic
 * synthesizer with its own 16-step sequencer, made for guitarists to
 * practise with, which failed, and then, its knobs turned while it
 * played, became acid house. The voice and its sequencer are
 * Voice_tb303.ml (over Diode_ladder.mli and Sequencer.mli); this is
 * the machine: its panel and its pattern.
 *
 * The panel and the pattern's grid are a part, Part_tb303.ml
 * (Component.mli, the office's idea, plan_tiny_reason.md), the same a
 * rack would hold. This program is its host at full size, with the
 * letters (Piano), space, the scope and the spectrum (Meters), the
 * presets.
 *
 * The real 303 is famously hard to program (a keypad, pitches and times
 * entered separately, blind); here the pattern is a grid to see and
 * click: the 16 steps as columns, a piano roll of an octave above C2 --
 * a click sets the step's note, again the same cell makes it a rest --
 * and under it each step's octave (down, as written, up), accent and
 * slide, toggled by a click. The step playing is lit. The knobs, the
 * 303's in its order (Tuning, Cut Off Freq, Resonance, Env Mod, Decay,
 * Accent), then the waveform, the tempo and the volume, turn by
 * dragging; run starts the pattern (space too). The letters play along,
 * a s d f g h j k the white keys from C, w e t y u the black ones, z and
 * x an octave. Under the grid, the scope and the spectrum: the squelch
 * seen.
 *
 * Parameter locks, which the 303 never had (Sequencer.mli): a click on
 * a step's number holds it -- the OP-XY's "hold a step, turn a knob",
 * with one mouse -- and the sound's knobs then show and set that step's
 * locks (a dot on the number: it has some; CLEAR: none). The rocker
 * says what they mean between steps, the step's only (Elektron's) or
 * points the knob goes through (the OP-XY's), SMOOTH how it glides
 * from one to the next. The preset "locks" is a worked example.
 *
 * Uses: Voice_tb303 (the voice, its patterns), Sequencer (the audio
 * clock's steps), Diode_ladder, Audio's instruments (the voice played
 * live), Part_tb303 (the panel and the grid, over Panel's widgets),
 * Component, Piano (the letters), Meters, Gui (the menu), Spectrum.
 * Not: the
 * effects rack, Scene2d, Sprite, File_menu.
 *
 * Exercises: the locks drawn as the knob's curve over the bar (the
 * value through the steps, as the OP-XY's screen shows it); a lock per
 * knob cleared; several patterns and a song (the 303's pattern chains);
 * the gate's length and the slide's time as knobs (the Devil Fish's
 * mods); swing (every other 16th late); the effects rack after it (a
 * delay and a distortion: the acid house of 1988 was mostly that);
 * saving patterns with the File menu.
 *)
open Playground
open Basics (* float arithmetics *)

type model = {
  panel : Component.part; (* the knobs and the pattern, Part_tb303 *)
  preset : int;
  keys : Piano.t; (* the letters only: no keyboard drawn *)
  space : bool; (* space held at the last frame *)
}

let presets = Voice_tb303.presets

(* the voice lives with the sound: the mixer pulls its blocks, and its
 * sequencer steps in them *)
let voice = Voice_tb303.create (snd (List.hd presets))
let inst : Instrument.t = Voice_tb303.instrument voice
let initial_model : model = { panel = Part_tb303.make voice; preset = 0; keys = Piano.initial ~octave:3; space = false }

(* the panel at its own size, under the title *)
let panel_box : Widget.box = { x = 0.; y = 160.; w = fst Part_tb303.natural; h = snd Part_tb303.natural }

(* the letters, a line played over the pattern: no keys to draw *)
let look : Piano.look =
  {
    keys = 0;
    left = 0.;
    top = 0.;
    white_width = 0.;
    white_height = 0.;
    black_height = 0.;
    letters_from = 0;
    velocity = 1.;
    octaves = (1, 5);
    by_depth = false;
    white_key = white;
    black_key = black;
    letter_on_white = black;
    letter_scale = 1.;
    letter_lift = 0.;
  }

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "tb303" (fun () -> inst));
  Gui.set_theme Theme.default;
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  (* a preset chosen through the panel's menu: it lets go of the step held *)
  let panel = if preset <> m.preset then m.panel.command (fst (List.nth presets preset)) else m.panel in
  (* the panel, unless the menu has the mouse *)
  let panel = if Gui.modal () then panel else Component.input_in ~scaled:false panel computer panel_box in
  let space = computer.keyboard.kspace in
  if space && not m.space then Voice_tb303.run voice (not (Voice_tb303.running voice));
  { panel; preset; keys = Piano.update look computer m.keys inst; space }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

(* the scope, from a rising zero crossing: a steady note stands still *)
let scope_view (samples : Signal.t) : shape list =
  let cx = -235. and cy = -260. and w = 440. and h = 150. in
  let start = ref 0 in
  (try
     for i = 1 to 1023 do
       if samples.(i -.. 1) < 0. && samples.(i) >= 0. then (
         start := i;
         raise Exit)
     done
   with Exit -> ());
  let point j = (cx - (w / 2.) + (float_of_int j * w / 256.), cy + (Float.max (-1.) (Float.min 1. samples.(!start +.. (j *.. 4))) * h / 2.)) in
  let segment (x1, y1) (x2, y2) =
    rectangle (rgb 120 255 140) (Float.hypot (x2 - x1) (y2 - y1) + 1.) 2.
    |> rotate (Float.atan2 (y2 - y1) (x2 - x1) * 180. / Float.pi)
    |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)
  in
  (rectangle (rgb 15 25 15) w h |> move cx cy) :: List.init 255 (fun j -> segment (point j) (point (j +.. 1)))

let view (computer : computer) (m : model) : shape list =
  let samples = Voice_tb303.recent voice in
  let p = Voice_tb303.patch voice in
  [ rectangle (rgb 215 215 220) computer.screen.width computer.screen.height ]
  @ [ words black "TinyTB303" |> scale 2.4 |> move (-380.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ Component.draw_in ~scaled:false m.panel panel_box ~active:true
  @ scope_view samples
  @ Meters.spectrum ~at:(235., -260.) ~size:(440., 150.) ~color:(rgb 230 80 40) ~back:(rgb 25 20 18) ~bars:60 samples
  @ [
      words (rgb 70 70 70)
        (Printf.sprintf "%s   %.0f BPM   cutoff %.0f Hz   letters: play along   space: run / stop"
           (if Voice_tb303.running voice then Printf.sprintf "step %d" (Voice_tb303.step voice +.. 1) else "stopped")
           p.bpm (Voice_tb303.cutoff_now voice))
      |> scale 1.3 |> move 0. (-360.);
    ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
