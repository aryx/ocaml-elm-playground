(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of the Roland Juno-106 (1984), the polyphonic
 * synthesizer everyone could afford: six voices of one oscillator each,
 * made big by its chorus. The voice is Voice_juno.ml over Vco,
 * Moog_ladder and Envelope; this is the synthesizer: its panel and a
 * keyboard.
 *
 * The panel is the 106's, a part, Part_juno.ml (Component.mli, the
 * office's idea, plan_tiny_reason.md): vertical sliders in its
 * sections, LFO, DCO, HPF, VCF, VCA, ENV, and under them its buttons,
 * each with its light, the chorus's among them. This program is its
 * host at full size.
 *
 * The keys play with the mouse or the letters (a s d f g h j k the
 * white keys from C, w e t y u the black ones, z and x an octave down
 * and up), every key at full: the Juno-106 has no velocity (a cheap
 * keyboard was part of the price). Under the panel, the spectrum and the
 * scope, both sides of the chorus.
 *
 * Uses: Voice_juno (the voice), Vco, Moog_ladder, Envelope, Polyphony,
 * Audio's instruments, Part_juno (the panel), Component, Piano,
 * Meters, Gui (the menu), Spectrum. Not: the
 * effects rack (the chorus is the Juno's own), Scene2d, Sprite,
 * File_menu.
 *
 * Exercises: the 106's 128 patches as banks (File_menu to save them,
 * its SysEx to read a real one's); the bender's lever (pitch and the
 * filter, the 106's); the hold button; the 60's arpeggiator.
 *)
open Playground
open Basics (* float arithmetics *)

type model = {
  panel : Component.part; (* the 106's panel, Part_juno *)
  preset : int;
  piano : Piano.t;
}

let presets = Voice_juno.presets

(* the synthesizer lives with the sound, not in the model: the mixer
 * pulls its blocks between frames (Instrument.mli) *)
let juno = Voice_juno.create (snd (List.hd presets))
let inst : Instrument.t = Voice_juno.instrument juno
let initial_model : model = { panel = Part_juno.make juno; preset = 0; piano = Piano.initial ~octave:4 }

(* the panel at its own size, under the title *)
let panel_box : Widget.box = { x = 0.; y = 250.; w = fst Part_juno.natural; h = snd Part_juno.natural }

let orange = rgb 235 120 50
let green = rgb 120 220 160

(* two octaves and a C, every key at full: the Juno-106 has no velocity *)
let look : Piano.look =
  {
    keys = 25;
    left = -420.;
    top = -175.;
    white_width = 56.;
    white_height = 270.;
    black_height = 165.;
    letters_from = 0;
    velocity = 1.;
    octaves = (1, 6);
    by_depth = false;
    white_key = rgb 250 250 245;
    black_key = rgb 20 20 20;
    letter_on_white = rgb 120 120 120;
    letter_scale = 1.6;
    letter_lift = 18.;
  }

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "juno" (fun () -> inst));
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  if preset <> m.preset then Voice_juno.set_patch juno (snd (List.nth presets preset));
  (* the panel, unless the menu has the mouse *)
  let panel = if Gui.modal () then m.panel else Component.input_in ~scaled:false m.panel computer panel_box in
  { panel; preset; piano = Piano.update look computer m.piano inst }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 200 195 185) computer.screen.width computer.screen.height ]
  @ [ words black "TinyJuno" |> scale 2.4 |> move (-370.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ Component.draw_in ~scaled:false m.panel panel_box ~active:true
  @ Meters.spectrum ~at:(-160., -70.) ~size:(620., 100.) ~color:green ~back:(rgb 20 25 20) (Voice_juno.recent juno)
  @ Meters.scope ~at:(330., -70.) ~size:(300., 100.) ~points:150 ~color:green ~back:(rgb 20 25 20) ~gain:3. (Voice_juno.recent juno)
  @ [
      words (rgb 70 70 70) (Printf.sprintf "voices %d of 6   every key at full: the 106 has no velocity" (Voice_juno.voices juno))
      |> scale 1.3 |> move 0. (-137.);
    ]
  @ Piano.view look computer m.piano ~lit:orange
  @ [ words black (Printf.sprintf "C%d" (Piano.octave m.piano)) |> scale 1.4 |> move (look.left + 20.) (look.top + 14.) ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
