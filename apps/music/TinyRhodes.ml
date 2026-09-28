(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of the Fender Rhodes (Mark I Stage 73, 1970) and its
 * Suitcase amplifier, with the Wurlitzer 200A (1974) and the Hohner
 * Clavinet D6 (1971) behind a switch: the electric pianos of soul,
 * jazz-funk and every ballad. The voice is Voice_rhodes.ml over Modal
 * and Pluck; this is the piano: its panel and a keyboard.
 *
 * The panel is a part, Part_rhodes.ml (Component.mli, the office's idea,
 * plan_tiny_reason.md): the same panel TinyReface's CP face shows
 * scaled in its case. This program is its host at full size, with the
 * keyboard (Piano), the spectrum and the scope (Meters), the presets.
 *
 * The knobs: the model; the voicing (how far the tine sits off its
 * pickup's centre, a screwdriver's job on the real one; the Wurlitzer's
 * reed nearer its plate); the hammer's hardness; the decay; the
 * Suitcase's vibrato, rate and depth; the volume. Beside them what
 * can't be seen on the real one: the pickup's curve, the flux against
 * the tip's position (the Rhodes' bell, the Wurlitzer's capacitance),
 * and on it the span the last note's tip swings across now -- press a
 * key softly, a short span on the bell's side, nearly a sine; hard, the
 * span across its top: the bark. The Suitcase's two speakers light as
 * the tremolo moves the sound between them.
 *
 * The keys play with the mouse or the letters (a s d f g h j k the
 * white keys from C, w e t y u the black ones, z and x an octave down
 * and up); with the mouse, the velocity is where the key is pressed,
 * soft at its back, hard at its front. Under the panel, the spectrum
 * and the scope.
 *
 * Uses: Voice_rhodes (the voice), Modal, Pluck, Polyphony, Audio's
 * instruments, Part_rhodes (the panel, over Panel's widgets),
 * Component, Piano, Meters, Gui (the menu), Spectrum. Not:
 * the effects rack (the Reface CP's row of effects: an exercise),
 * Scene2d, Sprite, File_menu.
 *
 * Exercises: the effects row after it, as the Reface CP has (drive,
 * the wah, the phaser, the delay: the rack's); the tine's arc and its
 * two polarisations (Pfeifle's model); a sustain pedal (the dampers
 * held off); the 88-key Rhodes' range.
 *)
open Playground
open Basics (* float arithmetics *)

type model = {
  panel : Component.part; (* the Stage 73's panel, Part_rhodes *)
  preset : int;
  keys : Piano.t;
}

let presets = Voice_rhodes.presets

(* the piano lives with the sound, not in the model: the mixer pulls
 * its blocks between frames (Instrument.mli) *)
let piano = Voice_rhodes.create (snd (List.hd presets))
let inst : Instrument.t = Voice_rhodes.instrument piano
let initial_model : model = { panel = Part_rhodes.make piano; preset = 0; keys = Piano.initial ~octave:4 }

(* the panel at its own size, under the title *)
let panel_box : Widget.box = { x = 0.; y = 200.; w = fst Part_rhodes.natural; h = snd Part_rhodes.natural }

let green = rgb 120 220 160

(* two octaves and a C; with the mouse, the velocity is where the key is
 * pressed *)
let look : Piano.look =
  {
    keys = 25;
    left = -420.;
    top = -175.;
    white_width = 56.;
    white_height = 270.;
    black_height = 165.;
    letters_from = 0;
    velocity = 0.8;
    octaves = (1, 6);
    by_depth = true;
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
  ignore (Audio.instrument "rhodes" (fun () -> inst));
  Gui.set_theme Theme.default;
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  if preset <> m.preset then Voice_rhodes.set_patch piano (snd (List.nth presets preset));
  (* the panel, unless the menu has the mouse *)
  let panel = if Gui.modal () then m.panel else Component.input_in ~scaled:false m.panel computer panel_box in
  { panel; preset; keys = Piano.update look computer m.keys inst }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 200 195 185) computer.screen.width computer.screen.height ]
  @ [ words black "TinyRhodes" |> scale 2.4 |> move (-360.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ Component.draw_in ~scaled:false m.panel panel_box ~active:true
  @ Meters.spectrum ~at:(-160., -95.) ~size:(620., 110.) ~color:green ~back:(rgb 20 25 20) (Voice_rhodes.recent piano)
  @ Meters.scope ~at:(330., -95.) ~size:(300., 110.) ~points:150 ~color:green ~back:(rgb 20 25 20) ~gain:3. (Voice_rhodes.recent piano)
  @ [
      words (rgb 70 70 70)
        (Printf.sprintf "voices %d   the mouse: soft at a key's back, hard at its front" (Voice_rhodes.voices piano))
      |> scale 1.3 |> move 0. (-162.);
    ]
  @ Piano.view look computer m.keys ~lit:(rgb 230 120 60)
  @ [ words black (Printf.sprintf "C%d" (Piano.octave m.keys)) |> scale 1.4 |> move (look.left + 20.) (look.top + 14.) ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
