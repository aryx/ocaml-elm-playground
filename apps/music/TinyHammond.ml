(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of the Hammond B-3 (Hammond Organ Company, 1955) and
 * its Leslie 122 (1965), the jazz, gospel and rock organ: nine
 * drawbars on a keyboard, sines added (additive synthesis), a
 * percussion on the attack, a vibrato, and a speaker cabinet whose horn
 * and drum turn. The voice is Voice_hammond.ml over Tonewheel.ml, the
 * cabinet Leslie.mli; this is their organ: a panel and a keyboard.
 *
 * The drawbars pull down with the mouse, 0 (in) to 8 (all the way
 * out), their colours the B-3's: brown the two below the note (16', 5
 * 1/3'), white the octaves (8', 4', 2', 1'), black the others; the
 * registration they make is written as its nine digits. The percussion
 * tabs, the Leslie's rockers (on, fast) and the vibrato's rotary switch
 * click; the click and the volume knobs turn by dragging. The keys play
 * with the mouse, or with the letters, several at once (a chord is the
 * point of an organ): a s d f g h j k the white keys from C, w e t y u
 * the black ones, z and x an octave down and up; space flips the
 * Leslie between slow and fast, the way an organist's foot does. Under
 * the panel, the spectrum (each drawbar a line, the percussion a
 * flash, the Leslie's wobble) and the cabinet, its horn and drum drawn
 * turning at their speeds.
 *
 * The panel is a part, Part_hammond.ml (Component.mli, the office's
 * idea): the same panel TinyReface shows scaled in its case, and a
 * rack its device (plan_tiny_reason.md). This program is its host at
 * full size, with what a stand-alone organ adds: the keyboard (Piano),
 * the spectrum (Meters), the cabinet, the presets, the foot switch.
 *
 * Uses: Voice_hammond (the voice), Tonewheel, Leslie, Polyphony (a
 * voice per key), Audio's instruments (the organ played live),
 * Part_hammond (the panel, over Panel's widgets), Piano, Meters,
 * Component, Gui (the preset menu), Spectrum (the display). Not: the
 * effects rack, Scene2d, Sprite, File_menu.
 *
 * Exercises: the lower manual and the pedals (the B-3's second
 * keyboard, its own drawbars, and 25 pedals of 16' and 8'); the
 * drawbars heard while a note sounds (here from the next note); the
 * Leslie's brake (stopped rotors, a still sound between the speeds);
 * saving registrations with the File menu (appkit_file_menu).
 *)
open Playground
open Basics (* float arithmetics *)

type model = {
  panel : Component.part; (* the B-3's panel, Part_hammond *)
  preset : int; (* an index in Voice_hammond.presets *)
  piano : Piano.t;
  horn_angle : number; (* the Leslie's rotors, turns, drawn *)
  drum_angle : number;
  space : bool; (* space held at the last frame *)
}

let presets = Voice_hammond.presets

(* the organ lives with the sound, not in the model: the mixer pulls its
 * blocks between frames (Instrument.mli) *)
let organ = Voice_hammond.create (snd (List.hd presets))
let inst : Instrument.t = Voice_hammond.instrument organ

let initial_model : model =
  { panel = Part_hammond.make organ; preset = 0; piano = Piano.initial ~octave:4; horn_angle = 0.; drum_angle = 0.; space = false }

(* the panel at its own size, its top under the title *)
let panel_box : Widget.box = { x = 0.; y = 245.; w = fst Part_hammond.natural; h = snd Part_hammond.natural }

(*****************************************************************************)
(* The keyboard *)
(*****************************************************************************)

(* two octaves and a C *)
let look : Piano.look =
  {
    keys = 25;
    left = -420.;
    top = -165.;
    white_width = 56.;
    white_height = 270.;
    black_height = 165.;
    letters_from = 0;
    velocity = 1.;
    octaves = (2, 6);
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
  ignore (Audio.instrument "hammond" (fun () -> inst));
  Gui.set_theme Theme.default;
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  if preset <> m.preset then Voice_hammond.set_patch organ (snd (List.nth presets preset));
  (* the panel, unless the menu has the mouse *)
  let panel = if Gui.modal () then m.panel else Component.input_in ~scaled:false m.panel computer panel_box in
  let piano = Piano.update look computer m.piano inst in
  (* space: the Leslie's speed, as the organist's foot switch *)
  let space = computer.keyboard.kspace in
  let patch = Voice_hammond.patch organ in
  if space && not m.space then Voice_hammond.set_patch organ { patch with leslie_fast = not patch.leslie_fast };
  let patch = Voice_hammond.patch organ in
  let horn, drum = Voice_hammond.rotors organ in
  let turn a speed = Float.rem (a + (speed / 60.)) 1. in
  {
    panel;
    preset;
    piano;
    horn_angle = (if patch.leslie then turn m.horn_angle horn else m.horn_angle);
    drum_angle = (if patch.leslie then turn m.drum_angle (-.drum) else m.drum_angle);
    space;
  }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 240 230 210

(* the Leslie seen from above: the drum a disc with its opening, the
 * horn a bar with a mouth at one end, each at its angle *)
let leslie_view (m : model) : shape list =
  let cx = 360. and cy = -55. in
  let deg a = a * 360. in
  let patch = Voice_hammond.patch organ in
  let on = patch.leslie in
  [
    rectangle (rgb 90 55 30) 200. 130. |> move cx cy;
    circle (rgb 50 35 25) 48. |> move cx cy;
    rectangle (rgb 200 190 170) 12. 44. |> move 0. 24. |> rotate (deg m.drum_angle) |> move cx cy;
    group [ rectangle (rgb 30 30 30) 84. 10.; rectangle (rgb 30 30 30) 12. 26. |> move 42. 0. ] |> rotate (deg m.horn_angle) |> move cx cy;
    words (if on then ink else rgb 150 130 110)
      (if not on then "LESLIE OFF" else if patch.leslie_fast then "FAST" else "SLOW")
    |> scale 1.1 |> move cx (cy - 52.);
  ]

let status () : string =
  let horn, drum = Voice_hammond.rotors organ in
  Printf.sprintf "%s   voices %d   horn %.1f, drum %.1f turns a second   space: the Leslie slow or fast"
    (Voice_hammond.of_registration (Voice_hammond.patch organ)) (Voice_hammond.voices organ) horn drum

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 215 205 190) computer.screen.width computer.screen.height ]
  @ [ words black "TinyHammond" |> scale 2.4 |> move (-360.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ Component.draw_in ~scaled:false m.panel panel_box ~active:true
  (* the spectrum of the last 2048 samples: each drawbar a line *)
  @ Meters.spectrum ~at:(-150., -55.) ~size:(640., 120.) ~color:(rgb 230 170 60) ~back:(rgb 25 18 12) (Voice_hammond.recent organ)
  @ leslie_view m
  @ [ words (rgb 70 70 70) (status ()) |> scale 1.3 |> move 0. (-137.) ]
  @ Piano.view look computer m.piano ~lit:(rgb 230 170 60)
  @ [ words black (Printf.sprintf "C%d" (Piano.octave m.piano)) |> scale 1.4 |> move (look.left + 20.) (look.top + 14.) ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
