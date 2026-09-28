(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of the Yamaha DX7 (1983), the FM synthesizer of the
 * 1980s: six operators, 32 algorithms, 145 parameters a voice. The
 * voice is Voice_dx7.ml over Fm_algorithm and Dx_envelope; this is the
 * synthesizer: its panel and a keyboard.
 *
 * The panel is a part, Part_dx7.ml (Component.mli, the office's idea,
 * plan_tiny_reason.md): the same panel TinyReface's DX face shows
 * scaled in its case. This program is its host at full size, with the
 * keyboard (Piano), the spectrum and the scope (Meters), the voices and
 * a cartridge's.
 *
 * The DX7 was edited as its front panel allowed: a two-line display, a
 * button to choose one parameter, a slider and two buttons (-1, +1) to
 * change it -- 145 parameters one at a time, with nothing to show how
 * they fit together, which is why most people played the presets and
 * why every synthesizer after it had knobs again. The left half of the
 * panel is that: the LCD, < PARAM > and < OP > to walk the parameters
 * (the second jumping an operator at a time), the data slider, -1 and
 * +1. The right half is what the DX7 hid: the algorithm drawn as the
 * graph it is (the carriers at the bottom, each modulator above what it
 * modulates, the fed-back one marked), each operator lit by how loud it
 * is now, a click on one choosing its output level; and the six
 * envelopes drawn, their four rates and levels as a shape.
 *
 * The keys play with the mouse or the letters (a s d f g h j k the
 * white keys from C, w e t y u the black ones, z and x an octave down
 * and up). With the mouse, the velocity is where the key is pressed:
 * soft at its back, hard at its front -- with FM, velocity changes the
 * timbre as much as the loudness (the electric piano's bark). Under the
 * panel, the spectrum and the scope.
 *
 * A cartridge: cart= a .syx file of 32 voices (a DX7 bulk dump, the
 * kind shared on the web), natively for now, its voices added to the
 * menu after ours, < and > beside it stepping through them:
 *   dune exec apps/music/TinyDX7.exe -- cart=rom1a.syx
 *
 * Uses: Voice_dx7 (the voice, its patches, the cartridge), Fm_algorithm
 * (the graph), Polyphony (16 voices, stealing), Audio's instruments and
 * fetch, Part_dx7 (the panel, over Panel's widgets), Component, Piano,
 * Meters, Gui (the menu), Spectrum. Not: the
 * effects rack, Scene2d, Sprite, File_menu.
 *
 * Exercises: the Reface DX's mode (four operators, its twelve
 * algorithms); saving a voice or a cartridge (File_menu, Voice_dx7's
 * to_cartridge); the operators switched on and off (the DX7's buttons
 * 1 to 6, for hearing one at a time); a MIDI keyboard's velocity and
 * its pitch bend.
 *)
open Playground
open Basics (* float arithmetics *)

type model = {
  panel : Component.part; (* the DX7's panel, Part_dx7 *)
  voices : (string * Voice_dx7.patch) list; (* ours, then a cartridge's *)
  voice : int; (* an index in [voices] *)
  keys : Piano.t;
  loaded : bool; (* the cart= flag looked at *)
  message : string;
}

(* the synthesizer lives with the sound, not in the model: the mixer
 * pulls its blocks between frames (Instrument.mli) *)
let dx7 = Voice_dx7.create (snd (List.hd Voice_dx7.presets))
let inst : Instrument.t = Voice_dx7.instrument dx7

(* the voice's number on the LCD: the menu's, a cartridge's voices in it *)
let number = ref 0

let initial_model : model =
  {
    panel = Part_dx7.make ~number:(fun () -> !number) dx7;
    voices = Voice_dx7.presets;
    voice = 0;
    keys = Piano.initial ~octave:4;
    loaded = false;
    message = "";
  }

(* a cartridge fetched by cart= arrives here *)
let fetched : string option option ref = ref None

(* the panel at its own size, under the title *)
let panel_box : Widget.box = { x = 0.; y = 197.; w = fst Part_dx7.natural; h = snd Part_dx7.natural }

let green = rgb 120 220 160

(* two octaves and a C; with the mouse, the velocity is where the key is
 * pressed -- with FM, the timbre as much as the loudness *)
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
  ignore (Audio.instrument "dx7" (fun () -> inst));
  (* cart=: fetched once, its voices added when they arrive *)
  let m =
    if m.loaded then m
    else begin
      Option.iter (fun src -> Audio.fetch src (fun bytes -> fetched := Some bytes)) (List.assoc_opt "cart" computer.flags);
      { m with loaded = true }
    end
  in
  let m =
    match !fetched with
    | None -> m
    | Some bytes -> (
        fetched := None;
        match Option.map Voice_dx7.cartridge bytes with
        | Some (Ok voices) ->
            let named = Array.to_list (Array.map (fun (p : Voice_dx7.patch) -> (String.trim p.name, p)) voices) in
            { m with voices = Voice_dx7.presets @ named; message = "cartridge: 32 voices" }
        | Some (Error e) -> { m with message = "cartridge: " ^ e }
        | None -> { m with message = "cartridge: can't be read" })
  in
  (* the menu, and < > for a cartridge's 32, more than the screen holds *)
  let count = List.length m.voices in
  let voice = Gui.menu computer ~at:(330., 482.) (List.map fst m.voices) m.voice in
  let voice = if Gui.button computer ~at:(250., 482.) "<" then (voice +.. count -.. 1) mod count else voice in
  let voice = if Gui.button computer ~at:(410., 482.) ">" then (voice +.. 1) mod count else voice in
  if voice <> m.voice then Voice_dx7.set_patch dx7 (snd (List.nth m.voices voice));
  number := voice;
  (* the panel, unless the menu has the mouse *)
  let panel = if Gui.modal () then m.panel else Component.input_in ~scaled:false m.panel computer panel_box in
  { m with panel; voice; keys = Piano.update look computer m.keys inst }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 200 195 185) computer.screen.width computer.screen.height ]
  @ [ words black "TinyDX7" |> scale 2.4 |> move (-380.) 482.; words black "voice" |> scale 1.5 |> move 190. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-190.) 482. ]
  @ [ words (rgb 70 70 70) m.message |> scale 1.3 |> move 20. 482. ]
  @ Component.draw_in ~scaled:false m.panel panel_box ~active:true
  (* the spectrum of the last 2048 samples, and the scope *)
  @ Meters.spectrum ~at:(-160., -95.) ~size:(620., 110.) ~color:green ~back:(rgb 20 25 20) (Voice_dx7.recent dx7)
  @ Meters.scope ~at:(330., -95.) ~size:(300., 110.) ~points:150 ~color:green ~back:(rgb 20 25 20) ~gain:3. (Voice_dx7.recent dx7)
  @ [
      words (rgb 70 70 70)
        (Printf.sprintf "voices %d   the mouse: soft at a key's back, hard at its front" (Voice_dx7.voices dx7))
      |> scale 1.3 |> move 0. (-162.);
    ]
  @ Piano.view look computer m.keys ~lit:green
  @ [ words black (Printf.sprintf "C%d" (Piano.octave m.keys)) |> scale 1.4 |> move (look.left + 20.) (look.top + 14.) ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) app)
