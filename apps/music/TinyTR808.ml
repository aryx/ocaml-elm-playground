(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of the Roland TR-808 Rhythm Composer (1980), the drum
 * machine of electro, hip hop, house and trap: eleven drums
 * synthesized, not sampled, and a 16-step sequencer; and of its
 * successor the TR-909 (1983), switched in by the 808 and 909 buttons:
 * its kick's falling pitch, its cymbals 6-bit samples, its ride and
 * crash, its grey and orange. The voices and the sequencer are
 * Voice_tr808.ml over Modal, Vco, Svf, Resample and Sequencer; this is
 * the machine: its panel, a part (Part_tr808.ml, Component.mli, the
 * office's idea, plan_tiny_reason.md), hosted at full size. The shuffle (the even sixteenths late) and the flam (FL,
 * the steps struck twice) work on both.
 *
 * The panel: a column per instrument, its knobs over its name (the
 * kick's level, tone and decay; the snare's level, tone and snappy; a
 * tom's level and tuning; ...); a click on the name strikes it and
 * makes its track the one the step buttons edit (AC the accents'). The
 * 16 step buttons in the 808's colours, red, orange, yellow and cream,
 * four by four (a beat each), their lights the track's hits, the step
 * playing lit as it runs; start/stop (and space), the tempo, the accent's
 * level, the volume. Under them what the 808 never showed: the whole
 * pattern at once, a row per instrument, its cells clicked to toggle.
 * The letters strike the drums live: a s d f g h j k l the kick, snare,
 * toms, rim shot, clap, cowbell and cymbal, q and w the closed and open
 * hats.
 *
 * Uses: Voice_tr808 (the drums, the sequencer), Modal, Vco, Svf,
 * Sequencer, Audio's instruments, Part_tr808 (the panel), Component,
 * Meters (the scope), Gui (the menu). Not: the effects rack, Scene2d, Sprite, File_menu.
 *
 * Exercises: the 808's patterns chained into a song (its A/B
 * variations and its rhythm track mode); patterns saved (File_menu,
 * the "BD.steps" text); the swing (every other sixteenth late, the
 * 909's); the congas, clave and maracas the 808 switched in for the
 * toms, rim shot and clap.
 *)
open Playground
open Basics (* float arithmetics *)

(* the letters striking the drums live *)
let letters : (string * Voice_tr808.instrument) list =
  [ ("a", BD); ("s", SD); ("d", LT); ("f", MT); ("g", HT); ("h", RS); ("j", CP); ("k", CB); ("l", CY); ("q", CH); ("w", OH) ]

type model = {
  panel : Component.part; (* the 808's panel, Part_tr808 *)
  preset : int;
  held : string list;
  space : bool;
}

let presets = Voice_tr808.presets

(* the machine lives with the sound, not in the model: the mixer pulls
 * its blocks between frames (Instrument.mli) *)
let tr808 = Voice_tr808.create (snd (List.hd presets))
let inst : Instrument.t = Voice_tr808.instrument tr808
let initial_model : model = { panel = Part_tr808.make tr808; preset = 0; held = []; space = false }

(* the panel at its own size, under the title *)
let panel_box : Widget.box = { x = 0.; y = 230.; w = fst Part_tr808.natural; h = snd Part_tr808.natural }

(* the grid under: a row per instrument, a cell per step *)
let grid_x (k : int) : number = -300. + (float_of_int k * 34.)
let grid_y (row : int) : number = -65. - (float_of_int row * 24.)

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "tr808" (fun () -> inst));
  Gui.set_theme Theme.default;
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  if preset <> m.preset then Voice_tr808.set_patch tr808 (snd (List.nth presets preset));
  (* the panel, unless the menu has the mouse *)
  let panel = if Gui.modal () then m.panel else Component.input_in ~scaled:false m.panel computer panel_box in
  (* space: start/stop *)
  let space = computer.keyboard.kspace in
  if space && not m.space then Voice_tr808.run tr808 (not (Voice_tr808.running tr808));
  (* the grid's cells *)
  let mouse = computer.mouse in
  let patch =
    List.fold_left
      (fun p (row, k) ->
        if mouse.mclick && Float.abs (mouse.mx - grid_x k) <= 15. && Float.abs (mouse.my - grid_y row) <= 11. then Part_tr808.with_step p row k else p)
      (Voice_tr808.patch tr808)
      (List.concat (List.init 13 (fun row -> List.init 16 (fun k -> (row, k)))))
  in
  Voice_tr808.set_patch tr808 patch;
  (* the letters, live *)
  let now = Set_.elements computer.keyboard.keys in
  List.iter (fun (k, i) -> if List.mem k now && not (List.mem k m.held) then Voice_tr808.hit tr808 i ~accent:false) letters;
  { panel; preset; held = now; space }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 40 40 40
let green = rgb 120 220 160

(* the whole pattern: a row per instrument, the accents and the flams,
 * the step playing a column lit *)
let grid_view () : shape list =
  let p = Voice_tr808.patch tr808 in
  let running = Voice_tr808.running tr808 and now = Voice_tr808.step tr808 in
  let rows = List.map (Voice_tr808.label p.machine) Voice_tr808.instruments @ [ "AC"; "FL" ] in
  List.concat
    (List.mapi
       (fun row label ->
         (words ink label |> scale 1. |> move (-345.) (grid_y row))
         :: List.init 16 (fun k ->
                let on = (Part_tr808.track p row).(k) in
                let c = if on then (if row >= 11 then Part_tr808.stripe p.machine else rgb 40 40 40) else if running && k = now then rgb 200 200 190 else rgb 235 232 222 in
                rectangle c 30. 20. |> move (grid_x k) (grid_y row)))
       rows)

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 200 195 185) computer.screen.width computer.screen.height ]
  @ [ words black "TinyTR808" |> scale 2.4 |> move (-360.) 482.; words black "pattern" |> scale 1.5 |> move 225. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ Component.draw_in ~scaled:false m.panel panel_box ~active:true
  @ grid_view ()
  @ Meters.scope ~at:(360., -200.) ~size:(200., 180.) ~points:120 ~color:green ~back:(rgb 20 25 20) ~window:2048 ~gain:2. (Voice_tr808.recent tr808)
  @ [
      words (rgb 70 70 70) "space: start/stop   a s d f g h j k l: the drums   q w: the hats   a name: struck and edited"
      |> scale 1.2 |> move 0. (-40.);
    ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
