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
 * its panel. The shuffle (the even sixteenths late) and the flam (FL,
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
 * Sequencer, Audio's instruments, Gui (the knobs, the buttons, the
 * menu), Spectrum. Not: the effects rack, Scene2d, Sprite, File_menu.
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
  patch : Voice_tr808.patch;
  preset : int;
  selected : int; (* the track the step buttons edit: an instrument's index, 11 the accents *)
  held : string list;
  space : bool;
}

let presets = Voice_tr808.presets
let initial_model : model = { patch = snd (List.hd presets); preset = 0; selected = 0; held = []; space = false }

(* the machine lives with the sound, not in the model: the mixer pulls
 * its blocks between frames (Instrument.mli) *)
let tr808 = Voice_tr808.create initial_model.patch
let inst : Instrument.t = Voice_tr808.instrument tr808

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 40 40 40;
    accent = rgb 220 90 40;
    edge = rgb 120 120 120;
    face = rgb 230 225 210;
    face_hot = rgb 245 240 225;
    face_down = rgb 200 195 180;
    text_size = 12.;
    dial = 26.;
    dial_face = rgb 35 35 35;
    pointer = rgb 240 240 240;
  }

(* each instrument's knobs on each machine: the patch's field, the word
 * on the panel (the 909's kick's "attack" is the tone's field) *)
let knobs_of (machine : int) (i : Voice_tr808.instrument) : (string * string) list =
  let same = List.map (fun f -> (f, f)) in
  match (machine, i) with
  | 0, BD -> same [ "level"; "tone"; "decay" ]
  | 0, SD -> same [ "level"; "tone"; "snappy" ]
  | 0, (LT | MT | HT) -> same [ "level"; "tuning" ]
  | 0, CY -> same [ "level"; "tone"; "decay" ]
  | 0, OH -> same [ "level"; "decay" ]
  | 0, _ -> same [ "level" ]
  | _, BD -> [ ("level", "level"); ("tuning", "tune"); ("tone", "attack"); ("decay", "decay") ]
  | _, SD -> [ ("level", "level"); ("tuning", "tune"); ("tone", "tone"); ("snappy", "snappy") ]
  | _, (LT | MT | HT) -> [ ("level", "level"); ("tuning", "tune"); ("decay", "decay") ]
  | _, (CH | OH) -> same [ "level"; "decay" ]
  | _, (CY | CB) -> [ ("level", "level"); ("tuning", "tune") ]
  | _, _ -> same [ "level" ]

let column_x (k : int) : number = -420. + (float_of_int k * 84.)
let name_y = 228.
let knob_y (row : int) : number = 425. - (float_of_int row * 50.)

(* the controls' row, and the step buttons *)
let controls_y = 190.
let step_x (k : int) : number = -435. + (float_of_int k * 58.)
let step_y = 85.

(* the 808's colours, four by four; the 909's grey *)
let step_color (machine : int) (k : int) : color =
  if machine = 1 then rgb 215 215 212
  else match k /.. 4 with 0 -> rgb 205 55 45 | 1 -> rgb 230 130 45 | 2 -> rgb 235 200 70 | _ -> rgb 235 230 210

(* the grid under: a row per instrument, a cell per step *)
let grid_x (k : int) : number = -300. + (float_of_int k * 34.)
let grid_y (row : int) : number = -65. - (float_of_int row * 24.)

(* the tracks the buttons edit: the instruments', then the accents (11)
 * and the flams (12) *)
let track (p : Voice_tr808.patch) (sel : int) : bool array = if sel = 11 then p.accents else if sel = 12 then p.flams else p.tracks.(sel)

let with_step (p : Voice_tr808.patch) (sel : int) (k : int) : Voice_tr808.patch =
  if sel = 11 then begin
    let a = Array.copy p.accents in
    a.(k) <- not a.(k);
    { p with accents = a }
  end
  else if sel = 12 then begin
    let f = Array.copy p.flams in
    f.(k) <- not f.(k);
    { p with flams = f }
  end
  else begin
    let tracks = Array.map Array.copy p.tracks in
    tracks.(sel).(k) <- not tracks.(sel).(k);
    { p with tracks }
  end

let knob_control (computer : computer) (p : Voice_tr808.patch) (name : string) (at : number * number) ~(from : number) ~(to_ : number) :
    Voice_tr808.patch =
  match List.find_opt (fun (k : Voice_tr808.knob) -> k.name = name) Voice_tr808.knobs with
  | None -> p
  | Some k ->
      let v = k.get p in
      let v' = Gui.knob computer ~at ~from ~to_ v in
      if v' <> v then k.put p v' else p

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "tr808" (fun () -> inst));
  Gui.set_theme Theme.default;
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  let patch = if preset <> m.preset then snd (List.nth presets preset) else m.patch in
  Gui.set_theme panel_theme;
  (* the instruments' knobs *)
  let patch =
    List.fold_left
      (fun p (k, i) ->
        let names = knobs_of patch.machine i in
        List.fold_left
          (fun p (row, (field, _)) -> knob_control computer p (Voice_tr808.name i ^ "." ^ field) (column_x k, knob_y row) ~from:0. ~to_:1.)
          p (List.mapi (fun row n -> (row, n)) names))
      patch
      (List.mapi (fun k i -> (k, i)) Voice_tr808.instruments)
  in
  (* the tempo, whole beats; the accent, the shuffle, the flam; the
   * volume *)
  let tempo = Float.round (Gui.knob computer ~at:(150., controls_y) ~from:60. ~to_:180. patch.tempo) in
  let patch = if tempo <> patch.tempo then { patch with tempo } else patch in
  let patch = knob_control computer patch "accent" (220., controls_y) ~from:0. ~to_:1. in
  let patch = knob_control computer patch "shuffle" (290., controls_y) ~from:0. ~to_:1. in
  let patch = knob_control computer patch "flam" (360., controls_y) ~from:0. ~to_:1. in
  let patch = knob_control computer patch "volume" (430., controls_y) ~from:0. ~to_:1. in
  (* the machine *)
  let patch = if Gui.button computer ~at:(10., controls_y) "808" then { patch with machine = 0 } else patch in
  let patch = if Gui.button computer ~at:(65., controls_y) "909" then { patch with machine = 1 } else patch in
  (* start/stop, and space *)
  let space = computer.keyboard.kspace in
  let run = Gui.button computer ~at:(-380., controls_y) (if Voice_tr808.running tr808 then "STOP" else "START") in
  if run || (space && not m.space) then Voice_tr808.run tr808 (not (Voice_tr808.running tr808));
  let mouse = computer.mouse in
  let click = mouse.mclick in
  (* a name clicked: struck, and its track the one edited; AC *)
  let selected =
    List.fold_left
      (fun sel (k, i) ->
        if click && Float.abs (mouse.mx - column_x k) <= 38. && Float.abs (mouse.my - name_y) <= 14. then begin
          Voice_tr808.hit tr808 i ~accent:false;
          k
        end
        else sel)
      m.selected
      (List.mapi (fun k i -> (k, i)) Voice_tr808.instruments)
  in
  let selected = if Gui.button computer ~at:(-280., controls_y) "AC" then 11 else selected in
  let selected = if Gui.button computer ~at:(-225., controls_y) "FL" then 12 else selected in
  (* the step buttons, and the grid's cells *)
  let patch =
    List.fold_left
      (fun p k -> if click && Float.abs (mouse.mx - step_x k) <= 24. && Float.abs (mouse.my - step_y) <= 28. then with_step p selected k else p)
      patch (List.init 16 (fun k -> k))
  in
  let patch =
    List.fold_left
      (fun p (row, k) ->
        if click && Float.abs (mouse.mx - grid_x k) <= 15. && Float.abs (mouse.my - grid_y row) <= 11. then with_step p row k else p)
      patch
      (List.concat (List.init 13 (fun row -> List.init 16 (fun k -> (row, k)))))
  in
  (* the letters, live *)
  let now = Set_.elements computer.keyboard.keys in
  List.iter (fun (k, i) -> if List.mem k now && not (List.mem k m.held) then Voice_tr808.hit tr808 i ~accent:false) letters;
  Voice_tr808.set_patch tr808 patch;
  { patch; preset; selected; held = now; space }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 40 40 40
let green = rgb 120 220 160

(* a line from one point to another, [w] wide *)
let segment (c : color) (w : number) (x1, y1) (x2, y2) : shape =
  let dx = x2 - x1 and dy = y2 - y1 in
  rectangle c (sqrt ((dx * dx) + (dy * dy))) w |> rotate (atan2 dy dx * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

(* the machine's colours: the 808's dark body, cream top and red
 * stripe; the 909's grey and orange *)
type look = { body : color; top : color; stripe : color; chosen : color; led : color }

let look (machine : int) : look =
  if machine = 1 then { body = rgb 95 95 98; top = rgb 205 205 202; stripe = rgb 235 120 40; chosen = rgb 235 120 40; led = rgb 255 140 30 }
  else { body = rgb 45 45 45; top = rgb 230 225 210; stripe = rgb 205 55 45; chosen = rgb 230 130 45; led = rgb 255 50 30 }

let track_name (m : model) : string =
  match m.selected with 11 -> "the accents" | 12 -> "the flams" | k -> Voice_tr808.label m.patch.machine (List.nth Voice_tr808.instruments k)

let panel_view (m : model) : shape list =
  let running = Voice_tr808.running tr808 and now = Voice_tr808.step tr808 in
  let machine = m.patch.machine in
  let l = look machine in
  let columns =
    List.concat
      (List.mapi
         (fun k i ->
           let names = knobs_of machine i in
           let chosen = m.selected = k in
           [ rectangle (if chosen then l.chosen else rgb 60 60 60) 76. 24. |> move (column_x k) name_y;
             words (if chosen then black else white) (Voice_tr808.label machine i) |> scale 1.3 |> move (column_x k) name_y ]
           @ List.mapi (fun row (_, word) -> words ink (String.uppercase_ascii word) |> scale 0.9 |> move (column_x k) (knob_y row - 22.)) names)
         Voice_tr808.instruments)
  in
  let steps =
    List.concat
      (List.init 16 (fun k ->
           let on = (track m.patch m.selected).(k) and playing = running && k = now in
           [
             rectangle (if playing then white else step_color machine k) 46. 52. |> move (step_x k) step_y;
             circle (if on then l.led else rgb 90 40 20) 5. |> move (step_x k) (step_y + 38.);
             words l.top (string_of_int (k +.. 1)) |> scale 0.9 |> move (step_x k) (step_y - 36.);
           ]))
  in
  let caption word x = words l.top word |> scale 1. |> move x (controls_y - 30.) in
  [ rectangle l.body 960. 440. |> move 0. 230.; rectangle l.top 960. 204. |> move 0. 348.; rectangle l.stripe 960. 6. |> move 0. 245. ]
  @ [ words l.top (if machine = 1 then "Rhythm Composer  TR-909" else "Rhythm Composer  TR-808") |> scale 1.3 |> move 330. 20.;
      caption (Printf.sprintf "TEMPO %.0f" m.patch.tempo) 150.; caption "ACCENT" 220.; caption "SHUFFLE" 290.; caption "FLAM" 360.;
      caption "VOLUME" 430.; words l.top ("editing: " ^ track_name m) |> scale 1.1 |> move (-120.) (controls_y - 30.) ]
  @ columns @ steps

(* the whole pattern: a row per instrument, the accents and the flams,
 * the step playing a column lit *)
let grid_view (m : model) : shape list =
  let running = Voice_tr808.running tr808 and now = Voice_tr808.step tr808 in
  let rows = List.map (Voice_tr808.label m.patch.machine) Voice_tr808.instruments @ [ "AC"; "FL" ] in
  List.concat
    (List.mapi
       (fun row label ->
         (words ink label |> scale 1. |> move (-345.) (grid_y row))
         :: List.init 16 (fun k ->
                let on = (track m.patch row).(k) in
                let c = if on then (if row >= 11 then (look m.patch.machine).stripe else rgb 40 40 40) else if running && k = now then rgb 200 200 190 else rgb 235 232 222 in
                rectangle c 30. 20. |> move (grid_x k) (grid_y row)))
       rows)

let scope_view (samples : Signal.t) : shape list =
  let cx = 360. and cy = -200. and w = 200. and h = 180. in
  let points = 120 in
  let at i = samples.(Array.length samples -.. 2048 +.. (i *.. 2048 /.. points)) in
  (rectangle (rgb 20 25 20) w h |> move cx cy)
  :: List.init (points -.. 1) (fun i ->
         let x i = cx - (w / 2.) + (float_of_int i / float_of_int points * w) in
         let y i = cy + (Float.max (-1.) (Float.min 1. (at i * 2.)) * h / 2.) in
         segment green 2. (x i, y i) (x (i +.. 1), y (i +.. 1)))

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 200 195 185) computer.screen.width computer.screen.height ]
  @ [ words black "TinyTR808" |> scale 2.4 |> move (-360.) 482.; words black "pattern" |> scale 1.5 |> move 225. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ panel_view m @ grid_view m @ scope_view (Voice_tr808.recent tr808)
  @ [
      words (rgb 70 70 70) "space: start/stop   a s d f g h j k l: the drums   q w: the hats   a name: struck and edited"
      |> scale 1.2 |> move 0. (-40.);
    ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
