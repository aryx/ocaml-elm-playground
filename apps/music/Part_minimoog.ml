(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground
open Basics (* float arithmetics *)

let kind = "minimoog"
let natural = (1000., 470.)

(* the panel's own coordinates are TinyMinimoog's screen's, the panel
 * centred at (0, 245): a box elsewhere moves it there *)
let centre_y = 245.
let offset (b : Widget.box) : number * number = (b.x, b.y - centre_y)

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

(* white on black, the knobs black with a white pointer, the lit half
 * of a rocker the Model D's blue *)
let theme : Theme.t =
  {
    Theme.default with
    text = rgb 235 235 235;
    accent = rgb 110 170 235;
    edge = rgb 150 150 150;
    face = rgb 70 70 70;
    face_hot = rgb 95 95 95;
    face_down = rgb 50 50 50;
    text_size = 15.;
    dial = 40.;
    dial_face = rgb 20 20 20;
    pointer = rgb 245 245 245;
  }

(* where each control sits, and the word under it *)
type place = { name : string; x : number; y : number; label : string }

let place name x y label = { name; x; y; label }

let places =
  [
    (* CONTROLLERS *)
    place "tune" (-425.) 360. "TUNE";
    place "glide" (-425.) 265. "GLIDE";
    place "mod.mix" (-425.) 170. "MOD MIX";
    place "mod.oscillators" (-450.) 80. "OSC";
    place "mod.filter" (-400.) 80. "FILTER";
    (* OSCILLATOR BANK *)
    place "osc1.range" (-300.) 360. "RANGE";
    place "osc1.wave" (-115.) 360. "WAVEFORM";
    place "osc2.range" (-300.) 255. "";
    place "osc2.frequency" (-210.) 255. "FREQUENCY";
    place "osc2.wave" (-115.) 255. "";
    place "osc3.range" (-300.) 150. "";
    place "osc3.frequency" (-210.) 150. "FREQUENCY";
    place "osc3.wave" (-115.) 150. "";
    place "osc3.keyboard" (-210.) 75. "OSC 3 CTRL";
    (* MIXER *)
    place "osc1.level" (-10.) 360. "OSC 1";
    place "osc1.on" (40.) 360. "";
    place "osc2.level" (-10.) 270. "OSC 2";
    place "osc2.on" (40.) 270. "";
    place "osc3.level" (-10.) 180. "OSC 3";
    place "osc3.on" (40.) 180. "";
    place "noise.level" (-10.) 90. "NOISE";
    place "noise.on" (40.) 90. "";
    (* MODIFIERS: the filter, then its contour, then the loudness's *)
    place "filter.cutoff" 115. 360. "CUTOFF";
    place "filter.emphasis" 195. 360. "EMPHASIS";
    place "filter.contour" 275. 360. "CONTOUR";
    place "filter.keyboard1" 335. 360. "KB 1";
    place "filter.keyboard2" 370. 360. "KB 2";
    place "filter.attack" 115. 260. "ATTACK";
    place "filter.decay" 195. 260. "DECAY";
    place "filter.sustain" 275. 260. "SUSTAIN";
    place "loudness.attack" 115. 150. "ATTACK";
    place "loudness.decay" 195. 150. "DECAY";
    place "loudness.sustain" 275. 150. "SUSTAIN";
    (* OUTPUT, and the Model D's two switches left of its keyboard *)
    place "volume" 430. 360. "VOLUME";
    place "decay.on" 410. 250. "DECAY";
    place "glide.on" 455. 250. "GLIDE";
  ]

let headers =
  [ ("CONTROLLERS", -425.); ("OSCILLATOR BANK", -210.); ("MIXER", 15.); ("MODIFIERS", 195.); ("OUTPUT", 430.) ]

(* the rotary switches' positions, short enough to fit around them *)
let short (name : string) : string list =
  if Filename.check_suffix name ".range" then [ "LO"; "32"; "16"; "8"; "4"; "2" ]
  else if name = "osc3.wave" then [ "tri"; "rev"; "saw"; "sq"; "wide"; "narr" ]
  else [ "tri"; "shark"; "saw"; "sq"; "wide"; "narr" ]

let control ((ui, p) : Immediate.t * Voice_minimoog.patch) (pl : place) : Immediate.t * Voice_minimoog.patch =
  match List.find_opt (fun (k : Voice_minimoog.knob) -> k.name = pl.name) Voice_minimoog.knobs with
  | None -> (ui, p)
  | Some k ->
      let v = k.get p in
      let c : Control.t = match k.control with Selector _ -> Selector (short pl.name) | c -> c in
      let ui, v' = Panel.control ui ~selector:Rotary ~at:(pl.x, pl.y) c v in
      (ui, if v' <> v then k.put p v' else p)

(*****************************************************************************)
(* input *)
(*****************************************************************************)

type state = { voice : Voice_minimoog.t; ui : Immediate.t }

(* a frame, the mouse [i] in the panel's coordinates *)
let step_ui (i : Widget.input) (st : state) : state =
  let ui = Immediate.frame i st.ui in
  let ui, patch = List.fold_left control (ui, Voice_minimoog.patch st.voice) places in
  Voice_minimoog.set_patch st.voice patch;
  { st with ui }

let input (computer : computer) (b : Widget.box) (st : state) : state =
  let dx, dy = offset b in
  step_ui (Panel.input computer ~dx ~dy) st

(*****************************************************************************)
(* draw *)
(*****************************************************************************)

let white_ink = rgb 235 235 235
let text (s : string) : shape = words white_ink s |> scale 1.2

let panel_view : shape list =
  let wood = rgb 120 70 35 in
  [
    rectangle (rgb 25 25 25) 960. 440. |> move 0. 245.;
    rectangle wood 22. 470. |> move (-489.) 245.;
    rectangle wood 22. 470. |> move 489. 245.;
  ]
  @ List.map (fun (h, x) -> words white_ink h |> scale 1.5 |> move x 440.) headers
  (* the lines between the sections *)
  @ List.map (fun x -> rectangle (rgb 90 90 90) 2. 400. |> move x 240.) [ -375.; -60.; 70.; 390. ]
  @ List.filter_map
      (fun pl ->
        if pl.label = "" then None
        else
          let below = if Filename.check_suffix pl.name ".range" || Filename.check_suffix pl.name ".wave" then 30. else 38. in
          Some (text pl.label |> move pl.x (pl.y - below)))
      places
  @ [ text "FILTER" |> move 195. 410.; text "FILTER CONTOUR" |> move 195. 300.; text "LOUDNESS CONTOUR" |> move 195. 190. ]

let draw (st : state) (b : Widget.box) ~active:_ : shape list =
  let dx, dy = offset b in
  List.map (move dx dy) panel_view @ Panel.shapes st.ui ~dx ~dy

(*****************************************************************************)
(* The part *)
(*****************************************************************************)

let rec part (st : state) : Component.part =
  {
    kind;
    height = (fun w -> snd natural * w / fst natural);
    natural = Some natural;
    draw = draw st;
    input = (fun computer b -> part (input computer b st));
    menu = "Preset" :: List.map fst Voice_minimoog.presets;
    command =
      (fun c ->
        Option.iter (Voice_minimoog.set_patch st.voice) (List.assoc_opt c Voice_minimoog.presets);
        part st);
    save = (fun () -> Voice_minimoog.to_string (Voice_minimoog.patch st.voice));
  }

(* painted once, untouched, so that a host can draw it before its first
 * input *)
let make (voice : Voice_minimoog.t) : Component.part =
  part (step_ui Panel.neutral { voice; ui = Immediate.set_theme theme Immediate.empty })

let load (voice : Voice_minimoog.t) (text : string) : Component.part =
  (match Voice_minimoog.of_string text with Ok p -> Voice_minimoog.set_patch voice p | Error _ -> ());
  make voice
