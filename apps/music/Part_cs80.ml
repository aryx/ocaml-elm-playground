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

let kind = "cs80"
let natural = (960., 462.)

(* the panel's own coordinates are TinyCS80's screen's, the panel
 * centred at (0, 221): a box elsewhere moves it there *)
let centre_y = 221.
let offset (b : Widget.box) : number * number = (b.x, b.y - centre_y)

(*****************************************************************************)
(* The panel's knobs *)
(*****************************************************************************)

let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 225 225 225;
    accent = rgb 210 60 50;
    edge = rgb 110 110 110;
    face = rgb 50 50 55;
    face_hot = rgb 70 70 75;
    face_down = rgb 35 35 40;
    text_size = 12.;
    dial = 26.;
    dial_face = rgb 20 20 22;
    pointer = rgb 240 240 240;
  }

(* a control of Voice_cs80.knobs, where it sits, the word under it *)
type place = { name : string; x : number; y : number; label : string }

let row (y : number) (prefix : string) (controls : (string * string) list) : place list =
  let n = List.length controls in
  let step = 900. / float_of_int n in
  List.mapi (fun i (name, label) -> { name = prefix ^ name; x = -450. + (step * (float_of_int i + 0.5)); y; label }) controls

let sound =
  [
    ("feet", "FEET");
    ("saw", "SAW");
    ("pulse", "PULSE");
    ("width", "WIDTH");
    ("pwm", "PWM");
    ("pwm_speed", "SPEED");
    ("noise", "NOISE");
    ("hpf", "HPF");
    ("hpf_res", "RES");
    ("lpf", "LPF");
    ("lpf_res", "RES");
    ("sine", "SINE");
  ]

let shape =
  [
    ("il", "IL");
    ("al", "AL");
    ("f_attack", "F.ATK");
    ("f_decay", "F.DEC");
    ("f_release", "F.REL");
    ("attack", "ATK");
    ("decay", "DEC");
    ("sustain", "SUS");
    ("release", "REL");
    ("level", "LEVEL");
    ("initial.brilliance", "V.BRIL");
    ("initial.level", "V.LVL");
    ("after.brilliance", "P.BRIL");
    ("after.level", "P.LVL");
  ]

let shared =
  [
    ("mix", "MIX I/II");
    ("detune", "DETUNE");
    ("sub.wave", "SUB");
    ("sub.speed", "SPEED");
    ("sub.vco", "VCO");
    ("sub.vcf", "VCF");
    ("sub.vca", "VCA");
    ("ring.speed", "RING");
    ("ring.depth", "DEPTH");
    ("ring.attack", "ATK");
    ("ring.decay", "DEC");
    ("chorus", "CHORUS");
    ("tremolo", "TREM");
    ("volume", "VOLUME");
  ]

let places = row 400. "I." sound @ row 335. "I." shape @ row 255. "II." sound @ row 190. "II." shape @ row 110. "" shared

let control ((ui, p) : Immediate.t * Voice_cs80.patch) (pl : place) : Immediate.t * Voice_cs80.patch =
  match List.find_opt (fun (k : Voice_cs80.knob) -> k.name = pl.name) Voice_cs80.knobs with
  | None -> (ui, p)
  | Some k ->
      let v = k.get p in
      let ui, v' = Panel.control ui ~selector:Rotary ~at:(pl.x, pl.y) k.control v in
      (ui, if v' <> v then k.put p v' else p)

(*****************************************************************************)
(* input *)
(*****************************************************************************)

type state = { voice : Voice_cs80.t; ui : Immediate.t }

(* a frame, the mouse [i] in the panel's coordinates *)
let step_ui (i : Widget.input) (st : state) : state =
  let ui = Immediate.frame i st.ui in
  let ui, patch = List.fold_left control (ui, Voice_cs80.patch st.voice) places in
  Voice_cs80.set_patch st.voice patch;
  { st with ui }

let input (computer : computer) (b : Widget.box) (st : state) : state =
  let dx, dy = offset b in
  step_ui (Panel.input computer ~dx ~dy) st

(*****************************************************************************)
(* draw *)
(*****************************************************************************)

let ink = rgb 225 225 225

(* the CS-80's black front, a stripe per section *)
let panel_view : shape list =
  let stripe y label = [ rectangle (rgb 45 45 50) 950. 120. |> move 0. y; words (rgb 210 60 50) label |> scale 1.2 |> move (-465.) (y + 48.) ] in
  [ rectangle (rgb 25 25 28) 960. 460. |> move 0. 220.; rectangle (rgb 150 110 70) 960. 12. |> move 0. 446. ]
  @ stripe 368. "I" @ stripe 223. "II"
  @ [ words (rgb 200 200 200) "YAMAHA  CS-80" |> scale 1.2 |> move 380. 436. ]
  @ List.map (fun pl -> words ink pl.label |> scale 0.9 |> move pl.x (pl.y - 26.)) places

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
    menu = "Preset" :: List.map fst Voice_cs80.presets;
    command =
      (fun c ->
        Option.iter (Voice_cs80.set_patch st.voice) (List.assoc_opt c Voice_cs80.presets);
        part st);
    save = (fun () -> Voice_cs80.to_string (Voice_cs80.patch st.voice));
  }

(* painted once, untouched, so that a host can draw it before its first
 * input *)
let make (voice : Voice_cs80.t) : Component.part =
  part (step_ui Panel.neutral { voice; ui = Immediate.set_theme panel_theme Immediate.empty })

let load (voice : Voice_cs80.t) (text : string) : Component.part =
  (match Voice_cs80.of_string text with Ok p -> Voice_cs80.set_patch voice p | Error _ -> ());
  make voice
