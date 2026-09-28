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

let kind = "rhodes"
let natural = (960., 460.)

(* the panel's own coordinates are TinyRhodes' screen's, the panel
 * centred at (0, 200): a box elsewhere moves it there *)
let centre_y = 200.
let offset (b : Widget.box) : number * number = (b.x, b.y - centre_y)

(*****************************************************************************)
(* The panel's knobs *)
(*****************************************************************************)

let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 230 230 230;
    accent = rgb 200 60 50;
    edge = rgb 120 120 120;
    face = rgb 55 55 55;
    face_hot = rgb 75 75 75;
    face_down = rgb 40 40 40;
    text_size = 15.;
    dial = 44.;
    dial_face = rgb 25 25 25;
    pointer = rgb 235 235 235;
  }

(* a control of Voice_rhodes.knobs, where it sits, the word under it *)
type place = { name : string; x : number; y : number; label : string }

let place name x y label = { name; x; y; label }

let places =
  [
    place "model" (-380.) 340. "";
    place "voicing" (-230.) 380. "VOICING";
    place "hardness" (-140.) 380. "HAMMER";
    place "decay" (-50.) 380. "DECAY";
    place "tremolo.rate" (110.) 380. "RATE";
    place "tremolo.depth" (200.) 380. "DEPTH";
    place "volume" (360.) 380. "VOLUME";
  ]

let control ((ui, p) : Immediate.t * Voice_rhodes.patch) (pl : place) : Immediate.t * Voice_rhodes.patch =
  match List.find_opt (fun (k : Voice_rhodes.knob) -> k.name = pl.name) Voice_rhodes.knobs with
  | None -> (ui, p)
  | Some k ->
      let v = k.get p in
      let ui, v' = Panel.control ui ~selector:Rotary ~at:(pl.x, pl.y) k.control v in
      (ui, if v' <> v then k.put p v' else p)

(*****************************************************************************)
(* input *)
(*****************************************************************************)

type state = { voice : Voice_rhodes.t; ui : Immediate.t }

(* a frame, the mouse [i] in the panel's coordinates *)
let step_ui (i : Widget.input) (st : state) : state =
  let ui = Immediate.frame i st.ui in
  let ui, patch = List.fold_left control (ui, Voice_rhodes.patch st.voice) places in
  Voice_rhodes.set_patch st.voice patch;
  { st with ui }

let input (computer : computer) (b : Widget.box) (st : state) : state =
  let dx, dy = offset b in
  step_ui (Panel.input computer ~dx ~dy) st

(*****************************************************************************)
(* draw *)
(*****************************************************************************)

let ink = rgb 230 230 230

(* the Stage 73's top: black, its silver strip and nameplate *)
let panel_view : shape list =
  [
    rectangle (rgb 25 25 25) 960. 460. |> move 0. 200.;
    rectangle (rgb 180 180 185) 960. 20. |> move 0. 420.;
    words (rgb 30 30 30) "Rhodes  MARK I  STAGE PIANO" |> scale 1.3 |> move (-250.) 420.;
    words (rgb 200 60 50) "SUITCASE VIBRATO" |> scale 1.1 |> move 155. 335.;
  ]
  @ List.filter_map (fun pl -> if pl.label = "" then None else Some (words ink pl.label |> scale 1.1 |> move pl.x (pl.y - 34.))) places

(* the pickup's curve against the tip's position, and on it the span
 * the last note's tip went over: where the sound comes from *)
let pickup_view (voice : Voice_rhodes.t) : shape list =
  let cx = -200. and cy = 170. and w = 460. and h = 200. in
  let p = Voice_rhodes.patch voice in
  let frame = [ rectangle (rgb 15 20 18) w h |> move cx cy ] in
  if p.model = 2 then frame @ [ words ink "the Clavinet: a string, its pickups under it" |> scale 1.2 |> move cx cy ]
  else begin
    let wurlitzer = p.model = 1 in
    (* the curve over the tip's positions shown, and its range *)
    let lo, hi = if wurlitzer then (-1., 0.95) else (-3., 3.) in
    let curve x = if wurlitzer then Voice_rhodes.capacitance x else Voice_rhodes.pickup ~voicing:p.voicing x in
    let top = if wurlitzer then curve hi else 1. and bottom = if wurlitzer then curve lo else 0. in
    let px x = cx - (w / 2.) + 20. + ((x - lo) / (hi - lo) * (w - 40.)) in
    let py v = cy - (h / 2.) + 25. + ((v - bottom) / (top - bottom) * (h - 55.)) in
    let steps = 80 in
    let xs = List.init (steps +.. 1) (fun i -> lo + ((hi - lo) * float_of_int i / float_of_int steps)) in
    let rec lines c width = function
      | a :: (b :: _ as rest) -> Meters.segment c width (px a, py (curve a)) (px b, py (curve b)) :: lines c width rest
      | _ -> []
    in
    let slo, shi = Voice_rhodes.span voice in
    let slo = Float.max lo slo and shi = Float.min hi shi in
    let swept = List.filter (fun x -> x >= slo && x <= shi) xs in
    let axis = Meters.segment (rgb 90 90 90) 2. (px lo, py bottom - 12.) (px hi, py bottom - 12.) in
    let span_bar = if shi > slo then [ Meters.segment (rgb 230 120 60) 6. (px slo, py bottom - 12.) (px shi, py bottom - 12.) ] else [] in
    frame
    @ [ words ink (if wurlitzer then "THE REED'S CAPACITOR: 1 / (1 - x)" else "THE PICKUP: FLUX AGAINST THE TIP") |> scale 1.1 |> move cx (cy + (h / 2.) - 14.) ]
    @ lines (rgb 120 150 130) 2. xs
    @ lines (rgb 230 120 60) 4. swept
    @ (axis :: span_bar)
    @ [ words (rgb 160 160 160) "the tip's swing now" |> scale 1. |> move cx (py bottom - 28.) ]
  end

(* the Suitcase's two speakers, each as bright as its side is loud *)
let speakers_view (voice : Voice_rhodes.t) : shape list =
  let p = Voice_rhodes.patch voice in
  let s = Voice_rhodes.pan voice and d = p.tremolo_depth in
  let left, right = if p.model = 0 then (1. - (d * (1. + s) / 2.), 1. - (d * (1. - s) / 2.)) else (1. - (d * (1. + s) / 2.), 1. - (d * (1. + s) / 2.)) in
  let speaker x level =
    let c = int_of_float (40. + (180. * level)) in
    group [ circle (rgb 50 50 50) 62.; circle (rgb c (c /.. 2) (c /.. 3)) 50.; circle (rgb 20 20 20) 14. ] |> move x 170.
  in
  [ rectangle (rgb 40 32 28) 330. 180. |> move 250. 170.; speaker 170. left; speaker 330. right ]

let draw (st : state) (b : Widget.box) ~active:_ : shape list =
  let dx, dy = offset b in
  List.map (move dx dy) (panel_view @ pickup_view st.voice @ speakers_view st.voice) @ Panel.shapes st.ui ~dx ~dy

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
    menu = "Preset" :: List.map fst Voice_rhodes.presets;
    command =
      (fun c ->
        Option.iter (Voice_rhodes.set_patch st.voice) (List.assoc_opt c Voice_rhodes.presets);
        part st);
    save = (fun () -> Voice_rhodes.to_string (Voice_rhodes.patch st.voice));
  }

(* painted once, untouched, so that a host can draw it before its first
 * input *)
let make (voice : Voice_rhodes.t) : Component.part =
  part (step_ui Panel.neutral { voice; ui = Immediate.set_theme panel_theme Immediate.empty })

let load (voice : Voice_rhodes.t) (text : string) : Component.part =
  (match Voice_rhodes.of_string text with Ok p -> Voice_rhodes.set_patch voice p | Error _ -> ());
  make voice
