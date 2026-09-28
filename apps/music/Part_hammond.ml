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

let kind = "hammond"
let natural = (1000., 470.)

(* the panel's own coordinates are TinyHammond's screen's, the panel
 * centred at (0, 245): a box elsewhere moves it there *)
let centre_y = 245.
let offset (b : Widget.box) : number * number = (b.x, b.y - centre_y)

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 240 230 210;
    accent = rgb 230 170 60;
    edge = rgb 150 130 110;
    face = rgb 80 60 45;
    face_hot = rgb 105 80 60;
    face_down = rgb 60 45 35;
    text_size = 15.;
    dial = 40.;
    dial_face = rgb 30 22 16;
    pointer = rgb 245 235 215;
  }

(* the drawbars: a column each, from the slot at the top down to the
 * tip, a step per level *)
let drawbar_x (i : int) : number = -300. + (float_of_int i * 54.)
let slot_y = 400.
let step = 30.
let drawbar_width = 34.

(* the B-3's colours: brown under the note, white the octaves, black
 * the rest *)
let drawbar_color (i : int) : color =
  match i with 0 | 1 -> rgb 120 70 40 | 2 | 3 | 5 | 8 -> rgb 240 235 225 | _ -> rgb 25 25 25

(* the level the mouse at [y] pulls a drawbar to *)
let level_at (y : number) : int = max 0 (min 8 (int_of_float (Float.round ((slot_y - y - (step / 2.)) / step))))

(* the tabs, switches and knobs: a control of Voice_hammond.knobs, where
 * it sits, the word under it *)
type place = { name : string; x : number; y : number; label : string }

let place name x y label = { name; x; y; label }

let places =
  [
    place "percussion" 180. 380. "ON";
    place "percussion.soft" 225. 380. "SOFT";
    place "percussion.fast" 270. 380. "FAST";
    place "percussion.third" 315. 380. "3RD";
    place "vibrato" 415. 370. "VIBRATO";
    place "leslie" 180. 250. "ON";
    place "leslie.fast" 225. 250. "FAST";
    place "click" 315. 250. "CLICK";
    place "volume" 415. 250. "VOLUME";
  ]

let headers = [ ("DRAWBARS", -84., 455.); ("PERCUSSION", 247., 425.); ("LESLIE", 202., 295.) ]

let control ((ui, p) : Immediate.t * Voice_hammond.patch) (pl : place) : Immediate.t * Voice_hammond.patch =
  match List.find_opt (fun (k : Voice_hammond.knob) -> k.name = pl.name) Voice_hammond.knobs with
  | None -> (ui, p)
  | Some k ->
      let v = k.get p in
      let ui, v' = Panel.control ui ~selector:Rotary ~at:(pl.x, pl.y) k.control v in
      (ui, if v' <> v then k.put p v' else p)

(*****************************************************************************)
(* input *)
(*****************************************************************************)

type state = {
  voice : Voice_hammond.t;
  ui : Immediate.t;
  pulling : int option; (* the drawbar the mouse holds *)
}

(* a frame, the mouse [i] in the panel's coordinates *)
let step_ui (i : Widget.input) (st : state) : state =
  let ui = Immediate.frame i st.ui in
  let ui, patch = List.fold_left control (ui, Voice_hammond.patch st.voice) places in
  (* the drawbars: pressed on one, it follows the mouse until let go *)
  let pulling =
    if not i.mdown then None
    else
      match st.pulling with
      | Some d -> Some d
      | None ->
          List.find_opt
            (fun d -> Float.abs (i.mx - drawbar_x d) <= drawbar_width / 2. && i.my <= slot_y + 10. && i.my >= slot_y - (10. * step))
            (List.init 9 (fun d -> d))
  in
  let patch =
    match pulling with
    | Some d when level_at i.my <> patch.drawbars.(d) ->
        let bars = Array.copy patch.drawbars in
        bars.(d) <- level_at i.my;
        { patch with drawbars = bars }
    | _ -> patch
  in
  Voice_hammond.set_patch st.voice patch;
  { st with ui; pulling }

let input (computer : computer) (b : Widget.box) (st : state) : state =
  let dx, dy = offset b in
  step_ui (Panel.input computer ~dx ~dy) st

(*****************************************************************************)
(* draw *)
(*****************************************************************************)

let ink = rgb 240 230 210
let text (s : string) : shape = words ink s |> scale 1.2

let drawbars_view (p : Voice_hammond.patch) : shape list =
  List.concat
    (List.mapi
       (fun i footage ->
         let level = p.drawbars.(i) and x = drawbar_x i in
         let tip = slot_y - (float_of_int (level +.. 1) * step) in
         [
           (* the slot, and the bar out of it *)
           rectangle (rgb 20 14 10) (drawbar_width + 6.) 12. |> move x slot_y;
           rectangle (rgb 170 160 140) 10. (slot_y - tip) |> move x ((slot_y + tip) / 2.);
           rectangle (drawbar_color i) drawbar_width (step - 2.) |> move x tip;
           words (if i = 0 || i = 1 || drawbar_color i = rgb 25 25 25 then ink else rgb 30 30 30) (string_of_int level)
           |> scale 1.3 |> move x tip;
           text footage |> move x (slot_y + 22.);
         ])
       Voice_hammond.footages)
  @ [ words (rgb 230 170 60) (Voice_hammond.of_registration p) |> scale 2. |> move (-385.) 300.; text "REGISTRATION" |> move (-385.) 335. ]

let panel_view (p : Voice_hammond.patch) : shape list =
  let wood = rgb 110 65 35 in
  [ rectangle (rgb 45 30 20) 960. 440. |> move 0. 245.; rectangle wood 22. 470. |> move (-489.) 245.; rectangle wood 22. 470. |> move 489. 245. ]
  @ List.map (fun (h, x, y) -> words ink h |> scale 1.4 |> move x y) headers
  @ [ rectangle (rgb 90 70 55) 2. 400. |> move 152. 240. ]
  @ List.filter_map
      (fun pl -> if pl.label = "" then None else Some (text pl.label |> move pl.x (pl.y - if pl.name = "vibrato" then 32. else 38.)))
      places
  @ drawbars_view p

let draw (st : state) (b : Widget.box) ~active:_ : shape list =
  let dx, dy = offset b in
  List.map (move dx dy) (panel_view (Voice_hammond.patch st.voice)) @ Panel.shapes st.ui ~dx ~dy

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
    menu = "Preset" :: List.map fst Voice_hammond.presets;
    command =
      (fun c ->
        Option.iter (Voice_hammond.set_patch st.voice) (List.assoc_opt c Voice_hammond.presets);
        part st);
    save = (fun () -> Voice_hammond.to_string (Voice_hammond.patch st.voice));
  }

(* painted once, untouched, so that a host can draw it before its first
 * input *)
let make (voice : Voice_hammond.t) : Component.part =
  part (step_ui Panel.neutral { voice; ui = Immediate.set_theme panel_theme Immediate.empty; pulling = None })

let load (voice : Voice_hammond.t) (text : string) : Component.part =
  (match Voice_hammond.of_string text with Ok p -> Voice_hammond.set_patch voice p | Error _ -> ());
  make voice
