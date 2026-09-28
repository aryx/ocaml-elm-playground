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

let kind = "tr808"
let natural = (960., 440.)

(* the panel's own coordinates are TinyTR808's screen's, the panel
 * centred at (0, 230): a box elsewhere moves it there *)
let centre_y = 230.
let offset (b : Widget.box) : number * number = (b.x, b.y - centre_y)

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

(*****************************************************************************)
(* input *)
(*****************************************************************************)

type state = {
  voice : Voice_tr808.t;
  ui : Immediate.t;
  selected : int; (* the track the step buttons edit: an instrument's index, 11 the accents *)
}

let box (x, y) (w, h) : Widget.box = { Widget.x; y; w; h }

let button (ui : Immediate.t) (at : number * number) (s : string) : Immediate.t * bool =
  Immediate.button ui (box at (Immediate.button_size (Immediate.theme ui) s)) s

let knob_control ((ui, p) : Immediate.t * Voice_tr808.patch) (name : string) (at : number * number) : Immediate.t * Voice_tr808.patch =
  match List.find_opt (fun (k : Voice_tr808.knob) -> k.name = name) Voice_tr808.knobs with
  | None -> (ui, p)
  | Some k ->
      let v = k.get p in
      let ui, v' = Immediate.knob ui (box at (Immediate.knob_size (Immediate.theme ui))) ~from:0. ~to_:1. v in
      (ui, if v' <> v then k.put p v' else p)

(* a frame, the mouse [i] in the panel's coordinates *)
let step_ui (i : Widget.input) (st : state) : state =
  let ui = Immediate.frame i st.ui in
  let patch = Voice_tr808.patch st.voice in
  (* the instruments' knobs *)
  let ui, patch =
    List.fold_left
      (fun acc (k, instr) ->
        let names = knobs_of patch.machine instr in
        List.fold_left
          (fun acc (row, (field, _)) -> knob_control acc (Voice_tr808.name instr ^ "." ^ field) (column_x k, knob_y row))
          acc (List.mapi (fun row n -> (row, n)) names))
      (ui, patch)
      (List.mapi (fun k instr -> (k, instr)) Voice_tr808.instruments)
  in
  (* the tempo, whole beats; the accent, the shuffle, the flam; the
   * volume *)
  let ui, tempo = Immediate.knob ui (box (150., controls_y) (Immediate.knob_size (Immediate.theme ui))) ~from:60. ~to_:180. patch.tempo in
  let tempo = Float.round tempo in
  let patch = if tempo <> patch.tempo then { patch with tempo } else patch in
  let ui, patch = knob_control (ui, patch) "accent" (220., controls_y) in
  let ui, patch = knob_control (ui, patch) "shuffle" (290., controls_y) in
  let ui, patch = knob_control (ui, patch) "flam" (360., controls_y) in
  let ui, patch = knob_control (ui, patch) "volume" (430., controls_y) in
  (* the machine *)
  let ui, b808 = button ui (10., controls_y) "808" in
  let patch = if b808 then { patch with machine = 0 } else patch in
  let ui, b909 = button ui (65., controls_y) "909" in
  let patch = if b909 then { patch with machine = 1 } else patch in
  (* start/stop *)
  let ui, run = button ui (-380., controls_y) (if Voice_tr808.running st.voice then "STOP" else "START") in
  if run then Voice_tr808.run st.voice (not (Voice_tr808.running st.voice));
  (* a name clicked: struck, and its track the one edited; AC, FL *)
  let selected =
    List.fold_left
      (fun sel (k, instr) ->
        if i.mclick && Float.abs (i.mx - column_x k) <= 38. && Float.abs (i.my - name_y) <= 14. then begin
          Voice_tr808.hit st.voice instr ~accent:false;
          k
        end
        else sel)
      st.selected
      (List.mapi (fun k instr -> (k, instr)) Voice_tr808.instruments)
  in
  let ui, ac = button ui (-280., controls_y) "AC" in
  let selected = if ac then 11 else selected in
  let ui, fl = button ui (-225., controls_y) "FL" in
  let selected = if fl then 12 else selected in
  (* the step buttons *)
  let patch =
    List.fold_left
      (fun p k -> if i.mclick && Float.abs (i.mx - step_x k) <= 24. && Float.abs (i.my - step_y) <= 28. then with_step p selected k else p)
      patch (List.init 16 (fun k -> k))
  in
  Voice_tr808.set_patch st.voice patch;
  { st with ui; selected }

let input (computer : computer) (b : Widget.box) (st : state) : state =
  let dx, dy = offset b in
  step_ui (Panel.input computer ~dx ~dy) st

(*****************************************************************************)
(* draw *)
(*****************************************************************************)

let ink = rgb 40 40 40

(* the machine's colours: the 808's dark body, cream top and red
 * stripe; the 909's grey and orange *)
type look = { body : color; top : color; stripe : color; chosen : color; led : color }

let look (machine : int) : look =
  if machine = 1 then { body = rgb 95 95 98; top = rgb 205 205 202; stripe = rgb 235 120 40; chosen = rgb 235 120 40; led = rgb 255 140 30 }
  else { body = rgb 45 45 45; top = rgb 230 225 210; stripe = rgb 205 55 45; chosen = rgb 230 130 45; led = rgb 255 50 30 }

let stripe (machine : int) : color = (look machine).stripe

let track_name (p : Voice_tr808.patch) (selected : int) : string =
  match selected with 11 -> "the accents" | 12 -> "the flams" | k -> Voice_tr808.label p.machine (List.nth Voice_tr808.instruments k)

let panel_view (st : state) : shape list =
  let p = Voice_tr808.patch st.voice in
  let running = Voice_tr808.running st.voice and now = Voice_tr808.step st.voice in
  let machine = p.machine in
  let l = look machine in
  let columns =
    List.concat
      (List.mapi
         (fun k i ->
           let names = knobs_of machine i in
           let chosen = st.selected = k in
           [ rectangle (if chosen then l.chosen else rgb 60 60 60) 76. 24. |> move (column_x k) name_y;
             words (if chosen then black else white) (Voice_tr808.label machine i) |> scale 1.3 |> move (column_x k) name_y ]
           @ List.mapi (fun row (_, word) -> words ink (String.uppercase_ascii word) |> scale 0.9 |> move (column_x k) (knob_y row - 22.)) names)
         Voice_tr808.instruments)
  in
  let steps =
    List.concat
      (List.init 16 (fun k ->
           let on = (track p st.selected).(k) and playing = running && k = now in
           [
             rectangle (if playing then white else step_color machine k) 46. 52. |> move (step_x k) step_y;
             circle (if on then l.led else rgb 90 40 20) 5. |> move (step_x k) (step_y + 38.);
             words l.top (string_of_int (k +.. 1)) |> scale 0.9 |> move (step_x k) (step_y - 36.);
           ]))
  in
  let caption word x = words l.top word |> scale 1. |> move x (controls_y - 30.) in
  [ rectangle l.body 960. 440. |> move 0. 230.; rectangle l.top 960. 204. |> move 0. 348.; rectangle l.stripe 960. 6. |> move 0. 245. ]
  @ [ words l.top (if machine = 1 then "Rhythm Composer  TR-909" else "Rhythm Composer  TR-808") |> scale 1.3 |> move 330. 20.;
      caption (Printf.sprintf "TEMPO %.0f" p.tempo) 150.; caption "ACCENT" 220.; caption "SHUFFLE" 290.; caption "FLAM" 360.;
      caption "VOLUME" 430.; words l.top ("editing: " ^ track_name p st.selected) |> scale 1.1 |> move (-120.) (controls_y - 30.) ]
  @ columns @ steps

let draw (st : state) (b : Widget.box) ~active:_ : shape list =
  let dx, dy = offset b in
  List.map (move dx dy) (panel_view st) @ Panel.shapes st.ui ~dx ~dy

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
    menu = "Pattern" :: List.map fst Voice_tr808.presets;
    command =
      (fun c ->
        Option.iter (Voice_tr808.set_patch st.voice) (List.assoc_opt c Voice_tr808.presets);
        part st);
    save = (fun () -> Voice_tr808.to_string (Voice_tr808.patch st.voice));
  }

(* painted once, untouched, so that a host can draw it before its first
 * input *)
let make (voice : Voice_tr808.t) : Component.part =
  part (step_ui Panel.neutral { voice; ui = Immediate.set_theme panel_theme Immediate.empty; selected = 0 })

let load (voice : Voice_tr808.t) (text : string) : Component.part =
  (match Voice_tr808.of_string text with Ok p -> Voice_tr808.set_patch voice p | Error _ -> ());
  make voice
