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

let kind = "tb303"
let natural = (960., 600.)

(* the panel's own coordinates are TinyTB303's screen's, the knobs'
 * strip and the grid centred at (0, 160): a box elsewhere moves them
 * there *)
let centre_y = 160.
let offset (b : Widget.box) : number * number = (b.x, b.y - centre_y)

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 30 30 30;
    accent = rgb 230 80 40;
    edge = rgb 90 90 95;
    face = rgb 160 160 165;
    face_hot = rgb 180 180 185;
    face_down = rgb 130 130 135;
    text_size = 15.;
    dial = 40.;
    dial_face = rgb 25 25 25;
    pointer = rgb 240 240 240;
  }

type place = { name : string; x : number; label : string }

let knob_y = 385.

let places =
  [
    { name = "tuning"; x = -400.; label = "TUNING" };
    { name = "cutoff"; x = -300.; label = "CUT OFF FREQ" };
    { name = "resonance"; x = -200.; label = "RESONANCE" };
    { name = "env.mod"; x = -100.; label = "ENV MOD" };
    { name = "decay"; x = 0.; label = "DECAY" };
    { name = "accent"; x = 100.; label = "ACCENT" };
    { name = "waveform"; x = 200.; label = "WAVEFORM" };
    { name = "tempo"; x = 300.; label = "TEMPO" };
    { name = "volume"; x = 400.; label = "VOLUME" };
  ]

(* a knob, turning the patch's value, or with a step selected and the
 * knob lockable, that step's lock (the knob showing it, or the
 * patch's value until locked) *)
let control (selected : int option) ((ui, p) : Immediate.t * Voice_tb303.patch) (pl : place) : Immediate.t * Voice_tb303.patch =
  match List.find_opt (fun (k : Voice_tb303.knob) -> k.name = pl.name) Voice_tb303.knobs with
  | None -> (ui, p)
  | Some k ->
      let step = match selected with Some c when List.mem k.name Voice_tb303.lockable && c < Array.length p.pattern -> Some c | _ -> None in
      let put p v =
        match step with
        | None -> k.put p v
        | Some c ->
            let pattern = Array.copy p.pattern in
            pattern.(c) <- Sequencer.lock pattern.(c) k.name v;
            { p with pattern }
      in
      let v = match step with Some c -> Option.value (List.assoc_opt k.name p.pattern.(c).locks) ~default:(k.get p) | None -> k.get p in
      (* short labels, to fit around the switch *)
      let c : Control.t = match k.control with Selector _ when pl.name = "waveform" -> Selector [ "saw"; "sq" ] | c -> c in
      let ui, v' = Panel.control ui ~selector:Rotary ~at:(pl.x, knob_y) c v in
      (ui, if v' <> v then put p v' else p)

(*****************************************************************************)
(* The pattern's grid *)
(*****************************************************************************)

(* 16 columns; 13 rows of pitch, C2 to C3, then the octave, the accent
 * and the slide rows *)
let base = 36 (* C2 *)
let columns = 16
let cell_w = 55.
let cell_h = 24.
let grid_left = -440.
let pitch_top = 262.
let column_x (c : int) : number = grid_left + ((float_of_int c + 0.5) * cell_w)
let pitch_y (r : int) : number = pitch_top - ((float_of_int (12 -.. r) + 0.5) * cell_h) (* row r = C2 + r semitones *)
let octave_y = pitch_top - (13.5 * cell_h) - 8.
let accent_y = octave_y - cell_h - 4.
let slide_y = accent_y - cell_h - 4.

(* a step's note split into its row (0 to 12) and its octave (-1, 0, 1) *)
let row_and_octave (n : int) : int * int =
  let d = n -.. base in
  if d >= 0 && d <= 12 then (d, 0) else if d < 0 then ((d +.. 12) mod 12, -1) else ((d -.. 12) mod 13, 1)

let under (x : number) (y : number) (row_y : number) : int option =
  if Float.abs (y - row_y) > cell_h / 2. then None
  else
    let c = int_of_float (Float.of_int (int_of_float ((x - grid_left) / cell_w))) in
    if x < grid_left || c < 0 || c >= columns then None else Some c

(* a click on the grid: the pattern with that step changed *)
let click (p : Sequencer.step array) (x : number) (y : number) : Sequencer.step array =
  let p = Array.init columns (fun i -> if i < Array.length p then p.(i) else Sequencer.rest) in
  let set c s =
    let q = Array.copy p in
    q.(c) <- s;
    q
  in
  let pitch_row = List.find_opt (fun r -> Float.abs (y - pitch_y r) <= cell_h / 2.) (List.init 13 (fun r -> r)) in
  match pitch_row with
  | Some r -> (
      match under x y (pitch_y r) with
      | None -> p
      | Some c -> (
          let s = p.(c) in
          match s.note with
          (* the same cell again: a rest *)
          | Some n when fst (row_and_octave n) = r -> set c { s with note = None }
          | Some n -> set c { s with note = Some (base +.. r +.. (12 *.. snd (row_and_octave n))) }
          | None -> set c { s with note = Some (base +.. r) }))
  | None -> (
      let toggle row_y f = Option.map (fun c -> set c (f p.(c))) (under x y row_y) in
      let octave (s : Sequencer.step) =
        match s.note with
        | None -> s
        | Some n ->
            let r, o = row_and_octave n in
            let o = if o = 1 then -1 else o +.. 1 in
            { s with note = Some (base +.. r +.. (12 *.. o)) }
      in
      match toggle octave_y octave with
      | Some q -> q
      | None -> (
          match toggle accent_y (fun s -> { s with accent = not s.accent }) with
          | Some q -> q
          | None -> Option.value (toggle slide_y (fun s -> { s with slide = not s.slide })) ~default:p))

(*****************************************************************************)
(* input *)
(*****************************************************************************)

type state = {
  voice : Voice_tb303.t;
  ui : Immediate.t;
  selected : int option; (* the step whose locks the knobs turn *)
}

let box (x, y) (w, h) : Widget.box = { Widget.x; y; w; h }

let button (ui : Immediate.t) (at : number * number) (s : string) : Immediate.t * bool =
  Immediate.button ui (box at (Immediate.button_size (Immediate.theme ui) s)) s

(* a frame, the mouse [i] in the panel's coordinates *)
let step_ui (i : Widget.input) (st : state) : state =
  let ui = Immediate.frame i st.ui in
  let selected = st.selected in
  let ui, patch = List.fold_left (control selected) (ui, Voice_tb303.patch st.voice) places in
  let ui, run_pressed = button ui (-400., 318.) (if Voice_tb303.running st.voice then "STOP" else "RUN") in
  if run_pressed then Voice_tb303.run st.voice (not (Voice_tb303.running st.voice));
  (* the locks: the selected step's cleared, what they mean between
   * steps *)
  let ui, patch =
    match selected with
    | Some c -> (
        let ui, clear = button ui (-300., 312.) "CLEAR" in
        match clear with
        | true ->
            let pattern = Array.copy patch.pattern in
            pattern.(c) <- Sequencer.unlock pattern.(c);
            (ui, { patch with pattern })
        | false -> (ui, patch))
    | None -> (ui, patch)
  in
  let ui, points = Immediate.rocker ui (box (-120., 303.) (Immediate.rocker_size (Immediate.theme ui))) (patch.locks = 1) in
  let locks = if points then 1 else 0 in
  let ui, smoothing =
    if locks = 1 then Immediate.knob ui (box (20., 303.) (Immediate.knob_size (Immediate.theme ui))) ~from:0. ~to_:1. patch.smoothing
    else (ui, patch.smoothing)
  in
  let patch = { patch with locks; smoothing } in
  (* a click on a step's number selects it (again: none) *)
  let number = if i.mclick then under i.mx i.my (pitch_top + 12.) else None in
  let selected = match number with Some c -> if selected = Some c then None else Some c | None -> selected in
  let patch = if i.mclick && number = None then { patch with pattern = click patch.pattern i.mx i.my } else patch in
  Voice_tb303.set_patch st.voice patch;
  { st with ui; selected }

let input (computer : computer) (b : Widget.box) (st : state) : state =
  let dx, dy = offset b in
  step_ui (Panel.input computer ~dx ~dy) st

(*****************************************************************************)
(* draw *)
(*****************************************************************************)

let ink = rgb 30 30 30
let text (s : string) : shape = words ink s |> scale 1.1

let grid_view (voice : Voice_tb303.t) (selected : int option) : shape list =
  let p = Voice_tb303.patch voice in
  let playing = if Voice_tb303.running voice then Some (Voice_tb303.step voice) else None in
  let names = [| "C"; "C#"; "D"; "Eb"; "E"; "F"; "F#"; "G"; "Ab"; "A"; "Bb"; "B"; "C" |] in
  let cell color x y = rectangle color (cell_w - 3.) (cell_h - 3.) |> move x y in
  let rows =
    List.concat
      (List.init 13 (fun r ->
           (text names.(r) |> move (grid_left - 20.) (pitch_y r))
           :: List.init columns (fun c ->
                  let black = List.mem (r mod 12) [ 1; 3; 6; 8; 10 ] in
                  cell (if black then rgb 150 150 155 else rgb 175 175 180) (column_x c) (pitch_y r))))
  in
  let steps =
    List.concat
      (List.init columns (fun c ->
           let s = if c < Array.length p.pattern then p.pattern.(c) else Sequencer.rest in
           let lit = playing = Some c in
           (if lit then [ rectangle (rgb 250 220 120) (cell_w - 3.) (13. * cell_h) |> move (column_x c) (pitch_top - (6.5 * cell_h)) ]
            else [])
           @ (match s.note with
             | None -> [ text "-" |> move (column_x c) (pitch_y 6) ]
             | Some n ->
                 let r, o = row_and_octave n in
                 [
                   cell (if s.accent then rgb 230 80 40 else rgb 40 40 45) (column_x c) (pitch_y r);
                   cell (rgb 200 200 205) (column_x c) octave_y;
                   text (match o with -1 -> "DOWN" | 1 -> "UP" | _ -> "") |> move (column_x c) octave_y;
                 ])
           @ [
               cell (if s.accent then rgb 230 80 40 else rgb 200 200 205) (column_x c) accent_y;
               cell (if s.slide then rgb 60 110 200 else rgb 200 200 205) (column_x c) slide_y;
             ]
           (* the step's number, lit when selected, a dot when it has locks *)
           @ (if selected = Some c then [ rectangle (rgb 230 80 40) (cell_w - 3.) 20. |> move (column_x c) (pitch_top + 12.) ] else [])
           @ (if s.locks <> [] then [ circle (rgb 60 110 200) 4. |> move (column_x c + 18.) (pitch_top + 12.) ] else [])
           @ [ text (string_of_int (c +.. 1)) |> move (column_x c) (pitch_top + 12.) ]))
  in
  [ rectangle (rgb 120 120 125) (float_of_int columns * cell_w + 6.) (13. * cell_h + 6.) |> move 0. (pitch_top - (6.5 * cell_h)) ]
  @ rows @ steps
  @ [ text "OCTAVE" |> move (grid_left - 30.) octave_y; text "ACCENT" |> move (grid_left - 30.) accent_y; text "SLIDE" |> move (grid_left - 30.) slide_y ]

(* the strip of knobs, silver, over the grid *)
let panel_view (st : state) : shape list =
  let p = Voice_tb303.patch st.voice in
  [ rectangle (rgb 215 215 220) 960. 600. |> move 0. 160. ]
  @ [ rectangle (rgb 195 195 200) 960. 175. |> move 0. 372.; words (rgb 230 80 40) "Bass Line" |> scale 1.6 |> move 380. 312. ]
  @ List.map (fun pl -> text pl.label |> move pl.x (knob_y - (if pl.name = "waveform" then 30. else 38.))) places
  @ [
      text (if p.locks = 1 then "LOCKS: POINTS" else "LOCKS: STEP") |> move (-205.) 303.;
      text (if p.locks = 1 then "SMOOTH" else "") |> move (-35.) 303.;
    ]
  @ [
      text
        (match st.selected with
        | Some c -> Printf.sprintf "step %d held: the sound's knobs lock it" (c +.. 1)
        | None -> "click a step's number: its locks")
      |> move 200. 312.;
    ]
  @ grid_view st.voice st.selected

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
    menu = "Preset" :: List.map fst Voice_tb303.presets;
    command =
      (fun c ->
        match List.assoc_opt c Voice_tb303.presets with
        | Some p ->
            Voice_tb303.set_patch st.voice p;
            part { st with selected = None }
        | None -> part st);
    save = (fun () -> Voice_tb303.to_string (Voice_tb303.patch st.voice));
  }

(* painted once, untouched, so that a host can draw it before its first
 * input *)
let make (voice : Voice_tb303.t) : Component.part =
  part (step_ui Panel.neutral { voice; ui = Immediate.set_theme panel_theme Immediate.empty; selected = None })

let load (voice : Voice_tb303.t) (text : string) : Component.part =
  (match Voice_tb303.of_string text with Ok p -> Voice_tb303.set_patch voice p | Error _ -> ());
  make voice
