(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Immediate.mli *)

(* a widget is its rectangle: where it is tells it from its neighbours *)
type id = float * float

(* who has the mouse: nobody, this widget (it was pressed inside it),
 * or something that is not a widget (the background) *)
type capture = Free | Held of id | Elsewhere

type t = {
  input : Widget.input;
  theme : Theme.t;
  (* the mouse button at the previous frame, to see a press begin *)
  was_down : bool;
  press : bool;
  capture : capture;
  (* the frame being built, in reverse order *)
  painted : Widget.paint list;
}

let empty =
  {
    input = Widget.no_input;
    theme = Theme.default;
    was_down = false;
    press = false;
    capture = Free;
    painted = [];
  }

let frame (input : Widget.input) (t : t) =
  let press = input.mdown && not t.was_down in
  let capture =
    if press then Elsewhere (* pressed; no widget has claimed it yet *)
    else if input.mdown then t.capture (* still held: the capture holds *)
      (* claude: the frame of the release still needs the capture, to
       * know where the press that ends in it began *)
    else if t.was_down then t.capture
    else Free
  in
  { t with input; press; was_down = input.mdown; capture; painted = [] }

let paint (t : t) = List.rev t.painted
let theme (t : t) = t.theme
let set_theme theme (t : t) = { t with theme }
let draw (t : t) ps = { t with painted = List.rev_append ps t.painted }
let id (b : Widget.box) : id = (b.x, b.y)

(* The three questions every widget asks about the mouse, and the only
 * place the capture changes: is it over me (hot), is it pressed in me
 * (held), did a press that began in me end in me (clicked)? *)
let interact (t : t) (b : Widget.box) =
  let i = t.input in
  let hot = Widget.contains b i.mx i.my in
  (* claude: the first hot widget of the frame claims the press. In
   * immediate mode the widgets are asked in drawing order, so with
   * overlapping widgets the one *behind* wins -- a retained tree hit
   * tests front to back instead (notes_gui.md section 6) *)
  let capture =
    if hot && t.press && t.capture = Elsewhere then Held (id b) else t.capture
  in
  let mine = capture = Held (id b) in
  let held = mine && i.mdown in
  (* claude: released inside, having been pressed inside -- or with
   * nothing pressed at all, which is a click that began and ended
   * between two updates (under 1/60 s) and would otherwise be lost *)
  let clicked = hot && i.mclick && (mine || capture = Free) in
  ({ t with capture }, hot, held, clicked)

(* the face a button shows: alive under the mouse, sunk while pressed *)
let face (th : Theme.t) ~hot ~held =
  if held then th.face_down else if hot then th.face_hot else th.face

let text_at (th : Theme.t) (b : Widget.box) s =
  Widget.Text (th.text, { b with h = th.text_size }, s)

let label (t : t) (b : Widget.box) s = draw t [ text_at t.theme b s ]

let button (t : t) (b : Widget.box) s =
  let t, hot, held, clicked = interact t b in
  let th = t.theme in
  let t =
    draw t
      ((Widget.Fill (face th ~hot ~held, b) :: Widget.frame th.edge th.border b)
      @ [ text_at th b s ])
  in
  (t, clicked)

let checkbox (t : t) (b : Widget.box) s checked =
  let t, hot, held, clicked = interact t b in
  let th = t.theme in
  let checked = if clicked then not checked else checked in
  let tick_box =
    { Widget.x = Widget.left b +. (th.row /. 2.); y = b.y; w = th.row; h = th.row }
  in
  let tick =
    if checked then [ Widget.Fill (th.accent, Widget.inset (th.padding /. 2.) tick_box) ]
    else []
  in
  let labelled =
    {
      b with
      x =
        Widget.right tick_box +. th.padding
        +. (Widget.text_width ~size:th.text_size s /. 2.);
    }
  in
  let t =
    draw t
      ((Widget.Fill (face th ~hot ~held, tick_box)
       :: Widget.frame th.edge th.border tick_box)
      @ tick
      @ [ text_at th labelled s ])
  in
  (t, checked)

let slider (t : t) (b : Widget.box) ~from ~to_ v =
  let t, hot, held, _clicked = interact t b in
  let th = t.theme in
  (* the knob's center travels between these two, its sides staying in *)
  let travel = max 0. (b.w -. th.knob) in
  let x0 = Widget.left b +. (th.knob /. 2.) in
  let v =
    if held && travel > 0. then
      let f = max 0. (min 1. ((t.input.mx -. x0) /. travel)) in
      from +. (f *. (to_ -. from))
    else v
  in
  let f =
    if to_ = from then 0. else max 0. (min 1. ((v -. from) /. (to_ -. from)))
  in
  let knob_x = x0 +. (f *. travel) in
  let track = { b with h = th.border *. 3. } in
  let filled_w = knob_x -. Widget.left track in
  let filled =
    { track with x = Widget.left track +. (filled_w /. 2.); w = filled_w }
  in
  let knob = { b with x = knob_x; w = th.knob } in
  let t =
    draw t
      ([ Widget.Fill (th.face_down, track); Widget.Fill (th.accent, filled) ]
      @ (Widget.Fill (face th ~hot ~held, knob) :: Widget.frame th.edge th.border knob))
  in
  (t, v)

let button_size (th : Theme.t) s =
  (Widget.text_width ~size:th.text_size s +. (2. *. th.padding), th.row)

let checkbox_size (th : Theme.t) s =
  ( th.row +. th.padding +. Widget.text_width ~size:th.text_size s +. th.padding,
    th.row )

let slider_size (th : Theme.t) = (th.slider_width, th.row)
