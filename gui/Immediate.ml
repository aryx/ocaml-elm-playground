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

(* a widget is its rectangle: where it is tells it from its neighbours
 * (Widget.id) *)
type id = Widget.id

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
  (* who has the keys, and in what order Tab walks *)
  focus : Focus.t;
  (* the menu whose items are showing, if any: while one is open it
     takes the mouse from every other widget (a "grab", which is what
     every toolkit does with a popup) *)
  open_menu : id option;
  (* the keys held at the previous frame, and the ones that went down
     at this one: a widget wants the edge, the playground gives the
     state *)
  keys_before : string list;
  pressed : string list;
  (* where the caret is in the focused field, in characters; clamped
     by the field, since only it knows how long its text is *)
  cursor : int;
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
    focus = Focus.none;
    open_menu = None;
    keys_before = [];
    pressed = [];
    cursor = 0;
    painted = [];
  }

(* the caret lands at the end of a field it has just been given *)
let at_the_end = max_int

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
  let pressed = List.filter (fun k -> not (List.mem k t.keys_before)) input.keys in
  let focus = Focus.frame t.focus in
  let tab = List.mem "Tab" pressed in
  let shift = List.mem "Shift" input.keys in
  let focus =
    if tab then if shift then Focus.previous focus else Focus.next focus else focus
  in
  {
    t with
    input;
    press;
    was_down = input.mdown;
    capture;
    focus;
    keys_before = input.keys;
    pressed;
    cursor = (if tab then at_the_end else t.cursor);
    painted = [];
  }

let paint (t : t) = List.rev t.painted
let theme (t : t) = t.theme
let set_theme theme (t : t) = { t with theme }
let draw (t : t) ps = { t with painted = List.rev_append ps t.painted }
let id = Widget.id

(* The three questions every widget asks about the mouse, and the only
 * place the capture changes: is it over me (hot), is it pressed in me
 * (held), did a press that began in me end in me (clicked)? *)
let interact (t : t) (b : Widget.box) =
  let i = t.input in
  let hot =
    Widget.contains b i.mx i.my
    (* claude: ... unless another widget's popup is showing, and then
     * the mouse is its, wherever it goes *)
    && (match t.open_menu with Some m -> m = Widget.id b | None -> true)
  in
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

let label (t : t) (b : Widget.box) s = draw t (Look.label t.theme b s)

let button ?(enabled = true) (t : t) (b : Widget.box) s =
  let t, hot, held, clicked = if enabled then interact t b else (t, false, false, false) in
  (draw t (Look.button t.theme b s ~hot ~held ~enabled), clicked)

let checkbox (t : t) (b : Widget.box) s checked =
  let t, hot, held, clicked = interact t b in
  let checked = if clicked then not checked else checked in
  (draw t (Look.checkbox t.theme b s ~checked ~hot ~held), checked)

let slider (t : t) (b : Widget.box) ~from ~to_ v =
  let t, hot, held, _clicked = interact t b in
  let th = t.theme in
  (* the knob's center travels over this much, its sides staying in *)
  let travel = max 0. (b.w -. th.knob) in
  let x0 = Widget.left b +. (th.knob /. 2.) in
  let v =
    if held && travel > 0. then
      let f = max 0. (min 1. ((t.input.mx -. x0) /. travel)) in
      from +. (f *. (to_ -. from))
    else v
  in
  let fraction = if to_ = from then 0. else max 0. (min 1. ((v -. from) /. (to_ -. from))) in
  (draw t (Look.slider th b ~fraction ~hot ~held), v)

let button_size (th : Theme.t) s =
  (Widget.text_width ~size:th.text_size s +. (2. *. th.padding), th.row)

let checkbox_size (th : Theme.t) s =
  ( th.row +. th.padding +. Widget.text_width ~size:th.text_size s +. th.padding,
    th.row )

let slider_size (th : Theme.t) = (th.slider_width, th.row)

let field ?(enabled = true) (t : t) (b : Widget.box) (text : string) =
  let th = t.theme in
  let me = id b in
  (* a field that is turned off takes neither the keys nor a place in
   * the tab order: it is there to be read, and to say that it would
   * matter if something else were different *)
  if not enabled then (draw t (Look.field th b text ~caret:None ~enabled:false), text)
  else
    (* a field can take the keys, so it is in the tab order *)
    let t = { t with focus = Focus.saw me t.focus } in
    let t, _hot, _held, clicked = interact t b in
    let cursor = max 0 (min (String.length text) t.cursor) in
    (* a click takes the keys, and puts the caret where it landed; a
     * click on the backdrop (a press that no widget claimed) gives
     * them up, and a click on another widget leaves that widget to
     * take them *)
    let t, cursor =
      if clicked then
        ( { t with focus = Focus.give me t.focus },
          Text.byte_of_column text (Look.field_column_at th b text ~caret:cursor t.input.mx) )
      else if t.input.mclick && t.capture = Elsewhere then
        ({ t with focus = Focus.clear t.focus }, cursor)
      else (t, cursor)
    in
    let focused = Focus.has me t.focus in
    let pressed k = List.mem k t.pressed in
    let text, cursor =
      if not focused then (text, cursor)
      else Text.edit ~typed:t.input.typed ~pressed text cursor
    in
    let t = draw t (Look.field th b text ~caret:(if focused then Some cursor else None) ~enabled:true) in
    ({ t with cursor }, text)

let field_size (th : Theme.t) = (th.field_width, th.row)

let progress (t : t) (b : Widget.box) fraction = draw t (Look.progress t.theme b fraction)
let progress_size (th : Theme.t) = (th.slider_width, th.row *. 0.6)

let menu (t : t) (b : Widget.box) (items : string list) (chosen : int) =
  let th = t.theme in
  let me = id b in
  let was_open = t.open_menu = Some me in
  let t, hot, held, clicked = interact t b in
  let n = List.length items in
  let under_mouse =
    let rec go i =
      if i >= n then None
      else if Widget.contains (Look.menu_item th b i) t.input.mx t.input.my then Some i
      else go (i + 1)
    in
    if was_open then go 0 else None
  in
  let chosen = if t.input.mclick then match under_mouse with Some i -> i | None -> chosen else chosen in
  let open_menu =
    if clicked then if was_open then None else Some me
      (* claude: a click anywhere while it is open closes it: on an
       * item it chose one, elsewhere it changed its mind *)
    else if was_open && t.input.mclick then None
    else t.open_menu
  in
  let t = { t with open_menu } in
  let label = match List.nth_opt items chosen with Some s -> s | None -> "" in
  let paint =
    Look.menu_closed th b label ~hot ~held
    @ if was_open then Look.menu_items th b items ~under:under_mouse else []
  in
  (draw t paint, chosen)

let menu_size (th : Theme.t) items =
  let widest =
    List.fold_left (fun acc s -> max acc (Widget.text_width ~size:th.text_size s)) 0. items
  in
  (widest +. (3. *. th.padding), th.row)
