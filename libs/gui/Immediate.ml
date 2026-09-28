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
(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type id = Widget.id

(* who has the mouse: nobody, this widget (it was pressed inside it),
 * or something that is not a widget (the background) *)
type capture = Free | Held of id | Elsewhere

type t = {
  input : Widget.input;
  theme : Theme.t;
  (* the mouse button at the previous frame, to see a press begin; and
     the right one's, for a right press *)
  was_down : bool;
  press : bool;
  was_rdown : bool;
  rpress : bool;
  (* a context menu asked for this frame, which has the mouse for the
     rest of it *)
  grab : id option;
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
  (* claude: how the mouse moved up since the last frame, and since the
   * press, up less down and in all: what a knob or a rotary switch,
   * dragged, turns by -- and what tells a click on one from a drag *)
  dy : float;
  dragged : float;
  moved : float;
  (* the frame being built, in reverse order *)
  painted : Widget.paint list;
  (* claude: and what goes over all of it, whenever it was asked for:
   * an open menu's items -- Dear ImGui's popup layer, so that a menu
   * can be asked for first, where its choice is needed, and still be
   * drawn on top *)
  overlay : Widget.paint list;
  (* claude: the fields asked for in the frame, kept into the next --
   * so that a click ending in a field moves the focus at the start of
   * that frame, before the fields asked earlier have drawn themselves
   * as still having it *)
  fields : Widget.box list;
}

(*****************************************************************************)
(* The toolkit's state *)
(*****************************************************************************)

let empty =
  {
    input = Widget.no_input;
    theme = Theme.default;
    was_down = false;
    press = false;
    was_rdown = false;
    rpress = false;
    grab = None;
    capture = Free;
    focus = Focus.none;
    open_menu = None;
    keys_before = [];
    pressed = [];
    cursor = 0;
    dy = 0.;
    dragged = 0.;
    moved = 0.;
    painted = [];
    overlay = [];
    fields = [];
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
  (* claude: the release of a press that a field has held since it went
   * down gives that field the keys -- known now, at the start of the
   * frame, so that every field is drawn as it will be after it *)
  let focus =
    match capture with
    | Held who when input.mclick ->
        if List.exists (fun b -> Widget.id b = who && Widget.contains b input.mx input.my) t.fields then Focus.give who focus
        else focus
    | _ -> focus
  in
  let dy = if press then 0. else input.my -. t.input.my in
  {
    t with
    input;
    press;
    dy;
    dragged = (if press then 0. else t.dragged +. dy);
    moved = (if press then 0. else t.moved +. Float.abs dy);
    was_down = input.mdown;
    rpress = input.mrdown && not t.was_rdown;
    was_rdown = input.mrdown;
    grab = None;
    capture;
    focus;
    keys_before = input.keys;
    pressed;
    cursor = (if tab then at_the_end else t.cursor);
    painted = [];
    overlay = [];
    fields = [];
  }

let paint (t : t) = List.rev t.painted @ List.rev t.overlay
let modal (t : t) = t.open_menu <> None
let theme (t : t) = t.theme
let set_theme theme (t : t) = { t with theme }
let draw (t : t) ps = { t with painted = List.rev_append ps t.painted }
let id = Widget.id

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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
    && (match t.grab with Some m -> m = Widget.id b | None -> true)
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

(*****************************************************************************)
(* The widgets *)
(*****************************************************************************)

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
  let v =
    if held then Option.value (Look.slider_value th b ~from ~to_ t.input.mx) ~default:v
    else v
  in
  let fraction = if to_ = from then 0. else max 0. (min 1. ((v -. from) /. (to_ -. from))) in
  (draw t (Look.slider th b ~fraction ~hot ~held), v)

(*****************************************************************************)
(* How big a widget wants to be *)
(*****************************************************************************)

let button_size (th : Theme.t) s =
  (Widget.text_width ~size:th.text_size s +. (2. *. th.padding), th.row)

let checkbox_size (th : Theme.t) s =
  ( th.row +. th.padding +. Widget.text_width ~size:th.text_size s +. th.padding,
    th.row )

let slider_size (th : Theme.t) = (th.slider_width, th.row)

let knob_travel = 200.

let knob (t : t) (b : Widget.box) ~from ~to_ v =
  let t, hot, held, _clicked = interact t b in
  (* turned by the mouse's move since the last frame, not set from where
   * it is: pressing a knob doesn't make it jump *)
  let v = if held && not t.press then max (min from to_) (min (max from to_) (v +. (t.dy /. knob_travel *. (to_ -. from)))) else v in
  let fraction = if to_ = from then 0. else (v -. from) /. (to_ -. from) in
  (draw t (Look.knob t.theme b ~fraction ~hot ~held), v)

let rocker (t : t) (b : Widget.box) on =
  let t, hot, held, clicked = interact t b in
  let on = if clicked then not on else on in
  (draw t (Look.rocker t.theme b ~on ~hot ~held), on)

let selector_step = 24.

let selector (t : t) (b : Widget.box) (labels : string list) (index : int) =
  let t, hot, held, clicked = interact t b in
  let n = List.length labels in
  (* a step each [selector_step] dragged, the drag spent as it steps; a
   * click that hardly moved, the next position, round again *)
  let t, index =
    if held && t.dragged >= selector_step then ({ t with dragged = t.dragged -. selector_step }, min (n - 1) (index + 1))
    else if held && t.dragged <= -.selector_step then ({ t with dragged = t.dragged +. selector_step }, max 0 (index - 1))
    else if clicked && t.moved < 3. && n > 0 then (t, (index + 1) mod n)
    else (t, index)
  in
  (draw t (Look.selector t.theme b labels ~index ~hot ~held), index)

let knob_size (th : Theme.t) = (th.dial +. 20., th.dial +. 20.)
let rocker_size (th : Theme.t) = (th.row *. 0.6, th.row *. 1.2)

let selector_size (th : Theme.t) labels =
  let widest = List.fold_left (fun m s -> max m (Widget.text_width ~size:(th.text_size *. 0.8) s)) 0. labels in
  (th.dial +. (2. *. (th.text_size *. 1.3)) +. widest, th.dial +. (2. *. th.text_size *. 1.3) +. th.text_size)

let field ?(enabled = true) (t : t) (b : Widget.box) (text : string) =
  let th = t.theme in
  let me = id b in
  (* a field that is turned off takes neither the keys nor a place in
   * the tab order: it is there to be read, and to say that it would
   * matter if something else were different *)
  if not enabled then (draw t (Look.field th b text ~caret:None ~enabled:false), text)
  else
    (* a field can take the keys, so it is in the tab order *)
    let t = { t with focus = Focus.saw me t.focus; fields = b :: t.fields } in
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
    (* claude: the caret is the focused field's alone: a field without
     * the keys clamping it to its own, shorter, text moved the caret of
     * the one being typed in (found by examples/gui4/tests/Unit_gui4) *)
    ((if focused then { t with cursor } else t), text)

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
  (* claude: drawn as it is after this frame's click, not before: open
   * on the frame of the click that opens it, as a retained toolkit
   * (which paints after its callbacks) and MVU (which draws the view
   * of the new model) show it -- rather than a frame late *)
  let t = draw t (Look.menu_closed th b label ~hot ~held) in
  let t =
    if open_menu = Some me then { t with overlay = List.rev_append (Look.menu_items th b items ~under:under_mouse) t.overlay } else t
  in
  (t, chosen)

let canvas (t : t) (b : Widget.box) =
  let t, hot, _held, _clicked = interact t b in
  let i = t.input in
  let at = (i.mx, i.my) in
  (* the press is the canvas's only if it claimed it: a widget asked
     before it, over it, took it first *)
  let events =
    (if hot then [ Widget.Hover at ] else [])
    @ (if hot && t.press && t.capture = Held (id b) then [ Widget.Press at ] else [])
    @ if hot && t.rpress then [ Widget.Right_press at ] else []
  in
  (t, events)

let context_menu (t : t) at items =
  let th = t.theme in
  let b = Look.context_box th at items in
  let under =
    List.find_opt (fun k -> Widget.contains (Look.menu_item th b k) t.input.mx t.input.my) (List.init (List.length items) Fun.id)
  in
  let answer = if t.input.mclick then match under with Some k -> `Chosen k | None -> `Dismissed else `Open in
  (* the mouse is the menu's for the rest of the frame: whatever is
     under it does not see the press *)
  let t = { t with grab = Some (id b); capture = (if t.press then Held (id b) else t.capture) } in
  ({ t with overlay = List.rev_append (Look.menu_items th b items ~under) t.overlay }, answer)

(* a list box: a click on a row selects it *)
let list (t : t) (b : Widget.box) items selected =
  let th = t.theme in
  let t, _hot, _held, clicked = interact t b in
  let selected =
    if clicked then
      let i = int_of_float ((Widget.top b -. t.input.my) /. th.row) in
      if i >= 0 && i < List.length items then Some i else selected
    else selected
  in
  (draw t (Look.list th b items ~selected), selected)

let list_size (th : Theme.t) = (th.field_width, th.row *. 5.)

let menu_size (th : Theme.t) items =
  let widest =
    List.fold_left (fun acc s -> max acc (Widget.text_width ~size:th.text_size s)) 0. items
  in
  (widest +. (3. *. th.padding), th.row)

let text_area (t : t) (b : Widget.box) (edit : Text_edit.t) =
  let th = t.theme in
  let me = id b in
  let t = { t with focus = Focus.saw me t.focus } in
  let t, _hot, held, clicked = interact t b in
  let width = Look.columns th b in
  (* enough scrolled that the caret's line is in view *)
  let caret_line, _ = Text_edit.place ~width edit (Text_edit.caret edit) in
  let first = max 0 (caret_line - Look.rows th b + 1) in
  let offset_at x y =
    let line, column = Look.text_area_place th b ~first x y in
    Text_edit.offset ~width edit ~line ~column
  in
  (* a click puts the caret where it landed; dragging from there
   * carries the other end of a selection with the mouse, which is the
   * only thing a text area does that a field does not *)
  let t, edit =
    if clicked then
      ({ t with focus = Focus.give me t.focus }, Text_edit.at (offset_at t.input.mx t.input.my) edit)
    else if held then (t, Text_edit.to_ (offset_at t.input.mx t.input.my) edit)
    else if t.input.mclick && t.capture = Elsewhere then ({ t with focus = Focus.clear t.focus }, edit)
    else (t, edit)
  in
  let focused = Focus.has me t.focus in
  let pressed k = List.mem k t.pressed in
  let shift = List.mem "Shift" t.input.keys in
  let control = List.mem "Control" t.input.keys in
  let edit =
    if not focused then edit
    else begin
      (* moving the caret: with shift the anchor stays where it is,
       * which is all a selection by keyboard is *)
      let move pos e = if shift then Text_edit.to_ pos e else Text_edit.at pos e in
      let text = Text_edit.to_string edit in
      let caret = Text_edit.caret edit in
      let line, column = Text_edit.place ~width edit caret in
      let edit =
        if control && pressed "z" then if shift then Text_edit.redo edit else Text_edit.undo edit
        else if control && pressed "y" then Text_edit.redo edit
          (* claude: a shortcut is not typing. Natively Control-C
           * produces no character at all, but a browser's keydown
           * still carries one, so the guard has to be here *)
        else if t.input.typed <> "" && not control then Text_edit.insert t.input.typed edit
        else if pressed "Enter" then Text_edit.insert "\n" edit
        else if pressed "Backspace" then Text_edit.delete_backward edit
        else if pressed "Delete" then Text_edit.delete_forward edit
        else if pressed "ArrowLeft" then move (Text.prev_char text caret) edit
        else if pressed "ArrowRight" then move (Text.next_char text caret) edit
        else if pressed "ArrowUp" then move (Text_edit.offset ~width edit ~line:(line - 1) ~column) edit
        else if pressed "ArrowDown" then move (Text_edit.offset ~width edit ~line:(line + 1) ~column) edit
        else if pressed "Home" then move (Text_edit.offset ~width edit ~line ~column:0) edit
        else if pressed "End" then move (Text_edit.offset ~width edit ~line ~column:width) edit
        else edit
      in
      edit
    end
  in
  (* the lines may have changed under the keystroke, so they are asked
   * for again: the text is the truth and the lines are a view of it *)
  let lines = Text_edit.lines ~width edit in
  let caret_line, caret_column = Text_edit.place ~width edit (Text_edit.caret edit) in
  let first = max 0 (caret_line - Look.rows th b + 1) in
  let t =
    draw t
      (Look.text_area th b lines ~range:(Text_edit.range edit)
         ~caret:(if focused then Some (caret_line - first, caret_column) else None)
         ~first)
  in
  (t, edit)

let text_area_size (th : Theme.t) = (th.field_width *. 1.6, th.row *. 5.)
