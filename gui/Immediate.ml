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

(* the face a button shows: alive under the mouse, sunk while pressed *)
let face (th : Theme.t) ~hot ~held =
  if held then th.face_down else if hot then th.face_hot else th.face

let text_at (th : Theme.t) (b : Widget.box) s =
  Widget.Text (th.text, { b with h = th.text_size }, s)

let label (t : t) (b : Widget.box) s = draw t [ text_at t.theme b s ]

let button ?(enabled = true) (t : t) (b : Widget.box) s =
  let t, hot, held, clicked = if enabled then interact t b else (t, false, false, false) in
  let th = t.theme in
  let t =
    draw t
      ((Widget.Fill ((if enabled then face th ~hot ~held else th.face_down), b)
       :: Widget.frame th.edge th.border b)
      @ [ (if enabled then text_at th b s else Widget.Text (th.edge, { b with h = th.text_size }, s)) ])
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

(* A character is not a byte. The playground hands over whatever the
 * layout produced ([typed]), and an accented letter is two bytes of
 * UTF-8, so stepping by one byte would cut one in half. These step
 * over a whole character instead: a byte whose top bits are 10 is the
 * continuation of the one before it. *)
let rec start_of_char s i =
  if i <= 0 then 0
  else if Char.code s.[i] land 0xC0 = 0x80 then start_of_char s (i - 1)
  else i

let prev_char s i = if i <= 0 then 0 else start_of_char s (i - 1)

let next_char s i =
  let n = String.length s in
  let rec go i = if i >= n || Char.code s.[i] land 0xC0 <> 0x80 then i else go (i + 1) in
  if i >= n then n else go (i + 1)

(* the string as its characters, each one to a cell *)
let chars s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let j = next_char s i in
      go j (String.sub s i (j - i) :: acc)
  in
  go 0 []

(* which cell a byte index falls in, and the other way round *)
let column s i = List.length (chars (String.sub s 0 (min i (String.length s))))

let byte_of_column s col =
  let rec go i col = if col <= 0 || i >= String.length s then i else go (next_char s i) (col - 1) in
  go 0 col

let field ?(enabled = true) (t : t) (b : Widget.box) (text : string) =
  let th = t.theme in
  let me = id b in
  (* a field that is turned off takes neither the keys nor a place in
   * the tab order: it is there to be read, and to say that it would
   * matter if something else were different *)
  if not enabled then
    let t =
      draw t
        ((Widget.Fill (th.face, b) :: Widget.frame th.edge th.border b)
        @ [ Widget.Text (th.edge, { b with h = th.text_size }, text) ])
    in
    (t, text)
  else
  (* a field can take the keys, so it is in the tab order *)
  let t = { t with focus = Focus.saw me t.focus } in
  let t, _hot, _held, clicked = interact t b in
  (* one cell per character, of the width Widget.text_width averages:
   * the stroke font is proportional, but laying the field out like a
   * terminal is what keeps the caret exactly where the person clicked
   * -- a real field asks the font where each glyph starts, and we
   * have no font to ask *)
  let advance = Widget.text_width ~size:th.text_size "x" in
  let inner = Widget.inset th.border b in
  let left = Widget.left inner +. (th.padding /. 2.) in
  let visible = max 1 (int_of_float ((inner.w -. th.padding) /. advance)) in
  (* the first cell shown: enough scrolled that the caret is in view *)
  let first_shown col = max 0 (col - visible + 1) in
  let cursor = max 0 (min (String.length text) t.cursor) in
  let shown = first_shown (column text cursor) in
  (* a click takes the keys, and puts the caret where it landed; a
   * click on the backdrop (a press that no widget claimed) gives them
   * up, and a click on another widget leaves that widget to take them *)
  let t, cursor =
    if clicked then
      let col = shown + int_of_float (Float.round ((t.input.mx -. left) /. advance)) in
      ({ t with focus = Focus.give me t.focus }, byte_of_column text (max 0 col))
    else if t.input.mclick && t.capture = Elsewhere then
      ({ t with focus = Focus.clear t.focus }, cursor)
    else (t, cursor)
  in
  let focused = Focus.has me t.focus in
  let pressed k = List.mem k t.pressed in
  let text, cursor =
    if not focused then (text, cursor)
    else
      let text, cursor =
        (* what the platform says was typed, whatever key made it *)
        if t.input.typed = "" then (text, cursor)
        else
          ( String.sub text 0 cursor ^ t.input.typed
            ^ String.sub text cursor (String.length text - cursor),
            cursor + String.length t.input.typed )
      in
      (* and the keys that produce no character at all *)
      let text, cursor =
        if pressed "Backspace" && cursor > 0 then
          let from = prev_char text cursor in
          (String.sub text 0 from ^ String.sub text cursor (String.length text - cursor), from)
        else (text, cursor)
      in
      let text, cursor =
        if pressed "Delete" && cursor < String.length text then
          let upto = next_char text cursor in
          (String.sub text 0 cursor ^ String.sub text upto (String.length text - upto), cursor)
        else (text, cursor)
      in
      let cursor = if pressed "ArrowLeft" then prev_char text cursor else cursor in
      let cursor = if pressed "ArrowRight" then next_char text cursor else cursor in
      let cursor = if pressed "Home" then 0 else cursor in
      let cursor = if pressed "End" then String.length text else cursor in
      (text, cursor)
  in
  let col = column text cursor in
  let shown = first_shown col in
  let glyphs =
    chars text
    |> List.filteri (fun i _ -> i >= shown && i < shown + visible)
    |> List.mapi (fun i c ->
           Widget.Text
             ( th.text,
               { Widget.x = left +. ((float_of_int i +. 0.5) *. advance); y = b.y; w = advance; h = th.text_size },
               c ))
  in
  (* a caret that does not blink: one thing less to depend on the
   * clock, and the golden frames stay the same picture every run *)
  let caret =
    if focused then
      [ Widget.Fill (th.text, { Widget.x = left +. (float_of_int (col - shown) *. advance); y = b.y; w = th.border; h = th.text_size }) ]
    else []
  in
  let t =
    draw t
      ((Widget.Fill (th.field_face, b)
       :: Widget.frame (if focused then th.accent else th.edge) th.border b)
      @ glyphs @ caret)
  in
  ({ t with cursor }, text)

let field_size (th : Theme.t) = (th.field_width, th.row)

let progress (t : t) (b : Widget.box) fraction =
  let th = t.theme in
  let f = max 0. (min 1. fraction) in
  let inner = Widget.inset th.border b in
  let w = inner.w *. f in
  draw t
    ((Widget.Fill (th.face_down, b) :: Widget.frame th.edge th.border b)
    @ [ Widget.Fill (th.accent, { inner with x = Widget.left inner +. (w /. 2.); w }) ])

let progress_size (th : Theme.t) = (th.slider_width, th.row *. 0.6)

let menu (t : t) (b : Widget.box) (items : string list) (chosen : int) =
  let th = t.theme in
  let me = id b in
  let was_open = t.open_menu = Some me in
  let t, hot, held, clicked = interact t b in
  (* the items hang below the box, a row each, as wide as it *)
  let item i = { b with y = Widget.bottom b -. (th.row *. (float_of_int i +. 0.5)); h = th.row } in
  let n = List.length items in
  let under_mouse =
    let rec go i =
      if i >= n then None
      else if Widget.contains (item i) t.input.mx t.input.my then Some i
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
  let closed_box =
    (Widget.Fill (face th ~hot ~held, b) :: Widget.frame th.edge th.border b)
    @ [
        text_at th b label;
        (* the letter v for the arrow every dropdown has: paint is
         * rectangles and text, and a triangle is neither *)
        Widget.Text
          ( th.text,
            { b with x = Widget.right b -. th.padding; h = th.text_size },
            "v" );
      ]
  in
  let popup =
    if not was_open then []
    else
      List.concat
        (List.mapi
           (fun i s ->
             let box = item i in
             [
               Widget.Fill
                 ((if under_mouse = Some i then th.face_hot else th.field_face), box);
               text_at th box s;
             ])
           items)
      @ Widget.frame th.edge th.border
          {
            b with
            y = Widget.bottom b -. (th.row *. float_of_int n /. 2.);
            h = th.row *. float_of_int n;
          }
  in
  (draw t (closed_box @ popup), chosen)

let menu_size (th : Theme.t) items =
  let widest =
    List.fold_left (fun acc s -> max acc (Widget.text_width ~size:th.text_size s)) 0. items
  in
  (widest +. (3. *. th.padding), th.row)
