(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Look.mli *)

(*****************************************************************************)
(* The widgets *)
(*****************************************************************************)

let face (th : Theme.t) ~hot ~held =
  if held then th.face_down else if hot then th.face_hot else th.face

let text_at (th : Theme.t) (b : Widget.box) s =
  Widget.Text (th.text, { b with h = th.text_size }, s)

let label (th : Theme.t) b s = [ text_at th b s ]

let button (th : Theme.t) (b : Widget.box) s ~hot ~held ~enabled =
  (Widget.Fill ((if enabled then face th ~hot ~held else th.face_down), b)
  :: Widget.frame th.edge th.border b)
  @ [ (if enabled then text_at th b s else Widget.Text (th.edge, { b with h = th.text_size }, s)) ]

let checkbox (th : Theme.t) (b : Widget.box) s ~checked ~hot ~held =
  let tick_box =
    { Widget.x = Widget.left b +. (th.row /. 2.); y = b.y; w = th.row; h = th.row }
  in
  let tick =
    if checked then [ Widget.Fill (th.accent, Widget.inset (th.padding /. 2.) tick_box) ] else []
  in
  let labelled =
    { b with x = Widget.right tick_box +. th.padding +. (Widget.text_width ~size:th.text_size s /. 2.) }
  in
  (Widget.Fill (face th ~hot ~held, tick_box) :: Widget.frame th.edge th.border tick_box)
  @ tick
  @ [ text_at th labelled s ]

let slider (th : Theme.t) (b : Widget.box) ~fraction ~hot ~held =
  let travel = max 0. (b.w -. th.knob) in
  let knob_x = Widget.left b +. (th.knob /. 2.) +. (max 0. (min 1. fraction) *. travel) in
  let track = { b with h = th.border *. 3. } in
  let filled_w = knob_x -. Widget.left track in
  let filled = { track with x = Widget.left track +. (filled_w /. 2.); w = filled_w } in
  let knob = { b with x = knob_x; w = th.knob } in
  [ Widget.Fill (th.face_down, track); Widget.Fill (th.accent, filled) ]
  @ (Widget.Fill (face th ~hot ~held, knob) :: Widget.frame th.edge th.border knob)

let progress (th : Theme.t) (b : Widget.box) fraction =
  let f = max 0. (min 1. fraction) in
  let inner = Widget.inset th.border b in
  let w = inner.w *. f in
  (Widget.Fill (th.face_down, b) :: Widget.frame th.edge th.border b)
  @ [ Widget.Fill (th.accent, { inner with x = Widget.left inner +. (w /. 2.); w }) ]

(*****************************************************************************)
(* A field's cells *)
(*****************************************************************************)

(* A field is laid out one character to a cell, like a terminal: that
 * is what lets a click say exactly where the caret goes, with no font
 * to ask how wide each glyph really is (Text.mli). *)
let advance (th : Theme.t) = Widget.text_width ~size:th.text_size "x"

let geometry (th : Theme.t) (b : Widget.box) =
  let inner = Widget.inset th.border b in
  let left = Widget.left inner +. (th.padding /. 2.) in
  let visible = max 1 (int_of_float ((inner.w -. th.padding) /. advance th)) in
  (left, visible)

(* scrolled far enough that the caret is in view *)
let first_shown ~visible col = max 0 (col - visible + 1)

let field_column_at (th : Theme.t) (b : Widget.box) text ~caret x =
  let left, visible = geometry th b in
  let shown = first_shown ~visible (Text.column text caret) in
  max 0 (shown + int_of_float (Float.round ((x -. left) /. advance th)))

let field (th : Theme.t) (b : Widget.box) text ~caret ~enabled =
  if not enabled then
    (Widget.Fill (th.face, b) :: Widget.frame th.edge th.border b)
    @ [ Widget.Text (th.edge, { b with h = th.text_size }, text) ]
  else
    let left, visible = geometry th b in
    let col = match caret with Some i -> Text.column text i | None -> 0 in
    let shown = match caret with Some _ -> first_shown ~visible col | None -> 0 in
    let glyphs =
      Text.chars text
      |> List.filteri (fun i _ -> i >= shown && i < shown + visible)
      |> List.mapi (fun i c ->
             Widget.Text
               ( th.text,
                 { Widget.x = left +. ((float_of_int i +. 0.5) *. advance th); y = b.y; w = advance th; h = th.text_size },
                 c ))
    in
    (* a caret that does not blink: one thing less to depend on the
     * clock, and the golden frames stay the same picture every run *)
    let caret_paint =
      match caret with
      | None -> []
      | Some _ ->
          [ Widget.Fill
              ( th.text,
                { Widget.x = left +. (float_of_int (col - shown) *. advance th); y = b.y; w = th.border; h = th.text_size } ) ]
    in
    (Widget.Fill (th.field_face, b)
    :: Widget.frame (if caret = None then th.edge else th.accent) th.border b)
    @ glyphs @ caret_paint

(*****************************************************************************)
(* A dropdown *)
(*****************************************************************************)

let menu_closed (th : Theme.t) (b : Widget.box) label ~hot ~held =
  (Widget.Fill (face th ~hot ~held, b) :: Widget.frame th.edge th.border b)
  @ [
      text_at th b label;
      (* the letter v for the arrow every dropdown has: paint is
       * rectangles and text, and a triangle is neither *)
      Widget.Text (th.text, { b with x = Widget.right b -. th.padding; h = th.text_size }, "v");
    ]

let menu_item (th : Theme.t) (b : Widget.box) i =
  { b with y = Widget.bottom b -. (th.row *. (float_of_int i +. 0.5)); h = th.row }

let menu_items (th : Theme.t) (b : Widget.box) items ~under =
  let n = List.length items in
  List.concat
    (List.mapi
       (fun i s ->
         let box = menu_item th b i in
         [ Widget.Fill ((if under = Some i then th.face_hot else th.field_face), box); text_at th box s ])
       items)
  @ Widget.frame th.edge th.border
      { b with y = Widget.bottom b -. (th.row *. float_of_int n /. 2.); h = th.row *. float_of_int n }

(* a list box: rows from the top, the selected one lit, the text
   against the left; rows past the bottom are not shown *)
let list_row (th : Theme.t) (b : Widget.box) i =
  { b with y = Widget.top b -. (th.row *. (float_of_int i +. 0.5)); h = th.row }

let list (th : Theme.t) (b : Widget.box) items ~selected =
  let fits = int_of_float (b.h /. th.row) in
  (Widget.Fill (th.field_face, b)
  :: List.concat
       (List.mapi
          (fun i s ->
            if i >= fits then []
            else
              let row = list_row th b i in
              let w = Widget.text_width ~size:th.text_size s in
              (if selected = Some i then [ Widget.Fill (th.face_hot, row) ] else [])
              @ [ text_at th { row with x = Widget.left row +. th.padding +. (w /. 2.); w } s ])
          items))
  @ Widget.frame th.edge th.border b

(* a line's height: the text, and a little air between lines *)
let line_height (th : Theme.t) = th.text_size *. 1.5
let columns (th : Theme.t) (b : Widget.box) =
  max 1 (int_of_float ((b.w -. (2. *. th.padding)) /. advance th))

let rows (th : Theme.t) (b : Widget.box) =
  max 1 (int_of_float ((b.h -. th.padding) /. line_height th))

(* where a line sits: the first one just under the top of the box *)
let line_y (th : Theme.t) (b : Widget.box) i =
  Widget.top b -. (th.padding /. 2.) -. (line_height th *. (float_of_int i +. 0.5))

let text_area_left (th : Theme.t) (b : Widget.box) = Widget.left b +. th.padding

let text_area_place (th : Theme.t) (b : Widget.box) ~first x y =
  let line = first + int_of_float ((Widget.top b -. (th.padding /. 2.) -. y) /. line_height th) in
  let column = int_of_float (Float.round ((x -. text_area_left th b) /. advance th)) in
  (max 0 line, max 0 column)

let text_area (th : Theme.t) (b : Widget.box) lines ~range ~caret ~first =
  let left = text_area_left th b in
  let shown = rows th b in
  let from, upto = range in
  let visible =
    lines |> List.filteri (fun i _ -> i >= first && i < first + shown)
  in
  let paint_line i (start, text) =
    let y = line_y th b i in
    let cells = Text.chars text in
    (* the part of this line that is selected, as cells *)
    let highlight =
      if upto <= from then []
      else
        let a = max 0 (Text.column text (max 0 (from - start))) in
        let b_ = min (List.length cells) (Text.column text (max 0 (upto - start))) in
        (* a line entirely inside the selection has its whole width lit *)
        let a = if from <= start then 0 else a in
        let b_ = if upto >= start + String.length text then List.length cells else b_ in
        if b_ <= a then []
        else
          [ Widget.Fill
              ( th.face_hot,
                { Widget.x = left +. ((float_of_int (a + b_) /. 2.) *. advance th);
                  y;
                  w = float_of_int (b_ - a) *. advance th;
                  h = th.text_size } ) ]
    in
    highlight
    @ List.mapi
        (fun c s ->
          Widget.Text
            ( th.text,
              { Widget.x = left +. ((float_of_int c +. 0.5) *. advance th); y; w = advance th; h = th.text_size },
              s ))
        cells
  in
  let caret_paint =
    match caret with
    | Some (line, column) when line >= first && line < first + shown ->
        [ Widget.Fill
            ( th.text,
              { Widget.x = left +. (float_of_int column *. advance th);
                y = line_y th b (line - first);
                w = th.border;
                h = th.text_size } ) ]
    | _ -> []
  in
  (Widget.Fill (th.field_face, b)
  :: Widget.frame (if caret = None then th.edge else th.accent) th.border b)
  @ List.concat (List.mapi paint_line visible)
  @ caret_paint
