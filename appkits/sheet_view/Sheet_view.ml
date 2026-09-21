(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sheet_view.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type geometry = {
  cols : int;
  rows : int;
  cell_w : float;
  cell_h : float;
  head_w : float;
  head_h : float;
}

let default = { cols = 5; rows = 7; cell_w = 104.; cell_h = 28.; head_w = 40.; head_h = 28. }

let size g =
  (g.head_w +. (float_of_int g.cols *. g.cell_w), g.head_h +. (float_of_int g.rows *. g.cell_h))

(*****************************************************************************)
(* Where a cell is *)
(*****************************************************************************)

let cell_box g (b : Widget.box) (col, row) : Widget.box =
  {
    Widget.x = Widget.left b +. g.head_w +. (g.cell_w *. (float_of_int col +. 0.5));
    y = Widget.top b -. g.head_h -. (g.cell_h *. (float_of_int row +. 0.5));
    w = g.cell_w;
    h = g.cell_h;
  }

let cell_at g (b : Widget.box) (x, y) =
  let col = int_of_float (Float.floor ((x -. Widget.left b -. g.head_w) /. g.cell_w)) in
  let row = int_of_float (Float.floor ((Widget.top b -. g.head_h -. y) /. g.cell_h)) in
  if col >= 0 && col < g.cols && row >= 0 && row < g.rows then Some (col, row) else None

(*****************************************************************************)
(* Selections *)
(*****************************************************************************)

let corners ((c1, r1), (c2, r2)) = ((min c1 c2, min r1 r2), (max c1 c2, max r1 r2))

let cells_of selection =
  let (c1, r1), (c2, r2) = corners selection in
  List.concat (List.init (r2 - r1 + 1) (fun dr -> List.init (c2 - c1 + 1) (fun dc -> (c1 + dc, r1 + dr))))

let name_of selection =
  let a, b = corners selection in
  if a = b then Formula.name_of_cell a
  else Formula.name_of_cell a ^ ":" ^ Formula.name_of_cell b

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

(* text where a spreadsheet puts it: numbers against the right edge,
 * everything else against the left -- VisiCalc's rule, and the reason
 * a column of figures reads as a column *)
let text_in (th : Theme.t) (b : Widget.box) ~right s =
  let w = Widget.text_width ~size:th.text_size s in
  let x =
    if right then Widget.right b -. (th.padding /. 2.) -. (w /. 2.)
    else Widget.left b +. (th.padding /. 2.) +. (w /. 2.)
  in
  Widget.Text (th.text, { b with x; w; h = th.text_size }, s)

(* the letters of a column, without the row number a cell's name carries *)
let column_letters col =
  let name = Formula.name_of_cell (col, 0) in
  String.sub name 0 (String.length name - 1)

let draw g (th : Theme.t) (b : Widget.box) sheet ~selection =
  let (left_col, top_row), (right_col, bottom_row) = corners selection in
  let in_selection (c, r) = c >= left_col && c <= right_col && r >= top_row && r <= bottom_row in
  let head_box col : Widget.box =
    { (cell_box g b (col, 0)) with y = Widget.top b -. (g.head_h /. 2.); h = g.head_h }
  in
  let row_head_box row : Widget.box =
    { (cell_box g b (0, row)) with x = Widget.left b +. (g.head_w /. 2.); w = g.head_w }
  in
  [ Widget.Fill (th.field_face, b) ]
  @ List.concat
      (List.init g.cols (fun col ->
           let box = head_box col in
           [
             Widget.Fill ((if col >= left_col && col <= right_col then th.face_hot else th.face), box);
             Look.text_at th box (column_letters col);
           ]))
  @ List.concat
      (List.init g.rows (fun row ->
           let box = row_head_box row in
           [
             Widget.Fill ((if row >= top_row && row <= bottom_row then th.face_hot else th.face), box);
             Look.text_at th box (string_of_int (row + 1));
           ]))
  @ List.concat
      (List.init g.rows (fun row ->
           List.concat
             (List.init g.cols (fun col ->
                  let box = cell_box g b (col, row) in
                  let v = Sheet.value sheet (col, row) in
                  let number = match v with Sheet.Number _ -> true | _ -> false in
                  let text = Sheet.show v in
                  (* a selection of more than one cell is shaded; one
                     cell is only ringed, below *)
                  (if in_selection (col, row) && fst selection <> snd selection then
                     [ Widget.Fill (th.face_hot, box) ]
                   else [])
                  @ Widget.frame (Color.rgb 225 225 220) 1. box
                  @ if text = "" then [] else [ text_in th box ~right:number text ]))))
  (* the ring around the whole selection: one rectangle, however many
     cells it covers *)
  @
  let a = cell_box g b (left_col, top_row) and z = cell_box g b (right_col, bottom_row) in
  Widget.frame th.accent 2.
    {
      Widget.x = (Widget.left a +. Widget.right z) /. 2.;
      y = (Widget.top a +. Widget.bottom z) /. 2.;
      w = Widget.right z -. Widget.left a;
      h = Widget.top a -. Widget.bottom z;
    }
