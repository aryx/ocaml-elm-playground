(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* 7GUIs, task 7: Cells -- a spreadsheet, which is the hardest of the
 * seven and the one that is a real program
 * (docs/claude_notes/plans/plan_gui_teaching.md, phase 7).
 *
 * Click a cell, type into the bar, press Enter. A formula starts with
 * '=' and may name other cells:
 *
 *   B1   =A1*2          C1   =SUM(A1:A4)
 *
 * Everything that depends on what you changed is recomputed, and
 * nothing else -- which is the line at the bottom, and the whole
 * subject: a spreadsheet is a **graph**, not a grid
 * (appkits/sheet/Sheet.mli). The grid is what it looks like.
 *
 * Three things in here are shaped for what comes next, rather than
 * for this example:
 *
 *   - the engine is appkits/sheet, which knows nothing of widgets, so
 *     the pair of applications planned over it (TinyVisiCalc's
 *     keyboard interface of 1979 and TinyExcel's mouse one of 1985)
 *     share it exactly as written;
 *   - the sheet's drawing is one function taking a rectangle
 *     ([draw_sheet] below), because a sheet has to be drawable inside
 *     a document one day: that is the shape appkits/embed's component
 *     protocol asks for -- a size, a drawing into a rectangle, and
 *     events while it is active;
 *   - and Sheet can write itself down and read itself back, which is
 *     the fourth thing that protocol asks for.
 *
 * The form around the sheet is a gui/Grid -- Tk's geometry manager,
 * here for the reason it exists: two rows whose labels are of
 * different widths, and whose fields have to start in the same place.
 *
 * What it deliberately does not do: a cursor you can move with the
 * arrows (a real spreadsheet has a mode for that, and VisiCalc's was
 * famous; here the arrows edit the bar's text, and clicking moves),
 * scrolling beyond the cells it shows, selection of a range by
 * dragging, and anything about formatting.
 *)
open Playground

type model = {
  sheet : Sheet.t;
  (* the cell being worked on, and the text in the bar for it *)
  cursor : Formula.cell;
  typing : string;
}

let opening =
  [
    ((0, 0), "item"); ((1, 0), "price"); ((2, 0), "qty"); ((3, 0), "total");
    ((0, 1), "screws"); ((1, 1), "0.1"); ((2, 1), "250"); ((3, 1), "=B2*C2");
    ((0, 2), "planks"); ((1, 2), "4.5"); ((2, 2), "12"); ((3, 2), "=B3*C3");
    ((0, 3), "glue"); ((1, 3), "3.2"); ((2, 3), "2"); ((3, 3), "=B4*C4");
    ((0, 4), "sum"); ((3, 4), "=SUM(D2:D4)");
  ]

let initial =
  {
    sheet = List.fold_left (fun s (c, text) -> Sheet.set c text s) Sheet.empty opening;
    cursor = (3, 4);
    typing = "=SUM(D2:D4)";
  }

(* --- the sheet, drawn into a rectangle ----------------------------- *)
(* shaped the way appkits/embed will ask for it: a size, and a drawing
   into whatever rectangle it is given *)

let shown_cols = 5
let shown_rows = 7
let cell_w = 104.
let cell_h = 28.
let head_w = 40.
let sheet_size = (head_w +. (float_of_int shown_cols *. cell_w), cell_h *. float_of_int (shown_rows + 1))

(* where a cell sits inside the sheet's rectangle *)
let cell_box (b : Widget.box) (col, row) : Widget.box =
  {
    Widget.x = Widget.left b +. head_w +. (cell_w *. (float_of_int col +. 0.5));
    y = Widget.top b -. (cell_h *. (float_of_int row +. 1.5));
    w = cell_w;
    h = cell_h;
  }

let cell_at (b : Widget.box) (x, y) : Formula.cell option =
  let col = int_of_float (Float.floor ((x -. Widget.left b -. head_w) /. cell_w)) in
  let row = int_of_float (Float.floor ((Widget.top b -. y -. cell_h) /. cell_h)) in
  if col >= 0 && col < shown_cols && row >= 0 && row < shown_rows then Some (col, row) else None

(* text put where a spreadsheet puts it: numbers against the right
   edge, everything else against the left -- VisiCalc's rule, and
   every spreadsheet's since *)
let text_in (th : Theme.t) (b : Widget.box) ~right s =
  let w = Widget.text_width ~size:th.text_size s in
  let x =
    if right then Widget.right b -. (th.padding /. 2.) -. (w /. 2.)
    else Widget.left b +. (th.padding /. 2.) +. (w /. 2.)
  in
  Widget.Text (th.text, { b with x; w; h = th.text_size }, s)

let draw_sheet (th : Theme.t) (b : Widget.box) sheet cursor =
  let head_box col : Widget.box = { (cell_box b (col, -1)) with y = Widget.top b -. (cell_h /. 2.) } in
  let row_head_box row : Widget.box =
    { (cell_box b (0, row)) with x = Widget.left b +. (head_w /. 2.); w = head_w }
  in
  [ Widget.Fill (th.field_face, b) ]
  (* the headers: A B C ... down the top, 1 2 3 ... down the side *)
  @ List.concat
      (List.init shown_cols (fun col ->
           let box = head_box col in
           [ Widget.Fill (th.face, box); Look.text_at th box (Formula.name_of_cell (col, 0) |> fun s -> String.sub s 0 (String.length s - 1)) ]))
  @ List.concat
      (List.init shown_rows (fun row ->
           let box = row_head_box row in
           [ Widget.Fill (th.face, box); Look.text_at th box (string_of_int (row + 1)) ]))
  (* the cells themselves *)
  @ List.concat
      (List.init shown_rows (fun row ->
           List.concat
             (List.init shown_cols (fun col ->
                  let box = cell_box b (col, row) in
                  let v = Sheet.value sheet (col, row) in
                  let number = match v with Sheet.Number _ -> true | _ -> false in
                  let text = Sheet.show v in
                  (* the lines between the cells, which is what makes
                     a grid look like one *)
                  Widget.frame (Color.rgb 225 225 220) 1. box
                  @ (if text = "" then [] else [ text_in th box ~right:number text ])))))
  (* and the cell being worked on *)
  @ Widget.frame th.accent 2. (cell_box b cursor)

(* --- the form around it -------------------------------------------- *)

type slot = Cell_label | Cell_name | Bar | Recalc_label | Recalc | Sheet_area | Title

let form () =
  let label s = Gui.label_size s in
  Grid.make ~gap:10. ~col_weights:[ (2, 1.) ]
    [
      Grid.item ~row:0 ~col:0 ~colspan:3 ~sticky:"ew" Title (label "7GUIs 7: Cells");
      Grid.item ~row:1 ~col:0 ~colspan:3 Sheet_area sheet_size;
      Grid.item ~row:2 ~col:0 ~sticky:"w" Cell_label (label "cell:");
      Grid.item ~row:2 ~col:1 ~sticky:"w" Cell_name (label "AA10");
      Grid.item ~row:2 ~col:2 ~sticky:"ew" Bar (Gui.field_size ());
      Grid.item ~row:3 ~col:0 ~sticky:"w" Recalc_label (label "recalculated:");
      Grid.item ~row:3 ~col:1 ~colspan:2 ~sticky:"w" Recalc
        (label "12 cells, the last time something changed");
    ]

(* a margin, since the grid gives its spare width to the bar's column
   and would otherwise run to the very edge of the screen *)
let places computer = Grid.arrange (Widget.inset 60. (Gui.area computer)) (form ())

(* Enter, as an edge: the bar has the keys, so the program has to see
   the key going down itself (Scene2d.pressed is the same idea) *)
let held = ref false

let update computer model =
  let at = places computer in
  let box slot : Widget.box = List.assoc slot at in
  let m = computer.mouse in
  let enter = computer.keyboard.kenter && not !held in
  held := computer.keyboard.kenter;
  Gui.label_in computer (box Title) "7GUIs 7: Cells";
  Gui.label_in computer (box Cell_label) "cell:";
  Gui.label_in computer (box Cell_name) (Formula.name_of_cell model.cursor);
  Gui.label_in computer (box Recalc_label) "recalculated:";
  Gui.label_in computer (box Recalc)
    (let n = Sheet.recalculated model.sheet in
     Printf.sprintf "%d cell%s, the last time something changed" n (if n = 1 then "" else "s"));
  let typing = Gui.field_in computer (box Bar) model.typing in
  (* Enter puts what is in the bar into the cell, and everything
     downstream of it follows *)
  let sheet, typing, cursor =
    if enter then
      let sheet = Sheet.set model.cursor typing model.sheet in
      let below = (fst model.cursor, min (shown_rows - 1) (snd model.cursor + 1)) in
      (sheet, Sheet.raw sheet below, below)
    else (model.sheet, typing, model.cursor)
  in
  (* clicking a cell works on it instead, with what was typed into it *)
  let cursor, typing =
    if m.mclick then
      match cell_at (box Sheet_area) (m.mx, m.my) with
      | Some c -> (c, Sheet.raw sheet c)
      | None -> (cursor, typing)
    else (cursor, typing)
  in
  { sheet; cursor; typing }

let view computer model =
  let s = computer.screen in
  let th = Gui.theme () in
  let at = places computer in
  (rectangle th.background s.width s.height
  :: Gui.shapes (draw_sheet th (List.assoc Sheet_area at) model.sheet model.cursor))
  @ Gui.draw ()
  @ [
      words (rgb 120 120 120) "click a cell, type in the bar, press Enter" |> move_y (-300.);
      words (rgb 120 120 120) "a formula starts with = : try =B2*C2 or =SUM(D2:D4)" |> move_y (-330.);
    ]

let app = game view update initial
let main = Playground_platform.run_app app
