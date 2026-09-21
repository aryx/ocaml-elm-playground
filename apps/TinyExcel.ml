(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyExcel: the same spreadsheet, six years later
 * (Microsoft, 1985, on the Macintosh; plan_gui_teaching.md, phase 8).
 *
 * The pair with apps/TinyVisiCalc.ml is the point, and it is worth
 * saying what is *shared* first: appkits/sheet, the whole engine --
 * the formula language, the dependency graph, the recalculation. Not
 * one line of it differs between 1979 and 1985. What differs is
 * everything you can see:
 *
 *                 TinyVisiCalc (1979)      TinyExcel (1985)
 *   the display   40 characters, green     shapes, and a toolkit
 *                 on black                 (gui/: widgets, Layout,
 *                                          Focus, Grid)
 *   pointing      there is none: the       click a cell, drag a
 *                 arrows move a cursor     *range*
 *   commands      a letter after a slash   a menu bar you can read
 *                 (/B, /C, /G) that you    before you choose
 *                 had to know
 *   the formula   typed into the cell,     a bar above the sheet,
 *                 on a line at the top     with the selection named
 *                                          beside it
 *   copying       /R replicate, asking     Fill Down and Fill Right,
 *                 cell by cell             one menu item
 *   recalculation row order or column      the dependency graph, and
 *                 order, one pass, press   no key to press
 *                 ! twice
 *
 * What it uses: appkits/sheet (the engine), appkits/sheet_view (a
 * sheet drawn into a rectangle, shared with what comes after),
 * gui/ through playground/Gui -- menu, field, button, label -- and
 * gui/Layout. What it does not use: gui/Grid (the sheet's cells are
 * uniform, so they are arithmetic rather than layout; a grid is for
 * forms, and examples/Gui7Cells has one), and gui/Text_edit (a
 * formula bar is one line, so a field is enough).
 *
 * The one real algorithm here, and the reason Fill Down is a menu
 * item rather than a copy: **a formula that moves**. Filling =B2*C2
 * down a column has to become =B3*C3, =B4*C4 -- references are
 * relative, and shifting them is Formula.shift. That is what made
 * spreadsheets useful (one formula written once, for a table of any
 * height), and it is also exactly where $A$1 comes from, which this
 * engine does not have: see Formula.mli.
 *
 * What it deliberately does not do: formatting of any kind (Excel's
 * whole Format menu -- fonts, alignment, borders, number formats),
 * column widths you can drag, cut and paste, sorting, more than one
 * sheet, and the chart here is a panel of bars rather than the
 * linked chart *document* Excel 1.0 had -- which, in this
 * repository's terms, is a component in a document, and so is
 * appkits/embed's business (phase 10).
 *
 * Exercises: formatting -- alignment and decimal places, the Format
 * menu this has none of; column widths you can drag, which is the
 * first thing a mouse makes possible; cut, copy and paste of a
 * range (appkits/document/Clipboard is there, and a range is a
 * string away); the chart as a *component* in a document rather
 * than a panel, which is phase 10.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  sheet : Sheet.t;
  (* a selection is two cells, and one cell is a selection of one *)
  anchor : Formula.cell;
  focus : Formula.cell;
  typing : string;
  charting : bool;
  menu_edit : int;
  menu_chart : int;
  was_down : bool;
}

let opening =
  [
    ((0, 0), "region"); ((1, 0), "price"); ((2, 0), "sold"); ((3, 0), "total");
    ((0, 1), "north"); ((1, 1), "4.5"); ((2, 1), "120"); ((3, 1), "=B2*C2");
    ((0, 2), "south"); ((1, 2), "4.5"); ((2, 2), "80");
    ((0, 3), "east"); ((1, 3), "3.2"); ((2, 3), "45");
    ((0, 4), "west"); ((1, 4), "3.2"); ((2, 4), "60");
    ((0, 5), "all"); ((3, 5), "=SUM(D2:D5)");
  ]

let initial =
  {
    sheet = List.fold_left (fun s (c, text) -> Sheet.set c text s) Sheet.empty opening;
    anchor = (3, 1);
    focus = (3, 1);
    typing = "=B2*C2";
    charting = false;
    menu_edit = 0;
    menu_chart = 0;
    was_down = false;
  }

(* index 0 is the menu's own name, so choosing it means nothing was
   chosen: a command menu fires once and goes back to its title,
   where a dropdown would keep what you picked *)
let edit_menu = [ "Edit"; "Clear"; "Fill Down"; "Fill Right" ]
let chart_menu = [ "Chart"; "Show"; "Hide" ]

let geometry = { Sheet_view.default with cols = 5; rows = 7 }

type slot = Menu_edit | Menu_chart | Name | Bar | Sheet_area | Status | Chart_area

let panel =
  Layout.(
    center
      (column ~gap:10.
         [
           row ~gap:8.
             [
               leaf Menu_edit (Gui.menu_size edit_menu);
               leaf Menu_chart (Gui.menu_size chart_menu);
             ];
           row ~gap:8.
             [ leaf Name (Gui.label_size "B2:D5"); stretch (leaf Bar (Gui.field_size ())) ];
           leaf Sheet_area (Sheet_view.size geometry);
           leaf Chart_area (Sheet_view.size geometry |> fun (w, _) -> (w, 120.));
           leaf Status (Gui.label_size "5 cells selected   recalculated 3   SUM 1234.5");
         ]))

let places computer = Layout.arrange (Gui.area computer) panel

(*****************************************************************************)
(* The menu commands *)
(*****************************************************************************)

(* Fill Down and Fill Right: the top cell of the selection copied into
   the others, its formula shifted by how far it moved -- which is the
   whole of why a spreadsheet is worth using *)
let fill (dc, dr) model =
  let from, _ = Sheet_view.corners (model.anchor, model.focus) in
  let source = Sheet.raw model.sheet from in
  let content = Formula.content_of source in
  List.fold_left
    (fun sheet ((c, r) as cell) ->
      if cell = from then sheet
      else
        let steps = ((c - fst from) * dc, (r - snd from) * dr) in
        let text =
          match content with
          | Formula.Formula e -> "=" ^ Formula.to_string (Formula.shift steps e)
          | _ -> source
        in
        Sheet.set cell text sheet)
    model.sheet
    (Sheet_view.cells_of (model.anchor, model.focus))

let clear model =
  List.fold_left
    (fun sheet cell -> Sheet.set cell "" sheet)
    model.sheet
    (Sheet_view.cells_of (model.anchor, model.focus))

let selected_numbers model =
  Sheet_view.cells_of (model.anchor, model.focus)
  |> List.filter_map (fun c ->
         match Sheet.value model.sheet c with Sheet.Number f -> Some f | _ -> None)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let enter_held = ref false

let update computer model =
  let at = places computer in
  let box slot : Widget.box = List.assoc slot at in
  let m = computer.mouse in
  let enter = computer.keyboard.kenter && not !enter_held in
  enter_held := computer.keyboard.kenter;
  (* The selection: click a cell, drag to take in more of them. The
     cells are drawn rather than asked for as widgets, so this reads
     the mouse itself -- and has to ask the toolkit whether a menu is
     showing, since a grab protects widgets and not drawings *)
  let anchor, focus =
    if Gui.modal () then (model.anchor, model.focus)
    else
      match Sheet_view.cell_at geometry (box Sheet_area) (m.mx, m.my) with
      | Some c when m.mdown && not model.was_down -> (c, c)
      | Some c when m.mdown -> (model.anchor, c)
      | _ -> (model.anchor, model.focus)
  in
  let moved = (anchor, focus) <> (model.anchor, model.focus) in
  let model = { model with anchor; focus } in
  (* the bar shows the cell the selection is focused on, unless it is
     being typed into *)
  let typing = if moved then Sheet.raw model.sheet focus else model.typing in
  let typing = Gui.field_in computer (box Bar) typing in
  Gui.label_in computer (box Name) (Sheet_view.name_of (anchor, focus));
  let sheet = if enter then Sheet.set focus typing model.sheet else model.sheet in
  let model = { model with sheet; typing } in
  (* the menus. A command menu is a dropdown whose first item is its
     own name: choosing anything else does it, and back it goes *)
  let picked = Gui.menu_in computer (box Menu_edit) edit_menu model.menu_edit in
  let model, changed =
    match List.nth_opt edit_menu picked with
    | Some "Clear" -> ({ model with sheet = clear model }, true)
    | Some "Fill Down" -> ({ model with sheet = fill (0, 1) model }, true)
    | Some "Fill Right" -> ({ model with sheet = fill (1, 0) model }, true)
    | _ -> (model, false)
  in
  let picked = Gui.menu_in computer (box Menu_chart) chart_menu model.menu_chart in
  let model =
    match List.nth_opt chart_menu picked with
    | Some "Show" -> { model with charting = true }
    | Some "Hide" -> { model with charting = false }
    | _ -> model
  in
  (* A command may have changed the cell the bar is showing -- but
     only then: refreshing the bar on every frame would put the cell's
     own text back over whatever is being typed into it, which is a
     field that cannot be edited *)
  let model =
    if changed then { model with typing = Sheet.raw model.sheet model.focus } else model
  in
  let numbers = selected_numbers model in
  Gui.label_in computer (box Status)
    (Printf.sprintf "%d cell%s selected   recalculated %d   %s"
       (List.length (Sheet_view.cells_of (model.anchor, model.focus)))
       (if List.length (Sheet_view.cells_of (model.anchor, model.focus)) = 1 then "" else "s")
       (Sheet.recalculated model.sheet)
       (if numbers = [] then "" else Printf.sprintf "SUM %g" (List.fold_left ( +. ) 0. numbers)));
  { model with menu_edit = 0; menu_chart = 0; was_down = m.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* a chart of what is selected: the bars Excel 1.0 could draw, though
   it drew them in a document of their own *)
let chart (th : Theme.t) (b : Widget.box) numbers =
  if numbers = [] then []
  else
    let n = List.length numbers in
    let top = List.fold_left max 1. numbers in
    let w = b.w /. float_of_int (max 8 n) in
    List.concat
      (List.mapi
         (fun i v ->
           let h = (b.h -. 20.) *. (v /. top) in
           [
             rectangle th.accent (w *. 0.6) h
             |> move (Widget.left b +. (w *. (float_of_int i +. 0.5))) (Widget.bottom b +. (h /. 2.));
           ])
         numbers)

let view computer model =
  let s = computer.screen in
  let th = Gui.theme () in
  let at = places computer in
  (rectangle th.background s.width s.height
  :: Gui.shapes
       (Sheet_view.draw geometry th (List.assoc Sheet_area at) model.sheet
          ~selection:(model.anchor, model.focus)))
  @ (if model.charting then chart th (List.assoc Chart_area at) (selected_numbers model) else [])
  @ Gui.draw ()
  @ [
      words (rgb 120 120 120) "click a cell, drag to select a range, type in the bar, press Enter"
      |> move_y (-330.);
      words (rgb 120 120 120) "Edit > Fill Down copies the top cell, moving its references"
      |> move_y (-360.);
    ]

let app = game view update initial
let main = Playground_platform.run_app app
