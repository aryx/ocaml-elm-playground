(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyOpenDoc: a document with no application (Apple, IBM and
 * Component Integration Labs, 1994-97; plan_gui_teaching.md, phase 10).
 *
 * OpenDoc's bet was that the application is the wrong unit. A letter
 * with a budget and a drawing in it is one document; why should it take
 * three programs, and why should the letter's program own the other
 * two? So: documents made of **parts**, each part edited by whatever
 * code knows its kind, in place, where it sits on the page -- click a
 * part once to select it, again to edit it, and the menu bar becomes
 * that part's. There is no word processor and no spreadsheet program;
 * there are only kinds of part.
 *
 * It had ancestors. Xerox Star (1981) already put text, pictures and
 * tables in one document, each edited where it was -- but as a fixed
 * set of kinds, built into one editor. The Andrew Toolkit (CMU, 1988)
 * opened the set: any "inset" in any other, anyone could write a new
 * kind. Microsoft's OLE 2 (1993) made the idea famous with in-place
 * activation -- the sheet in the Word document, the menus turning into
 * Excel's -- while keeping the applications. OpenDoc dropped them, and
 * was cancelled in 1997; and then the idea won on the web instead,
 * where a page is made of embedded things none of which the page's
 * author wrote, and a notebook is cells of different kinds.
 *
 * What it uses: appkits/embed (Component, what a part is; Compound, a
 * document of parts), and four kinds of part, each a small editor
 * over the engine of the application it is taken from -- Part_text
 * (TinyWord's), Part_sheet (TinyExcel's), Part_picture
 * (TinyMacPaint's), Part_drawing (TinyMacDraw's) -- plus
 * appkits/document's Undo, and the playground's menus. This file knows
 * none of them: it reaches them through the protocol, and through the
 * registry when a document is read back. The fourth came after the
 * others, and adding it here took a line in the registry and one in
 * the Insert menu, nothing else -- which is the whole point.
 *
 * What it demonstrates, besides: a part of a kind nobody here has
 * code for (the "equation" at the bottom) is shown as a placeholder
 * and saved back byte for byte -- the rule that a document survives a
 * program that cannot read all of it; an editing session in place is
 * one edit of the document, "Undo Edit sheet", recorded when it ends
 * and only if it changed something -- which is decided by comparing
 * what the part saves, since two parts, being functions, cannot be
 * compared; and File > Save then Revert goes through the saved text
 * and the registry, so what comes back is what was written.
 *
 * And resizing, with everything reflowing as the mouse moves: a
 * selected part's bottom handle gives it a height, the gap between two
 * parts of a row shares the row's width out anew. The layout is worked
 * out from the document every frame anyway, so live reflow costs
 * nothing -- immediate mode's gift. A height given is a proposal, as in
 * OpenDoc's frame negotiation: a part gets it if it is more than the
 * part needs, and what it needs otherwise (Compound). Widths are not
 * negotiated: a part whose content has a size of its own (the sheet,
 * the picture) spills over when its share is made too narrow for it --
 * unless it is made "Scale to Fit" (Edit menu), OLE's way: then it is
 * drawn scaled to its room, up or down, keeping its proportions, the
 * mouse mapped back so that the part never knows (Component.draw_in).
 * So a part given room is negotiated with (OpenDoc), scaled (OLE), or,
 * for a text, simply rewraps -- three answers to one question.
 *
 * What it deliberately does not do: containers that are parts (here
 * the rows and columns are the document's, see Compound); a part
 * flowing inside a text like a very large character, which is what
 * Word does with an embedded sheet; dragging parts around; linking, as
 * opposed to embedding (OLE's "L": a part that shows a file kept
 * elsewhere, updated when it changes); a toolbar per part, OLE's other
 * half of in-place activation; and scrolling, so the document has to
 * fit on its page.
 *
 * Exercises: a container part -- a Column as a Component.part holding
 * parts, so that a text can hold a sheet that holds a picture, as
 * OpenDoc's could; drag a part to move it, which is Compound.remove
 * then insert_after; a fifth kind (a chart of a sheet's column, which
 * TinyExcel draws already), added as the drawing was; the placeholder showing a
 * picture of the part saved with it, as OLE's cached metafile did.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  (* the document, and every version of it *)
  history : Compound.t Undo.t;
  (* during an editing session in place, the document as it is being
     changed -- recorded in the history as one edit when it ends *)
  editing : Compound.t option;
  selected : Compound.path option;
  (* the click that activated the part is still held: it was the
     document's, not the part's, and the part sees the mouse only once
     it is let go -- or the click that wakes a picture would paint *)
  waking : bool;
  (* what File > Save wrote, for File > Revert to read *)
  saved : string;
  said : string;
  (* a size being dragged -- a part's height by its bottom handle, or
     the gap between two parts of a row -- and the document as it
     reflows meanwhile, made one edit when the mouse is let go *)
  resizing : resize option;
  live : Compound.t option;
  was : string list;
  was_down : bool;
}

and resize = Height of Compound.path * float (* the part's top *) | Split of Compound.splitter

(* how each kind of part is read back: the only place the three are
   named *)
let registry : Component.registry =
  [
    (Part_text.kind, Part_text.load);
    (Part_sheet.kind, Part_sheet.load);
    (Part_picture.kind, Part_picture.load);
    (Part_drawing.kind, Part_drawing.load);
  ]

let doc model =
  match (model.editing, model.live) with Some d, _ | None, Some d -> d | None, None -> Undo.now model.history
let active model = model.editing <> None
let part model path = Option.get (Compound.get (doc model) path)

(*****************************************************************************)
(* The document it opens on *)
(*****************************************************************************)

let text ?(title = "") body =
  let r = Rich.of_string (title ^ body) in
  let r =
    if title = "" then r
    else Rich.restyle (fun s -> { (Style.toggle_bold s) with size = 26. }) (Rich.select ~anchor:0 ~caret:(String.length title) r)
  in
  Compound.Part (Part_text.make (Rich.at 0 r))

let budget =
  List.fold_left
    (fun s (cell, v) -> Sheet.set cell v s)
    Sheet.empty
    [ ((0, 0), "Paper"); ((1, 0), "12"); ((0, 1), "Ink"); ((1, 1), "30"); ((0, 2), "Stamps"); ((1, 2), "8"); ((0, 3), "Total"); ((1, 3), "=SUM(B1:B3)") ]

let sketch =
  Bitmap.change (Bitmap.create ~width:150 ~height:72) (fun b ->
      let black = Pattern.solid in
      Paint.stroke b ~brush:Paint.pencil black (0, 60) (149, 60);
      Paint.fill_rect b (List.nth Pattern.palette 10) (20, 32) (70, 60);
      Paint.frame_rect b black (20, 32) (70, 60);
      Paint.stroke b ~brush:Paint.pencil black (14, 32) (45, 10);
      Paint.stroke b ~brush:Paint.pencil black (45, 10) (76, 32);
      Paint.stroke b ~brush:Paint.pencil black (14, 32) (76, 32);
      Seed_fill.fill b (List.nth Pattern.palette 6) 45 25;
      Paint.fill_oval b Pattern.grey (110, 8) (135, 33);
      Paint.frame_oval b black (110, 8) (135, 33))

let opening =
  Compound.Column
    [
      text ~title:"TinyOpenDoc, 1994\n"
        "A document with no application: a text, a sheet and a picture, each a part. Click a part to select it, and \
         again to edit it where it is -- its menu joins the menu bar. Escape, or a click outside, puts it down.";
      Row [ Part (Part_sheet.make budget); Part (Part_picture.make sketch) ];
      text
        "The part below is of a kind this program has no code for. It is shown as a placeholder, and File > Save \
         writes it back exactly as it came.";
      Part (Component.placeholder ~kind:"equation" "\\sum_{i=1}^{n} i = n(n+1)/2");
    ]

let initial =
  {
    history = Undo.start opening;
    editing = None;
    selected = None;
    waking = false;
    saved = "";
    said = "";
    resizing = None;
    live = None;
    was = [];
    was_down = false;
  }

(*****************************************************************************)
(* The page *)
(*****************************************************************************)

let page_w = 760.
let page_h = 880.
let page_top = 430.
let margin = 20.
let left = (-.page_w /. 2.) +. margin
let top = page_top -. margin
let width = page_w -. (2. *. margin)
let laid_out model = fst (Compound.layout (doc model) ~left ~top ~width)
let splitters model = Compound.splitters (doc model) ~left ~top ~width

(* the bottom handle of the selected part, the one that gives it a
   height: where it is drawn *)
let height_grip (b : Widget.box) = (b.x, b.y -. ((b.h +. 6.) /. 2.))

(* A resize starting under the mouse, if any: the selected part's
   bottom handle, or a gap between two parts of a row *)
let resize_at model (mx, my) =
  let grip =
    match model.selected with
    | Some p when not (model.editing <> None) -> (
        match List.assoc_opt p (laid_out model) with
        | Some b ->
            let gx, gy = height_grip b in
            if Float.abs (mx -. gx) <= 8. && Float.abs (my -. gy) <= 8. then Some (Height (p, Widget.top b)) else None
        | None -> None)
    | _ -> None
  in
  match grip with
  | Some _ -> grip
  | None -> Option.map (fun s -> Split s) (List.find_opt (fun (s : Compound.splitter) -> Widget.contains s.grip mx my) (splitters model))

(* the document while the mouse drags a size: the one before the drag,
   resized -- and so everything below and beside it laid out again,
   every frame *)
let resized model r (mx, my) =
  let base = Undo.now model.history in
  match r with
  | Height (p, top) -> Compound.set_height base p (Some (Float.max 20. (top -. my)))
  | Split s ->
      let l, r = s.span in
      Compound.resize_row base s.row s.index ((mx -. l -. (Compound.gap /. 2.)) /. (r -. l -. Compound.gap))

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the end of an editing session in place: one edit, if it made one *)
let put_down model =
  match model.editing with
  | None -> model
  | Some d ->
      let before = Undo.now model.history in
      let kind = match model.selected with Some p -> (part model p).kind | None -> "part" in
      let history = if Compound.save d = Compound.save before then model.history else Undo.record ~name:("Edit " ^ kind) d model.history in
      { model with history; editing = None }

let record ~name d model =
  let model = put_down model in
  { model with history = Undo.record ~name d model.history }

(* where a part inserted after the selection lands *)
let next_path model =
  match (model.selected, doc model) with
  | Some p, _ -> List.rev (match List.rev p with last :: rest -> (last + 1) :: rest | [] -> [])
  | None, (Column kids | Row kids | Sized (_, (Column kids | Row kids))) -> [ List.length kids ]
  | None, _ -> [ 1 ]

let insert name node model =
  let model = put_down model in
  let at = next_path model in
  let d = Compound.insert_after (doc model) (Option.value model.selected ~default:[]) node in
  { (record ~name d model) with selected = Some at }

let menu_file = [ "File"; "Save"; "Revert"; "New" ]
let menu_edit = [ "Edit"; "Undo"; "Redo"; "Delete Part"; "Scale to Fit"; "Natural Size" ]
let menu_insert = [ "Insert"; "Text"; "Sheet"; "Picture"; "Drawing" ]
let menu_box i : Widget.box = { Widget.x = -410. +. (float_of_int i *. 95.); y = 470.; w = 90.; h = 30. }

let command items chosen model =
  match List.nth_opt items chosen with
  | Some "Save" ->
      let saved = Compound.save (doc model) in
      { (put_down model) with saved; said = Printf.sprintf "saved, %d bytes" (String.length saved) }
  | Some "Revert" when model.saved <> "" -> { (record ~name:"Revert" (Compound.load registry model.saved) model) with selected = None; said = "read back from what was saved" }
  | Some "New" -> { (record ~name:"New" (Compound.Column [ text "" ]) model) with selected = Some [ 0 ] }
  | Some "Undo" ->
      let model = put_down model in
      { model with history = Undo.undo model.history; selected = None }
  | Some "Redo" ->
      let model = put_down model in
      { model with history = Undo.redo model.history; selected = None }
  | Some "Delete Part" -> (
      match model.selected with Some p -> { (record ~name:"Delete" (Compound.remove (doc model) p) model) with selected = None } | None -> model)
  (* the two ways to give a part the room it is given: scale it (OLE),
     or let it insist on its own size (OpenDoc) *)
  | Some ("Scale to Fit" | "Natural Size" as c) -> (
      match model.selected with
      | Some p -> record ~name:c (Compound.set_scaled (doc model) p (c = "Scale to Fit")) model
      | None -> model)
  | Some "Text" -> insert "Insert Text" (text "A new text.") model
  | Some "Sheet" -> insert "Insert Sheet" (Part (Part_sheet.make Sheet.empty)) model
  | Some "Picture" -> insert "Insert Picture" (Part (Part_picture.make (Bitmap.create ~width:150 ~height:72))) model
  | Some "Drawing" -> insert "Insert Drawing" (Part (Part_drawing.make Drawing.empty)) model
  | _ -> model

let update computer model =
  let m = computer.mouse in
  let now = Set_.elements computer.keyboard.keys in
  let pressed key = List.mem key now && not (List.mem key model.was) in
  let model = command menu_file (Gui.menu_in computer (menu_box 0) menu_file 0) model in
  let model = command menu_edit (Gui.menu_in computer (menu_box 1) menu_edit 0) model in
  let model = command menu_insert (Gui.menu_in computer (menu_box 2) menu_insert 0) model in
  (* in-place activation: the active part's menu, in the host's bar *)
  let model =
    match (model.editing, model.selected) with
    | Some d, Some p ->
        let pt = part model p in
        if pt.menu = [] then model
        else
          let chosen = Gui.menu_in computer (menu_box 3) pt.menu 0 in
          if chosen > 0 then { model with editing = Some (Compound.set d p (pt.command (List.nth pt.menu chosen))) } else model
    | _ -> model
  in
  let press = m.mdown && not model.was_down in
  let model =
    if Gui.modal () then model
    else
      match (model.resizing, press) with
      (* a size being dragged: the document reflows as the mouse moves,
         and the drag is one edit when it is let go *)
      | Some r, _ when m.mdown -> { model with live = Some (resized model r (m.mx, m.my)) }
      | Some _, _ ->
          let before = Undo.now model.history in
          let model =
            match model.live with
            | Some d when Compound.save d <> Compound.save before -> { model with history = Undo.record ~name:"Resize" d model.history }
            | _ -> model
          in
          { model with resizing = None; live = None }
      | None, true when resize_at model (m.mx, m.my) <> None ->
          { model with resizing = resize_at model (m.mx, m.my); live = Some (Undo.now model.history) }
      | None, _ ->
      let boxes = laid_out model in
      (* a click: select a part, or activate the selected one, or put
         down the active one when it lands elsewhere *)
      let model =
        if press then
          match Compound.at_point boxes (m.mx, m.my) with
          | Some p when active model && model.selected = Some p -> model
          | Some p when model.selected = Some p -> { model with editing = Some (doc model); waking = true; said = "" }
          | Some p -> { (put_down model) with selected = Some p; said = "" }
          | None when Float.abs m.mx < page_w /. 2. && m.my < page_top -> { (put_down model) with selected = None }
          | None -> model
        else model
      in
      (* the active part gets the mouse and the keys, in its rectangle *)
      let model = if m.mdown then model else { model with waking = false } in
      match (model.editing, model.selected) with
      | Some _, _ when model.waking -> model
      | Some d, Some p -> (
          match List.assoc_opt p (laid_out model) with
          | Some b ->
              let part = Component.input_in ~scaled:(Compound.scaled d p) (part model p) computer b in
              { model with editing = Some (Compound.set d p part) }
          | None -> model)
      | _ -> model
  in
  let model =
    if active model then if pressed "Escape" then put_down model else model
    else if List.mem "Control" now && pressed "z" then command menu_edit 1 model
    else if List.mem "Control" now && pressed "y" then command menu_edit 2 model
    else if pressed "Backspace" || pressed "Delete" then command menu_edit 3 model
    else model
  in
  { model with was = now; was_down = m.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* a part selected: a thin frame and eight handles, as OLE drew it *)
let handles (b : Widget.box) =
  let c = rgb 40 90 200 in
  Gui.shapes (Widget.frame c 1. { b with w = b.w +. 6.; h = b.h +. 6. })
  @ List.concat_map
      (fun dx ->
        List.filter_map
          (fun dy ->
            if dx = 0. && dy = 0. then None
            else Some (rectangle c 7. 7. |> move (b.x +. (dx *. (b.w +. 6.) /. 2.)) (b.y +. (dy *. (b.h +. 6.) /. 2.))))
          [ -1.; 0.; 1. ])
      [ -1.; 0.; 1. ]

(* a part active: the hatched border of OLE's in-place editing *)
let hatched (b : Widget.box) =
  let band = 7. in
  let outer : Widget.box = { b with w = b.w +. (2. *. band) +. 4.; h = b.h +. (2. *. band) +. 4. } in
  let ticks n f = List.init n f in
  let along_x y = ticks (int_of_float (outer.w /. 8.)) (fun i -> rectangle (rgb 90 90 90) 2. 9. |> rotate 45. |> move (Widget.left outer +. 4. +. (float_of_int i *. 8.)) y) in
  let along_y x = ticks (int_of_float (outer.h /. 8.)) (fun i -> rectangle (rgb 90 90 90) 2. 9. |> rotate 45. |> move x (Widget.top outer -. 4. -. (float_of_int i *. 8.))) in
  [ rectangle (rgb 215 215 215) outer.w outer.h |> move b.x b.y ]
  @ along_x (Widget.top outer -. (band /. 2.))
  @ along_x (Widget.top outer -. outer.h +. (band /. 2.))
  @ along_y (Widget.left outer +. (band /. 2.))
  @ along_y (Widget.left outer +. outer.w -. (band /. 2.))
  @ [ rectangle white (b.w +. 4.) (b.h +. 4.) |> move b.x b.y ]

let view computer model =
  let d = doc model in
  let boxes = laid_out model in
  let parts =
    List.concat_map
      (fun (p, b) ->
        let on = model.selected = Some p in
        let frame = if on && active model then hatched b else if on then handles b else [] in
        frame @ Component.draw_in ~scaled:(Compound.scaled d p) (Option.get (Compound.get d p)) b ~active:(on && active model))
      boxes
  in
  (* the gap under the mouse, or being dragged, shows it can be *)
  let grip =
    let bar (s : Compound.splitter) = [ rectangle (rgb 40 90 200) 3. s.grip.h |> move s.grip.x s.grip.y ] in
    match model.resizing with
    | Some (Split s) -> ( match List.find_opt (fun (t : Compound.splitter) -> t.row = s.row && t.index = s.index) (splitters model) with Some t -> bar t | None -> [])
    | Some (Height _) -> []
    | None -> ( match resize_at model (computer.mouse.mx, computer.mouse.my) with Some (Split s) -> bar s | _ -> [])
  in
  let status =
    match (model.selected, active model) with
    | Some p, true -> Printf.sprintf "editing the %s in place -- Escape, or click outside, to put it down" (part model p).kind
    | Some p, false -> Printf.sprintf "a %s, selected -- click it again to edit it" (part model p).kind
    | None, _ -> ( match Undo.undo_name model.history with Some n -> "Undo " ^ n | None -> "click a part")
  in
  [
    rectangle (rgb 175 180 190) 1000. 1000.;
    rectangle (Gui.theme ()).face 1000. 40. |> move 0. 470.;
    rectangle white page_w page_h |> move 0. (page_top -. (page_h /. 2.));
  ]
  @ parts @ grip
  @ [ words (rgb 50 50 50) (status ^ if model.said = "" then "" else "     " ^ model.said) |> move 0. (-470.) ]
  @ Gui.draw ()

let app = game view update initial
let main = Playground_platform.run_app app
