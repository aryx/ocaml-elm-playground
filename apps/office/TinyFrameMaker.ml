(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyFrameMaker: a long document that lays itself out (FrameMaker,
 * Charles Corfield, Frame Technology, around 1986, first on Unix
 * workstations; Adobe's since 1995 -- from memory, to check).
 *
 * FrameMaker was made for the documents nobody lays out by hand:
 * manuals of a thousand pages, where a paragraph added in chapter one
 * moves every picture after it. Its answer is that nothing is placed,
 * everything flows:
 *
 * - **one text through a chain of frames**: the columns of every page,
 *   the text filling one and going on in the next, over as many pages
 *   as it takes (appkits/richtext/Flow);
 * - **master pages**: the pages are made from one, its columns, its
 *   header and its footer (here, "page 3 of 5") -- change it, one
 *   column or two, and every page follows;
 * - **anchored frames**: a sheet, a picture, a drawing tied to a place
 *   in the text and set just below its line, so that typing above it
 *   carries it down, from column to column and page to page. Its
 *   frames hold the same parts as TinyOpenDoc (appkits/embed), edited
 *   in place the same way: click once to select, again to edit, and
 *   the part's menu joins the bar.
 *
 * **Against TinyOpenDoc**, the other document made of parts here:
 *
 *   more powerful -- the parts are *in* the text and move with it,
 *   which TinyOpenDoc lists as what it cannot do; the text flows over
 *   pages and columns, so a document can be longer than a screen; and
 *   the master gives every page one layout, changed in one place;
 *
 *   more restricted -- there is one container, the text, and
 *   everything else hangs from it: no row of two parts side by side,
 *   no part holding a text; a frame is the column's width, and a part
 *   with a size of its own (the sheet, the picture) is *scaled* to it,
 *   as FrameMaker scaled an imported graphic -- try two columns, and
 *   the sheet shrinks into its column, its text with it (TinyOpenDoc
 *   can scale a part too, or let it insist on its size); and it is
 *   an application with guests (OLE's shape: FrameMaker owns the
 *   document) rather than OpenDoc's document with no owner. Where
 *   TinyOpenDoc is a tree of parts laid out by position, this is a
 *   stream laid out by order -- the web's two ways again, a grid of
 *   boxes and a flow of text.
 *
 * Its descendants in spirit, to be checked like the rest: ClarisWorks
 * (1991), where any document held frames of the others, live; Gobe
 * Productive on BeOS (around 2000), by the same people; and Apple's
 * Pages (2005), AppleWorks' successor, with its text flowing round
 * frames.
 *
 * What it uses: appkits/richtext (Rich, Page, Flow), appkits/embed
 * (Component) and the parts of apps/ (Part_sheet, Part_picture,
 * Part_drawing), Stroke_text, appkits/document's Undo, and the
 * playground's menus.
 *
 * What it deliberately does not do: FrameMaker's named paragraph
 * formats (its "catalog": change Heading once, and every heading
 * changes); cross-references and automatic numbering; frames of other
 * widths, placed anywhere, or with the text running round them;
 * selections in the text, and looks (TinyWord has them); several
 * master pages; tables; books of many files. It saves (File,
 * File_menu): the text with its looks, the master, and each
 * frame as its place, its kind and its saved text.
 *
 * Exercises: the paragraph catalog -- a style per paragraph, by name,
 * as TinyPowerPoint's master is a style per slide; a frame anchored
 * "at the top of the column" rather than below its line; widows and
 * orphans (a paragraph's last line alone at the top of a column --
 * move one more line with it); a table of contents made from the
 * headings, with their page numbers, which is Flow run twice.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a part tied to a place in the text *)
type anchored = { at : int; part : Component.part }
type master = One_column | Two_columns

(* the document: one text, the frames anchored in it (in the order of
   their places), and the master page *)
type doc = { text : Rich.t; frames : anchored list; master : master }

type model = {
  history : doc Undo.t;
  (* the document while a part is being edited in place, recorded as
     one edit when it is put down *)
  editing : doc option;
  selected : int option;
  waking : bool;
  (* whether the last thing done was typing, so that the next letter
     joins the same edit *)
  typing : bool;
  said : string;
  (* the document's name, and the File menu's dialog *)
  file : File_menu.t;
  was : string list;
  was_down : bool;
}

let doc m = match m.editing with Some d -> d | None -> Undo.now m.history

(*****************************************************************************)
(* The master page, and the layout it makes *)
(*****************************************************************************)

let page_w = 430.
let page_h = 640.
let page_top = 315.
let side = 34.
let body_top = 52.
let column_h = 536.
let column_gap = 18.
let body_w = page_w -. (2. *. side)
let per_page d = match d.master with One_column -> 1 | Two_columns -> 2
let column_w d = match d.master with One_column -> body_w | Two_columns -> (body_w -. column_gap) /. 2.

(* a frame's room: its part at the column's width, and some air *)
let frame_air = 12.
(* a frame is its column's width, and a part with a size of its own is
   scaled to it, up or down, as FrameMaker scaled an imported graphic
   to its anchored frame (Component.draw_in) *)
let frame_h d (a : anchored) = Component.fitted_height ~scaled:true a.part (column_w d) +. frame_air

let layout d =
  let page = Page.layout ~metrics:Stroke_text.metrics ~width:(column_w d) d.text in
  (page, Flow.flow ~column_height:column_h ~anchors:(List.map (fun a -> (a.at, frame_h d a)) d.frames) page)

let pages d (f : Flow.t) = max 1 ((f.columns + per_page d - 1) / per_page d)

(* two pages side by side, the spread that shows page [p] *)
let page_left p = if p mod 2 = 0 then -440. else 10.

(* where a column's top-left corner is on the screen, and whether its
   page is in the spread shown *)
let column_origin d c = (page_left (c / per_page d) +. side +. (float_of_int (c mod per_page d) *. (column_w d +. column_gap)), page_top -. body_top)
let shown ~spread d c = c / per_page d / 2 = spread

(* the box of an anchored frame's part, on the screen *)
let frame_box d (p : Flow.placed_frame) : Widget.box =
  let x0, top = column_origin d p.in_column in
  let h = p.height -. frame_air in
  { Widget.x = x0 +. (column_w d /. 2.); y = top -. p.at -. (frame_air /. 2.) -. (h /. 2.); w = column_w d; h }

(* the caret: its column, and where it is on the screen *)
let caret_place d page (f : Flow.t) =
  let x, baseline, h = Page.caret_at page (Rich.caret d.text) in
  match List.find_opt (fun (p : Flow.placed_line) -> p.line.baseline = baseline) f.lines with
  | Some p ->
      let x0, top = column_origin d p.column in
      Some (p.column, (x0 +. x, top -. p.top -. (baseline -. p.line.top)), h)
  | None -> None

(* the spread to show: the selected frame's, or the caret's *)
let spread_of m =
  let d = doc m in
  let page, f = layout d in
  let column =
    match m.selected with
    | Some i -> ( match List.find_opt (fun (p : Flow.placed_frame) -> p.frame = i) f.frames with Some p -> p.in_column | None -> 0)
    | None -> ( match caret_place d page f with Some (c, _, _) -> c | None -> 0)
  in
  column / per_page d / 2

(*****************************************************************************)
(* The document it opens on *)
(*****************************************************************************)

let body =
  "FrameMaker, 1986\n\
   A manual of a thousand pages is not laid out by hand. In FrameMaker nothing is placed: the text flows through \
   the columns of every page, from one to the next, and the pages come from a master page, with its columns, its \
   header and its footer.\n\n\
   A sheet, a picture or a drawing is anchored in the text, in a frame set just below the line it is tied to:\n\n\
   Type above it, and it moves down with the text, to the next column, to the next page. It is in the flow, not \
   on the page.\n\n\
   Try the Master menu: two columns, and the whole document lays itself out again, the frames with it. Click in \
   the text to type; click a frame to select it, and again to edit its part where it is, as in TinyOpenDoc.\n\n\
   Against TinyOpenDoc, this is more powerful and more restricted at once. More powerful, because the parts are in \
   the text and follow it, because the text runs over as many pages as it needs, and because every page is made \
   from one master. More restricted, because there is only one container, the text, and everything hangs from \
   it: no row of two parts side by side, no part holding a text, and a frame as wide as its column. A document \
   laid out by position against a document laid out by order.\n\n\
   Here is a drawing, anchored like the sheet:\n\n\
   And the text goes on after it, onto the next page if it must, with the page numbers in the footer following."

let figures =
  List.fold_left
    (fun s (cell, v) -> Sheet.set cell v s)
    Sheet.empty
    [ ((0, 0), "Chapters"); ((1, 0), "12"); ((0, 1), "Pages each"); ((1, 1), "80"); ((0, 2), "Pages"); ((1, 2), "=B1*B2") ]

let diagram =
  let add f d = fst (Drawing.add f d) in
  let white = { Figure.fill = Some 1.; pen = 2. } in
  Drawing.empty
  |> add (Figure.Rect (Figure.box (20., 120.) (120., 170.), white))
  |> add (Figure.Rect (Figure.box (180., 120.) (280., 170.), white))
  |> add (Figure.Line ((120., 145.), (180., 145.), { white with fill = None }))
  |> add (Figure.Oval (Figure.box (100., 20.) (200., 90.), { Figure.fill = Some 0.7; pen = 2. }))
  |> add (Figure.Line ((150., 90.), (70., 120.), { white with fill = None }))
  |> add (Figure.Line ((150., 90.), (230., 120.), { white with fill = None }))

let index_of s word =
  let n = String.length word in
  let rec go i = if String.sub s i n = word then i else go (i + 1) in
  go 0

let initial =
  let r = Rich.of_string body in
  let title = "FrameMaker, 1986" in
  let r = Rich.restyle (fun s -> { (Style.toggle_bold s) with size = 24. }) (Rich.select ~anchor:0 ~caret:(String.length title) r) in
  let after s = index_of body s + String.length s in
  {
    history =
      Undo.start
        {
          text = Rich.at 0 r;
          frames =
            [
              { at = after "tied to:"; part = Part_sheet.make figures };
              { at = after "like the sheet:"; part = Part_drawing.make diagram };
            ];
          master = One_column;
        };
    editing = None;
    selected = None;
    waking = false;
    typing = false;
    said = "";
    file = File_menu.start;
    was = [];
    was_down = false;
  }

(*****************************************************************************)
(* Editing *)
(*****************************************************************************)

let record ~name d m = { m with history = Undo.record ~name d m.history; typing = false }

(* the end of an editing session in place: one edit, if it made one *)
let put_down m =
  match m.editing with
  | None -> m
  | Some d ->
      let before = Undo.now m.history in
      let saved (d : doc) = List.map (fun a -> (a.at, a.part.save ())) d.frames in
      let m = { m with editing = None } in
      if saved d = saved before then m else record ~name:"Edit Frame" d m

(* the text changed at the caret, [n] bytes in or out: the frames
   after it move with it, as a marker in FrameMaker's text does *)
let retext f n d =
  let c = Rich.caret d.text in
  let moved at =
    if n > 0 then if at >= c then at + n else at
    (* deleting [c + n, c): what was after it comes back by n, what was
       inside it goes to where it started *)
    else if at >= c then at + n
    else if at > c + n then c + n
    else at
  in
  { d with text = f d.text; frames = List.map (fun a -> { a with at = moved a.at }) d.frames }

(* a run of typing is one edit: the first keystroke records it, the
   next ones amend it *)
let typed d m = if m.typing then { m with history = Undo.amend d m.history } else { (record ~name:"Typing" d m) with typing = true }

let insert_frame name part m =
  let m = put_down m in
  let d = doc m in
  let at = Rich.caret d.text in
  let frames = List.sort (fun a b -> compare a.at b.at) ({ at; part } :: d.frames) in
  let index = List.length (List.filter (fun a -> a.at <= at) frames) - 1 in
  { (record ~name d m) with selected = Some index }

let menus =
  [
    File_menu.items;
    [ "Edit"; "Undo"; "Redo"; "Delete Frame" ];
    [ "Insert"; "Sheet"; "Picture"; "Drawing" ];
    [ "Master"; "One Column"; "Two Columns" ];
  ]

let menu_box i : Widget.box = { Widget.x = -410. +. (float_of_int i *. 100.); y = 470.; w = 95.; h = 30. }

let command c m =
  match c with
  | "Undo" -> let m = put_down m in { m with history = Undo.undo m.history; selected = None; typing = false }
  | "Redo" -> let m = put_down m in { m with history = Undo.redo m.history; selected = None; typing = false }
  | "Delete Frame" -> (
      match m.selected with
      | Some i ->
          let m = put_down m in
          let d = doc m in
          { (record ~name:"Delete Frame" { d with frames = List.filteri (fun j _ -> j <> i) d.frames } m) with selected = None }
      | None -> m)
  | "Sheet" -> insert_frame "Anchor Sheet" (Part_sheet.make Sheet.empty) m
  | "Picture" -> insert_frame "Anchor Picture" (Part_picture.make (Bitmap.create ~width:150 ~height:72)) m
  | "Drawing" -> insert_frame "Anchor Drawing" (Part_drawing.make Drawing.empty) m
  | "One Column" -> let m = put_down m in record ~name:"One Column" { (doc m) with master = One_column } m
  | "Two Columns" -> let m = put_down m in record ~name:"Two Columns" { (doc m) with master = Two_columns } m
  | _ -> m

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* a click in the text: the place in it nearest the point, in a column
   of the spread shown *)
let place_at ~spread d page (f : Flow.t) (mx, my) =
  let hit (p : Flow.placed_line) =
    let x0, top = column_origin d p.column in
    shown ~spread d p.column && mx >= x0 -. 4. && mx <= x0 +. column_w d +. 4. && my <= top -. p.top && my > top -. p.top -. p.line.height
  in
  match List.find_opt hit f.lines with
  | Some p ->
      let x0, _ = column_origin d p.column in
      Some (Page.offset_at page (mx -. x0, p.line.top +. (p.line.height /. 2.)))
  | None -> None

let keyboard computer m =
  let k = computer.keyboard in
  let now = Set_.elements k.keys in
  let pressed key = List.mem key now && not (List.mem key m.was) in
  let d = doc m in
  if List.mem "Control" now then
    if pressed "z" then command "Undo" m else if pressed "y" then command "Redo" m else m
  else
    match m.selected with
    | Some _ -> if pressed "Backspace" || pressed "Delete" then command "Delete Frame" m else m
    | None ->
        let text = Rich.to_string d.text in
        let c = Rich.caret d.text in
        (* the caret moving is not an edit *)
        let move to_ = { m with history = Undo.amend { d with text = Rich.at to_ d.text } m.history; typing = false } in
        if k.typed <> "" then typed (retext (Rich.insert k.typed) (String.length k.typed) d) m
        else if pressed "Enter" then typed (retext (Rich.insert "\n") 1 d) m
        else if pressed "Backspace" && c > 0 then typed (retext Rich.delete_backward (Text.prev_char text c - c) d) m
        else if pressed "ArrowLeft" then move (Text.prev_char text c)
        else if pressed "ArrowRight" then move (Text.next_char text c)
        else m

(* A document is saved as data: its text with its looks, the master,
   and each frame as its place in the text, its kind and what it saves
   -- never the part itself, which is functions (Saved.mli) *)
type saved = { saved_text : Rich.t; saved_frames : (int * string * string) list; saved_master : master }

let kind = { File_menu.magic = "TinyFrameMaker 1"; extension = ".frame" }

let registry : Component.registry =
  [ (Part_sheet.kind, Part_sheet.load); (Part_picture.kind, Part_picture.load); (Part_drawing.kind, Part_drawing.load); (Part_text.kind, Part_text.load) ]

let reopened (r : saved File_menu.result) model =
  match r with
  | File_menu.Nothing -> model
  | File_menu.New -> { initial with history = Undo.start { text = Rich.of_string ""; frames = []; master = One_column }; file = model.file }
  | File_menu.Opened sv ->
      let frames = List.map (fun (at, kind, text) -> { at; part = Component.load registry ~kind text }) sv.saved_frames in
      { initial with history = Undo.start { text = Rich.at 0 sv.saved_text; frames; master = sv.saved_master }; file = model.file }

let update caps computer model =
  let m = computer.mouse in
  let now = Set_.elements computer.keyboard.keys in
  let current () =
    let d = doc (put_down model) in
    { saved_text = d.text; saved_frames = List.map (fun a -> (a.at, a.part.Component.kind, a.part.save ())) d.frames; saved_master = d.master }
  in
  if File_menu.busy model.file then
    let file, r = File_menu.dialog caps kind computer ~current model.file in
    reopened r { model with file; was_down = m.mdown; was = now }
  else
  let pressed key = List.mem key now && not (List.mem key model.was) in
  let model =
    List.fold_left
      (fun model (i, items) ->
        if i = 0 then
          let file, r = File_menu.menu_in caps kind computer (menu_box i) ~current model.file in
          reopened r { model with file }
        else
        match List.nth_opt items (Gui.menu_in computer (menu_box i) items 0) with
        | Some c when c <> List.hd items -> command c model
        | _ -> model)
      model
      (List.mapi (fun i items -> (i, items)) menus)
  in
  (* the active part's menu, in the bar *)
  let model =
    match (model.editing, model.selected) with
    | Some d, Some i -> (
        match List.nth_opt d.frames i with
        | Some a when a.part.menu <> [] ->
            let chosen = Gui.menu_in computer (menu_box 4) a.part.menu 0 in
            if chosen > 0 then
              let part = a.part.command (List.nth a.part.menu chosen) in
              { model with editing = Some { d with frames = List.mapi (fun j a -> if j = i then { a with part } else a) d.frames } }
            else model
        | _ -> model)
    | _ -> model
  in
  let model =
    if Gui.modal () then model
    else
      let d = doc model in
      let page, f = layout d in
      let spread = spread_of model in
      let frame_at =
        List.find_opt (fun (p : Flow.placed_frame) -> shown ~spread d p.in_column && Widget.contains (frame_box d p) m.mx m.my) f.frames
      in
      let model =
        if m.mdown && not model.was_down then
          match frame_at with
          | Some p when model.editing <> None && model.selected = Some p.frame -> model
          | Some p when model.selected = Some p.frame -> { model with editing = Some d; waking = true; typing = false }
          | Some p -> { (put_down model) with selected = Some p.frame; typing = false }
          | None -> (
              let model = { (put_down model) with selected = None } in
              let d = doc model in
              match place_at ~spread d page f (m.mx, m.my) with
              | Some o -> { model with history = Undo.amend { d with text = Rich.at o d.text } model.history; typing = false }
              | None -> model)
        else model
      in
      let model = if m.mdown then model else { model with waking = false } in
      (* the active part gets the mouse and the keys, in its frame *)
      match (model.editing, model.selected) with
      | Some _, _ when model.waking -> model
      | Some d, Some i -> (
          match List.find_opt (fun (p : Flow.placed_frame) -> p.frame = i) f.frames with
          | Some p ->
              let b = frame_box d p in
              let fed a = { a with part = Component.input_in ~scaled:true a.part computer b } in
              { model with editing = Some { d with frames = List.mapi (fun j a -> if j = i then fed a else a) d.frames } }
          | None -> model)
      | _ -> model
  in
  let model = if model.editing <> None then if pressed "Escape" then put_down model else model else keyboard computer model in
  { model with was = now; was_down = m.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view _computer model =
  let d = doc model in
  let page, f = layout d in
  let spread = spread_of model in
  let n = pages d f in
  let ink = rgb 20 20 20 in
  let pages_shown =
    List.concat_map
      (fun p ->
        if p >= n then []
        else
          let l = page_left p in
          let cx = l +. (page_w /. 2.) in
          [
            rectangle (rgb 110 110 110) page_w page_h |> move (cx +. 4.) (page_top -. (page_h /. 2.) -. 4.);
            rectangle white page_w page_h |> move cx (page_top -. (page_h /. 2.));
            (* the master's header and footer *)
            words (rgb 130 130 130) "TinyFrameMaker" |> move (l +. side +. 50.) (page_top -. 22.);
            rectangle (rgb 200 200 200) body_w 1. |> move cx (page_top -. 36.);
            words (rgb 130 130 130) (Printf.sprintf "page %d of %d" (p + 1) n) |> move cx (page_top -. page_h +. 22.);
          ])
      [ 2 * spread; (2 * spread) + 1 ]
  in
  let text =
    List.concat_map
      (fun (p : Flow.placed_line) ->
        if not (shown ~spread d p.column) then []
        else
          let x0, top = column_origin d p.column in
          List.concat_map
            (fun (g : Page.glyph) ->
              if g.text = "\n" || g.text = " " then []
              else Stroke_text.glyph ink g.style g.text ~x:(x0 +. g.x) ~baseline:(top -. p.top -. (g.baseline -. p.line.top)))
            p.line.cells)
      f.lines
  in
  let frames =
    List.concat_map
      (fun (p : Flow.placed_frame) ->
        if not (shown ~spread d p.in_column) then []
        else
          let b = frame_box d p in
          let a = List.nth d.frames p.frame in
          let on = model.selected = Some p.frame in
          let active = on && model.editing <> None in
          let border =
            if active then Gui.shapes (Widget.frame (rgb 90 90 90) 4. { b with w = b.w +. 10.; h = b.h +. 10. })
            else if on then Gui.shapes (Widget.frame (rgb 40 90 200) 2. { b with w = b.w +. 6.; h = b.h +. 6. })
            else Gui.shapes (Widget.frame (rgb 210 210 210) 1. { b with w = b.w +. 4.; h = b.h +. 4. })
          in
          border @ Component.draw_in ~scaled:true a.part b ~active)
      f.frames
  in
  (* the anchors, as FrameMaker showed them among the text symbols:
     a small mark where each frame is tied *)
  let anchors =
    List.filter_map
      (fun a ->
        let x, baseline, _ = Page.caret_at page a.at in
        match List.find_opt (fun (p : Flow.placed_line) -> p.line.baseline = baseline) f.lines with
        | Some p when shown ~spread d p.column ->
            let x0, top = column_origin d p.column in
            Some (rectangle (rgb 60 110 220) 6. 3. |> move (x0 +. x +. 3.) (top -. p.top -. (baseline -. p.line.top) -. 3.))
        | _ -> None)
      d.frames
  in
  let caret =
    match (model.selected, caret_place d page f) with
    | None, Some (c, (x, y), h) when shown ~spread d c -> [ rectangle ink 2. (h *. 0.8) |> move x (y +. (h *. 0.25)) ]
    | _ -> []
  in
  let status =
    Printf.sprintf "%s     %d pages, %s%s"
      (if File_menu.said model.file <> "" then File_menu.said model.file else File_menu.title model.file)
      n
      (match d.master with One_column -> "one column" | Two_columns -> "two columns")
      (match (model.editing, Undo.undo_name model.history) with
      | Some _, _ -> "     editing the frame in place -- Escape to put it down"
      | None, Some u -> "     Undo " ^ u
      | None, None -> "")
  in
  [ rectangle (rgb 160 160 165) 1000. 1000.; rectangle (Gui.theme ()).face 1000. 40. |> move 0. 470. ]
  @ pages_shown @ text @ frames @ anchors @ caret
  @ [ words (rgb 40 40 40) status |> move 0. (-455.) ]
  @ File_menu.view model.file
  @ Gui.draw ()

let app caps = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> File_menu.caps)))
