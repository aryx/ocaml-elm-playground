(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyOffice: the office suite as people know it today -- Microsoft
 * 365, Apple's iWork, LibreOffice -- rather than any one program of its
 * history.
 *
 * You start from a choice of what to make: a document, a spreadsheet, a
 * presentation, a picture, a drawing. Each opens in its own editor, and
 * every one of those editors can hold things made by the others: a
 * sheet in a letter, a drawing over a sheet, a picture on a slide. They
 * float where you put them; you drag them, and drag a corner to resize
 * them; in a text, the text runs round them as you do. Click one again
 * and you edit it where it is, the menu bar turning into its editor's
 * while the host's File menu stays -- and Escape brings the host back.
 *
 * **Why a new program, and not the old ones grown.** The Tiny programs
 * before this one are each a period piece, true to the program they
 * are named after, and what they cannot do is what their originals
 * could not do -- which is the point of them, and why this one is
 * separate rather than a change to them:
 *
 * - TinyWord, TinyExcel, TinyPowerPoint, TinyMacPaint and TinyMacDraw
 *   are each one kind of document, and none of them can hold another's
 *   (only TinyPowerPoint holds parts, one per slide, in a fixed place).
 *   There is no suite: five programs that share nothing but code.
 *
 * - TinyOpenDoc (OpenDoc, 1994-97) is a document of parts with no
 *   application at all -- and so no kind of document either: nothing is
 *   "a spreadsheet with a drawing in it". Its parts are laid out *by
 *   position*, a tree of rows and columns: a part cannot float over
 *   another, cannot sit anywhere but in its row, and a text cannot run
 *   round it. Its sizes are negotiated or scaled, but only within the
 *   tree's own slots.
 *
 * - TinyFrameMaker (FrameMaker, around 1986) lays its frames out *by
 *   order*: each tied to a place in one text, set below its line, as
 *   wide as its column. A frame cannot sit beside a paragraph with the
 *   text running round it, cannot be dragged, only anchored; and the
 *   text is the only container -- a sheet cannot hold anything.
 *
 * What TinyOffice has that none of them has, and what a modern suite
 * is made of:
 *
 * - **a start screen**: the kind first, then the editor for it;
 * - **every kind a host**: a document, a sheet, a presentation, a
 *   picture and a drawing can each hold objects of the others (OLE's
 *   shape, 1993: applications that embed each other, rather than
 *   OpenDoc's parts without applications);
 * - **free-floating objects**: placed anywhere on the page, over what is
 *   there, dragged, resized by their corners, brought to the front or
 *   sent to the back -- a part with a size of its own scaled to its
 *   frame (or kept at its natural size), a text box reflowing inside it;
 * - **text that wraps round them**, as Publisher (1991) and Pages
 *   (2005) do: the lines beside an object are shortened to the room it
 *   leaves (appkits/richtext/Page's ~around), live, as it is dragged;
 * - **in-place editing with menu merging**: the object's editor takes
 *   the menu bar, the host keeping only File -- OLE 2's rule.
 *
 * What it uses: appkits/embed (Component, the protocol, and its
 * draw_in/input_in scaling), the parts of apps/ (Part_text, Part_sheet,
 * Part_picture, Part_drawing) as both the main content of the sheet,
 * picture and drawing documents and the objects floating on any of
 * them, appkits/richtext (Rich, Page and its text round boxes) for the
 * document and the slides, apps/Stroke_text, appkits/document's Undo,
 * and the playground's menus.
 *
 * What it deliberately does not do: saving (plan_io.md, whose Saved
 * module the parts' saves are ready for); objects anchored to a place
 * in the text so that they move with it (TinyFrameMaker has that); a
 * text running on both sides of an object (it takes the wider side);
 * rotation; the presentation's show and its master (TinyPowerPoint has
 * them); several pages of a document; collaboration, and the cloud.
 *
 * Exercises: saving every kind through Saved, the objects with their
 * boxes; wrap "on both sides", a line's two stretches filled in turn;
 * "move with text", an object tied to a paragraph (TinyFrameMaker's
 * anchor, kept free to be dragged); snapping an object to the others'
 * edges while it is dragged; a chart part, drawn from a sheet object's
 * cells, refreshed when they change -- OLE's *linking* beside its
 * embedding.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type kind = Document | Spreadsheet | Presentation | Picture | Drawing_doc

let kinds = [ Document; Spreadsheet; Presentation; Picture; Drawing_doc ]
let name = function Document -> "Document" | Spreadsheet -> "Spreadsheet" | Presentation -> "Presentation" | Picture -> "Picture" | Drawing_doc -> "Drawing"

(* an object floating on the page: its part, the slide it is on, its
   top-left corner and size (the page's coordinates, y down), and
   whether a part with a size of its own is scaled to it *)
type obj = { part : Component.part; slide : int; x : float; y : float; w : float; h : float; scaled : bool }

(* what the document is before anything floats on it: a text per slide
   (a document is one slide), or a part of its own kind *)
type body = Texts of Rich.t list | Main of Component.part

type doc = { kind : kind; body : body; objects : obj list; slide : int }

type drag =
  | Moving of float * float (* where on the object the mouse holds it *)
  | Resizing of int (* by that corner *)

type model = {
  (* the start screen, before a kind is chosen *)
  start : bool;
  history : doc Undo.t;
  (* the document while an object is dragged, one edit on release *)
  live : doc option;
  (* the document while an object is edited in place, one edit when it
     is put down *)
  editing : doc option;
  selected : int option;
  drag : drag option;
  (* where the press began, and whether it was on the object already
     selected: a click there, without a drag, edits it in place *)
  pressed_at : float * float;
  again : bool;
  (* a run of typing, or of edits to the main part, is one edit *)
  run : bool;
  was : string list;
  was_down : bool;
}

let doc m = match (m.editing, m.live) with Some d, _ | None, Some d -> d | None, None -> Undo.now m.history

(*****************************************************************************)
(* The page *)
(*****************************************************************************)

let page_size = function Presentation -> (800., 560.) | Document -> (620., 820.) | _ -> (820., 820.)
let margin = 40.

(* the page's top-left corner on the screen *)
let origin k =
  let w, h = page_size k in
  (-.w /. 2., if h > 700. then 430. else 400.)

let to_page k (sx, sy) =
  let l, t = origin k in
  (sx -. l, t -. sy)

let box_on_screen k x y w h : Widget.box =
  let l, t = origin k in
  { Widget.x = l +. x +. (w /. 2.); y = t -. y -. (h /. 2.); w; h }

let obj_box k (o : obj) = box_on_screen k o.x o.y o.w o.h
let on_slide (d : doc) (o : obj) = o.slide = d.slide
let text_width k = fst (page_size k) -. (2. *. margin)

(* the main part of a sheet, picture or drawing: the page's width less
   a margin, as tall as it is at that width *)
let main_box k (p : Component.part) =
  let w = fst (page_size k) -. 40. in
  box_on_screen k 20. 20. w (p.height w)

(* the text of the slide shown, laid out round the objects on it *)
let wrap_room = 12.

let layout d =
  match d.body with
  | Texts ts ->
      let r = List.nth ts d.slide in
      let around =
        List.filter_map
          (fun o ->
            if on_slide d o then
              Some (o.x -. margin -. wrap_room, o.y -. margin -. wrap_room, o.x +. o.w -. margin +. wrap_room, o.y +. o.h -. margin +. wrap_room)
            else None)
          d.objects
      in
      Some (r, Page.layout ~around ~metrics:Stroke_text.metrics ~width:(text_width d.kind) r)
  | Main _ -> None

(* the index of the object on top at a point of the screen *)
let object_at d (mx, my) =
  let hits = List.filter (fun (_, o) -> on_slide d o && Widget.contains (obj_box d.kind o) mx my) (List.mapi (fun i o -> (i, o)) d.objects) in
  match List.rev hits with (i, _) :: _ -> Some i | [] -> None

(* the four corners of an object on the screen: top-left, top-right,
   bottom-right, bottom-left *)
let corners (b : Widget.box) = [ (Widget.left b, Widget.top b); (Widget.right b, Widget.top b); (Widget.right b, Widget.bottom b); (Widget.left b, Widget.bottom b) ]

let corner_at d i (mx, my) =
  match List.nth_opt d.objects i with
  | Some o ->
      let cs = List.mapi (fun c p -> (c, p)) (corners (obj_box d.kind o)) in
      Option.map fst (List.find_opt (fun (_, (x, y)) -> Float.abs (x -. mx) <= 8. && Float.abs (y -. my) <= 8.) cs)
  | None -> None

(*****************************************************************************)
(* New documents, one of each kind *)
(*****************************************************************************)

let styled ?(bold = false) size s = Rich.of_string ~style:{ Style.plain with size; bold } s

let with_title title body =
  let r = Rich.of_string (title ^ "\n" ^ body) in
  Rich.at 0 (Rich.restyle (fun s -> { (Style.toggle_bold s) with size = 26. }) (Rich.select ~anchor:0 ~caret:(String.length title) r))

let budget =
  List.fold_left
    (fun s (cell, v) -> Sheet.set cell v s)
    Sheet.empty
    [ ((0, 0), "Paper"); ((1, 0), "12"); ((0, 1), "Ink"); ((1, 1), "30"); ((0, 2), "Stamps"); ((1, 2), "8"); ((0, 3), "Total"); ((1, 3), "=SUM(B1:B3)") ]

let shapes =
  let add f d = fst (Drawing.add f d) in
  let st = { Figure.fill = Some 1.; pen = 2. } in
  Drawing.empty
  |> add (Figure.Rect (Figure.box (30., 110.) (130., 170.), st))
  |> add (Figure.Rect (Figure.box (170., 110.) (270., 170.), st))
  |> add (Figure.Line ((130., 140.), (170., 140.), { st with fill = None }))
  |> add (Figure.Oval (Figure.box (100., 20.) (200., 80.), { Figure.fill = Some 0.7; pen = 2. }))

let obj ?(slide = 0) part x y w h = { part; slide; x; y; w; h; scaled = part.Component.natural <> None }

let fresh kind =
  match kind with
  | Document ->
      {
        kind;
        slide = 0;
        body =
          Texts
            [
              with_title "TinyOffice"
                "A document, the first of the five kinds. The sheet on the right floats on the page: drag it, and \
                 this text runs round it as you do; drag one of its corners, and it is scaled to its new size. \
                 Click it again to edit it where it is -- the menu bar becomes the sheet's, the File menu stays, \
                 and Escape brings the document back.\n\n\
                 Insert puts a text box, a sheet, a picture or a drawing on the page, and every kind of document \
                 can hold every other: a drawing over a spreadsheet, a picture on a slide. Arrange brings an \
                 object to the front or sends it back, and scales it or gives it its natural size.\n\n\
                 File > New goes back to the choice of the five kinds.";
            ];
        objects = [ obj (Part_sheet.make budget) 360. 150. 220. 106. ];
      }
  | Presentation ->
      {
        kind;
        slide = 0;
        body =
          Texts
            [
              with_title "A presentation" "\nwith a drawing floating on its first slide, and a sheet on its second -- Slide > Next.";
              with_title "The figures" "\nThe same sheet as in a document, on a slide.";
            ];
        objects = [ obj (Part_drawing.make shapes) 440. 200. 300. 200.; obj ~slide:1 (Part_sheet.make budget) 400. 190. 330. 159. ];
      }
  | Spreadsheet ->
      let sheet =
        List.fold_left
          (fun s (cell, v) -> Sheet.set cell v s)
          Sheet.empty
          [ ((0, 0), "Month"); ((1, 0), "Sales"); ((0, 1), "Jan"); ((1, 1), "120"); ((0, 2), "Feb"); ((1, 2), "150"); ((0, 3), "Mar"); ((1, 3), "90"); ((0, 5), "Total"); ((1, 5), "=SUM(B2:B4)") ]
      in
      { kind; slide = 0; body = Main (Part_sheet.make ~cols:7 ~rows:18 sheet); objects = [ obj (Part_drawing.make shapes) 430. 60. 330. 220. ] }
  | Picture ->
      let bits =
        Bitmap.change (Bitmap.create ~width:190 ~height:150) (fun b ->
            Paint.fill_oval b Pattern.grey (120, 20) (170, 70);
            Paint.frame_oval b Pattern.solid (120, 20) (170, 70);
            Paint.stroke b ~brush:Paint.pencil Pattern.solid (0, 120) (189, 120))
      in
      { kind; slide = 0; body = Main (Part_picture.make bits); objects = [ obj (Part_text.make (styled 18. "A caption, in a text box floating on the picture.")) 40. 340. 300. 60. ] }
  | Drawing_doc ->
      { kind; slide = 0; body = Main (Part_drawing.make ~max_height:560. shapes); objects = [ obj (Part_sheet.make budget) 440. 380. 300. 144. ] }

let opening = fresh Document

let initial =
  { start = true; history = Undo.start opening; live = None; editing = None; selected = None; drag = None; pressed_at = (0., 0.); again = false; run = false; was = []; was_down = false }

(*****************************************************************************)
(* Editing *)
(*****************************************************************************)

let record ~name d m = { m with history = Undo.record ~name d m.history; run = false }
let set_obj i f d = { d with objects = List.mapi (fun j o -> if j = i then f o else o) d.objects }

(* the end of an editing session in place: one edit, if it made one *)
let put_down m =
  match m.editing with
  | None -> m
  | Some d ->
      let before = Undo.now m.history in
      let saved (d : doc) = List.map (fun o -> o.part.save ()) d.objects in
      let m = { m with editing = None } in
      if saved d = saved before then m else record ~name:"Edit Object" d m

(* a new object, in the middle of the page, selected *)
let insert name part m =
  let m = put_down m in
  let d = doc m in
  let pw, ph = page_size d.kind in
  let w, h = match part.Component.natural with Some (w, h) -> (w, h) | None -> (300., 180.) in
  let o = obj ~slide:d.slide part ((pw -. w) /. 2.) ((ph -. h) /. 2.) w h in
  { (record ~name { d with objects = d.objects @ [ o ] } m) with selected = Some (List.length d.objects) }

let edit_text f m =
  let d = doc m in
  match d.body with
  | Texts ts -> { d with body = Texts (List.mapi (fun i r -> if i = d.slide then f r else r) ts) }
  | Main _ -> d

let a_run ?(name = "Typing") d m = if m.run then { m with history = Undo.amend d m.history } else { (record ~name d m) with run = true }

let host_menus d =
  [ [ "File"; "New" ]; [ "Edit"; "Undo"; "Redo"; "Delete" ]; [ "Insert"; "Text Box"; "Sheet"; "Picture"; "Drawing" ]; [ "Arrange"; "Scale to Fit"; "Natural Size"; "Bring to Front"; "Send to Back" ] ]
  @
  match (d.kind, d.body) with
  | Document, _ -> [ [ "Format"; "Bold"; "Italic"; "Bigger"; "Smaller" ] ]
  | Presentation, _ -> [ [ "Format"; "Bold"; "Italic"; "Bigger"; "Smaller" ]; [ "Slide"; "New Slide"; "Next"; "Previous" ] ]
  | _, Main p -> [ p.menu ]
  | _, Texts _ -> []

(* the menu bar: the host's -- or, while an object is edited in place,
   File and the object's own, OLE 2's menu merging *)
let menus m =
  let d = doc m in
  match (m.editing, m.selected) with
  | Some _, Some i -> ( match List.nth_opt d.objects i with Some o when o.part.menu <> [] -> [ [ "File"; "New" ]; o.part.menu ] | _ -> [ [ "File"; "New" ] ])
  | _ -> host_menus d

let menu_box i : Widget.box = { Widget.x = -410. +. (float_of_int i *. 102.); y = 472.; w = 98.; h = 30. }

let command ~menu c m =
  let d = doc m in
  let on_selected f = match m.selected with Some i -> f i | None -> m in
  match (menu, c, m.editing, m.selected) with
  | "File", "New", _, _ -> { initial with start = true }
  (* the object's own menu, while it is edited in place *)
  | _, c, Some e, Some i -> { m with editing = Some (set_obj i (fun o -> { o with part = o.part.command c }) e) }
  | _ -> (
  match (menu, c) with
  | _, "Undo" -> let m = put_down m in { m with history = Undo.undo m.history; selected = None; run = false }
  | _, "Redo" -> let m = put_down m in { m with history = Undo.redo m.history; selected = None; run = false }
  | _, "Delete" -> on_selected (fun i -> { (record ~name:"Delete" { d with objects = List.filteri (fun j _ -> j <> i) d.objects } m) with selected = None })
  | _, "Text Box" -> insert "Insert Text Box" (Part_text.make (styled 18. "A text box: type in it.")) m
  | _, "Sheet" -> insert "Insert Sheet" (Part_sheet.make budget) m
  | _, "Picture" -> insert "Insert Picture" (Part_picture.make (Bitmap.create ~width:120 ~height:80)) m
  | _, "Drawing" -> insert "Insert Drawing" (Part_drawing.make shapes) m
  | _, ("Scale to Fit" | "Natural Size") ->
      on_selected (fun i ->
          let scaled = c = "Scale to Fit" in
          record ~name:c
            (set_obj i
               (fun o ->
                 match (scaled, o.part.natural) with
                 (* its natural size: the frame made that size again *)
                 | false, Some (w, h) -> { o with scaled; w; h }
                 | _ -> { o with scaled })
               d)
            m)
  | _, "Bring to Front" ->
      on_selected (fun i ->
          let o = List.nth d.objects i in
          { (record ~name:c { d with objects = List.filteri (fun j _ -> j <> i) d.objects @ [ o ] } m) with selected = Some (List.length d.objects - 1) })
  | _, "Send to Back" ->
      on_selected (fun i ->
          let o = List.nth d.objects i in
          { (record ~name:c { d with objects = o :: List.filteri (fun j _ -> j <> i) d.objects } m) with selected = Some 0 })
  | "Format", ("Bold" | "Italic" | "Bigger" | "Smaller") ->
      (* the look of what is typed next, as TinyWord's with nothing
         selected *)
      let f =
        match c with
        | "Bold" -> Style.toggle_bold
        | "Italic" -> Style.toggle_italic
        | "Bigger" -> fun s -> { s with size = s.size *. 1.25 }
        | _ -> fun s -> { s with size = s.size /. 1.25 }
      in
      { (record ~name:c (edit_text (Rich.restyle f) m) m) with selected = None }
  | "Slide", "New Slide" -> (
      match d.body with
      | Texts ts ->
          let at = d.slide + 1 in
          let ts = List.concat (List.mapi (fun i r -> if i = d.slide then [ r; with_title "A new slide" "" ] else [ r ]) ts) in
          let objects = List.map (fun (o : obj) -> if o.slide >= at then { o with slide = o.slide + 1 } else o) d.objects in
          { (record ~name:c { d with body = Texts ts; slide = at; objects } m) with selected = None }
      | Main _ -> m)
  | "Slide", ("Next" | "Previous") -> (
      match d.body with
      | Texts ts ->
          let slide = max 0 (min (List.length ts - 1) (d.slide + if c = "Next" then 1 else -1)) in
          { m with history = Undo.amend { d with slide } m.history; selected = None }
      | Main _ -> m)
  | _, c -> (
      (* a command of the main part's own menu *)
      match d.body with
      | Main p when List.mem c p.menu -> record ~name:c { d with body = Main (p.command c) } m
      | _ -> m))

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let tile i : Widget.box = { Widget.x = -360. +. (float_of_int i *. 180.); y = 30.; w = 150.; h = 180. }

let text_keys computer m =
  let k = computer.keyboard in
  let now = Set_.elements k.keys in
  let pressed key = List.mem key now && not (List.mem key m.was) in
  let d = doc m in
  match d.body with
  | Main _ -> m
  | Texts ts ->
      let r = List.nth ts d.slide in
      let text = Rich.to_string r and c = Rich.caret r in
      let move to_ = { m with history = Undo.amend (edit_text (Rich.at to_) m) m.history; run = false } in
      if k.typed <> "" then a_run (edit_text (Rich.insert k.typed) m) m
      else if pressed "Enter" then a_run (edit_text (Rich.insert "\n") m) m
      else if pressed "Backspace" then a_run (edit_text Rich.delete_backward m) m
      else if pressed "ArrowLeft" then move (Text.prev_char text c)
      else if pressed "ArrowRight" then move (Text.next_char text c)
      else m

(* the mouse went down on the page, not on the object being edited *)
let press m (mx, my) =
  let d = doc m in
  let m = { m with pressed_at = (mx, my); again = false; run = false } in
  match Option.bind m.selected (fun i -> Option.map (fun c -> (i, c)) (corner_at d i (mx, my))) with
  | Some (_, c) when m.editing = None -> { m with drag = Some (Resizing c); live = Some d }
  | _ -> (
      match object_at d (mx, my) with
      | Some i ->
          let m = put_down m in
          let d = doc m in
          let o = List.nth d.objects i in
          let b = obj_box d.kind o in
          {
            m with
            again = m.selected = Some i;
            selected = Some i;
            drag = Some (Moving (mx -. Widget.left b, Widget.top b -. my));
            live = Some d;
          }
      | None -> (
          let m = { (put_down m) with selected = None } in
          let d = doc m in
          (* in a text, the caret goes where the click was *)
          match layout d with
          | Some (_, page) ->
              let px, py = to_page d.kind (mx, my) in
              let o = Page.offset_at page (px -. margin, py -. margin) in
              { m with history = Undo.amend (edit_text (Rich.at o) m) m.history }
          | None -> m))

let dragging m (mx, my) =
  let base = Undo.now m.history in
  match (m.drag, m.selected) with
  | Some (Moving (gx, gy)), Some i ->
      let x, y = to_page base.kind (mx -. gx, my +. gy) in
      { m with live = Some (set_obj i (fun o -> { o with x; y }) base) }
  | Some (Resizing c), Some i ->
      let o = List.nth base.objects i in
      let px, py = to_page base.kind (mx, my) in
      (* the opposite corner stays where it is *)
      let ax = if c = 0 || c = 3 then o.x +. o.w else o.x and ay = if c = 0 || c = 1 then o.y +. o.h else o.y in
      let x = Float.min ax px and y = Float.min ay py in
      let w = Float.max 30. (Float.abs (px -. ax)) and h = Float.max 30. (Float.abs (py -. ay)) in
      { m with live = Some (set_obj i (fun o -> { o with x; y; w; h }) base) }
  | _ -> m

let release m (mx, my) =
  let before = Undo.now m.history in
  let boxes d = List.map (fun o -> (o.x, o.y, o.w, o.h)) d.objects in
  let moved = Float.abs (mx -. fst m.pressed_at) +. Float.abs (my -. snd m.pressed_at) > 3. in
  let m =
    match (m.drag, m.live) with
    (* a click on the object already selected, with no drag: edit it
       where it is *)
    | Some (Moving _), _ when m.again && not moved -> { m with editing = Some before; live = None }
    | Some d, Some l when boxes l <> boxes before -> { (record ~name:(match d with Moving _ -> "Move" | Resizing _ -> "Resize") l m) with live = None }
    | _ -> { m with live = None }
  in
  { m with drag = None }

let update computer model =
  let mouse = computer.mouse in
  let now = Set_.elements computer.keyboard.keys in
  let pressed key = List.mem key now && not (List.mem key model.was) in
  let press_edge = mouse.mdown && not model.was_down in
  let model =
    if model.start then
      (* the start screen: the kinds, as tiles *)
      match List.find_opt (fun (i, _) -> press_edge && Widget.contains (tile i) mouse.mx mouse.my) (List.mapi (fun i k -> (i, k)) kinds) with
      | Some (_, k) -> { initial with start = false; history = Undo.start (fresh k) }
      | None -> model
    else
      let model =
        List.fold_left
          (fun model (i, items) ->
            match List.nth_opt items (Gui.menu_in computer (menu_box i) items 0) with
            | Some c when c <> List.hd items -> command ~menu:(List.hd items) c model
            | _ -> model)
          model
          (List.mapi (fun i items -> (i, items)) (menus model))
      in
      if Gui.modal () then model
      else
        let d = doc model in
        let p = (mouse.mx, mouse.my) in
        let in_active =
          match (model.editing, model.selected) with
          | Some _, Some i -> ( match List.nth_opt d.objects i with Some o -> Widget.contains (obj_box d.kind o) mouse.mx mouse.my | None -> false)
          | _ -> false
        in
        let model =
          match model.drag with
          | Some _ when mouse.mdown -> dragging model p
          | Some _ -> release model p
          | None when press_edge && not in_active -> press model p
          | None -> model
        in
        let d = doc model in
        (* who gets the mouse and the keys: the object edited in place,
           or else -- no object selected -- the document's own content *)
        match (model.editing, model.selected, d.body) with
        | Some d, Some i, _ -> (
            match List.nth_opt d.objects i with
            | Some o ->
                let part = Component.input_in ~scaled:o.scaled o.part computer (obj_box d.kind o) in
                let model = { model with editing = Some (set_obj i (fun o -> { o with part }) d) } in
                if pressed "Escape" then put_down model else model
            | None -> model)
        | None, Some _, _ ->
            if pressed "Backspace" || pressed "Delete" then command ~menu:"Edit" "Delete" model
            else if pressed "Escape" then { model with selected = None }
            else model
        | None, None, Main p when model.drag = None ->
            let p' = p.input computer (main_box d.kind p) in
            if p'.save () <> p.save () then a_run ~name:"Edit" { d with body = Main p' } model
            else { model with history = Undo.amend { d with body = Main p' } model.history }
        | None, None, Texts _ -> text_keys computer model
        | _ -> model
  in
  { model with was = now; was_down = mouse.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let kind_icon k (b : Widget.box) =
  let at dx dy s = s |> move (b.x +. dx) (b.y +. dy) in
  let ink = rgb 60 60 70 in
  match k with
  | Document -> List.init 6 (fun i -> at 0. (40. -. (float_of_int i *. 14.)) (rectangle ink (if i = 5 then 50. else 80.) 4.))
  | Spreadsheet ->
      List.init 5 (fun i -> at 0. (40. -. (float_of_int i *. 18.)) (rectangle ink 90. 2.))
      @ List.init 4 (fun i -> at (-45. +. (float_of_int i *. 30.)) 4. (rectangle ink 2. 74.))
  | Presentation -> [ at 0. 10. (rectangle ink 96. 66.); at 0. 10. (rectangle white 90. 60.); at 0. 30. (rectangle ink 60. 6.); at 0. 4. (rectangle ink 40. 4.) ]
  | Picture -> [ at 0. 10. (rectangle ink 90. 70.); at 0. 10. (rectangle white 84. 64.); at 20. 25. (circle (rgb 120 120 120) 10.); at 0. (-12.) (rectangle ink 84. 3.) ]
  | Drawing_doc -> [ at (-20.) 20. (rectangle ink 40. 30.); at 25. 0. (oval (rgb 150 150 150) 40. 30.); at 0. 10. (rectangle ink 30. 2. |> rotate 30.) ]

let start_view () =
  [ rectangle (rgb 235 236 240) 1000. 1000.; words (rgb 30 30 40) "TinyOffice" |> scale 2.5 |> move 0. 260.; words (rgb 100 100 110) "What would you like to make?" |> move 0. 190. ]
  @ List.concat
      (List.mapi
         (fun i k ->
           let b = tile i in
           [ rectangle (rgb 200 200 205) (b.w +. 4.) (b.h +. 4.) |> move (b.x +. 3.) (b.y -. 3.); rectangle white b.w b.h |> move b.x b.y ]
           @ kind_icon k b
           @ [ words (rgb 40 40 50) (name k) |> move b.x (b.y -. 70.) ])
         kinds)
  @ [ words (rgb 120 120 130) "each kind can hold the others: a sheet in a document, a drawing on a sheet, a picture on a slide" |> move 0. (-150.) ]

let view _computer model =
  if model.start then start_view ()
  else
    let d = doc model in
    let pw, ph = page_size d.kind in
    let l, t = origin d.kind in
    let ink = rgb 20 20 20 in
    let text =
      match layout d with
      | Some (r, page) ->
          let glyphs =
            List.concat_map
              (fun (g : Page.glyph) ->
                if g.text = "\n" || g.text = " " then []
                else Stroke_text.glyph ink g.style g.text ~x:(l +. margin +. g.x) ~baseline:(t -. margin -. g.baseline))
              (Page.glyphs page)
          in
          let caret =
            if model.selected <> None then []
            else
              let x, baseline, h = Page.caret_at page (Rich.caret r) in
              [ rectangle ink 2. (h *. 0.8) |> move (l +. margin +. x) (t -. margin -. baseline +. (h *. 0.25)) ]
          in
          glyphs @ caret
      | None -> []
    in
    let main = match d.body with Main p -> Component.draw_in ~scaled:false p (main_box d.kind p) ~active:(model.selected = None) | Texts _ -> [] in
    let objects =
      List.concat
        (List.mapi
           (fun i o ->
             if not (on_slide d o) then []
             else
               let b = obj_box d.kind o in
               let selected = model.selected = Some i in
               let active = selected && model.editing <> None in
               let frame =
                 if active then Gui.shapes (Widget.frame (rgb 90 90 90) 4. { b with w = b.w +. 10.; h = b.h +. 10. })
                 else if selected then
                   Gui.shapes (Widget.frame (rgb 40 90 200) 1. b) @ List.map (fun (x, y) -> rectangle (rgb 40 90 200) 9. 9. |> move x y) (corners b)
                 else []
               in
               (rectangle white b.w b.h |> move b.x b.y) :: (Component.draw_in ~scaled:o.scaled o.part b ~active @ frame))
           d.objects)
    in
    let slides = match d.body with Texts ts when d.kind = Presentation -> Printf.sprintf "     slide %d of %d" (d.slide + 1) (List.length ts) | _ -> "" in
    let status =
      Printf.sprintf "%s%s     %s" (name d.kind) slides
        (match (model.editing, model.selected, Undo.undo_name model.history) with
        | Some _, _, _ -> "editing the object in place -- Escape to go back to the " ^ String.lowercase_ascii (name d.kind)
        | None, Some _, _ -> "selected: drag it, drag a corner, or click it again to edit it"
        | None, None, Some u -> "Undo " ^ u
        | None, None, None -> "")
    in
    [
      rectangle (rgb 165 168 175) 1000. 1000.;
      rectangle (Gui.theme ()).face 1000. 40. |> move 0. 472.;
      rectangle (rgb 120 120 128) pw ph |> move (l +. (pw /. 2.) +. 5.) (t -. (ph /. 2.) -. 5.);
      rectangle white pw ph |> move (l +. (pw /. 2.)) (t -. (ph /. 2.));
    ]
    @ main @ text @ objects
    @ [ words (rgb 40 40 40) status |> move 0. (-455.) ]
    @ Gui.draw ()

let app = game view update initial
let main = Playground_platform.run_app app
