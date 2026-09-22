(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyMacDraw: a picture made of objects (MacDraw, Apple, 1984;
 * plan_gui_remaining.md).
 *
 * MacDraw shipped the same year as MacPaint and is its opposite. In
 * MacPaint a rectangle, once drawn, is only the dots it left; to move
 * it you select dots. In MacDraw it stays a rectangle: click it and it
 * shows its handles, drag it and it moves, drag a handle and it
 * resizes, fill it grey, send it behind the oval, group it with its
 * label and move the two as one. Paint against draw -- a bitmap against
 * a list of objects -- is still Photoshop against Illustrator, and the
 * two TinyMacs side by side are the clearest way to see it.
 *
 * What the objects cost, and teach (appkits/draw):
 *
 * - **the order is the depth**: the drawing is a list from back to
 *   front, drawn from its start and hit-tested from its end;
 * - **a hollow shape is only its outline**: a click in the middle of
 *   an unfilled rectangle goes through it, to what is behind -- try it
 *   on the rectangle in front of the grey oval;
 * - **resizing is a map of the points**, from the old bounds to the
 *   new, so a group resized scales everything in it;
 * - **everything is data**, so undo keeps old drawings, and whether a
 *   drag changed anything is a plain comparison -- no functions inside
 *   to make that impossible, as there were in TinyOpenDoc's parts.
 *
 * What it uses: appkits/draw (Figure, Drawing), appkits/document
 * (Undo), apps/Figure_shapes to draw the figures (and Stroke_text for
 * their text), and the playground's menus.
 *
 * The tools: the arrow (click to select, Shift-click to add, drag on
 * nothing for a marquee, drag a selection to move it, a handle to
 * resize), text, line, rectangle, oval -- which draw once and go back
 * to the arrow, as MacDraw's did; Shift keeps a line to 45 degrees and
 * a rectangle or an oval square. Fill and Pen menus change the
 * selection and what comes next; Arrange brings to front, sends to
 * back, groups, ungroups and aligns; Layout turns the grid on, which
 * snaps what is drawn and moved to it.
 *
 * What it deliberately does not do: MacDraw's other shapes (rounded
 * rectangles, arcs, polygons, freehand); patterns (the objects are
 * filled with greys, since they are drawn as shapes and not as dots);
 * text of several lines, and text styles; rotation; rulers; pages and
 * printing. It saves (File, apps/File_menu): the Drawing.t, as it is.
 *
 * Exercises: rounded rectangles (the corner radius a fifth handle);
 * polygons, clicked point by point, and their hit test (a point in a
 * polygon: count the edges a ray from it crosses); rotation, which
 * makes the bounds of a figure a question rather than a field; "Paste
 * as picture" from TinyMacPaint, the two programs meeting.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type tool = Arrow | Text_tool | Line_tool | Rect_tool | Oval_tool

let tools = [ Arrow; Text_tool; Line_tool; Rect_tool; Oval_tool ]

(* what a drag is doing, since where the mouse went down *)
type drag =
  | Moving of Figure.point (* the selection, the mouse last here *)
  | Resizing of Drawing.id * int (* a figure, by that handle *)
  | Marquee of Figure.point * Figure.point
  | Creating of Figure.point * Figure.point

type model = {
  (* the drawing, and every version of it *)
  history : Drawing.t Undo.t;
  (* the document's name, and the File menu's dialog *)
  file : File_menu.t;
  (* the drawing while a drag changes it, made one edit on release *)
  live : Drawing.t option;
  selection : Drawing.id list;
  tool : tool;
  drag : drag option;
  (* the style of what is drawn next *)
  fill : float option;
  pen : float;
  (* figures cut or copied: values, like everything else here *)
  clip : Figure.t list;
  grid : bool;
  (* a text being typed, just made with the text tool *)
  typing : Drawing.id option;
  was : string list;
  was_down : bool;
}

let text_size = 22.
let text_style = { Style.plain with size = text_size }

(* a text figure's box, from its text: where it starts, and its width *)
let text_figure (x, y) s =
  let w = Float.max 12. (Stroke_text.metrics text_style s) in
  Figure.Text ({ x0 = x; x1 = x +. w; y1 = y; y0 = y -. (text_size *. 1.3) }, s, text_size)

let grey g = Some g
let solid = { Figure.fill = grey 0.55; pen = 2. }
let hollow = { Figure.fill = None; pen = 2. }
let white_box = { Figure.fill = grey 1.; pen = 2. }

let opening =
  let add f d = fst (Drawing.add f d) in
  let labelled x y s d =
    (* a box and its label, grouped: they move and resize as one *)
    let d, b = Drawing.add (Figure.Rect (Figure.box (x, y) (x +. 150., y -. 70.), white_box)) d in
    let d, t = Drawing.add (text_figure (x +. 20., y -. 20.) s) d in
    fst (Drawing.group [ b; t ] d)
  in
  Drawing.empty
  |> add (text_figure (-340., 400.) "TinyMacDraw, 1984")
  (* a grey oval with a hollow rectangle in front of it: a click in the
     rectangle's middle selects the oval behind *)
  |> add (Figure.Oval (Figure.box (-320., 80.) (-80., 280.), solid))
  |> add (Figure.Rect (Figure.box (-260., 40.) (-20., 230.), { hollow with pen = 4. }))
  |> add (text_figure (-320., 20.) "click inside the rectangle")
  |> labelled 60. 300. "Objects"
  |> labelled 260. 300. "not dots"
  |> add (Figure.Line ((210., 265.), (260., 265.), hollow))
  |> add (Figure.Oval (Figure.box (100., 60.) (380., 180.), { Figure.fill = grey 0.85; pen = 1. }))
  |> add (Figure.Line ((-300., -100.), (380., -300.), { hollow with pen = 6. }))

let initial =
  {
    history = Undo.start opening;
    file = File_menu.start;
    live = None;
    selection = [];
    tool = Arrow;
    drag = None;
    fill = grey 1.;
    pen = 2.;
    clip = [];
    grid = false;
    typing = None;
    was = [];
    was_down = false;
  }

let drawing m = match m.live with Some d -> d | None -> Undo.now m.history
let record ~name d m = { m with history = Undo.record ~name d m.history; live = None }

(*****************************************************************************)
(* The page *)
(*****************************************************************************)

let page_left = -380.
let page_right = 460.
let page_top = 440.
let page_bottom = -400.
let on_page (x, y) = x >= page_left && x <= page_right && y <= page_top && y >= page_bottom

let snap m (x, y) =
  if m.grid then
    let g = 16. in
    (Float.round (x /. g) *. g, Float.round (y /. g) *. g)
  else (x, y)

(* what a drag from a to z with a drawing tool makes; Shift keeps a
   line to 45 degrees and a box square *)
let made m shift tool ((ax, ay) as a) (zx, zy) =
  let style = { Figure.fill = m.fill; pen = m.pen } in
  let dx = zx -. ax and dy = zy -. ay in
  let square =
    if shift then
      let s = Float.max (Float.abs dx) (Float.abs dy) in
      (ax +. Float.copy_sign s dx, ay +. Float.copy_sign s dy)
    else (zx, zy)
  in
  match tool with
  | Line_tool ->
      let z =
        if shift then
          (* the nearest of the eight directions *)
          let angle = Float.round (Float.atan2 dy dx /. (Float.pi /. 4.)) *. (Float.pi /. 4.) in
          let len = Float.sqrt ((dx *. dx) +. (dy *. dy)) in
          (ax +. (len *. Float.cos angle), ay +. (len *. Float.sin angle))
        else (zx, zy)
      in
      Some (Figure.Line (a, z, { style with fill = None }))
  | Rect_tool -> Some (Figure.Rect (Figure.box a square, style))
  | Oval_tool -> Some (Figure.Oval (Figure.box a square, style))
  | Arrow | Text_tool -> None

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

let edit ~name f m =
  let d = drawing m in
  let d' = f d in
  if d' = d then m else record ~name d' m

let delete m = { (edit ~name:"Delete" (Drawing.delete m.selection) m) with selection = [] }
let copy m = { m with clip = List.filter_map (Drawing.get (drawing m)) m.selection }
let cut m = delete (copy m)

let paste m =
  let d, ids =
    List.fold_left
      (fun (d, ids) f ->
        let d, id = Drawing.add (Figure.translate 24. (-24.) f) d in
        (d, ids @ [ id ]))
      (drawing m, []) m.clip
  in
  if ids = [] then m else { (record ~name:"Paste" d m) with selection = ids }

let duplicate m =
  let d, ids = Drawing.duplicate m.selection (drawing m) in
  if ids = [] then m else { (record ~name:"Duplicate" d m) with selection = ids }

let group m =
  match Drawing.group m.selection (drawing m) with
  | d, Some id -> { (record ~name:"Group" d m) with selection = [ id ] }
  | _, None -> m

let ungroup m =
  match m.selection with
  | [ id ] -> (
      match Drawing.ungroup id (drawing m) with
      | _, [] -> m
      | d, ids -> { (record ~name:"Ungroup" d m) with selection = ids })
  | _ -> m

(* a fill or a pen: for the selection, and for what comes next *)
let restyle name f m =
  let m = edit ~name (fun d -> List.fold_left (fun d id -> Drawing.update id (Figure.restyle f) d) d m.selection) m in
  let s = f { Figure.fill = m.fill; pen = m.pen } in
  { m with fill = s.fill; pen = s.pen }

let undo m = { m with history = Undo.undo m.history; selection = []; typing = None }
let redo m = { m with history = Undo.redo m.history; selection = []; typing = None }

let menus =
  [
    File_menu.items;
    [ "Edit"; "Undo"; "Redo"; "Cut"; "Copy"; "Paste"; "Duplicate"; "Delete"; "Select All" ];
    [ "Arrange"; "Bring to Front"; "Send to Back"; "Group"; "Ungroup"; "Align Lefts"; "Align Centers"; "Align Tops" ];
    [ "Fill"; "None"; "White"; "Light Grey"; "Grey"; "Dark Grey"; "Black" ];
    [ "Pen"; "1"; "2"; "4"; "8" ];
    [ "Layout"; "Grid"; "No Grid" ];
  ]

let menu_box i : Widget.box = { Widget.x = -410. +. (float_of_int i *. 105.); y = 472.; w = 100.; h = 30. }

let command c m =
  match c with
  | "Undo" -> undo m
  | "Redo" -> redo m
  | "Cut" -> cut m
  | "Copy" -> copy m
  | "Paste" -> paste m
  | "Duplicate" -> duplicate m
  | "Delete" -> delete m
  | "Select All" -> { m with selection = List.map fst (Drawing.figures (drawing m)); tool = Arrow }
  | "Bring to Front" -> edit ~name:"Bring to Front" (Drawing.to_front m.selection) m
  | "Send to Back" -> edit ~name:"Send to Back" (Drawing.to_back m.selection) m
  | "Group" -> group m
  | "Ungroup" -> ungroup m
  | "Align Lefts" -> edit ~name:"Align" (Drawing.align Drawing.Lefts m.selection) m
  | "Align Centers" -> edit ~name:"Align" (Drawing.align Drawing.Centers m.selection) m
  | "Align Tops" -> edit ~name:"Align" (Drawing.align Drawing.Tops m.selection) m
  | "None" -> restyle "Fill" (fun s -> { s with fill = None }) m
  | "White" -> restyle "Fill" (fun s -> { s with fill = grey 1. }) m
  | "Light Grey" -> restyle "Fill" (fun s -> { s with fill = grey 0.8 }) m
  | "Grey" -> restyle "Fill" (fun s -> { s with fill = grey 0.55 }) m
  | "Dark Grey" -> restyle "Fill" (fun s -> { s with fill = grey 0.3 }) m
  | "Black" -> restyle "Fill" (fun s -> { s with fill = grey 0. }) m
  | ("1" | "2" | "4" | "8") as w -> restyle "Pen" (fun s -> { s with pen = float_of_string w }) m
  | "Grid" -> { m with grid = true }
  | "No Grid" -> { m with grid = false }
  | _ -> m

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let tool_box i : Widget.box = { Widget.x = -440.; y = 400. -. (float_of_int i *. 46.); w = 44.; h = 42. }
let handle_size = 8.

(* the handle of the one selected figure under a point, if any *)
let handle_at m p =
  match m.selection with
  | [ id ] -> (
      match Drawing.get (drawing m) id with
      | Some f ->
          let hs = List.mapi (fun i h -> (i, h)) (Figure.handles f) in
          List.find_opt (fun (_, (hx, hy)) -> Float.abs (hx -. fst p) <= handle_size && Float.abs (hy -. snd p) <= handle_size) hs
          |> Option.map (fun (i, _) -> (id, i))
      | None -> None)
  | _ -> None

let press m shift p =
  match m.tool with
  | Arrow -> (
      match handle_at m p with
      | Some (id, h) -> { m with drag = Some (Resizing (id, h)) }
      | None -> (
          match Drawing.at ~tolerance:4. (drawing m) p with
          | Some id ->
              let selection =
                if shift then if List.mem id m.selection then List.filter (( <> ) id) m.selection else m.selection @ [ id ]
                else if List.mem id m.selection then m.selection
                else [ id ]
              in
              { m with selection; drag = (if List.mem id selection then Some (Moving (snap m p)) else None) }
          | None -> { m with selection = (if shift then m.selection else []); drag = Some (Marquee (p, p)) }))
  | Text_tool ->
      let d, id = Drawing.add (text_figure p "") (drawing m) in
      { (record ~name:"New Text" d m) with selection = [ id ]; typing = Some id; tool = Arrow }
  | Line_tool | Rect_tool | Oval_tool -> { m with drag = Some (Creating (snap m p, snap m p)) }

let dragging m shift p =
  let base = Undo.now m.history in
  match m.drag with
  | Some (Moving (lx, ly)) ->
      let x, y = snap m p in
      let d = Drawing.move m.selection (x -. lx) (y -. ly) (drawing m) in
      { m with live = Some d; drag = Some (Moving (x, y)) }
  | Some (Resizing (id, h)) -> { m with live = Some (Drawing.update id (fun f -> Figure.drag_handle f h (snap m p)) base) }
  | Some (Marquee (a, _)) -> { m with drag = Some (Marquee (a, p)) }
  | Some (Creating (a, _)) -> (
      let z = snap m p in
      match made m shift m.tool a z with
      | Some f -> { m with live = Some (fst (Drawing.add f base)); drag = Some (Creating (a, z)) }
      | None -> m)
  | None -> m

(* the mouse let go: a drag that changed the drawing is one edit *)
let release m =
  let base = Undo.now m.history in
  let m =
    match m.drag with
    | Some (Marquee (a, z)) ->
        let inside = Drawing.within (drawing m) (Figure.box a z) in
        { m with selection = m.selection @ List.filter (fun i -> not (List.mem i m.selection)) inside }
    | Some (Creating (a, z)) ->
        let big = Float.abs (fst z -. fst a) +. Float.abs (snd z -. snd a) > 4. in
        (match (big, made m false m.tool a z, m.live) with
        | true, Some _, Some d ->
            let id = fst (List.nth (Drawing.figures d) (List.length (Drawing.figures d) - 1)) in
            let name = match m.tool with Line_tool -> "New Line" | Rect_tool -> "New Rectangle" | _ -> "New Oval" in
            { (record ~name d m) with selection = [ id ] }
        | _ -> { m with live = None })
        |> fun m -> { m with tool = Arrow }
    | Some (Moving _) -> (
        match m.live with Some d when d <> base -> record ~name:"Move" d m | _ -> { m with live = None })
    | Some (Resizing _) -> (
        match m.live with Some d when d <> base -> record ~name:"Resize" d m | _ -> { m with live = None })
    | None -> m
  in
  { m with drag = None; live = None }

(* typing into a text just made; Enter or Escape ends it *)
let type_text computer m id =
  let k = computer.keyboard in
  let now = Set_.elements k.keys in
  let pressed key = List.mem key now && not (List.mem key m.was) in
  match Drawing.get (drawing m) id with
  | Some (Figure.Text (b, s, _)) ->
      let s' =
        if k.typed <> "" then s ^ k.typed
        else if pressed "Backspace" && s <> "" then String.sub s 0 (Text.prev_char s (String.length s))
        else s
      in
      let m = if s' <> s then { m with history = Undo.amend (Drawing.update id (fun _ -> text_figure (b.x0, b.y1) s') (drawing m)) m.history } else m in
      if pressed "Enter" || pressed "Escape" then
        (* an empty text is no text *)
        if s' = "" then { m with history = Undo.amend (Drawing.delete [ id ] (drawing m)) m.history; typing = None; selection = [] }
        else { m with typing = None }
      else m
  | _ -> { m with typing = None }

let keyboard computer m =
  let k = computer.keyboard in
  let now = Set_.elements k.keys in
  let pressed key = List.mem key now && not (List.mem key m.was) in
  if List.mem "Control" now then
    if pressed "z" then if k.kshift then redo m else undo m
    else if pressed "y" then redo m
    else if pressed "x" then cut m
    else if pressed "c" then copy m
    else if pressed "v" then paste m
    else if pressed "d" then duplicate m
    else if pressed "g" then group m
    else if pressed "u" then ungroup m
    else if pressed "a" then command "Select All" m
    else m
  else if pressed "Backspace" || pressed "Delete" then delete m
  else m

(* a drawing, saved as the Drawing.t it is: its figures, by id and in
   depth order *)
let kind = { File_menu.magic = "TinyMacDraw 1"; extension = ".draw" }

let reopened (r : Drawing.t File_menu.result) model =
  match r with
  | File_menu.Nothing -> model
  | File_menu.New -> { initial with history = Undo.start Drawing.empty; file = model.file }
  | File_menu.Opened d -> { initial with history = Undo.start d; file = model.file }

let update caps computer model =
  let mouse = computer.mouse in
  let now = Set_.elements computer.keyboard.keys in
  if File_menu.busy model.file then
    let file, r = File_menu.dialog caps kind computer ~current:(fun () -> Undo.now model.history) model.file in
    reopened r { model with file; was_down = mouse.mdown; was = now }
  else
  let shift = computer.keyboard.kshift in
  let press_edge = mouse.mdown && not model.was_down in
  let model =
    List.fold_left
      (fun m (i, items) ->
        if i = 0 then
          let file, r = File_menu.menu_in caps kind computer (menu_box i) ~current:(fun () -> Undo.now m.history) m.file in
          reopened r { m with file }
        else
        match List.nth_opt items (Gui.menu_in computer (menu_box i) items 0) with
        | Some c when c <> List.hd items -> command c { m with typing = None }
        | _ -> m)
      model
      (List.mapi (fun i items -> (i, items)) menus)
  in
  let p = (mouse.mx, mouse.my) in
  let model =
    if Gui.modal () then model
    else
      match List.find_opt (fun (i, _) -> press_edge && Widget.contains (tool_box i) mouse.mx mouse.my) (List.mapi (fun i t -> (i, t)) tools) with
      | Some (_, tool) -> { model with tool; typing = None }
      | None ->
          if press_edge && on_page p then press { model with typing = None } shift p
          else if mouse.mdown && model.drag <> None then dragging model shift p
          else if (not mouse.mdown) && model.drag <> None then release model
          else model
  in
  let model = match model.typing with Some id -> type_text computer model id | None -> keyboard computer model in
  { model with was = now; was_down = mouse.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* an icon, in ink on paper: inverted when the tool is in use *)
let icon (b : Widget.box) tool ~ink ~paper =
  let at dx dy s = s |> move (b.x +. dx) (b.y +. dy) in
  match tool with
  | Arrow ->
      [ at 0. 0. (polygon ink [ (-6., 12.); (-6., -8.); (-1., -3.); (3., -12.); (6., -10.); (2., -1.); (8., -1.) ]) ]
  | Text_tool -> [ at 0. 0. (words ink "A") ]
  | Line_tool -> [ at 0. 0. (rectangle ink 28. 2. |> rotate 45.) ]
  | Rect_tool -> [ at 0. 0. (rectangle ink 26. 18.); at 0. 0. (rectangle paper 22. 14.) ]
  | Oval_tool -> [ at 0. 0. (oval ink 28. 20.); at 0. 0. (oval paper 24. 16.) ]

let tool_name = function Arrow -> "Arrow" | Text_tool -> "Text" | Line_tool -> "Line" | Rect_tool -> "Rectangle" | Oval_tool -> "Oval"

let view _computer model =
  let d = drawing model in
  let page_w = page_right -. page_left and page_h = page_top -. page_bottom in
  let page_cx = (page_left +. page_right) /. 2. and page_cy = (page_top +. page_bottom) /. 2. in
  let grid =
    if not model.grid then []
    else
      List.init (int_of_float (page_w /. 16.)) (fun i ->
          rectangle (rgb 225 225 235) 1. page_h |> move (page_left +. (float_of_int i *. 16.)) page_cy)
      @ List.init (int_of_float (page_h /. 16.)) (fun i ->
            rectangle (rgb 225 225 235) page_w 1. |> move page_cx (page_bottom +. (float_of_int i *. 16.)))
  in
  let handles =
    List.concat_map
      (fun id ->
        match Drawing.get d id with
        | Some f -> List.map (fun (x, y) -> rectangle black handle_size handle_size |> move x y) (Figure.handles f)
        | None -> [])
      model.selection
  in
  let marquee =
    match model.drag with
    | Some (Marquee (a, z)) ->
        let (b : Figure.box) = Figure.box a z in
        let c = rgb 90 90 90 in
        [
          Figure_shapes.segment c 1. (b.x0, b.y0) (b.x1, b.y0);
          Figure_shapes.segment c 1. (b.x1, b.y0) (b.x1, b.y1);
          Figure_shapes.segment c 1. (b.x1, b.y1) (b.x0, b.y1);
          Figure_shapes.segment c 1. (b.x0, b.y1) (b.x0, b.y0);
        ]
    | _ -> []
  in
  let palette =
    List.concat
      (List.mapi
         (fun i tool ->
           let b = tool_box i in
           let on = tool = model.tool in
           let ink, paper = if on then (white, black) else (black, white) in
           [ rectangle paper b.w b.h |> move b.x b.y ] @ Gui.shapes (Widget.frame black 1. b) @ icon b tool ~ink ~paper)
         tools)
  in
  let caret =
    match model.typing with
    | Some id -> (
        match Drawing.get d id with Some (Figure.Text (b, _, size)) -> [ rectangle black 2. size |> move (b.x1 +. 2.) (b.y1 -. (size *. 0.6)) ] | _ -> [])
    | None -> []
  in
  let status =
    Printf.sprintf "%s     %s     %d selected%s%s"
      (if File_menu.said model.file <> "" then File_menu.said model.file else File_menu.title model.file)
      (tool_name model.tool) (List.length model.selection)
      (match Undo.undo_name model.history with Some n -> "     Undo " ^ n | None -> "")
      (if model.grid then "     grid" else "")
  in
  [
    rectangle (rgb 150 150 150) 1000. 1000.;
    rectangle (Gui.theme ()).face 1000. 40. |> move 0. 472.;
    rectangle white page_w page_h |> move page_cx page_cy;
  ]
  @ grid
  @ List.concat_map (fun (_, f) -> Figure_shapes.figure f) (Drawing.figures d)
  @ handles @ marquee @ caret @ palette
  @ [ words (rgb 30 30 30) status |> move 0. (-450.) ]
  @ File_menu.view model.file
  @ Gui.draw ()

let app caps = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> File_menu.caps)))
