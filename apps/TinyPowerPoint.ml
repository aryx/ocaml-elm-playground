(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyPowerPoint: the first presentation program (Robert Gaskins and
 * Dennis Austin, Forethought, 1987 -- first called Presenter, bought by
 * Microsoft the same year; plan_gui_teaching.md).
 *
 * PowerPoint 1.0 ran on the Macintosh, in black and white, and what it
 * made was printed on overhead transparencies. Its idea was that a
 * talk is written before it is drawn, so it is several views of one
 * thing:
 *
 *   Outline   the talk as indented text: a line against the edge is a
 *             slide's title, an indented one a point on it -- the whole
 *             talk typed without drawing anything (appkits/slides)
 *   Slide     one slide, as it will look
 *   Sorter    every slide at a glance, to see the talk's shape
 *   Show      one slide at a time, filling the screen
 *
 * and a **master**: the look of every slide -- the background, where
 * the title goes, the slide numbers -- said once, and inherited by all
 * of them, so that changing the master changes the whole deck. It is
 * the idea of a style sheet, applied to pages rather than paragraphs.
 *
 * The plan had turned this program down, since stripped of its looks a
 * slide is a title and a list, which the toolkit already draws -- unless
 * it came as a host of components. It does: a slide can carry a sheet
 * or a picture, the same parts as TinyOpenDoc's (appkits/embed), edited
 * where they sit. And the four views are one lesson of their own: a
 * slide's drawing is a list of shapes made at one size, so the sorter's
 * thumbnails and the show's full screen are the same list, scaled -- no
 * second renderer, no cached bitmaps.
 *
 * What it uses: appkits/slides (Outline), appkits/embed (Component)
 * and the parts of apps/ (Part_sheet, Part_picture), appkits/richtext
 * (Rich, Page) with apps/Stroke_text for the text, appkits/document's
 * Undo, and the playground's text area and menus.
 *
 * Two undos, deliberately: the outline's own (Control-Z in it, the
 * text area's piece table), and the deck's (Edit > Undo) for the master
 * and the parts. One history for both would have to decide whether
 * undoing a change of master also undoes the typing done since.
 *
 * What it deliberately does not do: editing text on the slide itself
 * (it is edited in the outline); text boxes, lines and shapes placed
 * freely on a slide; more than one part per slide, and a part kept
 * with its slide when slides are inserted before it in the outline (it
 * stays with the slide's number); notes pages; printing; saving;
 * colour, which came with PowerPoint 2.0 (1988).
 *
 * Exercises: drag a thumbnail in the sorter to move a slide, which is
 * moving its lines in the outline; notes pages -- a slide's notes as
 * lines indented under a marker in the outline; a title master apart
 * from the slide master, as PowerPoint soon had; the "build", one
 * point more shown at each click of the show; a part tied to its slide
 * by a marker line in the outline, so that it moves with it.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type view = Slide_view | Outline_view | Sorter | Show
type background = Plain | Rules | Frame | Band

(* the look of every slide, said once *)
type master = { background : background; centered : bool; numbers : bool; footer : string }

(* what the deck's own undo keeps: the look, and the parts on the
   slides, by slide number *)
type deck = { master : master; parts : (int * Component.part) list }

type model = {
  (* the talk, as text: the slides are made from it every frame *)
  outline : Text_edit.t;
  history : deck Undo.t;
  view : view;
  current : int;
  (* the deck while a part is being edited in place; recorded when it
     is put down, as TinyOpenDoc does *)
  editing : deck option;
  (* the click that woke the part is still held: not the part's *)
  waking : bool;
  (* a transition of the show: frames still to go, and from which
     slide, pushed which way *)
  push : int * int * int;
  was : string list;
  was_down : bool;
}

let deck model = match model.editing with Some d -> d | None -> Undo.now model.history
let slides model = Outline.parse (Text_edit.to_string model.outline)
let count model = max 1 (List.length (slides model))

let talk =
  "TinyPowerPoint, 1987\n\
  \  Robert Gaskins and Dennis Austin\n\
  \    at Forethought, first called Presenter\n\
  \  For the Macintosh, in black and white\n\
  \  Printed on overhead transparencies\n\
   The outline is the talk\n\
  \  The ideas first, in order, as text\n\
  \  A line against the edge is a slide\n\
  \    and an indented line a point on it\n\
  \  Try View > Outline, and type\n\
   The master is the look\n\
  \  Said once, for every slide\n\
  \  Change it in the Master menu: they all change\n\
   A slide is a document too\n\
  \  A sheet, embedded\n\
  \  Click it to edit it where it is\n\
   The sorter, and the show\n\
  \  Every slide at a glance\n\
  \  Then one at a time, the whole screen\n\
  \  The same drawing each time, only scaled"

let figures =
  List.fold_left
    (fun s (cell, v) -> Sheet.set cell v s)
    Sheet.empty
    [ ((0, 0), "Slides"); ((1, 0), "5"); ((0, 1), "Minutes"); ((1, 1), "15"); ((0, 2), "Each"); ((1, 2), "=B2/B1") ]

let initial =
  {
    outline = Text_edit.of_string talk;
    history =
      Undo.start
        {
          master = { background = Rules; centered = false; numbers = true; footer = "TinyPowerPoint" };
          parts = [ (3, Part_sheet.make figures) ];
        };
    view = Slide_view;
    current = 0;
    editing = None;
    waking = false;
    push = (0, 0, 0);
    was = [];
    was_down = false;
  }

(*****************************************************************************)
(* A slide, drawn -- once, at one size, around (0, 0) *)
(*****************************************************************************)

let slide_w = 720.
let slide_h = 540.
let margin = 48.

(* text laid out and drawn with the pen, its top-left at (left, top) *)
let text_block ?(align = Page.Left) ink r ~left ~top ~width =
  let page = Page.layout ~align ~metrics:Stroke_text.metrics ~width r in
  let shapes =
    List.concat_map
      (fun (g : Page.glyph) ->
        if g.text = "\n" || g.text = " " then []
        else Stroke_text.glyph ink g.style g.text ~x:(left +. g.x) ~baseline:(top -. g.baseline))
      (Page.glyphs page)
  in
  (shapes, Page.height page)

let styled size ?(bold = false) s = Rich.of_string ~style:{ Style.plain with size; bold } s

(* where the part goes on a slide: the right half of the body *)
let part_box ~body_top (p : Component.part) : Widget.box =
  let w = (slide_w /. 2.) -. margin in
  let h = Float.min (p.height w) (body_top +. (slide_h /. 2.) -. margin) in
  { Widget.x = w /. 2. +. 10.; y = body_top -. (h /. 2.) -. 10.; w; h }

(* the title's height, and so where the body starts *)
let body_top (s : Outline.slide) =
  let width = slide_w -. (2. *. margin) in
  let _, h = text_block black (styled 40. ~bold:true s.title) ~left:0. ~top:0. ~width in
  (slide_h /. 2.) -. margin -. h -. 30.

let slide_shapes ?(active = false) m (s : Outline.slide) ~number ~total =
  let master = m.master in
  let width = slide_w -. (2. *. margin) in
  let left = (-.slide_w /. 2.) +. margin and top = (slide_h /. 2.) -. margin in
  let title_ink = if master.background = Band then white else black in
  let title, title_h =
    text_block ~align:(if master.centered then Page.Center else Page.Left) title_ink (styled 40. ~bold:true s.title) ~left ~top ~width
  in
  let body = body_top s in
  let part = List.assoc_opt number m.parts in
  (* the points, one under the other, each with its bullet; in the
     left half when there is a part on the right *)
  let points_w = match part with Some _ -> (slide_w /. 2.) -. margin -. 10. | None -> width in
  let points, _ =
    List.fold_left
      (fun (acc, y) (level, text) ->
        let indent = 34. *. float_of_int level in
        let size = if level = 1 then 26. else 21. in
        let shapes, h = text_block black (styled size text) ~left:(left +. indent) ~top:y ~width:(points_w -. indent) in
        let bullet =
          if level = 1 then rectangle black 9. 9. |> move (left +. indent -. 18.) (y -. (size *. 0.62))
          else rectangle black 10. 3. |> move (left +. indent -. 18.) (y -. (size *. 0.62))
        in
        ((bullet :: shapes) @ acc, y -. h -. 12.))
      ([], body) s.points
  in
  let decoration =
    match master.background with
    | Plain -> []
    | Rules ->
        let y = top -. title_h -. 12. in
        [ rectangle black width 4. |> move 0. y; rectangle black width 1. |> move 0. (y -. 7.) ]
    | Frame ->
        Gui.shapes (Widget.frame black 3. { Widget.x = 0.; y = 0.; w = slide_w -. 24.; h = slide_h -. 24. })
        @ Gui.shapes (Widget.frame black 1. { Widget.x = 0.; y = 0.; w = slide_w -. 36.; h = slide_h -. 36. })
    | Band ->
        let h = title_h +. margin +. 16. in
        [ rectangle black slide_w h |> move 0. ((slide_h /. 2.) -. (h /. 2.)) ]
  in
  let footer =
    let y = (-.slide_h /. 2.) +. 26. in
    fst (text_block black (styled 14. master.footer) ~left ~top:(y +. 10.) ~width)
    @ if master.numbers then fst (text_block ~align:Page.Right black (styled 14. (Printf.sprintf "%d / %d" (number + 1) total)) ~left ~top:(y +. 10.) ~width) else []
  in
  let embedded =
    match part with
    | Some p ->
        let b = part_box ~body_top:body p in
        (if active then Gui.shapes (Widget.frame (rgb 120 120 120) 4. { b with w = b.w +. 12.; h = b.h +. 12. }) else [])
        @ p.draw b ~active
    | None -> []
  in
  [ rectangle white slide_w slide_h ] @ decoration @ title @ points @ embedded @ footer
  @ Gui.shapes (Widget.frame black 1. { Widget.x = 0.; y = 0.; w = slide_w; h = slide_h })

(* a slide drawn at a place and a size: the same shapes, grouped *)
let placed shapes ~x ~y ~scale:k = group shapes |> scale k |> move x y

let nth_slide model n = Option.value (List.nth_opt (slides model) n) ~default:{ Outline.title = ""; points = [] }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* where the slide view puts the slide on the screen *)
let slide_y = 30.

let put_down model =
  match model.editing with
  | None -> model
  | Some d ->
      let before = Undo.now model.history in
      let saved (d : deck) = List.map (fun (n, (p : Component.part)) -> (n, p.save ())) d.parts in
      let history = if saved d = saved before then model.history else Undo.record ~name:"Edit Part" d model.history in
      { model with history; editing = None }

let record ~name f model =
  let model = put_down model in
  { model with history = Undo.record ~name (f (Undo.now model.history)) model.history }

let master_edit name f = record ~name:("Master: " ^ name) (fun d -> { d with master = f d.master })
let go n model = { (put_down model) with current = max 0 (min (count model - 1) n) }

let new_slide model =
  let text = Text_edit.to_string model.outline in
  let at = Outline.start_of text (model.current + 1) in
  let piece = if at = String.length text then "\nNew slide\n  A point" else "New slide\n  A point\n" in
  let outline = Text_edit.insert piece (Text_edit.at at model.outline) in
  go (model.current + 1) { model with outline }

let menus =
  [
    [ "File"; "New" ];
    [ "Edit"; "Undo"; "Redo"; "Delete Part" ];
    [ "View"; "Slide"; "Outline"; "Sorter"; "Show" ];
    [ "Insert"; "New Slide"; "Sheet"; "Picture" ];
    [ "Master"; "Plain"; "Rules"; "Frame"; "Band"; "Centered Titles"; "Left Titles"; "Numbers" ];
  ]

let menu_box i : Widget.box = { Widget.x = -410. +. (float_of_int i *. 95.); y = 470.; w = 90.; h = 30. }

let command c model =
  match c with
  | "New" -> { initial with outline = Text_edit.of_string "Untitled\n  A point"; history = Undo.start { (Undo.now initial.history) with parts = [] } }
  | "Undo" -> let model = put_down model in { model with history = Undo.undo model.history }
  | "Redo" -> let model = put_down model in { model with history = Undo.redo model.history }
  | "Delete Part" -> record ~name:"Delete Part" (fun d -> { d with parts = List.remove_assoc model.current d.parts }) model
  | "Slide" -> { (put_down model) with view = Slide_view }
  | "Outline" -> { (put_down model) with view = Outline_view }
  | "Sorter" -> { (put_down model) with view = Sorter }
  | "Show" -> { (put_down model) with view = Show; push = (0, 0, 0) }
  | "New Slide" -> new_slide model
  | "Sheet" -> record ~name:"Insert Sheet" (fun d -> { d with parts = (model.current, Part_sheet.make Sheet.empty) :: List.remove_assoc model.current d.parts }) model
  | "Picture" ->
      record ~name:"Insert Picture"
        (fun d -> { d with parts = (model.current, Part_picture.make (Bitmap.create ~width:140 ~height:110)) :: List.remove_assoc model.current d.parts })
        model
  | "Plain" -> master_edit c (fun m -> { m with background = Plain }) model
  | "Rules" -> master_edit c (fun m -> { m with background = Rules }) model
  | "Frame" -> master_edit c (fun m -> { m with background = Frame }) model
  | "Band" -> master_edit c (fun m -> { m with background = Band }) model
  | "Centered Titles" -> master_edit c (fun m -> { m with centered = true }) model
  | "Left Titles" -> master_edit c (fun m -> { m with centered = false }) model
  | "Numbers" -> master_edit c (fun m -> { m with numbers = not m.numbers }) model
  | _ -> model

(* the sorter's thumbnails: three to a row *)
let thumb_scale = 0.28
let thumb i = (-300. +. (float_of_int (i mod 3) *. 300.), 330. -. (float_of_int (i / 3) *. 200.))

let push_frames = 14

let update computer model =
  let m = computer.mouse in
  let now = Set_.elements computer.keyboard.keys in
  let pressed key = List.mem key now && not (List.mem key model.was) in
  let click = m.mdown && not model.was_down in
  (* the menus: not in the show, which is the whole screen *)
  let model =
    if model.view = Show then model
    else
      List.fold_left
        (fun model (i, items) -> match List.nth_opt items (Gui.menu_in computer (menu_box i) items 0) with Some c when c <> List.hd items -> command c model | _ -> model)
        model
        (List.mapi (fun i items -> (i, items)) menus)
  in
  (* in-place activation: the part's menu joins the bar *)
  let model =
    match (model.editing, List.assoc_opt model.current (deck model).parts) with
    | Some d, Some p when p.menu <> [] && model.view = Slide_view ->
        let chosen = Gui.menu_in computer (menu_box 5) p.menu 0 in
        if chosen > 0 then { model with editing = Some { d with parts = (model.current, p.command (List.nth p.menu chosen)) :: List.remove_assoc model.current d.parts } }
        else model
    | _ -> model
  in
  let model =
    if Gui.modal () then model
    else
      match model.view with
      | Outline_view ->
          (* the talk, typed; the slide beside it follows the caret *)
          let outline = Gui.text_area_in computer { Widget.x = -190.; y = 20.; w = 560.; h = 820. } model.outline in
          let current = Outline.slide_at (Text_edit.to_string outline) (Text_edit.caret outline) in
          { model with outline; current = min current (count model - 1) }
      | Sorter ->
          if click then
            match List.find_opt (fun i -> let x, y = thumb i in Float.abs (m.mx -. x) < slide_w *. thumb_scale /. 2. && Float.abs (m.my -. y) < slide_h *. thumb_scale /. 2.) (List.init (count model) Fun.id) with
            | Some i -> { model with current = i; view = Slide_view }
            | None -> model
          else model
      | Show ->
          let next dir model =
            let n = max 0 (min (count model - 1) (model.current + dir)) in
            if n = model.current then model else { model with current = n; push = (push_frames, model.current, dir) }
          in
          let model =
            if click || pressed "ArrowRight" || pressed " " || pressed "PageDown" then next 1 model
            else if pressed "ArrowLeft" || pressed "PageUp" then next (-1) model
            else if pressed "Escape" then { model with view = Slide_view }
            else model
          in
          let frames, from, dir = model.push in
          { model with push = (max 0 (frames - 1), from, dir) }
      | Slide_view -> (
          let s = nth_slide model model.current in
          let part = List.assoc_opt model.current (deck model).parts in
          let box = Option.map (fun p -> let b = part_box ~body_top:(body_top s) p in { b with y = b.y +. slide_y }) part in
          let on_part = match box with Some b -> Widget.contains b m.mx m.my | None -> false in
          let model =
            if click && on_part && model.editing = None then { model with editing = Some (deck model); waking = true }
            else if click && not on_part then put_down model
            else model
          in
          let model = if m.mdown then model else { model with waking = false } in
          match (model.editing, part, box) with
          | Some d, Some _, Some b when not model.waking ->
              let p = List.assoc model.current d.parts in
              { model with editing = Some { d with parts = (model.current, p.input computer b) :: List.remove_assoc model.current d.parts } }
          | Some _, _, _ -> if pressed "Escape" then put_down model else model
          | None, _, _ ->
              if pressed "ArrowRight" || pressed "ArrowDown" || pressed "PageDown" then go (model.current + 1) model
              else if pressed "ArrowLeft" || pressed "ArrowUp" || pressed "PageUp" then go (model.current - 1) model
              else model)
  in
  { model with was = now; was_down = m.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view _computer model =
  let d = deck model in
  let total = count model in
  let drawn ?active n = slide_shapes ?active d (nth_slide model n) ~number:n ~total in
  let bar = [ rectangle (Gui.theme ()).face 1000. 40. |> move 0. 470. ] in
  let desk = rectangle (rgb 160 160 165) 1000. 1000. in
  let status s = [ words (rgb 40 40 40) s |> move 0. (-470.) ] in
  let undo = match Undo.undo_name model.history with Some n -> "     Undo " ^ n | None -> "" in
  match model.view with
  | Show ->
      (* the whole screen, and a push from one slide to the next: both
         slides drawn, moved -- nothing but move *)
      let k = 1000. /. slide_w in
      let frames, from, dir = model.push in
      let t = 1. -. (float_of_int frames /. float_of_int push_frames) in
      let t = t *. t *. (3. -. (2. *. t)) in
      [ rectangle black 1000. 1000. ]
      @ (if frames > 0 then [ placed (drawn from) ~x:(-.float_of_int dir *. 1000. *. t) ~y:0. ~scale:k ] else [])
      @ [ placed (drawn model.current) ~x:(if frames > 0 then float_of_int dir *. 1000. *. (1. -. t) else 0.) ~y:0. ~scale:k ]
  | Slide_view ->
      [ desk ] @ bar
      @ [ placed (drawn ~active:(model.editing <> None) model.current) ~x:0. ~y:slide_y ~scale:1. ]
      @ status
          (Printf.sprintf "slide %d of %d -- arrows to move%s" (model.current + 1) total
             (if model.editing <> None then "     editing the part -- Escape to put it down" else undo))
      @ Gui.draw ()
  | Outline_view ->
      [ desk ] @ bar
      @ [ words (rgb 40 40 40) (Printf.sprintf "slide %d, as you type" (model.current + 1)) |> move 290. 240. ]
      @ [ placed (drawn model.current) ~x:290. ~y:90. ~scale:0.5 ]
      @ status "a line against the edge is a slide's title; indent for its points"
      @ Gui.draw ()
  | Sorter ->
      [ desk ] @ bar
      @ List.concat
          (List.init total (fun i ->
               let x, y = thumb i in
               (if i = model.current then [ rectangle (rgb 40 90 200) ((slide_w *. thumb_scale) +. 10.) ((slide_h *. thumb_scale) +. 10.) |> move x y ] else [])
               @ [ placed (drawn i) ~x ~y ~scale:thumb_scale; words black (string_of_int (i + 1)) |> move x (y -. 90.) ]))
      @ status ("click a slide to open it" ^ undo)
      @ Gui.draw ()

let app = game view update initial
let main = Playground_platform.run_app app
