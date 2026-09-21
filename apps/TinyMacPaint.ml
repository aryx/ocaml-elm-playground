(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyMacPaint: the program that came with the Macintosh (Bill
 * Atkinson, Apple, 1984; plan_gui_teaching.md, phase 11).
 *
 * It was the other half of the Mac's argument, beside MacWrite: that a
 * computer could be used by pointing, and that what it made was a
 * picture. Everything in it is still in every paint program: a palette
 * of tools down the side, a palette of patterns along the bottom, a
 * drag that draws, a bucket that pours, a dotted rectangle whose dots
 * crawl -- "marching ants" -- round a piece of the picture you can pick
 * up and move.
 *
 * And it is a *paint* program, not a drawing one: the picture is dots
 * and nothing else. Once a rectangle is down it is not a rectangle any
 * more, only the black dots it left, and to move it you select the
 * dots. MacDraw (1984 too) kept the shapes instead, and the difference
 * -- a bitmap against a list of objects -- is still the difference
 * between Photoshop and Illustrator.
 *
 * What it uses: appkits/paint (Bitmap, the picture as bits; Pattern;
 * Paint, Bresenham's lines and the ovals; Seed_fill, the bucket;
 * Packbits, which the clipboard carries a piece of picture in),
 * appkits/document (Undo, Clipboard), and the playground's menus. The
 * palettes are drawn by the program, as TinyWord's toolbar is, so they
 * ask Gui.modal before taking a click. No backend changed: the picture
 * is drawn as rectangles, one per run of black dots merged with the
 * runs under it (Bitmap.rectangles).
 *
 * Every tool paints into a copy of the picture made when the mouse
 * went down, and each frame of the drag amends that copy -- so a whole
 * stroke, a whole rubber-banded oval, a whole move is one "Undo". And
 * what the history holds is always what is on the screen, the piece
 * being dragged included: a selection carries the picture it was
 * lifted out of, and every move puts it down again on that.
 *
 * What it deliberately does not do: MacPaint's other tools -- the
 * lasso, the hand, text, the spray can, polygons, rounded rectangles;
 * FatBits, the magnified view for editing dot by dot; the line widths;
 * brush shapes; patterns you edit; the page of 576 by 720 dots and
 * printing it; and saving to a file, which is Bitmap.to_string one
 * step away.
 *
 * Exercises: the spray can (dots of the pattern at random in a disc,
 * a few per frame); FatBits, each dot drawn eight times bigger, with
 * the pencil working in it; the lasso, which is Seed_fill again -- fill
 * the outside of the loop, and the selection is what the fill did not
 * reach; double-clicking the eraser to erase everything, as MacPaint
 * did; the shift key keeping a line straight and an oval round.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type tool = Select | Pencil | Brush | Eraser | Bucket | Line | Rect | Filled_rect | Oval | Filled_oval

let tools = [ Select; Bucket; Pencil; Brush; Eraser; Line; Rect; Filled_rect; Oval; Filled_oval ]

(* a piece of the picture picked up by the selection: where it is, and
   the picture it was lifted out of, to put it down on again *)
type floating = { piece : Bitmap.t; fx : int; fy : int; under : Bitmap.t }

(* what a drag is doing, since where the mouse went down *)
type drag =
  | Drawing of int * int (* a tool, from there *)
  | Marquee of int * int (* a selection being drawn *)
  | Moving of int * int (* the floating piece, held at that dot of it *)

type model = {
  (* the picture, and every version of it *)
  history : Bitmap.t Undo.t;
  tool : tool;
  pattern : Pattern.t;
  clip : Clipboard.t;
  drag : drag option;
  (* the picture when the drag started, for the shapes that stretch *)
  before : Bitmap.t;
  (* the mouse's dot at the previous frame, where a stroke goes on from *)
  last : int * int;
  (* the pencil draws white when it starts on black, as MacPaint's did *)
  pencil_black : bool;
  (* a selection: left, top, right, bottom, inclusive *)
  selection : (int * int * int * int) option;
  floating : floating option;
  was : string list;
  was_down : bool;
}

let pic_w = 340
let pic_h = 250

let pattern n = List.nth Pattern.palette n
let bricks = pattern 10
let diagonal = pattern 6
let weave = pattern 11

(* a picture to start from, drawn by the same tools the person has *)
let house =
  Bitmap.change (Bitmap.create ~width:pic_w ~height:pic_h) (fun b ->
      let black = Pattern.solid in
      Paint.stroke b ~brush:Paint.pencil black (0, 200) (pic_w - 1, 200);
      Paint.fill_rect b bricks (60, 110) (170, 200);
      Paint.frame_rect b black (60, 110) (170, 200);
      (* the roof: three lines, and the bucket *)
      Paint.stroke b ~brush:Paint.pencil black (50, 110) (115, 60);
      Paint.stroke b ~brush:Paint.pencil black (115, 60) (180, 110);
      Paint.stroke b ~brush:Paint.pencil black (50, 110) (180, 110);
      Seed_fill.fill b diagonal 115 90;
      Paint.fill_rect b black (98, 150) (122, 200);
      Paint.fill_rect b Pattern.white (136, 128) (160, 150);
      Paint.frame_rect b black (136, 128) (160, 150);
      (* the sun, and a tree *)
      Paint.fill_oval b Pattern.grey (250, 25) (300, 75);
      Paint.frame_oval b black (250, 25) (300, 75);
      Paint.fill_rect b black (238, 160) (244, 200);
      Paint.fill_oval b weave (210, 100) (272, 165);
      Paint.frame_oval b black (210, 100) (272, 165))

let initial =
  {
    history = Undo.start house;
    tool = Brush;
    pattern = Pattern.solid;
    clip = Clipboard.empty;
    drag = None;
    before = house;
    last = (0, 0);
    pencil_black = true;
    selection = None;
    floating = None;
    was = [];
    was_down = false;
  }

let picture model = Undo.now model.history
let record ~name b model = { model with history = Undo.record ~name b model.history }
let amend b model = { model with history = Undo.amend b model.history }

(*****************************************************************************)
(* The window *)
(*****************************************************************************)

(* the picture is shown two screen pixels per dot, the Mac's dots
   being about that size next to today's *)
let zoom = 2.
let pic_left = -300.
let pic_top = 390.

(* the dot under a point of the screen, and back *)
let dot_at (x, y) = (int_of_float (Float.floor ((x -. pic_left) /. zoom)), int_of_float (Float.floor ((pic_top -. y) /. zoom)))
let on_picture (x, y) = x >= 0 && y >= 0 && x < pic_w && y < pic_h

(* a rectangle of dots, as a playground rectangle *)
let dots color (x, y, w, h) =
  rectangle color (float_of_int w *. zoom) (float_of_int h *. zoom)
  |> move (pic_left +. ((float_of_int x +. (float_of_int w /. 2.)) *. zoom)) (pic_top -. ((float_of_int y +. (float_of_int h /. 2.)) *. zoom))

(*****************************************************************************)
(* The selection *)
(*****************************************************************************)

let order (x0, y0) (x1, y1) = (min x0 x1, min y0 y1, max x0 x1, max y0 y1)
let inside (l, t, r, b) (x, y) = x >= l && x <= r && y >= t && y <= b

(* the piece's rectangle, when there is one; else the dotted one *)
let selected model =
  match model.floating with
  | Some f -> Some (f.fx, f.fy, f.fx + Bitmap.width f.piece - 1, f.fy + Bitmap.height f.piece - 1)
  | None -> model.selection

(* putting the piece down where it is now: onto the picture it was
   lifted from, so that moving it again leaves no trace behind *)
let put_down f = Bitmap.change f.under (fun b -> Bitmap.blit ~src:f.piece ~dst:b ~x:f.fx ~y:f.fy)

(* picking the selected dots up: white left behind, and the history
   told once, "Move", however far they then go *)
let lift model (l, t, r, b) =
  let under = Bitmap.change (picture model) (fun p -> Paint.fill_rect p Pattern.white (l, t) (r, b)) in
  let f = { piece = Bitmap.sub (picture model) ~x:l ~y:t ~w:(r - l + 1) ~h:(b - t + 1); fx = l; fy = t; under } in
  record ~name:"Move" (put_down f) { model with floating = Some f; selection = None }

let deselect model = { model with floating = None; selection = None }

let copy model =
  match selected model with
  | Some (l, t, r, b) ->
      let piece = Bitmap.sub (picture model) ~x:l ~y:t ~w:(r - l + 1) ~h:(b - t + 1) in
      { model with clip = Clipboard.put (Bitmap.to_string piece) model.clip }
  | None -> model

let clear ~name model =
  match selected model with
  | Some (l, t, r, b) ->
      (* a piece picked up is cleared by not putting it down *)
      let base =
        match model.floating with
        | Some f -> f.under
        | None -> Bitmap.change (picture model) (fun p -> Paint.fill_rect p Pattern.white (l, t) (r, b))
      in
      deselect (record ~name base model)
  | None -> model

let cut model = clear ~name:"Cut" (copy model)

(* a pasted piece arrives floating in the top-left corner, ready to be
   dragged where it goes *)
let paste model =
  match Clipboard.get model.clip with
  | Some s ->
      let f = { piece = Bitmap.of_string s; fx = 8; fy = 8; under = picture model } in
      record ~name:"Paste" (put_down f) { model with floating = Some f; selection = None; tool = Select }
  | None -> model

let undo model = deselect { model with history = Undo.undo model.history }
let redo model = deselect { model with history = Undo.redo model.history }

(*****************************************************************************)
(* The tools *)
(*****************************************************************************)

let name = function
  | Select -> "Select"
  | Pencil -> "Pencil"
  | Brush -> "Brush"
  | Eraser -> "Eraser"
  | Bucket -> "Fill"
  | Line -> "Line"
  | Rect -> "Rectangle"
  | Filled_rect -> "Filled Rectangle"
  | Oval -> "Oval"
  | Filled_oval -> "Filled Oval"

(* what a drag from [a] to [z] leaves on the picture [b] *)
let shape tool pattern b a z =
  match tool with
  | Line -> Paint.stroke b ~brush:Paint.pencil Pattern.solid a z
  | Rect -> Paint.frame_rect b Pattern.solid a z
  | Filled_rect ->
      Paint.fill_rect b pattern a z;
      Paint.frame_rect b Pattern.solid a z
  | Oval -> Paint.frame_oval b Pattern.solid a z
  | Filled_oval ->
      Paint.fill_oval b pattern a z;
      Paint.frame_oval b Pattern.solid a z
  | Select | Pencil | Brush | Eraser | Bucket -> ()

(* the tools that leave a trail, and what they leave it with *)
let trail model =
  match model.tool with
  | Pencil -> Some (Paint.pencil, if model.pencil_black then Pattern.solid else Pattern.white)
  | Brush -> Some (Paint.round 3, model.pattern)
  | Eraser -> Some (Paint.square 14, Pattern.white)
  | _ -> None

(* the mouse went down on the picture, at dot p *)
let press model p =
  match model.tool with
  | Select -> (
      match (model.floating, model.selection) with
      | Some f, _ when inside (Option.get (selected model)) p -> { model with drag = Some (Moving (fst p - f.fx, snd p - f.fy)) }
      | None, Some sel when inside sel p ->
          let (l, t, _, _) = sel in
          { (lift model sel) with drag = Some (Moving (fst p - l, snd p - t)) }
      | _ -> { (deselect model) with drag = Some (Marquee (fst p, snd p)) })
  | Bucket -> record ~name:"Fill" (Bitmap.change (picture model) (fun b -> Seed_fill.fill b model.pattern (fst p) (snd p))) model
  | tool ->
      let model = { model with pencil_black = not (Bitmap.get (picture model) (fst p) (snd p)) } in
      let now = picture model in
      let b =
        Bitmap.change now (fun b ->
            match trail model with
            | Some (brush, pat) -> Paint.stroke b ~brush pat p p
            | None -> shape tool model.pattern b p p)
      in
      { (record ~name:(name tool) b model) with drag = Some (Drawing (fst p, snd p)); before = now; last = p }

(* the mouse, still down, is now at dot p *)
let drag model p =
  match model.drag with
  | Some (Moving (gx, gy)) -> (
      match model.floating with
      | Some f ->
          let f = { f with fx = fst p - gx; fy = snd p - gy } in
          amend (put_down f) { model with floating = Some f }
      | None -> model)
  | Some (Marquee (x, y)) -> { model with selection = Some (order (x, y) p) }
  | Some (Drawing (x, y)) -> (
      match trail model with
      (* a trail goes on from where it was *)
      | Some (brush, pat) -> { (amend (Bitmap.change (picture model) (fun b -> Paint.stroke b ~brush pat model.last p)) model) with last = p }
      (* a shape starts again from the picture before it, stretched to
         the mouse: the "rubber band" *)
      | None -> amend (Bitmap.change model.before (fun b -> shape model.tool model.pattern b (x, y) p)) model)
  | None -> model

let release model =
  let model =
    match (model.drag, model.selection) with
    (* a click with the selection tool, no drag: nothing selected *)
    | Some (Marquee _), Some (l, t, r, b) when l = r && t = b -> { model with selection = None }
    | _ -> model
  in
  { model with drag = None }

(*****************************************************************************)
(* The palettes: drawn, their icons drawn with shapes *)
(*****************************************************************************)

let tool_size = 40.

(* two columns, down the left, as MacPaint had them *)
let tool_box i : Widget.box =
  { Widget.x = -440. +. (float_of_int (i mod 2) *. (tool_size +. 2.)); y = 370. -. (float_of_int (i / 2) *. (tool_size +. 2.)); w = tool_size; h = tool_size }

let swatch_w = 44.
let swatch_h = 30.

(* along the bottom, after the one in use *)
let swatch_box i : Widget.box =
  { Widget.x = -200. +. (float_of_int i *. (swatch_w +. 4.)); y = -160.; w = swatch_w; h = swatch_h }

let current_box : Widget.box = { Widget.x = -290.; y = -160.; w = 70.; h = 40. }

(* a pattern shown in a box, dot by dot, two pixels a dot like the
   picture -- through Bitmap.rectangles too *)
let swatch (b : Widget.box) p =
  let w = int_of_float (b.w /. zoom) and h = int_of_float (b.h /. zoom) in
  let bits = Bitmap.change (Bitmap.create ~width:w ~height:h) (fun bits -> Paint.fill_rect bits p (0, 0) (w - 1, h - 1)) in
  (rectangle white b.w b.h |> move b.x b.y)
  :: List.map
       (fun (x, y, rw, rh) ->
         rectangle black (float_of_int rw *. zoom) (float_of_int rh *. zoom)
         |> move
              (b.x -. (b.w /. 2.) +. ((float_of_int x +. (float_of_int rw /. 2.)) *. zoom))
              (b.y +. (b.h /. 2.) -. ((float_of_int y +. (float_of_int rh /. 2.)) *. zoom)))
       (Bitmap.rectangles bits)
  @ Gui.shapes (Widget.frame black 1. b)

(* an icon, in ink on paper: inverted when the tool is the one in use,
   as MacPaint showed it *)
let icon (b : Widget.box) tool ~ink ~paper =
  let at dx dy s = s |> move (b.x +. dx) (b.y +. dy) in
  let outline w h = [ at 0. 0. (rectangle ink w h); at 0. 0. (rectangle paper (w -. 4.) (h -. 4.)) ] in
  match tool with
  | Select ->
      (* a dotted square *)
      List.concat_map
        (fun i ->
          let d = -12. +. (float_of_int i *. 6.) in
          [ at d 12. (rectangle ink 3. 2.); at d (-12.) (rectangle ink 3. 2.); at 12. d (rectangle ink 2. 3.); at (-12.) d (rectangle ink 2. 3.) ])
        [ 0; 1; 2; 3; 4 ]
  | Pencil -> [ at 0. 0. (rectangle ink 5. 26. |> rotate (-45.)); at (-10.) (-10.) (rectangle ink 3. 3.) ]
  | Brush -> [ at 4. 4. (rectangle ink 4. 20. |> rotate (-45.)); at (-7.) (-7.) (circle ink 5.) ]
  | Eraser -> [ at 0. 0. (rectangle ink 24. 13. |> rotate 30.); at 0. 0. (rectangle paper 20. 9. |> rotate 30.) ]
  | Bucket ->
      [ at 0. 0. (rectangle ink 17. 17. |> rotate 30.); at 0. 0. (rectangle paper 13. 13. |> rotate 30.); at 11. (-8.) (rectangle ink 3. 8.) ]
  | Line -> [ at 0. 0. (rectangle ink 30. 2. |> rotate (-45.)) ]
  | Rect -> outline 26. 20.
  | Filled_rect -> [ at 0. 0. (rectangle ink 26. 20.); at 0. 0. (rectangle (rgb 150 150 150) 22. 16.) ]
  | Oval -> [ at 0. 0. (oval ink 28. 20.); at 0. 0. (oval paper 24. 16.) ]
  | Filled_oval -> [ at 0. 0. (oval ink 28. 20.); at 0. 0. (oval (rgb 150 150 150) 24. 16.) ]

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type slot = File | Edit_

let menu_file = [ "File"; "New" ]
let menu_edit = [ "Edit"; "Undo"; "Redo"; "Cut"; "Copy"; "Paste"; "Clear"; "Select All" ]

let menu_box = function
  | File -> ({ Widget.x = -410.; y = 470.; w = 80.; h = 30. } : Widget.box)
  | Edit_ -> { Widget.x = -325.; y = 470.; w = 80.; h = 30. }

let select_all model = { (deselect model) with tool = Select; selection = Some (0, 0, pic_w - 1, pic_h - 1) }

let command items chosen model =
  match List.nth_opt items chosen with
  | Some "New" -> { initial with history = Undo.start (Bitmap.create ~width:pic_w ~height:pic_h) }
  | Some "Undo" -> undo model
  | Some "Redo" -> redo model
  | Some "Cut" -> cut model
  | Some "Copy" -> copy model
  | Some "Paste" -> paste model
  | Some "Clear" -> clear ~name:"Clear" model
  | Some "Select All" -> select_all model
  | _ -> model

let keyboard computer model =
  let now = Set_.elements computer.keyboard.keys in
  let pressed key = List.mem key now && not (List.mem key model.was) in
  let model =
    if List.mem "Control" now then
      if pressed "z" then if computer.keyboard.kshift then redo model else undo model
      else if pressed "y" then redo model
      else if pressed "x" then cut model
      else if pressed "c" then copy model
      else if pressed "v" then paste model
      else if pressed "a" then select_all model
      else model
    else if pressed "Backspace" || pressed "Delete" then clear ~name:"Clear" model
    else model
  in
  { model with was = now }

let update computer model =
  let m = computer.mouse in
  (* the menus first, so that an open one gets the click *)
  let model = command menu_file (Gui.menu_in computer (menu_box File) menu_file 0) model in
  let model = command menu_edit (Gui.menu_in computer (menu_box Edit_) menu_edit 0) model in
  let model =
    if Gui.modal () then model
    else
      let p = dot_at (m.mx, m.my) in
      let hit box = List.find_opt (fun (i, _) -> Widget.contains (box i) m.mx m.my) in
      let pressed = m.mdown && not model.was_down in
      match (model.drag, pressed) with
      | Some _, _ when m.mdown -> drag model p
      | Some _, _ -> release model
      | None, true when on_picture p -> press model p
      | None, true -> (
          match hit tool_box (List.mapi (fun i t -> (i, t)) tools) with
          | Some (_, tool) -> { (deselect model) with tool }
          | None -> (
              match hit swatch_box (List.mapi (fun i p -> (i, p)) Pattern.palette) with
              | Some (_, pattern) -> { model with pattern }
              | None -> model))
      | None, false -> model
  in
  let model = keyboard computer model in
  { model with was_down = m.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* marching ants: a dotted rectangle whose dashes move along it, which
   is how a selection shows it is alive *)
let ants computer (l, t, r, b) =
  let phase = int_of_float (spin 0.5 computer.time /. 45.) in
  let dash x y = dots (if (x + y + phase) mod 8 < 4 then black else white) (x, y, 1, 1) in
  List.init (r - l + 1) (fun i -> dash (l + i) t)
  @ List.init (r - l + 1) (fun i -> dash (l + i) b)
  @ List.init (b - t + 1) (fun i -> dash l (t + i))
  @ List.init (b - t + 1) (fun i -> dash r (t + i))

(* The picture's rectangles are recomputed only when the picture
   changed -- Elm's lazy, a drawing remembered for the value it was
   made from. Comparing by == (the same value, not an equal one) is
   instant, and it is right because a picture in the history is never
   changed in place (Bitmap.change): the same bitmap is the same
   picture. Between two strokes, most frames draw the one before. *)
let drawn : (Bitmap.t * shape list) option ref = ref None

let picture_shapes b =
  match !drawn with
  | Some (b', shapes) when b' == b -> shapes
  | _ ->
      let shapes = List.map (dots black) (Bitmap.rectangles b) in
      drawn := Some (b, shapes);
      shapes

(* the palette never changes: drawn once *)
let swatches = lazy (List.concat (List.mapi (fun i p -> swatch (swatch_box i) p) Pattern.palette))

let view computer model =
  let th = Gui.theme () in
  let pw = float_of_int pic_w *. zoom and ph = float_of_int pic_h *. zoom in
  let cx = pic_left +. (pw /. 2.) and cy = pic_top -. (ph /. 2.) in
  let window =
    [
      (* the window: a frame, a striped title bar with its name *)
      rectangle black (pw +. 4.) (ph +. 30.) |> move cx (cy +. 13.);
      rectangle white pw 24. |> move cx (pic_top +. 13.);
    ]
    @ List.init 6 (fun i -> rectangle black (pw -. 20.) 1. |> move cx (pic_top +. 4. +. (float_of_int i *. 3.5)))
    @ [ rectangle white 100. 24. |> move cx (pic_top +. 13.); words black "untitled" |> move cx (pic_top +. 13.) ]
    @ [ rectangle white pw ph |> move cx cy ]
  in
  let picture = picture_shapes (picture model) in
  let marquee = match selected model with Some sel -> ants computer sel | None -> [] in
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
  let patterns = swatch current_box model.pattern @ Lazy.force swatches in
  let status =
    Printf.sprintf "%s     %s     %s" (name model.tool)
      (match Undo.undo_name model.history with Some n -> "Undo " ^ n | None -> "")
      (if Clipboard.has model.clip then "a piece on the clipboard" else "")
  in
  [
    (* the Mac's desktop grey, and its menu bar *)
    rectangle (rgb 150 150 150) 1000. 1000.;
    rectangle th.face 1000. 40. |> move 0. 470.;
  ]
  @ window @ picture @ marquee @ palette @ patterns
  @ [ words (rgb 40 40 40) status |> move 0. (-440.) ]
  @ Gui.draw ()

let app = game view update initial
let main = Playground_platform.run_app app
