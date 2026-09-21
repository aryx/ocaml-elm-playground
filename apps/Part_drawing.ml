(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

let kind = "drawing"
let magic = "drawing 1"

(* the drawing's own page, y up, fitted to whatever rectangle the part
   is given *)
let page_w = 300.
let page_h = 200.

type state = {
  d : Drawing.t;
  selection : Drawing.id list;
  (* a drag: moving the selection (the mouse last here), or resizing
     a figure by a handle *)
  moving : Figure.point option;
  resizing : (Drawing.id * int) option;
  (* how tall it may grow: a third of a screen in a document, a page
     when it is the document *)
  max_h : float;
  was_down : bool;
  was : string list;
}

(* the page as big as the part's rectangle allows, against its left *)
let zoom (b : Widget.box) = Float.min (b.w /. page_w) (b.h /. page_h)

(* the part's rectangle and the page, both ways *)
let to_page (b : Widget.box) (x, y) = ((x -. Widget.left b) /. zoom b, (y -. Widget.bottom b) /. zoom b)

let draw st (b : Widget.box) ~active =
  let k = zoom b in
  let handles =
    if not active then []
    else
      List.concat_map
        (fun id ->
          match Drawing.get st.d id with
          | Some f -> List.map (fun (x, y) -> rectangle black (6. /. k) (6. /. k) |> move x y) (Figure.handles f)
          | None -> [])
        st.selection
  in
  let page = rectangle white page_w page_h |> move (page_w /. 2.) (page_h /. 2.) in
  let shapes = (page :: List.concat_map (fun (_, f) -> Figure_shapes.figure f) (Drawing.figures st.d)) @ handles in
  (* the page's origin, its bottom-left, on the part's *)
  [ group shapes |> scale k |> move (Widget.left b) (Widget.bottom b) ]
  @ Gui.shapes
      (Widget.frame (rgb 120 120 120) 1.
         { Widget.x = Widget.left b +. (page_w *. k /. 2.); y = Widget.bottom b +. (page_h *. k /. 2.); w = page_w *. k; h = page_h *. k })

let input computer (b : Widget.box) st =
  let m = computer.mouse in
  let now = Set_.elements computer.keyboard.keys in
  let pressed key = List.mem key now && not (List.mem key st.was) in
  let p = to_page b (m.mx, m.my) in
  let press = m.mdown && not st.was_down in
  let st =
    if press && Widget.contains b m.mx m.my then
      (* a handle of the one selected figure, or a figure, or nothing *)
      let handle =
        match st.selection with
        | [ id ] -> (
            match Drawing.get st.d id with
            | Some f ->
                let tol = 6. /. zoom b in
                List.find_opt (fun (_, (hx, hy)) -> Float.abs (hx -. fst p) <= tol && Float.abs (hy -. snd p) <= tol) (List.mapi (fun i h -> (i, h)) (Figure.handles f))
                |> Option.map (fun (i, _) -> (id, i))
            | None -> None)
        | _ -> None
      in
      match (handle, Drawing.at ~tolerance:(4. /. zoom b) st.d p) with
      | Some h, _ -> { st with resizing = Some h }
      | None, Some id ->
          let selection =
            if computer.keyboard.kshift then if List.mem id st.selection then List.filter (( <> ) id) st.selection else st.selection @ [ id ]
            else if List.mem id st.selection then st.selection
            else [ id ]
          in
          { st with selection; moving = (if List.mem id selection then Some p else None) }
      | None, None -> { st with selection = [] }
    else if m.mdown then
      match (st.moving, st.resizing) with
      | Some (lx, ly), _ -> { st with d = Drawing.move st.selection (fst p -. lx) (snd p -. ly) st.d; moving = Some p }
      | None, Some (id, h) -> { st with d = Drawing.update id (fun f -> Figure.drag_handle f h p) st.d }
      | None, None -> st
    else { st with moving = None; resizing = None }
  in
  let st = if pressed "Backspace" || pressed "Delete" then { st with d = Drawing.delete st.selection st.d; selection = [] } else st in
  { st with was_down = m.mdown; was = now }

(* a new shape, in the middle of the page, selected *)
let add st f =
  let d, id = Drawing.add f st.d in
  { st with d; selection = [ id ] }

let style = { Figure.fill = Some 1.; pen = 2. }
let middle w h = Figure.box ((page_w -. w) /. 2., (page_h -. h) /. 2.) ((page_w +. w) /. 2., (page_h +. h) /. 2.)

let command c st =
  let restyle f = { st with d = List.fold_left (fun d id -> Drawing.update id (Figure.restyle f) d) st.d st.selection } in
  match c with
  | "Rectangle" -> add st (Figure.Rect (middle 90. 60., style))
  | "Oval" -> add st (Figure.Oval (middle 90. 60., style))
  | "Line" -> add st (Figure.Line ((110., 70.), (190., 130.), { style with fill = None }))
  | "Grey" -> restyle (fun s -> { s with fill = Some 0.55 })
  | "White" -> restyle (fun s -> { s with fill = Some 1. })
  | "Hollow" -> restyle (fun s -> { s with fill = None })
  | "Bring to Front" -> { st with d = Drawing.to_front st.selection st.d }
  | "Send to Back" -> { st with d = Drawing.to_back st.selection st.d }
  | "Delete" -> { st with d = Drawing.delete st.selection st.d; selection = [] }
  | _ -> st

let rec part st : Component.part =
  {
    kind;
    (* the page's shape at the width given, but no taller than a
       third of a screen *)
    height = (fun w -> Float.min st.max_h (w *. page_h /. page_w));
    (* it fits its page to whatever room it is given already *)
    natural = None;
    draw = draw st;
    input = (fun computer b -> part (input computer b st));
    menu = [ "Drawing"; "Rectangle"; "Oval"; "Line"; "Grey"; "White"; "Hollow"; "Bring to Front"; "Send to Back"; "Delete" ];
    command = (fun c -> part (command c st));
    (* a drawing is data: Marshal writes it, behind its line *)
    save = (fun () -> Saved.to_string ~magic st.d);
  }

let make ?(max_height = 220.) d =
  part { d; selection = []; moving = None; resizing = None; max_h = max_height; was_down = false; was = [] }

let load s = match Saved.of_string ~magic s with Some d -> make d | None -> Component.placeholder ~kind s
