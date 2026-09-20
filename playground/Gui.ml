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

(* See Gui.mli. The adapter between gui/ (rectangles and paint) and
 * the playground (a computer and shapes). *)

(* The frame being built: the widgets update asks for, until view
 * takes them with [draw]. Dear ImGui calls this the context, and has
 * exactly one too. *)
let ui = ref Immediate.empty

(* whether [draw] has taken the last frame, so the next widget starts
 * a new one *)
let closed = ref true

let input_of (computer : computer) : Widget.input =
  let m = computer.mouse and k = computer.keyboard in
  {
    Widget.mx = m.mx;
    my = m.my;
    mdown = m.mdown;
    mclick = m.mclick;
    typed = k.typed;
    wheel = m.mwheel;
    keys = Set_.elements k.keys;
  }

(* every widget goes through here: open the frame if it is the first
 * of this update, ask gui/, keep the new state, return the answer *)
let widget computer f =
  if !closed then (
    ui := Immediate.frame (input_of computer) !ui;
    closed := false);
  let state, answer = f !ui in
  ui := state;
  answer

let theme () = Immediate.theme !ui
let box ~at:(x, y) (w, h) : Widget.box = { Widget.x; y; w; h }

let area (computer : computer) : Widget.box =
  let s = computer.screen in
  { Widget.x = 0.; y = 0.; w = s.width; h = s.height }

(* the widgets, in a rectangle somebody else decided (a layout) *)
let button_in ?enabled computer b s = widget computer (fun u -> Immediate.button ?enabled u b s)

let checkbox_in computer b s checked =
  widget computer (fun u -> Immediate.checkbox u b s checked)

let slider_in computer b ~from ~to_ v =
  widget computer (fun u -> Immediate.slider u b ~from ~to_ v)

let label_in computer b s = widget computer (fun u -> (Immediate.label u b s, ()))
let field_in ?enabled computer b text = widget computer (fun u -> Immediate.field ?enabled u b text)
let text_area_in computer b edit = widget computer (fun u -> Immediate.text_area u b edit)
let progress_in computer b f = widget computer (fun u -> (Immediate.progress u b f, ()))
let menu_in computer b items chosen = widget computer (fun u -> Immediate.menu u b items chosen)

(* how big each one wants to be, for a layout to place *)
let button_size s = Immediate.button_size (theme ()) s
let checkbox_size s = Immediate.checkbox_size (theme ()) s
let slider_size () = Immediate.slider_size (theme ())
let field_size () = Immediate.field_size (theme ())
let text_area_size () = Immediate.text_area_size (theme ())
let progress_size () = Immediate.progress_size (theme ())
let menu_size items = Immediate.menu_size (theme ()) items

let label_size s =
  let th = theme () in
  (Widget.text_width ~size:th.text_size s, th.row)

(* and the same, placed by hand at a point: the simple way, which
 * needs no layout at all *)
let button ?enabled computer ~at s = button_in ?enabled computer (box ~at (button_size s)) s

let checkbox computer ~at s checked =
  checkbox_in computer (box ~at (checkbox_size s)) s checked

let slider computer ~at ~from ~to_ v =
  slider_in computer (box ~at (slider_size ())) ~from ~to_ v

let label computer ~at s = label_in computer (box ~at (label_size s)) s
let field ?enabled computer ~at text = field_in ?enabled computer (box ~at (field_size ())) text
let text_area computer ~at edit = text_area_in computer (box ~at (text_area_size ())) edit
let progress computer ~at f = progress_in computer (box ~at (progress_size ())) f
let menu computer ~at items chosen = menu_in computer (box ~at (menu_size items)) items chosen

let shape_of_paint = function
  | Widget.Fill (color, (b : Widget.box)) -> rectangle color b.w b.h |> move b.x b.y
  | Widget.Text (color, (b : Widget.box), s) ->
      words color s |> scale (b.h /. words_font_size) |> move b.x b.y

let draw () =
  closed := true;
  Immediate.paint !ui |> List.map shape_of_paint

let shapes paint = List.map shape_of_paint paint
let input computer = input_of computer

let set_theme th = ui := Immediate.set_theme th !ui
