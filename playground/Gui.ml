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

let box ~at:(x, y) (w, h) : Widget.box = { Widget.x; y; w; h }

let button computer ~at s =
  widget computer (fun u ->
      Immediate.button u (box ~at (Immediate.button_size (Immediate.theme u) s)) s)

let checkbox computer ~at s checked =
  widget computer (fun u ->
      Immediate.checkbox u
        (box ~at (Immediate.checkbox_size (Immediate.theme u) s))
        s checked)

let slider computer ~at ~from ~to_ v =
  widget computer (fun u ->
      Immediate.slider u (box ~at (Immediate.slider_size (Immediate.theme u))) ~from ~to_ v)

let label computer ~at s =
  widget computer (fun u ->
      let th = Immediate.theme u in
      (Immediate.label u (box ~at (Widget.text_width ~size:th.text_size s, th.row)) s, ()))

let shape_of_paint = function
  | Widget.Fill (color, (b : Widget.box)) -> rectangle color b.w b.h |> move b.x b.y
  | Widget.Text (color, (b : Widget.box), s) ->
      words color s |> scale (b.h /. words_font_size) |> move b.x b.y

let draw () =
  closed := true;
  Immediate.paint !ui |> List.map shape_of_paint

let theme () = Immediate.theme !ui
let set_theme th = ui := Immediate.set_theme th !ui
