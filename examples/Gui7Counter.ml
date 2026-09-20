(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* 7GUIs, task 1: Counter
 * (docs/claude_notes/plans/plan_gui_teaching.md, phase 3).
 *
 * 7GUIs (Eugen Kiss, 2014) is seven tasks chosen to expose where each
 * way of building an interface hurts -- a benchmark for GUI toolkits
 * the way n-body is one for physics engines. The first is the
 * smallest program that has an interface at all: a number and a
 * button that increases it.
 *
 * Trivial, and that is the point: it is the baseline every
 * architecture is measured against, and here it is four lines of
 * update. In phase 4 the same task will be written with callbacks,
 * with MVC and in MVU, beside this immediate-mode one, and the
 * comparison starts from how little there is to compare.
 *
 * What the task asks for: a read-only field showing the count, and a
 * button. The field is read-only here in the simplest possible way --
 * a label -- since a field you cannot type in is a label with a box
 * around it.
 *)
open Playground

(* the whole model: the count. 7GUIs' baseline is a program with one
   number in it *)
let initial = 0

type slot = Count | Button

let panel =
  Layout.(
    center
      (column ~gap:10.
         [ leaf Count (Gui.field_size ()); stretch (leaf Button (Gui.button_size "count")) ]))

let places computer = Layout.arrange (Gui.area computer) panel

let update computer model =
  let at = places computer in
  Gui.label_in computer (List.assoc Count at) (string_of_int model);
  if Gui.button_in computer (List.assoc Button at) "count" then model + 1 else model

let view computer _model =
  let s = computer.screen in
  (rectangle (Gui.theme ()).background s.width s.height :: Gui.draw ())
  @ [ words black "7GUIs 1: Counter" |> move_y 200. ]

let app = game view update initial
let main = Playground_platform.run_app app
