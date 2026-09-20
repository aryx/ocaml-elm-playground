(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The three inputs an application needs and a game never did
 * (docs/claude_notes/plans/plan_gui_teaching.md, phase 0):
 *
 *   - computer.keyboard.typed: the characters a key press produced,
 *     which the key names in [keys] cannot give ("A" rather than "a"
 *     with shift, an accented letter from a dead key, whatever a
 *     non-US layout puts on a key). Type, and the line grows;
 *     backspace, which produces no character, is read from [kbackspace]
 *     instead -- the two are different questions, and this example is
 *     here to make that concrete.
 *   - computer.mouse.mwheel: notches since the last frame, positive
 *     scrolling up. The list on the right scrolls.
 *   - computer.mouse.mdouble: this frame carried a double click. The
 *     square remembers the last one.
 *
 * All three are transients: set by an event, seen by one update, then
 * cleared -- like mouse.mdx. So a program reads them once and keeps
 * what it wants.
 *
 * What it deliberately does not do: a cursor you can move, a
 * selection, or any widget at all. Those are gui/ (phase 1 onwards);
 * this is only the input.
 *)
open Playground

type model = { line : string; scroll : number; doubles : int }

let initial = { line = ""; scroll = 0.; doubles = 0 }

let update computer model =
  let k = computer.keyboard and m = computer.mouse in
  let line = model.line ^ k.typed in
  (* claude: backspace is a key, not a character: it is in [keys], never
   * in [typed] *)
  let line =
    if k.kbackspace && String.length line > 0 then String.sub line 0 (String.length line - 1)
    else line
  in
  {
    line;
    scroll = max 0. (min 20. (model.scroll +. m.mwheel));
    doubles = (if m.mdouble then model.doubles + 1 else model.doubles);
  }

let view computer model =
  let s = computer.screen in
  (* claude: a blinking caret, with the playground's own wave *)
  let cursor = if wave 0. 1. 1. computer.time > 0.5 then "|" else " " in
  [
    rectangle (rgb 250 250 245) s.width s.height;
    (* what was typed *)
    words black "type something:" |> move_y 200.;
    rectangle white 520. 50. |> move_y 150.;
    rectangle (rgb 200 200 200) 520. 50. |> fade 0.3 |> move_y 150.;
    words black (model.line ^ cursor) |> move_y 150.;
    words (rgb 120 120 120) "(backspace deletes: a key, not a character)" |> move_y 110.;
    (* the wheel *)
    words black "scroll the wheel:" |> move_y 30. |> move_x (-180.);
    rectangle white 200. 120. |> move_y (-50.) |> move_x (-180.);
    words black (Printf.sprintf "%.0f" model.scroll)
    |> scale 2. |> move_y (-50.) |> move_x (-180.);
    (* the double click *)
    words black "double-click here:" |> move_y 30. |> move_x 180.;
    square (if model.doubles mod 2 = 0 then blue else lightPurple) 120.
    |> move_y (-50.) |> move_x 180.;
    words black (string_of_int model.doubles) |> scale 2. |> move_y (-50.) |> move_x 180.;
  ]

let app = game view update initial
let main = Playground_platform.run_app app
