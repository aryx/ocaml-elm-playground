(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A text you can edit, and the structure underneath it
 * (docs/claude_notes/plans/plan_gui_teaching.md, phase 5; the piece
 * table is gui/Text_edit, explained in notes_gui.md section 7).
 *
 * Type into it, select by dragging or with shift and the arrows,
 * Control-Z to go back, Control-Y (or Control-Shift-Z) to go forward
 * again. The line at the bottom is the point of the example: it shows
 * what the structure underneath is doing --
 *
 *   pieces    how many pieces the table is in. The text starts as
 *             one; every edit splits at most two more out of it, and
 *             typing one letter after another extends the last piece
 *             instead of adding one, so the number stays small next
 *             to the number of keystrokes
 *   versions  how far back you can go. Each is a list of pieces and
 *             nothing else: the text itself was never copied, and
 *             the original was never touched
 *
 * That is the whole argument for a piece table over a gap buffer or a
 * string, and it is a line of numbers on the screen rather than a
 * claim.
 *
 * The model of this program is a Text_edit.t and a bool, which is the
 * other half of the lesson: the text, its caret, its selection and
 * its history are *one value* in the model, so a program can save it,
 * replay it or hand it to somebody else without asking the toolkit
 * for anything.
 *
 * What it deliberately does not do: cut and paste (a clipboard is
 * appkits/document, phase 6), a scroll bar (it scrolls to keep the
 * caret in view, and that is all), styles, or optimal line breaking
 * -- the wrap here is greedy, and Knuth-Plass is TinyWord's, in phase
 * 9.
 *)
open Playground

let opening =
  "A piece table keeps the original text, an append buffer, and a \
   list of pieces saying what to read from where.\n\n\
   Nothing is ever overwritten, so undo is just the older list of \
   pieces -- try it: type here, then press Control-Z.\n"

(* the whole model: a text that carries its caret, its selection and
   its history *)
let initial = Text_edit.of_string opening

type slot = Title | Area | Undo | Redo | Reset | Status

let panel =
  Layout.(
    center
      (column ~gap:12.
         [
           leaf Title (Gui.label_size "a text, and the pieces it is made of");
           stretch (leaf Area (560., 260.));
           row ~gap:12.
             [
               leaf Undo (Gui.button_size "undo");
               leaf Redo (Gui.button_size "redo");
               (* a fixed gap and not a [spacer]: a spacer takes
                  whatever room it is offered, and the [center] above
                  offers the whole screen, which would make this
                  column as wide as the window *)
               space 60.;
               leaf Reset (Gui.button_size "reset");
             ];
           leaf Status (Gui.label_size "pieces 00   versions back 00   forward 00   caret 0000");
         ]))

let places computer = Layout.arrange (Gui.area computer) panel

let update computer model =
  let at = places computer in
  let box slot = List.assoc slot at in
  Gui.label_in computer (box Title) "a text, and the pieces it is made of";
  let text = Gui.text_area_in computer (box Area) model in
  let text = if Gui.button_in computer (box Undo) "undo" then Text_edit.undo text else text in
  let text = if Gui.button_in computer (box Redo) "redo" then Text_edit.redo text else text in
  let text = if Gui.button_in computer (box Reset) "reset" then Text_edit.of_string opening else text in
  Gui.label_in computer (box Status)
    (Printf.sprintf "pieces %d   versions back %d   forward %d   caret %d"
       (Text_edit.pieces text) (Text_edit.undos text) (Text_edit.redos text) (Text_edit.caret text));
  text

let view computer model =
  let s = computer.screen in
  (rectangle (Gui.theme ()).background s.width s.height :: Gui.draw ())
  @ [
      words (rgb 120 120 120)
        "click to put the caret, drag to select, shift and the arrows to extend"
      |> move_y (-260.);
      words (rgb 120 120 120) "Control-Z goes back, Control-Y forward" |> move_y (-290.);
      (* what is selected, in figures: a selection is two positions *)
      words (rgb 120 120 120)
        (let a, b = Text_edit.range model in
         Printf.sprintf "selection %d..%d (%d characters)" a b (b - a))
      |> move_y (-320.);
    ]

let app = game view update initial
let main = Playground_platform.run_app app
