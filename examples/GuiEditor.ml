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
 * The model of this program is a document holding that text, which
 * is the other half of the lesson: the text, its caret, its
 * selection and its history are *one value*, so a program can save
 * it, replay it or hand it to somebody else without asking the
 * toolkit for anything.
 *
 * Around it, what every application has (appkits/document, phase 6):
 *
 *   Document   what it holds, what it is called, and whether there
 *              is anything to save -- which is a comparison and not
 *              a flag, so the star in the title goes out by itself
 *              when you undo back to the version you saved. Try it:
 *              type, save, type again, then Control-Z back
 *   Clipboard  Control-C, Control-X, Control-V, in this program only
 *              (the system's clipboard is the backend's business,
 *              see Clipboard.mli)
 *
 * Saving writes nothing: there is no file here, and the browser has
 * none at all. What a document knows is whether it *needs* saving.
 *
 * What it deliberately does not do: a scroll bar (it scrolls to keep
 * the caret in view, and that is all), styles, or optimal line
 * breaking -- the wrap here is greedy, and Knuth-Plass is TinyWord's,
 * in phase 9.
 *
 * Exercises: the system clipboard (SDL has one natively, the browser
 * asks permission); a scroll bar, which needs a scroll position that
 * is not the caret's; coalescing a run of keystrokes into one undo
 * step, which every real editor does and this one does not; a piece
 * *tree* rather than a list, which is what VS Code went to.
 *)
open Playground

let opening =
  "A piece table keeps the original text, an append buffer, and a \
   list of pieces saying what to read from where.\n\n\
   Nothing is ever overwritten, so undo is just the older list of \
   pieces -- try it: type here, then press Control-Z.\n"

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = { doc : Text_edit.t Document.t; clip : Clipboard.t }

(* the document holds a text that carries its own caret, selection and
   history. [equal] compares the texts, because Text_edit's undo
   rebuilds its record rather than keeping the old one, so the pointer
   comparison a document uses by default would never say "clean"
   again (Document.mli) *)
let initial =
  {
    doc =
      Document.create ~path:"notes.txt"
        ~equal:(fun a b -> Text_edit.to_string a = Text_edit.to_string b)
        (Text_edit.of_string opening);
    clip = Clipboard.empty;
  }

type slot = Title | Area | Undo_ | Redo | Save | Status

let panel =
  Layout.(
    center
      (column ~gap:12.
         [
           leaf Title (Gui.label_size "notes.txt *");
           stretch (leaf Area (560., 260.));
           row ~gap:12.
             [
               leaf Undo_ (Gui.button_size "undo");
               leaf Redo (Gui.button_size "redo");
               (* a fixed gap and not a [spacer]: a spacer takes
                  whatever room it is offered, and the [center] above
                  offers the whole screen, which would make this
                  column as wide as the window *)
               space 60.;
               leaf Save (Gui.button_size "save");
             ];
           leaf Status (Gui.label_size "pieces 00   versions back 00   forward 00   caret 0000");
         ]))

let places computer = Layout.arrange (Gui.area computer) panel

(* did this key go down at this frame? The widgets work that out for
   themselves; a program reading a shortcut has to do it by hand, and
   Scene2d.pressed is the same idea for a game *)
let held = ref []

let pressed (computer : computer) key =
  let now = Set_.elements computer.keyboard.keys in
  List.mem key now && not (List.mem key !held)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update computer model =
  let at = places computer in
  let box slot = List.assoc slot at in
  let control = Set_.mem "Control" computer.keyboard.keys in
  Gui.label_in computer (box Title) (Document.title model.doc);
  let text = Document.content model.doc in
  (* cut, copy and paste, which the text area knows nothing about: it
     edits a text, and what is on the clipboard is the program's *)
  let clip, text =
    if control && pressed computer "c" then (Clipboard.put (Text_edit.selected text) model.clip, text)
    else if control && pressed computer "x" then
      (Clipboard.put (Text_edit.selected text) model.clip, Text_edit.delete_backward text)
    else if control && pressed computer "v" then
      (model.clip, match Clipboard.get model.clip with Some s -> Text_edit.insert s text | None -> text)
    else (model.clip, text)
  in
  let text = Gui.text_area_in computer (box Area) text in
  let text = if Gui.button_in computer (box Undo_) "undo" then Text_edit.undo text else text in
  let text = if Gui.button_in computer (box Redo) "redo" then Text_edit.redo text else text in
  let doc = Document.put text model.doc in
  let doc =
    if Gui.button_in ~enabled:(Document.dirty doc) computer (box Save) "save" then
      Document.mark_saved doc
    else doc
  in
  Gui.label_in computer (box Status)
    (Printf.sprintf "pieces %d   versions back %d   forward %d   caret %d"
       (Text_edit.pieces text) (Text_edit.undos text) (Text_edit.redos text) (Text_edit.caret text));
  held := Set_.elements computer.keyboard.keys;
  { doc; clip }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view computer model =
  let s = computer.screen in
  (rectangle (Gui.theme ()).background s.width s.height :: Gui.draw ())
  @ [
      words (rgb 120 120 120)
        "click to put the caret, drag to select, shift and the arrows to extend"
      |> move_y (-260.);
      words (rgb 120 120 120)
        "Control-Z goes back, Control-Y forward, Control-C/X/V copy, cut and paste"
      |> move_y (-290.);
      (* what is selected, in figures: a selection is two positions *)
      words (rgb 120 120 120)
        (let a, b = Text_edit.range (Document.content model.doc) in
         Printf.sprintf "selection %d..%d (%d characters)%s" a b (b - a)
           (match Clipboard.get model.clip with
           | Some s -> Printf.sprintf "   clipboard: %d" (String.length s)
           | None -> "   clipboard: empty"))
      |> move_y (-320.);
    ]

let app = game view update initial
let main = Playground_platform.run_app app
