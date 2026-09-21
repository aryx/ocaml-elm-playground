(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The same program, four ways: 7GUIs' Counter with callbacks, with
 * MVC, with MVU, and in immediate mode -- all four running at once,
 * side by side, on one screen
 * (docs/claude_notes/plans/plan_gui_teaching.md, phase 4;
 * notes_gui.md section 4).
 *
 * This is the teaching artifact the whole gui/ corner exists for. The
 * four columns below draw the same widgets, through the same Look, on
 * the same Widget.input: everything that differs between them is
 * *wiring*, and it is all visible in this one file.
 *
 *   callbacks   the count is in a ref, what you read is in a label,
 *               and the callback keeps them equal -- by hand, every
 *               time, in every callback that can change either
 *   MVC         the count is in the model; the view re-reads it when
 *               the model says it changed; the controller is the
 *               callback that changes the model
 *   MVU         the count is the model; the view is a function of it,
 *               rebuilt every frame; a click is a message, and update
 *               answers it with a new model
 *   immediate   there is no widget and no state: [if button then
 *               count + 1], every frame
 *
 * The numbers, counted in this file (code lines, comments and blanks
 * left out):
 *
 *   callbacks   12 lines, and a ref and a label object of its own
 *   MVC         11 lines, and a model object and an observer
 *   MVU         12 lines, and a model, a message type and a view
 *   immediate    3 lines, and nothing of its own at all: the count is
 *               the playground's model, where a game's score already
 *               lives
 *
 * But the length is not the point -- at this size they are all short,
 * and anyone claiming one is dramatically shorter is choosing the
 * example. What differs is *how many places hold the count*: two with
 * callbacks, one everywhere else. That is the whole of why callbacks
 * lost, and it is visible in a program this small. (7GUIs' Flight
 * Booker is where it stops being a matter of taste; see
 * examples/Gui7Flight.ml, and its four lines of rules.)
 *
 * One more thing this program shows by accident, and it is worth
 * saying: the three retained columns are built *once*, at start-up,
 * so their rectangles are decided before anything knows how big the
 * screen is. A real retained toolkit therefore needs relayout, and a
 * way to tell a widget it moved; immediate mode gets that for free,
 * since it is told where to be every frame.
 *
 * Exercises: write 7GUIs' Flight Booker four ways in this file and
 * count the lines again -- it is the task where the four stop being
 * the same length; add a fifth column for signals (the
 * observable-per-value idea that SolidJS and Svelte brought back);
 * break the callbacks column on purpose, by forgetting the
 * set_text, and watch which columns notice.
 *)
open Playground

let theme = Gui.theme ()

(* the four columns, laid out once: the screen is 1000 x 1000 here
   (Playground.to_screen), which a retained tree has to assume *)
type slot = Title | Count | Bump

let column =
  Layout.(
    center
      (column ~gap:10.
         [
           leaf Title (Gui.label_size "immediate");
           leaf Count (Gui.field_size ());
           stretch (leaf Bump (Gui.button_size "count"));
         ]))

let boxes n =
  let width = 250. in
  let area : Widget.box = { Widget.x = -375. +. (float_of_int n *. width); y = 0.; w = width; h = 300. } in
  Layout.arrange area column

let box n slot : Widget.box = List.assoc slot (boxes n)

(* ------------------------------------------------------------------ *)
(* 1. callbacks: the count is in a ref, the label is told by hand      *)
(* ------------------------------------------------------------------ *)
let cb_count = ref 0
let cb_shown = Retained.label (box 0 Count) "0"

let cb_ui =
  Retained.window
    (Retained.group
       [
         Retained.label (box 0 Title) "callbacks";
         cb_shown;
         Retained.button (box 0 Bump) "count" (fun () ->
             incr cb_count;
             (* the line that is the whole argument: two places hold
                the count, and this is where they are made equal *)
             Retained.set_text cb_shown (string_of_int !cb_count));
       ])

(* ------------------------------------------------------------------ *)
(* 2. MVC: the model is the truth, the view re-reads it when told      *)
(* ------------------------------------------------------------------ *)
let mvc_model = Mvc.create 0
let mvc_shown = Retained.label (box 1 Count) "0"

let mvc_ui =
  Retained.window
    (Retained.group
       [
         Retained.label (box 1 Title) "MVC";
         mvc_shown;
         (* the controller: it changes the model and nothing else *)
         Retained.button (box 1 Bump) "count" (fun () -> Mvc.change mvc_model (fun n -> n + 1));
       ])

(* the view: it never holds the count, it shows it *)
let () = Mvc.on_change mvc_model (fun () -> Retained.set_text mvc_shown (string_of_int (Mvc.get mvc_model)))

(* ------------------------------------------------------------------ *)
(* 3. MVU: a message, an update, a view that is a function of it       *)
(* ------------------------------------------------------------------ *)
type msg = Bumped

let mvu_update Bumped n = n + 1

let mvu_view n =
  Mvu.group
    [
      Mvu.label (box 2 Title) "MVU";
      Mvu.label (box 2 Count) (string_of_int n);
      Mvu.button (box 2 Bump) "count" Bumped;
    ]

let mvu_model = ref 0
let mvu_state = ref Mvu.empty
let mvu_paint = ref []

(* ------------------------------------------------------------------ *)
(* 4. immediate mode: no widget, no state of its own                   *)
(* ------------------------------------------------------------------ *)

let update computer model =
  let input = Gui.input computer in
  (* immediate: the count is the playground's model, and that is all.
     The button is asked for before the label, so that the label shows
     this frame's click -- in immediate mode that ordering is yours to
     choose and plain to see, where in the other three it is decided
     by when the callback runs *)
  Gui.label_in computer (box 3 Title) "immediate";
  let model = if Gui.button_in computer (box 3 Bump) "count" then model + 1 else model in
  Gui.label_in computer (box 3 Count) (string_of_int model);
  (* callbacks, and MVC, which is callbacks plus a model *)
  Retained.handle input cb_ui;
  Retained.handle input mvc_ui;
  (* MVU: view the model, the click becomes a message, update answers
     it, and what is drawn is the view of the new model *)
  let state, m, paint = Mvu.step theme input !mvu_state ~view:mvu_view ~update:mvu_update !mvu_model in
  mvu_state := state;
  mvu_paint := paint;
  mvu_model := m;
  model

let view computer model =
  let s = computer.screen in
  (rectangle theme.background s.width s.height :: Gui.draw ())
  @ Gui.shapes (Retained.paint theme cb_ui)
  @ Gui.shapes (Retained.paint theme mvc_ui)
  @ Gui.shapes !mvu_paint
  @ [
      words black "one counter, four architectures" |> move_y 260.;
      words (rgb 120 120 120) "the same widgets, the same paint: only the wiring differs"
      |> move_y 230.;
      words (rgb 120 120 120)
        (Printf.sprintf "places holding the count: callbacks 2, MVC 1, MVU 1, immediate 1 (and it is %d)"
           model)
      |> move_y (-220.);
      words (rgb 120 120 120) "lines of wiring in this file: 12, 11, 12, and 3"
      |> move_y (-190.);
      words (rgb 120 120 120)
        (Printf.sprintf "MVC has told its views %d times" (Mvc.notifications mvc_model))
      |> move_y (-250.);
    ]

let app = game view update 0
let main = Playground_platform.run_app app
