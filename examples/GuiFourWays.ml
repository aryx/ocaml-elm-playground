(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The same programs, four ways: 7GUIs' Counter, Temperature
 * Converter, Flight Booker, Timer and Circle Drawer, each with callbacks, with MVC,
 * with MVU, and in immediate mode -- all four running at once, side by
 * side, on one screen; the menu at the top picks the task
 * (docs/claude_notes/tutorials/notes_gui.md section 4).
 *
 * This is the teaching artifact the whole gui/ corner exists for. The
 * four columns draw the same widgets, through the same Look, on the
 * same Widget.input: everything that differs between them is
 * *wiring*, and it is in examples/gui4/, one file per task with its
 * four versions one under the other (Gui4Counter.ml,
 * Gui4Temperature.ml, Gui4Flight.ml, Gui4Timer.ml, Gui4Circles.ml). examples/gui4/tests/Unit_gui4 checks that, fed
 * the same session, the four paint the same picture frame after frame.
 *
 *   callbacks   the state is in the widgets and in refs, and every
 *               callback keeps the rest in step -- by hand, every
 *               time, in every callback that can break it
 *   MVC         the state is in a model; the views re-read it when
 *               the model says it changed; the controllers are the
 *               callbacks that change the model
 *   MVU         the state is the model; the view is a function of it,
 *               rebuilt every frame; an action is a message, and
 *               update answers it with a new model
 *   immediate   no widget and no state: [if button then ...], every
 *               frame
 *
 * The numbers, counted in examples/gui4 (code lines, each version's own
 * code, the shared rules and layout left out) are in notes_gui.md
 * section 16. The length is not the point -- at this size they are all
 * short. What differs is *where the state lives*, and the task where
 * that stops being a matter of taste is Flight Booker: with callbacks,
 * every handler must call the one check that turns Book on and off,
 * and forgetting it in one is a bug nobody sees until that handler
 * runs. Circle Drawer is the same lesson at full size: a canvas whose
 * picture every callback must repaint, a popup, a dialog, and an undo
 * whose steps are not the program's events (a whole drag of the
 * slider is one) -- where MVU's view, a function of the model, has
 * nothing to keep in line at all.
 *
 * What writing the four ways of each task found, since the test
 * insisted they paint alike: in immediate mode a widget is drawn when
 * it is asked for, so the order matters -- a menu asked for last turned
 * the return date on a frame late (the menu now goes first, its items
 * on the toolkit's overlay), and two fields shared one caret the
 * unfocused one could move; the retained toolkit put no caret where a
 * click landed; and the four disagreed about whether clicking a button
 * takes the keys from a field (they now agree it does not, the Mac's
 * rule). Circle Drawer found that the same slider arithmetic, written
 * once in each toolkit, rounded differently on arm64 (a multiply and
 * an add fused into one instruction in one copy only): the toolkits
 * now share Look.slider_value.
 *
 * Right-click a circle for its menu: the playground's right button,
 * which the widgets now see (Widget.input's mrdown), as they do the
 * canvas and the context menu every toolkit grew for this task.
 *
 * Exercises: CRUD four ways -- the one where the retained list holds a
 * row and the program must translate it back to a person after every
 * filter; a fifth column for signals (the observable-per-value idea
 * SolidJS and Svelte brought back); break the callbacks column on
 * purpose, by forgetting a check in Gui4Flight.ml, and watch the test
 * fail.
 *)
open Playground

let theme = Gui.theme ()

(* the tasks, and the four ways of each, built once: a retained tree
   decides its rectangles before anything knows how big the screen is,
   and the screen is 1000 x 1000 here (Playground.to_screen) *)
let tasks =
  [
    ("Counter", Gui4Counter.make);
    ("Temperature", Gui4Temperature.make);
    ("Flight Booker", Gui4Flight.make);
    ("Timer", Gui4Timer.make);
    ("Circle Drawer", Gui4Circles.make);
  ]

let panel n : Widget.box = { Widget.x = -375. +. (float_of_int n *. 250.); y = -30.; w = 240.; h = 440. }

let runners =
  List.map (fun (_, make) -> List.mapi (fun n (arch, _) -> make theme (panel n) arch) Gui4.architectures) tasks

let menu_box : Widget.box = { Widget.x = 0.; y = 300.; w = 200.; h = 32. }
let painted = ref []

(* the model is only which task is shown: the four programs keep their
   own state, each its own way *)
let update computer task =
  let was_modal = Gui.modal () in
  let task = Gui.menu_in computer menu_box (List.map fst tasks) task in
  (* while the menu has the mouse -- and on the frame it lets go of it --
     the four programs do not see it, or the click that picks an item
     would land on what is under it too *)
  let input = Gui.input computer in
  let input =
    if was_modal || Gui.modal () then { input with mx = 1e6; my = 1e6; mdown = false; mclick = false; mrdown = false } else input
  in
  painted := List.map (fun (r : Gui4.runner) -> r.step input) (List.nth runners task);
  task

let view computer task =
  let s = computer.screen in
  (rectangle theme.background s.width s.height :: List.concat_map Gui.shapes !painted)
  @ List.mapi (fun n (_, name) -> words black name |> move (panel n).x 220.) Gui4.architectures
  @ List.map (fun n -> rectangle (rgb 200 200 200) 1. 440. |> move ((panel n).x +. 125.) (-30.)) [ 0; 1; 2 ]
  @ [
      words black (Printf.sprintf "%s, four architectures" (fst (List.nth tasks task))) |> move_y 360.;
      words (rgb 120 120 120) "the same widgets, the same paint: only the wiring differs" |> move_y 260.;
      words (rgb 120 120 120) "the wiring: examples/gui4/, one file per task, the four ways one under the other"
      |> move_y (-290.);
    ]
  (* last, so that the menu's items are over the four columns *)
  @ Gui.draw ()

let app = game view update 0
let main = Playground_platform.run_app app
