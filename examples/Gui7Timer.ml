(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* 7GUIs, task 4: Timer
 * (docs/claude_notes/plans/plan_gui_teaching.md, phase 3).
 *
 * A bar that fills as time passes, the elapsed time in figures, a
 * slider for the duration, and a reset button. The task is about
 * *concurrency*: something changes without anybody touching anything,
 * and the interface must follow.
 *
 * Which is the one 7GUIs task this playground answers before it is
 * asked. A game is already a program where the world moves on its
 * own: update runs 60 times a second whether or not there was an
 * input, and view draws whatever the model then says. There is no
 * timer to start, no subscription to remember to cancel, no callback
 * that fires after the widget it was going to update is gone -- three
 * of the classic bugs of this task, and none of them can be written
 * here.
 *
 * The two rules it does ask for:
 *   - the bar stops at the duration, and time keeps being measured
 *     (drag the duration back up and it carries on filling);
 *   - dragging the duration below the elapsed time fills the bar
 *     completely, and does not reset anything.
 *
 * Elapsed time is counted in *frames*, not from the clock: the
 * playground's update is a fixed 1/60 of a second (Playground.game),
 * which keeps this deterministic -- the same run gives the same
 * picture, which is what the golden frames need and what -fixed-time
 * would otherwise freeze.
 *
 * Exercises: a pause button (and whether pausing is a mode or a
 * duration of zero); several timers at once; elapsed time taken from
 * computer.time instead of frames, and what that does to the golden
 * frame under -fixed-time.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = { frames : int; duration : number (* seconds *) }

let initial = { frames = 0; duration = 10. }
let elapsed model = float_of_int model.frames /. 60.

type slot = Bar_label | Bar | Figure | Duration_label | Duration | Reset

let panel =
  Layout.(
    center
      (column ~gap:10.
         [
           leaf Bar_label (Gui.label_size "Elapsed Time");
           stretch (leaf Bar (Gui.progress_size ()));
           leaf Figure (Gui.label_size "10.0s");
           space 16.;
           leaf Duration_label (Gui.label_size "Duration");
           stretch (leaf Duration (Gui.slider_size ()));
           space 16.;
           stretch (leaf Reset (Gui.button_size "Reset"));
         ]))

let places computer = Layout.arrange (Gui.area computer) panel

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update computer model =
  let at = places computer in
  let box slot = List.assoc slot at in
  let elapsed = elapsed model in
  Gui.label_in computer (box Bar_label) "Elapsed Time";
  (* the bar is full once the elapsed time reaches the duration; a
     duration of zero is all of it, not a division by zero *)
  Gui.progress_in computer (box Bar)
    (if model.duration <= 0. then 1. else min 1. (elapsed /. model.duration));
  Gui.label_in computer (box Figure) (Printf.sprintf "%.1fs" elapsed);
  Gui.label_in computer (box Duration_label) "Duration";
  let duration = Gui.slider_in computer (box Duration) ~from:0. ~to_:30. model.duration in
  let reset = Gui.button_in computer (box Reset) "Reset" in
  (* time passes whatever the duration says: turning the duration back
     up carries on from where the clock really is *)
  { frames = (if reset then 0 else model.frames + 1); duration }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view computer model =
  let s = computer.screen in
  (rectangle (Gui.theme ()).background s.width s.height :: Gui.draw ())
  @ [
      words black "7GUIs 4: Timer" |> move_y 240.;
      words (rgb 120 120 120) (Printf.sprintf "duration %.1fs" model.duration) |> move_y (-200.);
    ]

let app = game view update initial
let main = Playground_platform.run_app app
