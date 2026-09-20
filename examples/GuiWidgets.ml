(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Widgets, in the playground's idiom
 * (docs/claude_notes/plans/plan_gui_teaching.md, phase 1; the toolkit
 * underneath is gui/, explained in notes_gui.md).
 *
 * The whole point is what [update] looks like: the widgets are asked
 * for there, as questions about this frame --
 *
 *   if Gui.button computer ~at "reset" then initial else ...
 *   radius = Gui.slider computer ~at ~from ~to_ model.radius
 *
 * -- and [view] takes their shapes with [Gui.draw ()]. No button
 * object, no callback, no message type: that is immediate mode, and
 * it fits because Playground.game's update is
 * [computer -> 'memory -> 'memory], with nowhere for a callback to go.
 *
 * Compare with examples/Typing.ml, which draws its field and its
 * boxes by hand: the same pixels, none of the mouse logic (a press
 * that ends outside is not a click, a slider dragged off itself keeps
 * the mouse) that a widget has and hand-placed [words] never do.
 *
 * What it deliberately does not do: text fields (typing needs focus,
 * phase 3), rows and columns (layout, phase 2) -- every widget here
 * is placed by hand at an absolute position, which is fine until the
 * first resize.
 *)
open Playground

type model = { radius : number; speed : number; spinning : bool; angle : number }

let initial = { radius = 80.; speed = 1.; spinning = true; angle = 0. }

(* the left column: a label above each widget, since a slider does not
 * know what it is for *)
let col = -230.

let update computer model =
  Gui.label computer ~at:(col, 190.) "reset it all:";
  let reset = Gui.button computer ~at:(col, 150.) "reset" in
  Gui.label computer ~at:(col, 100.) "radius:";
  let radius = Gui.slider computer ~at:(col, 60.) ~from:10. ~to_:150. model.radius in
  Gui.label computer ~at:(col, 10.) "turns per second:";
  let speed = Gui.slider computer ~at:(col, -30.) ~from:0. ~to_:2. model.speed in
  let spinning = Gui.checkbox computer ~at:(col, -90.) "spinning" model.spinning in
  if reset then initial
  else
    {
      radius;
      speed;
      spinning;
      (* claude: 60 frames a second, so a turn per second is 6 degrees
       * a frame *)
      angle = (if spinning then model.angle +. (6. *. speed) else model.angle);
    }

let view computer model =
  let s = computer.screen in
  [ rectangle (Gui.theme ()).background s.width s.height ]
  @ Gui.draw ()
  @ [
      words black (Printf.sprintf "radius %.0f" model.radius) |> move 180. 190.;
      group
        [
          circle blue model.radius;
          (* the hand of a clock, to see it turn *)
          rectangle white (model.radius *. 0.8) 6. |> move_x (model.radius *. 0.4);
        ]
      |> rotate model.angle |> move 180. 0.;
    ]

let app = game view update initial
let main = Playground_platform.run_app app
