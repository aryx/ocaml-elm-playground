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
 * (docs/claude_notes/plans/plan_gui_teaching.md, phases 1 and 2; the
 * toolkit underneath is gui/, explained in notes_gui.md).
 *
 * The whole point is what [update] looks like: the widgets are asked
 * for there, as questions about this frame --
 *
 *   if Gui.button_in computer box "reset" then initial else ...
 *   radius = Gui.slider_in computer box ~from ~to_ model.radius
 *
 * -- and [view] takes their shapes with [Gui.draw ()]. No button
 * object, no callback, no message type: that is immediate mode, and
 * it fits because Playground.game's update is
 * [computer -> 'memory -> 'memory], with nowhere for a callback to go.
 *
 * Where each one goes is the other half, and a value of its own:
 * [screen] below is a layout -- a column of widgets on the left, the
 * disc on the right, space shared out between them -- and
 * [Layout.arrange] turns it into a rectangle per name. Being a value,
 * and a pure function of the room available, it is computed by
 * [update] to ask the widgets and again by [view] to draw the disc,
 * with no state in between. The panel's widgets are [stretch]ed, so
 * they all come out the width of the widest.
 *
 * Compare with examples/Typing.ml, which draws its field and its
 * boxes by hand: the same pixels, none of the mouse logic (a press
 * that ends outside is not a click, a slider dragged off itself keeps
 * the mouse) that a widget has and hand-placed [words] never do.
 *
 * What it deliberately does not do: text fields (typing needs focus,
 * phase 3).
 *
 * Exercises: a widget of your own (a group of radio buttons is the
 * smallest interesting one: only one of them is on); a second theme,
 * switched with a key, to see what Theme.t is for; a panel that lays
 * itself out for a window twice the size (which needs
 * Playground's Resized, still a TODO).
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = { radius : number; speed : number; spinning : bool; angle : number }

let initial = { radius = 80.; speed = 1.; spinning = true; angle = 0. }

(* what the layout gives a rectangle for: the widgets, and the two
 * things [view] draws itself *)
type slot =
  | Title
  | Reset
  | Radius_label
  | Radius
  | Speed_label
  | Speed
  | Spinning
  | Readout
  | Disc

(* the panel: a column of widgets, each asking the theme how big it
 * wants to be, stretched to one width *)
let panel =
  Layout.(
    column ~gap:6.
      [
        leaf Title (Gui.label_size "the widgets:");
        stretch (leaf Reset (Gui.button_size "reset"));
        space 14.;
        leaf Radius_label (Gui.label_size "radius:");
        stretch (leaf Radius (Gui.slider_size ()));
        space 14.;
        leaf Speed_label (Gui.label_size "turns per second:");
        stretch (leaf Speed (Gui.slider_size ()));
        space 14.;
        leaf Spinning (Gui.checkbox_size "spinning");
      ])

(* and the screen: the panel on the left, the disc on the right, the
 * spacer between them taking whatever is left over *)
let screen =
  Layout.(
    pad 60.
      (row ~gap:40.
         [
           center panel;
           spacer;
           center (column ~gap:16. [ leaf Readout (Gui.label_size "radius 150"); leaf Disc (320., 320.) ]);
         ]))

let places computer = Layout.arrange (Gui.area computer) screen

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update computer model =
  let at = places computer in
  let box slot = List.assoc slot at in
  Gui.label_in computer (box Title) "the widgets:";
  let reset = Gui.button_in computer (box Reset) "reset" in
  Gui.label_in computer (box Radius_label) "radius:";
  let radius = Gui.slider_in computer (box Radius) ~from:10. ~to_:150. model.radius in
  Gui.label_in computer (box Speed_label) "turns per second:";
  let speed = Gui.slider_in computer (box Speed) ~from:0. ~to_:2. model.speed in
  let spinning = Gui.checkbox_in computer (box Spinning) "spinning" model.spinning in
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

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view computer model =
  let s = computer.screen in
  let at = places computer in
  let readout : Widget.box = List.assoc Readout at in
  let disc : Widget.box = List.assoc Disc at in
  [ rectangle (Gui.theme ()).background s.width s.height ]
  @ Gui.draw ()
  @ [
      words black (Printf.sprintf "radius %.0f" model.radius)
      |> move readout.x readout.y;
      group
        [
          circle blue model.radius;
          (* the hand of a clock, to see it turn *)
          rectangle white (model.radius *. 0.8) 6. |> move_x (model.radius *. 0.4);
        ]
      |> rotate model.angle |> move disc.x disc.y;
    ]

let app = game view update initial
let main = Playground_platform.run_app app
