(* A music part's widgets (Part_hammond.mli): its own immediate-mode
 * toolkit, an Immediate.t kept in the part's state, not the Playground's
 * one global Gui. That is what lets a part be drawn anywhere, scaled
 * (Component.draw_in): its widgets are painted in the part's own
 * coordinates, and the mouse is moved into them before they see it.
 *
 *   host's screen                 the part's own coordinates
 *   +------------------+          +-------------+
 *   |   +---------+    |   -dx    |  knob at    |
 *   |   | knob .  |    |  ---->   |  (180, 250) |
 *   |   +---------+    |   -dy    +-------------+
 *   +------------------+
 *
 * A part draws in its own coordinates and moves its shapes by
 * (dx, dy); the mouse comes the other way. *)

(* what the person does now, the mouse moved by (-dx, -dy) into the
   part's coordinates *)
val input : Playground.computer -> dx:float -> dy:float -> Widget.input

(* nobody's mouse, far away: the first paint of a part never touched *)
val neutral : Widget.input

(* a selector drawn as the rotary switch (Look's), or as a knob that
   turns in steps (the Reface's way) *)
type selector_look = Rotary | Stepped_knob

(* [control ui ~selector ~at c v]: the widget a voice's knob asks for,
   by its kind (Control.t), centred at [at], and its value after this
   frame *)
val control : Immediate.t -> selector:selector_look -> at:float * float -> Control.t -> float -> Immediate.t * float

(* the widgets' paint of this frame, moved by (dx, dy) *)
val shapes : Immediate.t -> dx:float -> dy:float -> Playground.shape list
