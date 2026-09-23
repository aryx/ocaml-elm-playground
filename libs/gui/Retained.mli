(* The oldest answer: the widgets are objects that own their state,
 * and you hang functions on them (notes_gui.md section 4).
 *
 *   let count = ref 0 in
 *   let shown = label box "0" in
 *   let bump  = button box "count" (fun () ->
 *                 incr count;
 *                 set_text shown (string_of_int !count))
 *
 * Tk (Ousterhout, Tcl 1988, Tk 1991), Motif, Win32, GTK, Swing, and
 * every "drag a button onto a form" tool ever shipped. It is the
 * easiest thing to learn -- a button is a thing, you say what it does
 * -- and it scales badly for one precise reason:
 *
 *   **the truth is scattered.** The count lives in a ref, what the
 *   person reads lives in a label, and keeping the two equal is your
 *   job, in every callback that can change either. Forget one and the
 *   screen says something the program does not believe. Every
 *   refresh-the-view bug in history lives here.
 *
 * "Retained" is the name of the other half of the deal: the widgets
 * are retained between frames, so they keep their own hot, held,
 * focused and caret, and the toolkit walks them rather than being
 * told about them (compare Immediate.mli). That is also why a
 * retained tree can hit test front to back, and why layout can ask a
 * widget how big it wants to be before drawing it -- the two things
 * immediate mode pays for.
 *
 * Here it is small on purpose: the same widgets, drawn by the same
 * Look, so that comparing this with Immediate, Mvc and Mvu compares
 * wiring and nothing else. What is missing is everything a real
 * retained toolkit grows next: destroying widgets, reparenting,
 * relayout on change, and the event *bubbling* that turns a tree of
 * objects into a tree of handlers. *)

(*****************************************************************************)
(* {1 The widget and the window} *)
(*****************************************************************************)

(* a widget: a rectangle, its own state, and what it does *)
type t

(* the window: the widgets, and the little the toolkit itself
 * remembers (the previous frame's keys and mouse button) *)
type ui

(*****************************************************************************)
(* {1 Making widgets} *)
(*****************************************************************************)

val button : Widget.box -> string -> (unit -> unit) -> t
val label : Widget.box -> string -> t

(* [field box text on_change]: its text is its own, and [on_change] is
 * told after every keystroke -- which is the moment the count-and-
 * label problem above starts, because now two places hold the text *)
val field : Widget.box -> string -> (string -> unit) -> t

(* [slider box ~from ~to_ value on_change]: the value is the slider's
 * own, [on_change] told while it is dragged *)
val slider : Widget.box -> from:float -> to_:float -> float -> (float -> unit) -> t

(* a bar filled to a fraction, 0 to 1 *)
val progress : Widget.box -> float -> t

(* [menu box items chosen on_choose]: a dropdown showing its chosen
 * item; while its items show, it has the mouse *)
val menu : Widget.box -> string list -> int -> (int -> unit) -> t

val group : t list -> t

(* [canvas box on_event]: the program's own drawing, which it keeps
 * there with [set_drawing] -- the canvas holds a picture, like a label
 * holds its text, and the program has to put a new one in after every
 * change, in every callback that makes one (the problem above, again,
 * and here the most expensive form of it) *)
val canvas : Widget.box -> (Widget.canvas_event -> unit) -> t
val set_drawing : t -> Widget.paint list -> unit

(* [context_menu items on_close]: a menu that shows only when [popup]
 * opens it at a point, has the mouse while it shows, and closes at the
 * next click -- [on_close (Some i)] on an item, [on_close None]
 * anywhere else. Put it last in the tree, where it is drawn on top. *)
val context_menu : string list -> (int option -> unit) -> t
val popup : t -> float * float -> unit

(*****************************************************************************)
(* {1 Reaching back into them (from a callback)} *)
(*****************************************************************************)

(* what a callback reaches back into the widgets with *)
val text : t -> string
val set_text : t -> string -> unit
val set_enabled : t -> bool -> unit

(* a hidden widget -- a group, and all it holds -- is neither drawn nor
 * handled, as if it were not in the tree: how a dialog comes and goes
 * in a toolkit that cannot add and remove widgets *)
val set_shown : t -> bool -> unit

(* a slider's value or a progress bar's fraction, and a menu's choice *)
val value : t -> float
val set_value : t -> float -> unit
val chosen : t -> int

(*****************************************************************************)
(* {1 Running the window} *)
(*****************************************************************************)

val window : t -> ui

(* one frame: the mouse and keys in, the callbacks fire as it walks
 * the tree *)
val handle : Widget.input -> ui -> unit

val paint : Theme.t -> ui -> Widget.paint list
