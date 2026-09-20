(* Immediate mode: widgets that do not exist (notes_gui.md section 3).
 *
 * There is no button object, no tree, no callback: each frame, a
 * program *asks* for a button -- draw a rectangle here with this
 * label, and tell me whether it was clicked -- and the answer is a
 * bool that lasts as long as the [if] around it.
 *
 *   frame N     ask: draw it, test the mouse -> false
 *   frame N+1   ask: draw it, test the mouse -> TRUE   -> act
 *   frame N+2   ask: draw it, test the mouse -> false
 *
 * Casey Muratori showed this in 2005 ("Immediate-Mode Graphical User
 * Interfaces", a video from inside a game engine) and Omar Cornut's
 * Dear ImGui (2014) made it the tool every game engine now has. It
 * fits this playground for a precise reason: [Playground.game]'s
 * update is [computer -> 'memory -> 'memory], with no message type,
 * so a callback has nowhere to go and a message has nothing to be.
 *
 * The one thing that must survive between frames is *who has the
 * mouse*: press inside a slider and it keeps the mouse until you
 * release, even if you wander off it -- the "capture" of section 6,
 * two lines that nobody writes first and every dragging interface
 * needs. That, plus the theme, is the whole of [t].
 *
 * A widget is told apart from its neighbours by its *rectangle*:
 *
 *   let id (b : Widget.box) = (b.x, b.y)
 *
 * Dear ImGui hashes the label instead, which is why two buttons
 * called "OK" in one window are its classic bug. Here two buttons
 * with the same label are fine and two buttons in the same place are
 * not -- a bug you can see on the screen, which is the better one to
 * have.
 *
 * Everything here is a value: a widget takes the toolkit's state and
 * gives back a new one, plus its answer. The mutable frame buffer
 * that makes the playground's [Gui] read as it does is in
 * playground/Gui.ml, and nowhere else.
 *
 * Worked example, a button at (0, 100) of 200 x 40, the mouse
 * resting at (0, 100) on it:
 *
 *   frame  mdown  mclick   hot    held    button returns
 *     1    false  false    true   false   false
 *     2    true   false    true   TRUE    false     (pressed in it)
 *     3    false  TRUE     true   false   TRUE      (released in it)
 *     4    false  false    true   false   false
 *
 * and if between frames 2 and 3 the mouse leaves the button, frame
 * 3's answer is false: a press that ends outside is not a click,
 * which is how every toolkit since the Macintosh (1984) has let you
 * change your mind.
 *
 * What it costs, honestly: state that must persist (a scroll
 * position, which field has focus) has no widget to live in and needs
 * a table like [capture] above; and nothing can be measured before it
 * is drawn, which is why layout (phase 2) is the hard part of
 * immediate mode and not of the others.
 *)

(* the toolkit's state: the theme, who has the mouse, and the paint of
 * the frame being built *)
type t

(* before the first frame: the default theme, nobody holding anything *)
val empty : t

(* [frame input t]: start a frame -- [input] is what the person is
 * doing now, the paint of the last frame is dropped, the capture
 * survives *)
val frame : Widget.input -> t -> t

(* what the widgets of this frame drew, in the order they asked,
 * back to front *)
val paint : t -> Widget.paint list

val theme : t -> Theme.t
val set_theme : Theme.t -> t -> t

(* {1 The widgets} *)

(* [label t box s]: [s], centered in [box]. No state, no answer: it is
 * here so that a label is themed like everything else. *)
val label : t -> Widget.box -> string -> t

(* [button t box s]: a button labelled [s], true the frame it is
 * clicked (pressed and released inside, above) *)
val button : t -> Widget.box -> string -> t * bool

(* [checkbox t box s checked]: a box with [s] beside it, and the value
 * it has after this frame -- [not checked] the frame it is clicked *)
val checkbox : t -> Widget.box -> string -> bool -> t * bool

(* [slider t box ~from ~to_ v]: a slider between [from] and [to_],
 * showing [v], and the value it has after this frame: [v] unless the
 * mouse is pressed in it, in which case the mouse's x says the value.
 * The knob's center travels over [box.w - theme.knob] pixels, so a
 * 220-wide slider with an 18-wide knob puts [from] at x = -101 and
 * [to_] at x = 101, relative to its center. *)
val slider : t -> Widget.box -> from:float -> to_:float -> float -> t * float

(* {1 How big a widget wants to be}
 *
 * The answer immediate mode can give without layout: from the theme
 * and the label alone. (What it cannot give is how big it wants to be
 * *given the room available* -- that is phase 2's constraints.) *)

(* [button_size theme s]: wide enough for [s] plus padding on both
 * sides, one row high *)
val button_size : Theme.t -> string -> float * float

(* [checkbox_size theme s]: the tick box, then the label *)
val checkbox_size : Theme.t -> string -> float * float

(* [slider_size theme]: the theme's, whatever the value *)
val slider_size : Theme.t -> float * float
