(* How a widget is drawn -- and nothing about who keeps its state.
 *
 * This module exists for the comparison that this whole corner of the
 * repository is for (notes_gui.md section 4): the same widgets are
 * wired four ways -- immediate mode (Immediate), callbacks
 * (Retained), model-view-controller (Mvc) and model-view-update
 * (Mvu) -- and if each drew its own buttons, the comparison would be
 * measuring paint rather than architecture. So the paint lives here,
 * once, and each architecture is only its wiring.
 *
 * Every function takes what to draw and the *state to draw it in* --
 * hot, held, checked, where the caret is -- and answers with paint.
 * It knows nothing of mice, frames or callbacks; the question "is it
 * hot?" belongs to whoever owns the state, and that is exactly what
 * the four disagree about.
 *
 *     Immediate   hot is computed now, from the mouse, and forgotten
 *     Retained    hot is a mutable field of the button object
 *     Mvc         the view asks the model, the controller sets it
 *     Mvu         hot comes from the model, by way of a message
 *)

(* the face a button shows: alive under the mouse, sunk while pressed *)
val face : Theme.t -> hot:bool -> held:bool -> Color.t

(* text centered in a box, at the theme's size *)
val text_at : Theme.t -> Widget.box -> string -> Widget.paint

val label : Theme.t -> Widget.box -> string -> Widget.paint list
val button : Theme.t -> Widget.box -> string -> hot:bool -> held:bool -> enabled:bool -> Widget.paint list

val checkbox :
  Theme.t -> Widget.box -> string -> checked:bool -> hot:bool -> held:bool -> Widget.paint list

(* [fraction] is where the knob sits, 0 at the left end, 1 at the right *)
val slider : Theme.t -> Widget.box -> fraction:float -> hot:bool -> held:bool -> Widget.paint list

(* [slider_value theme box ~from ~to_ mx]: the value of a slider whose
 * knob is dragged to [mx] -- its center under the mouse, kept in the
 * track; None when the box is too narrow to have a track.
 *
 * Here once, for the three toolkits, rather than written in each: the
 * same formula written twice can come out a last bit apart. On arm64,
 * OCaml's native code fuses a multiplication and an addition into one
 * instruction (fmadd, one rounding instead of two) where the pattern
 * of the code allows it, and one toolkit's copy was fused and another's
 * not -- found by examples/gui4/tests/Unit_gui4, when a circle drawn
 * from each version's value was not the same picture. *)
val slider_value : Theme.t -> Widget.box -> from:float -> to_:float -> float -> float option

val progress : Theme.t -> Widget.box -> float -> Widget.paint list

(* [caret] is the byte index the caret sits at, or [None] when the
 * field does not have the keys; the text scrolls sideways to keep the
 * caret in view *)
val field :
  Theme.t -> Widget.box -> string -> caret:int option -> enabled:bool -> Widget.paint list

(* [field_column_at theme box text ~caret x]: which character of
 * [text] the point [x] falls on, given where the caret is (which is
 * what says how far the text has scrolled) -- how a click becomes a
 * caret position *)
val field_column_at : Theme.t -> Widget.box -> string -> caret:int -> float -> int

(* [text_area theme box lines ~range ~caret ~first]: several lines of
 * text in a box, from line [first] down, with the selection [range]
 * (in offsets into the whole text) highlighted and the caret -- as
 * (line, column), or [None] when the area does not have the keys --
 * drawn where it belongs.
 *
 * Laid out one character to a cell, like the field and for the same
 * reason (Text.mli): a click lands exactly where it looks. *)
val text_area :
  Theme.t ->
  Widget.box ->
  (int * string) list ->
  range:int * int ->
  caret:(int * int) option ->
  first:int ->
  Widget.paint list

(* how many characters across, and how many lines down, fit in a box:
 * what the caller needs to wrap its text and to scroll *)
val columns : Theme.t -> Widget.box -> int
val rows : Theme.t -> Widget.box -> int

(* [text_area_place theme box ~first x y]: the (line, column) a point
 * falls on -- how a click becomes a caret *)
val text_area_place : Theme.t -> Widget.box -> first:int -> float -> float -> int * int

(* a dropdown, closed (with its arrow) and open (its items, the one
 * under the mouse lit) *)
(* a list box, and where its row [i] is *)
val list : Theme.t -> Widget.box -> string list -> selected:int option -> Widget.paint list
val list_row : Theme.t -> Widget.box -> int -> Widget.box

val menu_closed : Theme.t -> Widget.box -> string -> hot:bool -> held:bool -> Widget.paint list
val menu_item : Theme.t -> Widget.box -> int -> Widget.box
val menu_items : Theme.t -> Widget.box -> string list -> under:int option -> Widget.paint list

(* a context menu opened at a point: the box its items hang from (with
 * [menu_item] and [menu_items]), the first item's top-left corner just
 * off the point, as wide as its widest item *)
val context_box : Theme.t -> float * float -> string list -> Widget.box
