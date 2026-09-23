(* A drawing: its figures, from the one at the back to the one in
 * front, each with an id that stays its own while it is moved,
 * resized, sent back or brought forward -- a selection is a list of
 * ids, not of places in the list.
 *
 * The order is the whole of "in front of": a figure is drawn after
 * everything behind it, and a click is tried on everything in front of
 * it first. So the list is drawn from the start and hit-tested from the
 * end -- the same two walks, in opposite directions, as any toolkit's
 * tree (notes_gui.md section 6).
 *
 * A drawing is a value: every function here returns a new one, so the
 * application's undo is keeping the old ones (appkits/document/Undo).
 *
 * Grouping is MacDraw's own: the figures chosen become one Group,
 * taking the place in the order of the one that was furthest in front;
 * ungrouping puts them back there, in their order, with new ids. *)

(*****************************************************************************)
(* {1 The figures} *)
(*****************************************************************************)

type id = int
type t

val empty : t

(* [add figure t]: in front of everything, and its id *)
val add : Figure.t -> t -> t * id

(* back to front *)
val figures : t -> (id * Figure.t) list
val get : t -> id -> Figure.t option

(*****************************************************************************)
(* {1 Clicking and selecting} *)
(*****************************************************************************)

(* [at ~tolerance t point]: the figure in front at that point, if any *)
val at : tolerance:float -> t -> Figure.point -> id option

(* the figures entirely inside a box: what a marquee drag selects *)
val within : t -> Figure.box -> id list

(*****************************************************************************)
(* {1 Editing} *)
(*****************************************************************************)

val update : id -> (Figure.t -> Figure.t) -> t -> t
val move : id list -> float -> float -> t -> t
val delete : id list -> t -> t

(* in front of all the others, or behind them -- keeping their order
 * among themselves *)
val to_front : id list -> t -> t
val to_back : id list -> t -> t

(* [group ids t]: one Group of them, and its id (None for fewer than
 * two) *)
val group : id list -> t -> t * id option

(* [ungroup id t]: its figures back, and their ids ([] if it was not a
 * group) *)
val ungroup : id -> t -> t * id list

(* copies, a little down and to the right, in front; and their ids *)
val duplicate : id list -> t -> t * id list

(*****************************************************************************)
(* {1 Lining up} *)
(*****************************************************************************)

type side = Lefts | Rights | Tops | Bottoms | Centers

(* [align side ids t]: the figures lined up on the left edge of the
 * leftmost of them (and so on), or on their common horizontal centre *)
val align : side -> id list -> t -> t

(* the box round some figures *)
val bounds : t -> id list -> Figure.box option
