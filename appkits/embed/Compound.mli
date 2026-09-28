(* A document made of parts: a tree whose leaves are parts and whose
 * nodes stack their children -- down the page, or across it:
 *
 *   Column [ Part text;                      +-------------------+
 *            Row [ Part sheet;               | text              |
 *                  Part picture ];           +---------+---------+
 *            Part text ]                     | sheet   | picture |
 *                                            +---------+---------+
 *                                            | text              |
 *                                            +-------------------+
 *
 * Laid out as Flutter would (gui/Layout): the width goes down, each
 * part says how tall it is at that width, the heights come back up. A
 * row shares its width out by its children's shares (equal, unless a
 * person dragged the gap between two of them) and is as tall as its
 * tallest part.
 *
 * What a person chose about a node's size is a [Sized] wrapper round
 * it: a height, and a share of its row's width. The height is a
 * proposal, OpenDoc's **frame negotiation**: the document gives a part
 * the height it was given, but never less than the part asks for at
 * that width -- a person can give a part more room, not less than it
 * needs (nothing here can clip what a part draws). The wrappers are
 * not in the paths: a path goes through one to what it wraps.
 *
 * A part is found by its **path**, the child numbers from the root:
 * above, the picture is [1; 1]. A path is what the document keeps to
 * say which part is selected or active -- a value, where a toolkit
 * would keep a pointer.
 *
 * In OpenDoc the containers were parts too -- a text part could hold a
 * sheet, which could hold a picture. Here the nesting is the
 * document's own two kinds of node, which is less general and much
 * smaller; the parts never know they are nested.
 *
 * Saved as text, each node on a line of its own, each part's text
 * after its kind and its length -- so that a part's text can be
 * anything, newlines included, and the reader never has to understand
 * it to skip it:
 *
 *   column 2
 *   part text 5
 *   Hello
 *   row 1
 *   part counter 2
 *   42
 *
 * and a Sized node as "sized <height or -> <share>", followed by
 * "scaled" if it is, before what it wraps.
 *)

(*****************************************************************************)
(* {1 The document} *)
(*****************************************************************************)

(* a height a person gave (None: the part's own), a share of the row
 * the node is in (1 unless changed), and whether the part is scaled
 * to its room (Component.draw_in) rather than negotiated with *)
type sizing = { height : float option; share : float; scaled : bool }

type t = Part of Component.part | Column of t list | Row of t list | Sized of sizing * t
type path = int list

(*****************************************************************************)
(* {1 Layout} *)
(*****************************************************************************)

(* the room between parts *)
val gap : float

(* [layout doc ~left ~top ~width]: where every part goes, as (path,
 * rectangle) in the playground's coordinates (y up, boxes by their
 * centre), with the document's top-left corner at ([left], [top]);
 * and the document's height *)
val layout : t -> left:float -> top:float -> width:float -> (path * Widget.box) list * float

(* the part a point is on, in a layout *)
val at_point : (path * Widget.box) list -> float * float -> path option

(*****************************************************************************)
(* {1 Resizing parts} *)
(*****************************************************************************)

(* Where two children of a row meet, for a person to drag: the row's
 * path, the child before the gap, the gap itself, and the left and
 * right edges of the two children together *)
type splitter = { row : path; index : int; grip : Widget.box; span : float * float }

val splitters : t -> left:float -> top:float -> width:float -> splitter list

(* [set_height doc path h]: the height a person gave the node at
 * [path] (None to give it back its own) *)
val set_height : t -> path -> float option -> t

(* Whether the part at [path] is scaled to its room, and making it so:
 * a scaled part is as tall as its proportions make it at its width
 * (or as it was given), and drawn to fit it -- no negotiation, since
 * any room fits; a part with no size of its own (Component.natural)
 * cannot be scaled, and is negotiated with as before *)
val scaled : t -> path -> bool
val set_scaled : t -> path -> bool -> t

(* [resize_row doc path i fraction]: the row at [path] with its
 * children [i] and [i+1] sharing their room so that the first has
 * [fraction] of it (kept between a tenth and nine tenths) *)
val resize_row : t -> path -> int -> float -> t

(*****************************************************************************)
(* {1 Changing the parts} *)
(*****************************************************************************)

val get : t -> path -> Component.part option

(* [set doc path part]: the document with that part replaced *)
val set : t -> path -> Component.part -> t

(* [insert_after doc path node]: [node] added just after the part at
 * [path], in the same column or row; at the end of the root, for an
 * empty path *)
val insert_after : t -> path -> t -> t

(* [remove doc path]: without the part at [path]; a row or column left
 * with one child is replaced by that child, and an empty one goes --
 * except the root, which stays *)
val remove : t -> path -> t

(*****************************************************************************)
(* {1 Saving} *)
(*****************************************************************************)

val save : t -> string

(* [load registry text]: the document back, each part through its
 * kind's loader, and a placeholder for a kind [registry] does not have *)
val load : Component.registry -> string -> t
