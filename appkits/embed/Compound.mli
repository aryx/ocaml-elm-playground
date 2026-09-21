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
 * row shares its width equally and is as tall as its tallest part.
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
 *)

type t = Part of Component.part | Column of t list | Row of t list
type path = int list

(* the room between parts *)
val gap : float

(* [layout doc ~left ~top ~width]: where every part goes, as (path,
 * rectangle) in the playground's coordinates (y up, boxes by their
 * centre), with the document's top-left corner at ([left], [top]);
 * and the document's height *)
val layout : t -> left:float -> top:float -> width:float -> (path * Widget.box) list * float

(* the part a point is on, in a layout *)
val at_point : (path * Widget.box) list -> float * float -> path option

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

val save : t -> string

(* [load registry text]: the document back, each part through its
 * kind's loader, and a placeholder for a kind [registry] does not have *)
val load : Component.registry -> string -> t
