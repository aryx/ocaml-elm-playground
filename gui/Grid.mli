(* A table: rows and columns that line up, which rows of rows cannot
 * do (Layout.mli, and notes_gui.md section 5).
 *
 *   a column of rows                 a grid
 *   +----------------------+         +---------+------------+
 *   | Name   [        ]    |         | Name    | [        ] |
 *   +----------------------+         +---------+------------+
 *   | Address [       ]    |         | Address | [        ] |
 *   +----------------------+         +---------+------------+
 *   each row sizes itself,           a column is as wide as its
 *   so the fields start in           widest cell, so the fields
 *   different places                 all start in the same place
 *
 * That is the whole difference, and it is why Tk has both [pack] and
 * [grid] (Ousterhout; grid arrived in Tk 4.1, 1996). Every dialog
 * anybody has ever laid out is this, and a column of rows is the
 * wrong tool for it however good the rows are.
 *
 * Tk's vocabulary, kept on purpose since it is the one people know:
 *
 *   -row, -column           where it goes
 *   -rowspan, -columnspan   how many it covers
 *   -sticky "nsew"          which edges it is pulled to: with "ew" it
 *                           fills its cell across, with "" it sits in
 *                           the middle at its own size
 *   -weight                 who gets the room left over when the
 *                           table is given more than it needs
 *
 * Two deviations from Tk, both deliberate: a grid with no weights
 * sits in the middle of the room it is given rather than in the
 * top-left corner (which is what the rest of this toolkit does), and
 * the room a spanning cell needs beyond its columns is given to the
 * *last* column of the span, as Tk does, rather than shared.
 *
 * Worked example, the two-row form above at 300 x 100, gap 10, with
 * "Name" 60 wide, "Address" 90 wide and the fields 120:
 *
 *   columns: 90 (the wider label) and 120 (the fields)
 *   rows:    36 and 36
 *   natural: 220 x 82
 *
 * and both fields start at the same x, which was the point.
 *
 * Not here, and worth knowing they exist in Tk: -padx/-pady per cell
 * (put a [pad] around the thing instead), -uniform (columns forced to
 * one width), and -minsize. *)

(* one thing in the table *)
type 'a item

val item :
  ?rowspan:int ->
  ?colspan:int ->
  ?sticky:string ->
  row:int ->
  col:int ->
  'a ->
  float * float ->
  'a item

type 'a t

(* [make ?gap ?row_weights ?col_weights items]: the table. A weight is
 * given per index, e.g. [~col_weights:[ (1, 1.) ]] to give all the
 * spare width to column 1 -- the usual thing for a form, whose
 * fields grow and whose labels do not. *)
val make :
  ?gap:float -> ?row_weights:(int * float) list -> ?col_weights:(int * float) list -> 'a item list -> 'a t

(* how big the table wants to be: the columns and rows it needs *)
val measure : 'a t -> float * float

(* the rectangle each thing ends up with, given the one the table gets *)
val arrange : Widget.box -> 'a t -> ('a * Widget.box) list

(* the widths and heights it worked out, for a caller drawing rules
 * between the columns (a spreadsheet does) *)
val columns : 'a t -> float list
val rows : 'a t -> float list
