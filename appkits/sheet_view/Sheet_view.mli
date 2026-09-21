(* A sheet, drawn into a rectangle: the cells, the headers, the
 * selection, and where a click landed.
 *
 * It is here, rather than inside the application that first needed
 * it, for the reason a sheet is worth writing twice over: the pair of
 * applications (TinyVisiCalc's interface of 1979 and TinyExcel's of
 * 1985) share an engine, and the one with a mouse shares this too --
 * and a sheet has to be drawable *inside a document* one day, which
 * is the same question again (appkits/embed's component protocol: a
 * size, a drawing into a rectangle, events while it is active).
 *
 *     head_w
 *    |<--->|<-- cell_w -->|
 *    +-----+------+------+------+     head_h, the row of letters
 *    |     |  A   |  B   |  C   |
 *    +-----+------+------+------+  -
 *    |  1  |      |      |      |  |  cell_h
 *    +-----+------+------+------+  -
 *    |  2  |      |######|      |     the selection, shaded
 *    +-----+------+------+------+
 *
 * A **selection is two cells**, an anchor and a focus, and a single
 * cell is a selection of one -- the same shape as a caret being a
 * selection of length zero (gui/Text_edit). Everything that works on
 * "the selection" then works on one cell without a special case.
 *
 * Numbers are drawn against the right edge of their cell and
 * everything else against the left. That is VisiCalc's rule and every
 * spreadsheet's since, and it is the reason a column of figures reads
 * as a column. *)

(* how big the cells are, and how many of them are shown *)
type geometry = {
  cols : int;
  rows : int;
  cell_w : float;
  cell_h : float;
  head_w : float;
  head_h : float;
}

val default : geometry

(* how big a sheet of that geometry wants to be, for a layout *)
val size : geometry -> float * float

(* where a cell sits inside the rectangle the sheet was given *)
val cell_box : geometry -> Widget.box -> Formula.cell -> Widget.box

(* and which cell a point is on, if any: how a click becomes a
 * selection *)
val cell_at : geometry -> Widget.box -> float * float -> Formula.cell option

(* [draw geometry theme box sheet ~selection]: the whole thing. The
 * selection is (anchor, focus), in either order; without one, nothing
 * is shown selected -- a sheet sitting in a document, not being
 * edited (apps/Part_sheet). *)
val draw :
  ?selection:Formula.cell * Formula.cell ->
  geometry ->
  Theme.t ->
  Widget.box ->
  Sheet.t ->
  Widget.paint list

(* the two corners of a selection, in order: (left, top) and
 * (right, bottom), whichever way round it was dragged *)
val corners : Formula.cell * Formula.cell -> Formula.cell * Formula.cell

(* every cell of a selection, row by row *)
val cells_of : Formula.cell * Formula.cell -> Formula.cell list

(* how a selection is named in a formula bar: "B2", or "B2:D5" *)
val name_of : Formula.cell * Formula.cell -> string
