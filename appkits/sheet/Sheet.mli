(* A spreadsheet: cells, what they hold, and what has to be
 * recalculated when one of them changes.
 *
 * The idea a spreadsheet *is*, and it is not the grid: a cell's
 * formula names other cells, so the sheet is a **graph** -- an arrow
 * from every cell to the cells that read it -- and changing one cell
 * means recomputing the cells downstream of it, in an order where
 * nothing is computed before what it reads.
 *
 *        A1 --------> B1 = A1*2 --------> C1 = B1+A2
 *                                     ^
 *        A2 --------------------------+
 *
 *   change A1  ->  recompute B1, then C1        (2 cells)
 *   change A2  ->  recompute C1                 (1 cell)
 *
 * That is a **topological order** of the part of the graph that can
 * be affected, and the algorithm is Kahn's (1962): take a cell with
 * nothing left to wait for, compute it, and cross it off the lists of
 * the cells that were waiting for it. What is left over when nothing
 * can be taken is exactly a **cycle**, which is how a spreadsheet
 * finds A1 = B1 + 1, B1 = A1 + 1 and says so instead of looping.
 *
 * Worth knowing what VisiCalc itself did (Bricklin and Frankston,
 * 1979), because it explains a generation of spreadsheet habits: it
 * recalculated in row order, or column order, your choice -- so a
 * formula that read a cell *below* it got the previous value, and
 * users were told to press the recalculate key twice, or to lay
 * their sheet out so that it flowed one way. Lotus 1-2-3 (1983)
 * brought the "natural order" recalculation this module does.
 *
 * The number to watch, and what [recalculated] is for: changing one
 * cell of a thousand should recompute *what depends on it*, and not
 * the thousand. A sheet that recomputes everything is correct and
 * unusable, which is the difference between a demonstration and a
 * spreadsheet.
 *
 * What it deliberately does not do: several sheets, absolute
 * references and copying formulas (the $A$1 of a real one, which is
 * about what happens to a formula that is moved), formatting of any
 * kind,
 * and lazy recalculation -- everything downstream of a change is
 * recomputed at once, where a big spreadsheet computes only what is
 * on the screen and leaves the rest until asked. *)

type value = Number of float | Text of string | Error of string | Empty

type t

val empty : t

(* [set cell text t]: what was typed into a cell -- a number, some
 * text, or a formula starting with '='. Everything downstream is
 * recalculated, in an order that respects what reads what. *)
val set : Formula.cell -> string -> t -> t

(* what was typed in, and what it came to *)
val raw : t -> Formula.cell -> string
val value : t -> Formula.cell -> value

(* how a value reads on the screen: a number without its trailing
 * zeros, text as itself, an error as "#" and why *)
val show : value -> string

(* the cells that hold something, for saving and for drawing *)
val cells : t -> Formula.cell list

(* how many cells the last [set] recomputed -- the number that says
 * whether this is a spreadsheet or a demonstration *)
val recalculated : t -> int

(* {1 Saving}
 *
 * One line per cell, its name and what was typed into it, tab
 * separated:
 *
 *   A1<TAB>12
 *   B1<TAB>=A1*2
 *
 * Small enough to read, and the reason it is here rather than in an
 * application: a sheet that can be embedded in a document has to be
 * able to write itself down (the component protocol of
 * plan_gui_teaching.md's appkits/embed). *)
val to_string : t -> string
val of_string : string -> t
