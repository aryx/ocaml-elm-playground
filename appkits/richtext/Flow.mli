(* One text poured through a chain of columns -- FrameMaker's (1986)
 * and every page-layout program's "flow": the text fills the first
 * column, goes on at the top of the second where the first stopped,
 * and so on across pages, for as many columns as it takes.
 *
 * And **anchored frames**: a frame (a picture, a sheet, a drawing)
 * tied to a place in the text, and set just below the line that place
 * is on, in the same column -- or at the top of the next, if it does
 * not fit in what is left of this one. Since the frame follows the
 * text, typing above it moves it down, from column to column and page
 * to page: the frame is *in* the text's flow, which is the difference
 * between a frame anchored and a frame placed.
 *
 * All the columns are one width here, so the text is laid out once
 * (Page) and its lines are dealt out into columns of a given height:
 *
 *   a line goes where the last thing ended, unless it does not fit
 *   there, and then at the top of the next column; after the line an
 *   anchor is on, its frame, the same way
 *
 * Worked example: five lines, each 22.4 high (plain text of size 16),
 * into columns 50 high, with a frame 30 high anchored in the first
 * line:
 *
 *   column 0:  line 0 at 0                   (22.4 of 50)
 *              the frame: 22.4 + 30 > 50, so to the next column
 *   column 1:  the frame at 0                 (30)
 *              line 1: 30 + 22.4 > 50, so to the next column
 *   column 2:  line 1 at 0, line 2 at 22.4
 *   column 3:  line 3 at 0, line 4 at 22.4
 *
 * A line or a frame taller than a whole column goes at the top of the
 * next one and runs over its bottom: there is nowhere better for it. *)

type placed_line = { line : Page.line; column : int; top : float (* from the column's top *) }

(* an anchored frame: which one (its index among the anchors given),
 * its column, its top, its height *)
type placed_frame = { frame : int; in_column : int; at : float; height : float }

type t = { lines : placed_line list; frames : placed_frame list; columns : int }

(* [flow ~column_height ~anchors page]: the lines of [page] and the
 * frames of [anchors] -- each a place in the text and a height, in the
 * order of their places -- dealt out into columns [column_height] high *)
val flow : column_height:float -> anchors:(int * float) list -> Page.t -> t
