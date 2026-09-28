(* Table_layout: a table's grid, and how wide its columns are.

   Tables came with Netscape 1.1 (1995, after the HTML 3.0 draft;
   standard in HTML 3.2), and pages used them at once for what they
   were not meant for: the layout of the whole page, columns side by
   side, the first "web design". A table is rows (<tr>) of cells (<td>,
   <th> a heading cell), a cell as wide as <td colspan=n> columns.

   **The grid**: rows and cells read from the tree, through the groups
   HTML 4 added around rows (<thead>, <tbody>, <tfoot>), each cell at
   the first column its row has free:

     <tr><td colspan=2>a<td>b     row 0: a at 0 (2 columns), b at 2
     <tr><td>c<td>d<td>e          row 1: c at 0, d at 1, e at 2

   **The widths** -- the automatic table layout (CSS 2.1 section
   17.5.2.2, which wrote down what Netscape did). A column's width
   depends on its cells' contents, and a cell's height on its width, so
   each cell is asked two widths: its **minimum** (its widest word: it
   can wrap down to that, not further) and its **maximum** (everything
   on one line); Html_layout gets them by laying the cell out at width
   0 and at a width without limit. A column's are its cells' largest;
   then, the table's room W and the columns' sums of both:

     W >= sum of max    every column at its max (the table shrinks to
                        its content, unless it has width=: then the
                        room left is shared in proportion to the maxes)
     W <= sum of min    every column at its min, the table overflows
     between            each column its min, and the room left shared
                        in proportion to (max - min): a column that
                        wraps more gets more

   A cell of several columns that needs more than they give has the
   difference spread over them, evenly.

   Worked example (notes_browser.md section 12, and the tests):

     | a            | 3.50 |   column 1: min 60 ("tomato"), max 110
     | tomato soup  | 4.20 |   column 2: min 40,            max 40
                               sums: min 100, max 150
     W = 200:  150 <= 200: 110 and 40
     W = 120:  between: 20 beyond the mins, shared 50 : 0: 80 and 40
     W =  90:  90 < 100: 60 and 40, and the table overflows

   Not done: rowspan= (a cell down several rows), the fixed layout
   (table-layout: fixed, the first row decides), a column's width=.

   Reference: W3C, CSS 2.1, section 17.5.2.2 (automatic table layout);
   HTML 3.2, "Tables"; notes_browser.md section 12. *)

(* a cell of the grid: its element, row, first column, columns *)
type cell = { element : Dom.element; row : int; column : int; span : int }

(* the table's cells, row by row (caption apart), and its number of
 * columns *)
val grid : Dom.element -> cell list * int

(* the table's <caption>, if it has one *)
val caption : Dom.element -> Dom.element option

(* [columns n cells]: each column's min and max, from each cell's (its
 * min and max, the spacing between the columns it spans included) *)
val columns : int -> (cell * (float * float)) list -> spacing:float -> (float * float) array

(* [widths ~room ~fixed columns]: each column's width, [room] the
 * table's (its cells' spacing taken out), [fixed] when the table said
 * its width (width=): then it takes all of [room] *)
val widths : room:float -> fixed:bool -> (float * float) array -> float array
