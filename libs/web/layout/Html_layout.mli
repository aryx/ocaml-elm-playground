(* Html_layout: a page's tree laid out -- every block a box with a
   place and a size, every word a fragment on a line.

   Layout turns the tree with its looks into geometry: where each thing
   goes, on a page as wide as the window and as tall as it takes. The
   coordinates are the typesetter's (Page.mli's): x from the page's
   left, y **down** from its top; the app turns them over to draw.

   Two kinds of layout, nested:

   **Blocks** (p, h1, ul, li, pre, the body...) are stacked downwards,
   each as wide as its parent less its indents (Looks.box), each
   starting where the last ended plus the margin between them -- of
   which, where two touch, only the larger counts (CSS's margin
   collapsing: why two paragraphs are one blank line apart, not two).
   Ours collapses siblings' margins only; CSS also collapses a parent's
   with its first and last child's (the body's 8 with an h1's 21.4), an
   exercise.

   **Inline content** (text, and b, a, tt... inside a block) is cut into
   words, each measured in its look by the caller's [metrics] (this
   module never sees a font: the app gives Hershey's widths, the tests
   a character as wide as its size), and set on **lines**. A block
   holding both blocks and inline content (<li>text<ul>...</ul>, the
   body with loose text) gets an anonymous block around each run of
   inline content, so that a block's children are either all blocks or
   all lines -- CSS's "anonymous block boxes".

   A line's words share a **baseline**. Each word's look gives its
   ascent (0.8 em) and descent (0.2 em), and the line height (1.2 em,
   Looks.leading) spreads the rest half above and half below (CSS's
   "half-leading"); the line is as tall as its tallest parts need:

     size 10:  ascent 8, descent 2, line 12: 9 above the baseline, 3 below
     size 20:  16, 4, 24:                    18 above, 6 below
     both on one line: 18 above, 6 below, a line 24 high

   Worked example (the tests'), a character as wide as its size, the
   root's size 10, a page 200 wide:

     <h1>Menu</h1><p>Soup of the day</p>

     body   x 8, width 184 (its margins 8), y 8
     h1     y 8 + 13.4 = 21.4       its margin: 0.67 em of 20
            "Menu" x 8..88, line 21.4..45.4, baseline 21.4 + 18 = 39.4
     p      y 45.4 + 13.4 = 58.8    max (13.4, 11.2): collapsed
            "Soup" x 8, "of" 58, "the" 88, "day" 128 (a space 10),
            line 58.8..70.8, baseline 58.8 + 9 = 67.8
     body   ends at 70.8 + 11.2 (the p's margin) = 82, the page at 90

   **Lines are broken** where the page says (<br>, a newline in <pre>)
   and where the next word does not fit. What is broken is not a word
   but a *unit*: the words stuck together with no space between them,
   which a break must not separate ("home" in a link and the "." after
   it; "bo" and "ld" in <b>bo</b>ld). How the units are shared out
   between lines is a [breaker]'s choice:

     greedy    fill a line, break before the unit that does not fit
               (a unit wider than a line gets a line of its own and
               overflows): what every browser does, the default here
     other     the caller's: TinyMosaic's wrap=pretty passes Knuth and
               Plass's optimal breaker (appkits/typeset's Linebreak),
               the paragraph scored whole -- CSS's text-wrap: pretty

   Worked example (the tests'), the same metrics, a page 208 wide, so
   the paragraph 192:

     <p>Soup of the day and salads</p>
       line 1 "Soup of the day and"   x 8..198   40+10+20+10+30+10+30+10+30 = 190
       line 2 "salads"                x 8..68    190 + 10 + 60 = 260 > 192: broken
       (baselines 28.2 and 40.2)

   <pre>'s lines are never broken, and overflow.

   **An image** (<img>, Mosaic's addition, 1993) is a word that is a
   picture: a unit of its own width, on the baseline, as tall as it is
   (its bottom on the baseline, no leading; with align=middle, its
   middle there), the line as tall as it needs. Its size is known two
   ways: the page says it (width= and height=: the layout is right
   before the picture arrives, which is what those attributes are for),
   or the picture has arrived and been decoded ([picture_size]). Until
   one of them, the image is its alt text, and the page is laid out
   again when it comes -- the text below it jumps down. Worked example
   (the tests'), the same metrics, the root's size 10:

     <p>A <img src=g.gif width=30 height=50> B
       "A" x 8..18;  the image x 28..58;  "B" x 68..78
       the line: the image's 50 above the baseline (more than the
       text's 9), the text's 3 below: 53 high, its baseline 50 down

   **Netscape's extensions**, when the root's look honours them
   (Looks.root ~extensions): an image's width= and height=; <hr
   size= width= align=> (a rule's thickness, its width in pixels or a
   percentage, centred by default); and **floats**, Netscape 1.0's
   <img align=left> and align=right: the picture taken out of the
   line, put against the left or right edge, and the lines beside it
   shortened until its bottom -- the page's floats are shared by all
   its blocks, so a picture floated in a short paragraph shortens the
   next one's lines too, and <br clear=left|right|all> moves the next
   line below them. Beside floats, lines are filled one at a time,
   each as wide as the room at its top (greedy, whatever the breaker:
   Knuth and Plass set a paragraph of one width), a float met in a
   line put below that line (at its top if it comes first), a word too
   wide for the room moved below the float. Worked example (the
   tests'), the same metrics, a page 200 wide (the body's 184):

     <img src=g.gif width=40 height=30 align=left>aa bb cc dd ee ff
       the image at x 8..48, y 8..38; the lines beside it start at
       48 + 6 (the gap), 138 wide: "aa bb cc dd" x 54..164 (with
       " ee", 170: too wide), from y 8, its baseline 17; "ee ff"
       from y 20 (still beside it), its baseline 29

   CSS 2.1 wrote down what Netscape did (section 9.5, floats; "clear"),
   and much more: a float's own margins, floats that do not fit side by
   side going down, a block's box extending under a float (only its
   lines are shortened) -- the exercise of the clearfix.

   **A table** (Netscape 1.1, when honoured) is a block of cells in rows
   and columns: its grid and its columns' widths are Table_layout's,
   each cell asked its two widths by being laid out twice -- at width 0,
   where every word is a line of its own and the widest is its minimum,
   and without limit, where its lines are whole and the widest is its
   maximum (lines not aligned then: a centred line at an unlimited
   width would be far away). Then each row is as tall as its tallest
   cell, a cell's content in the middle of it (valign=top, bottom
   otherwise), the cells cellspacing= apart (2) and their content
   cellpadding= inside (1), the whole border= inside its frame (0); a
   <caption> above, as wide as the table; the table on the left, or
   centred by align=center or a <center> around it. A cell's floats are
   its own. Worked example (the tests'), the same metrics:

     <table border=1><tr><td>a<td>bb</table>
       columns 1 + 10 + 1 = 12 and 22; the table 1 + 2 + 12 + 2 + 22
       + 2 + 1 = 42 wide from x 8, its cells from 11 and 25, "a" at 12
       and "bb" at 26; the row from 11, 14 high: the table 20 high

   **A form's control** (Mosaic 2.0) is a box in the line too, its size
   from its kind (Forms), in the look it is in: a text field size=
   characters wide (20), a checkbox or a radio button 0.9 em square, a
   button its label and some room, a select its widest option, a
   textarea cols= by rows=; a hidden one is nothing.

   **A list item's marker** -- a bullet, or its number in an <ol> -- is
   the item box's [marker], drawn by the app to the left of the item's
   first line ([first_baseline]), in the indent its list made (CSS's
   "list-style-position: outside").

   Reference: W3C, CSS 2.1, chapter 8 (the box model, collapsing
   margins), 9.2 (block and inline boxes, anonymous block boxes) and
   10.8 (line height, half-leading); Web Browser Engineering, chapters
   3 ("Formatting Text") and 5 ("Laying Out Pages"). *)

(* the width of a string in a look: the caller's font *)
type metrics = Looks.t -> string -> float

(* an image in a line: its src (as the page wrote it), its size, and
 * whether its middle or its bottom is on the baseline (align=middle,
 * or bottom, Mosaic's default) *)
type picture = { src : string; height : float; middle : bool }

(* a form's control in a line (Forms): the page's element (the key of
 * its value, which the browser keeps), and its height; its bottom a
 * quarter of it below the baseline, where its own text's baseline
 * falls *)
type control = { element : Dom.element; control_height : float }

(* a word (or, in <pre>, a line's text; or an image or a control, [text]
 * ""), where it goes *)
type fragment = {
  text : string;
  look : Looks.t;
  x : float; (* its left edge *)
  width : float;
  baseline : float;
  picture : picture option;
  control : control option;
  element : Dom.element; (* the innermost element it is in: a click on it is on that *)
}

(* a line, and the names on it a #fragment can scroll to: <a name=x>,
 * an inline element's id=x (an anchor with no text before it, alone,
 * is a line of no height where it is) *)
type line = { top : float; height : float; baseline : float; fragments : fragment list; anchors : string list }

type kind =
  | Block of Dom.element
  | Anonymous (* the lines of a run of inline content *)
  | Rule of Dom.element (* hr *)

(* a list item's: a bullet (ul, dir, menu), or its number (ol) *)
type marker = Bullet | Number of int

type box = {
  kind : kind;
  x : float;
  y : float;
  width : float;
  height : float;
  children : box list; (* its blocks, in order *)
  lines : line list; (* an Anonymous box's *)
  floats : fragment list; (* an Anonymous box's floats (pictures), placed *)
  marker : marker option; (* a list item's *)
  background : Looks.color option; (* a style sheet's background-color *)
}

(* a unit to set: the space before it (in its first word's look) and
 * its width *)
type unit_ = { space : float; width : float }

(* [breaker ~measure units]: the lines, each the indexes of its first
 * and last unit, in order, together all the units *)
type breaker = measure:float -> unit_ array -> (int * int) list

(* fill each line, break before what does not fit *)
val greedy : breaker

(* [layout metrics ?breaker ?picture_size ?style ~root ~width html]: the
 * tree laid out on a page [width] wide, its root's look [root], its
 * lines broken by [breaker] (greedy), the size of an image of src s,
 * [picture_size s], if the caller has it (none: every image is its
 * width= and height=, or its alt text), each element's look and box
 * the table's (Looks) then its style sheets' declarations, [style e]
 * (Css.cascade's; none by default: Mosaic's looks) *)
val layout :
  metrics ->
  ?breaker:breaker ->
  ?picture_size:(string -> (float * float) option) ->
  ?style:(Dom.element -> (string * string) list) ->
  root:Looks.t ->
  width:float ->
  Dom.element ->
  box

(* the baseline of a box's first line, if it has one *)
val first_baseline : box -> float option

(* every fragment of the page, in document order (a box's floats after
 * its lines) *)
val fragments : box -> fragment list
