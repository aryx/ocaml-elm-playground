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

(* a word (or, in <pre>, a line's text), where it goes *)
type fragment = {
  text : string;
  look : Looks.t;
  x : float; (* its left edge *)
  width : float;
  baseline : float;
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
  marker : marker option; (* a list item's *)
}

(* a unit to set: the space before it (in its first word's look) and
 * its width *)
type unit_ = { space : float; width : float }

(* [breaker ~measure units]: the lines, each the indexes of its first
 * and last unit, in order, together all the units *)
type breaker = measure:float -> unit_ array -> (int * int) list

(* fill each line, break before what does not fit *)
val greedy : breaker

(* [layout metrics ?breaker ~root ~width html]: the tree laid out on a
 * page [width] wide, its root's look [root], its lines broken by
 * [breaker] (greedy) *)
val layout : metrics -> ?breaker:breaker -> root:Looks.t -> width:float -> Dom.element -> box

(* the baseline of a box's first line, if it has one *)
val first_baseline : box -> float option

(* every fragment of the page, in document order *)
val fragments : box -> fragment list
