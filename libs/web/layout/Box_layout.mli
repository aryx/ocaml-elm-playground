(* Box_layout: a page laid out by CSS 2.1's box model -- TinyChrome's
   layout, over each element's computed style (Computed).

   (notes_css_engine.md section 7.) Html_layout, the teaching browsers',
   reads Mosaic's looks and a few declarations; this one reads nothing
   but the computed style, so that what a page's style sheets say is
   what it gets. It keeps Html_layout's lines and fragments (a word, a
   picture, a control, where it goes), so that Hit, the form controls
   and the drawing of words work on both ([as_html_layout]).

   **The box.** Every block is a content box inside its padding, its
   border and its margin; the box kept here is the border box (what a
   background fills), its border's widths with it:

     margin-left | border | padding | content | padding | border | margin-right

   Their sum is the containing block's width: CSS 2.1's section 10.3.3,
   one equation, the autos its unknowns. A width auto takes what the
   margins, borders and paddings leave; a width given with both margins
   auto shares the rest between them -- centring:

     containing block 976, width 400, padding 10, border 1, margins auto
       (976 - 400 - 22) / 2 = 277 each: the box at x 277, 422 wide

   box-sizing: border-box counts padding and border inside the width
   (GitHub's and Google's sheets set it everywhere); min-width and
   max-width clamp it (max-width: 100%, the responsive picture).

   **Vertical margins collapse** where two touch, the larger counting
   (a negative one subtracted): between siblings, and between a block
   and its first child (or last) when no border or padding comes
   between them -- the body's 8 and an h1's 21.4 are 21.4 above the h1,
   not 29.4, the h1's margin showing outside the body. An empty block's
   two margins are one. A block whose own layout is apart (a block
   formatting context: overflow other than visible, a float, an
   inline-block, a table cell, the root) keeps its margins in.

   **Inline content** is set on lines as in Html_layout -- words cut at
   spaces, units the words with no space between, a baseline shared --
   but each word's room above and below the baseline is its own
   element's line-height (normal: 1.2 em) split as half-leading around
   its font's ascent and descent (0.8 and 0.2 em), and the block's own
   font gives every line a minimum (the "strut": an empty line is as
   tall as a line of the block's text). white-space: normal and nowrap
   (no break between its words), pre and pre-wrap (the lines kept),
   pre-line (spaces collapsed, newlines kept); text-align, text-transform:
   uppercase, vertical-align: sub and super (the baseline moved).

   **An inline element's box** (a <code> on a grey background, a
   badge, a navigation's link with its padding) is not a block: its
   left margin, border and padding are a spacer word joined to its
   first word, its right ones one joined to its last, so the line makes
   room for them; its background and border, if it has one, are a box
   per line it is on ([backdrops]), from its first word there to its
   last, as tall as its font plus its vertical padding and border --
   which do not make the line taller, and may overlap the lines around,
   as in every browser:

     a <span style="padding: 0 5px; background: yellow">b</span> c
       "a" at 0, the span's box from 20 (the space after "a") to 40,
       "b" at 25, "c" after the box and a space: 50

   **An inline-block** (a button, a navigation's item) is laid out as a
   block of its own width -- given, or **shrink-to-fit**: its content's
   widest line, at most the room there is, at least its widest word --
   and set in the line as one unit, its last line's baseline on the
   line's. A picture is a unit of its own size (width and height, the
   page's attributes being style: Cascade's presentational hints; one
   of them and the picture's ratio; or the picture's own, once it has
   come), a form's control one of Html_layout.control_size's.

   **Floats** are taken out of the flow, laid out shrink-to-fit, and put
   against the left or right edge where they meet a line (below a line
   that has begun); the lines beside them are shortened until their
   bottom, the blocks' boxes are not (only their lines: the clearfix's
   story). clear moves a block below them. A block formatting context
   beside a float is narrowed instead (overflow: hidden beside a
   sidebar), and holds its own floats, its height enclosing them.

     float: left, width 40, height 30; then "aa bb cc", a page 200
       the float at x 8..48; the line beside it from 48, 144 wide

   **Positioning**: relative moves a box by its offsets after it is laid
   out (what is around it stays); absolute takes it out of the flow,
   placed by its offsets in the nearest positioned ancestor's padding
   box (its place in the flow where they are auto), laid out
   shrink-to-fit, and drawn after the rest (the boxes of the page's
   root, last); fixed is absolute in the window's first screen (it
   scrolls with the page here: an exercise).

   **Tables** (display: table) are Html_layout's: the grid and the
   columns' widths from Table_layout, each cell asked its minimum and
   maximum by being laid out at width 0 and without limit; each row as
   tall as its tallest cell, the cell's content at its top, middle or
   bottom (vertical-align); cellspacing= apart (2), a cell without a
   background showing its row's.

   **Lists**: a list-item's marker (a bullet, or its number when its
   list-style-type counts: decimal, lower-alpha...), drawn outside, left
   of its first line.

   Laid out as blocks, and said: display: flex and inline-flex (C5's
   Flex_layout), grid (the plan's exercise). Not done: rowspan=, bottom and right of an absolute box
   whose top and left are auto, fixed boxes staying on screen, z-index
   (the page's order is the drawing's).

   Worked example (the tests'), a character as wide as its size, the
   root's font 10 (line-height normal: 12), a page 200 wide:

     <body style="margin: 8px"><div style="width: 100px; margin: 0
       auto; padding: 5px; border: 2px solid">ab</div>

     body   x 8, width 184
     div    margins (184 - 100 - 14) / 2 = 35: x 43, width 114 (its
            border box), y 8 (the body's margin and the div's 0
            collapsed); "ab" at x 43 + 2 + 5 = 50, its line from 15, 12
            high: the div 12 + 14 = 26 high

   Reference: W3C, CSS 2.1, chapters 8 (the box model, collapsing
   margins), 9 (the visual formatting model: block and inline
   formatting contexts, floats, positioning), 10 (widths and heights:
   10.3.3's equation, shrink-to-fit in 10.3.5, line height in 10.8) and
   17 (tables); Web Browser Engineering, chapters 5 and 6;
   notes_css_engine.md section 7. *)

type box = {
  element : Dom.element option; (* None: an anonymous box of lines *)
  style : Computed.t; (* its element's; an anonymous box's, its block's *)
  x : float; (* the border box, page's coordinates (y down) *)
  y : float;
  width : float;
  height : float;
  border : float * float * float * float; (* its widths: top, right, bottom, left *)
  children : box list; (* its blocks; an anonymous box's inline-blocks and floats *)
  lines : Html_layout.line list; (* an anonymous box's *)
  backdrops : box list; (* an anonymous box's: its inline elements' boxes, a piece per line, drawn under its words *)
  marker : Html_layout.marker option; (* a list item's *)
}

(* [layout metrics ?picture_size ~viewport styles root]: the page laid
 * out in a window [viewport] (width, height), [styles e] each element's
 * computed style (Computed.styles), [picture_size src] a picture's own
 * size if it has come *)
val layout :
  Html_layout.metrics ->
  ?picture_size:(string -> (float * float) option) ->
  viewport:float * float ->
  (Dom.element -> Computed.t) ->
  Dom.element ->
  box

(* a picture's address: its src=, or else the first of its srcset=
 * (the pages that give only srcset=, the sizes chosen by the browser) *)
val picture_src : Dom.element -> string option

(* the look a fragment of text in this style is drawn with (Browser_draw
 * draws looks): its size, weight, slant, face, colour, decoration;
 * [link] the href around it *)
val look_of : Computed.t -> link:string option -> Looks.t

(* the same page as Html_layout's boxes (their borders and styles
 * dropped): for Hit, the form controls, the anchors *)
val as_html_layout : box -> Html_layout.box

(* every fragment of the page: each box's lines, then its children's
 * (an inline-block's words after its line's) *)
val fragments : box -> Html_layout.fragment list
