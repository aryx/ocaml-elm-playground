(* Looks: what each element of a page looks like -- Mosaic's fixed
   table, computed down the tree.

   Before style sheets, a browser decided the looks. Mosaic's were in
   its X resources (the font of each heading, the colour of a link),
   the page could only choose among elements, and every browser since
   has kept the same table as its default: CSS 2.1 wrote it down in its
   appendix D, "Default style sheet for HTML 4", from which the numbers
   here come.

     element                 look                      box
     h1 ... h6               bold; 2, 1.5, 1.17, 1,    a block, margins
                             0.83, 0.75 em             0.67 ... 1.67 em
     p, dl, blockquote,      -                         a block, margins
       ul, ol, dir, menu                               1.12 em
     b, strong               bold                      inline
     i, em, cite, var, dfn   italic                    inline
     tt, code, kbd, samp     fixed width               inline
     pre                     fixed width, lines kept   a block, margins 1 em
     a href=                 blue, underlined          inline
     u / s, strike, del      underlined / struck       inline
     big / small, sub, sup   1.17 / 0.83 em            inline
     center                  lines centred             a block
     ul, ol, dir, menu, dd   -                         indented 40
     blockquote              -                         indented 40, both sides
     hr                      -                         a rule, margins 0.5 em
     head, title, script,    -                         not shown
       style, meta, link
     body                    -                         margins 8

   (px in CSS: here the page's units, the root's size 16 as a browser's
   text is 16 px). And HTML 3.2's align= on a paragraph, a heading or a
   div (center, right).

   **A look is inherited**: the text inside <b> inside <h1> is bold and
   heading-sized, so an element's look is its parent's with its own
   changes made ([look parent e]), computed going down the tree -- the
   one idea CSS kept as it is. **A box is not**: a paragraph's margins
   are not its words' ([box]). An em is the element's own size: an
   h1's 0.67 em margin is 0.67 of its 2 em, 21.4 at a root of 16.

   **Netscape's extensions** (Dtd.origin), honoured only when the
   root's look says so ([root ~extensions:true], TinyNetscape's);
   otherwise an extension element is an unknown tag (its content in
   its parent's look, a box of nothing) and an extension attribute is
   not seen, as in Mosaic:

     element / attribute     look
     center                  lines centred, a block
     font size=1..7, +n, -n  1 to 7 on HTML's scale, 3 the root's size:
                             x 0.63 0.82 1 1.13 1.5 2 3 (CSS's
                             keywords, x-small to xx-large, and 48 px)
     font color=             #rrggbb, or one of HTML 3.2's 16 names
     body text= link= vlink= the text's, a link's, a visited link's
     p, h1-h6 align=         as div's

   Phase 8 turns this table into the first style sheet of a cascade,
   the "user agent style sheet" -- which is what it always was.

   Reference: W3C, CSS 2.1, appendix D (the default style sheet for
   HTML 4) and section 6.2 (inheritance); HTML 3.2 (align=); Mosaic's
   Mosaic.ad resources. *)

type color = int * int * int (* red, green, blue, 0-255 *)
type align = Left | Center | Right

type t = {
  size : float; (* an em, in the page's units *)
  bold : bool;
  italic : bool;
  underline : bool;
  strike : bool;
  monospace : bool;
  color : color;
  link : string option; (* the href of the link the text is in *)
  pre : bool; (* spaces and newlines kept, lines never broken *)
  align : align; (* where a block's lines go *)
  link_color : color; (* a link's, blue; body link= *)
  visited_color : color; (* a visited link's, purple; body vlink= *)
  base : float; (* the root's size: <font size=3> *)
  extensions : bool; (* Netscape's honoured *)
}

(* the root's look: black text of [size], all off; Netscape's extensions
 * honoured if [extensions] (false) *)
val root : ?extensions:bool -> size:float -> unit -> t

(* #rrggbb, or one of HTML 3.2's 16 colour names (Windows' VGA palette:
 * black, silver, gray, white, maroon, red, purple, fuchsia, green,
 * lime, olive, yellow, navy, blue, teal, aqua), any case *)
val color_of_string : string -> color option

(* <font size=>'s factor of the root's size: "1" to "7", or "+n", "-n"
 * from 3, kept within 1..7 *)
val font_scale : string -> float option

(* the height of a line, in ems: CSS's "normal", about what fonts ask *)
val leading : float

(* [look parent e]: e's look, its parent's being [parent] *)
val look : t -> Dom.element -> t

type display =
  | Block (* on its own lines, stacked with its siblings *)
  | Inline (* in its parent's lines *)
  | Rule (* a horizontal line: hr *)
  | Hidden (* not shown at all *)

type box = {
  display : display;
  margin_top : float;
  margin_bottom : float;
  indent : float; (* from the parent's left edge: margin and padding *)
  right : float; (* the same from its right edge *)
}

(* the box of e, its look being [look] (for its ems, and whether
 * Netscape's elements are known) *)
val box : t -> Dom.element -> box
