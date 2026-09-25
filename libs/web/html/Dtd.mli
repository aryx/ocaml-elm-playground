(* Dtd: what HTML says about its elements, as data -- which have no
   content, which belong in the head, which end which.

   HTML was an SGML language, and SGML described a language by its DTD
   (Document Type Definition), a grammar that also said which tags a
   writer could leave out:

     <!ELEMENT P  - O (%text)*>      P: start tag required (-), end tag
                                     omissible (O); contains text
     <!ELEMENT LI - O %flow>         LI: the same
     <!ELEMENT BR - O EMPTY>         BR: no content, so no end tag

   An SGML parser read the DTD and inferred the missing tags: a <p>
   cannot contain a <p>, so the second one ends the first. Browsers
   never read DTDs; they hard-coded the rules. MMM (1996) did it in
   between, and this module follows it: the rules as OCaml data, one
   table per question (MMM's dtd.ml has HTML 2.0's and 3.2's), read by
   Html_tree's one algorithm.

   The questions, and HTML 3.2's answers (with a few of HTML5's where
   the pages of the time already relied on them):

     void        no content, never pushed:   br hr img input meta ...
     head        belongs in the head:         title meta link base style script
     closes x y  starting an x ends an open y:
                   p   by a block (address blockquote center dir div dl
                       form h1-h6 hr menu ol p pre table ul, and li dt dd)
                   li  by li;  dt, dd by dt or dd;  option by option
                   h1-h6 by h1-h6 (a heading holds no heading)
                   tr by tr;  td, th by td, th or tr
     stops x y   looking down the stack for what x closes, y stops the
                 search: an li in a nested list does not end the outer
                 list's li, nor does one inside a blockquote; nothing is
                 closed across a table cell

   **Where each thing comes from.** HTML 2.0 (RFC 1866, 1995) wrote
   down what Mosaic read; Netscape (1.0, December 1994, and 1.1, 1995)
   added to it what pages wanted and did not ask anyone -- the
   "Netscape extensions", most of them in HTML 3.2 two years later. A
   page of 1995 mixes both, so the table below says which is which,
   and the tree keeps the mark (Html_lexer's tokens, Dom's elements):
   the core first, the extensions apart, and a browser honours only
   what it knows -- TinyMosaic the core, TinyNetscape the extensions
   too, as Mosaic and Netscape did (an extension unknown to a browser
   is an unknown tag: ignored, its content shown).

     Netscape's elements      font (size, color), basefont, center,
                              blink, nobr, wbr; and (1.1) table,
                              caption, tr, td, th (with HTML 4's
                              thead, tbody, tfoot)
     Netscape's attributes    body: bgcolor, text, link, vlink, alink,
                                    background (1.1)
                              hr: size, width, align, noshade
                              br: clear
                              img: width, height, border, hspace, vspace
                              ul: type;  ol: type, start;  li: type, value
                              p, h1-h6: align (1.1)
     and values               img align=left, right (HTML 2.0 had top,
                              middle, bottom): pictures the text flows
                              around, the first floats

   What is not in the table counts as core: HTML 3.2's own (div) is
   left so.

   Reference: HTML 3.2 Reference Specification (W3C, 1997), its DTD;
   RFC 1866 (HTML 2.0), section 9, its DTD; Netscape Communications,
   "Extensions to HTML" and "Extensions to HTML 2.0" (1994, 1995);
   WHATWG HTML, 13.2.6 "Tree construction" (the lists of "special"
   elements and of what each start tag closes); MMM's dtd.ml. *)

(* where an element or an attribute comes from *)
type origin =
  | Core (* HTML 2.0, what Mosaic read *)
  | Netscape (* Netscape's extensions *)

(* <br>, <img>...: no content, so no end tag and never on the stack *)
val is_void : string -> bool

(* <title>, <meta>...: in the head, when met before the body started *)
val is_head_element : string -> bool

(* the block elements: those that end an open <p> *)
val is_block : string -> bool

(* [closes x y]: a start tag x ends an open element y *)
val closes : string -> string -> bool

(* [stops x y]: looking for what x closes, an open y stops the search *)
val stops : string -> string -> bool

(* where the element of this name comes from *)
val element_origin : string -> origin

(* [attribute_origin element (name, value)]: where an attribute of a
 * core element comes from, its value included (img's align=left) *)
val attribute_origin : string -> string * string -> origin
