(* Html_tree: tokens to a tree, the page's mistakes repaired.

   The tree a page means is not the one its tags spell. It omits end
   tags (legal in SGML, and in every page), omits <html>, <head> and
   <body>, and closes what it never opened. The worked example of
   notes_browser.md section 4, and the tests':

     <title>Lunch</title>            html
     <h1>Menu</h1>                    +- head
     <p>Soup of the day               |   +- title: "Lunch"
     <p>Salads:                       +- body
     <ul>                                 +- h1: "Menu"
     <li>tomato                           +- p: "Soup of the day\n"
     <li><b>cucumber</b>                  +- p: "Salads:\n"
     </ul>                                +- ul
                                              +- li: "tomato\n"
                                              +- li
                                                  +- b: "cucumber"

   (whitespace-only text left out of the picture). Four repairs made it:
   <html>, <head>, <body> implied; the second <p> closing the first;
   <ul> closing the open <p>; the second <li> closing the first.

   **The algorithm is a stack of open elements**, its top the element
   new content goes into:

     a start tag x   1. while an open element y that x closes (Dtd.closes)
                        is found looking down the stack, before one that
                        stops the search (Dtd.stops): pop down to y, y
                        included;
                     2. x becomes a child of the top;
                     3. x is pushed, unless it is void (Dtd.is_void).
     an end tag x    pop down to the nearest open x, if there is one
                     before the search is stopped (a table cell, the
                     root); if there is none, **ignore it** (a stray
                     </b>). </p> with no open <p> makes an empty one,
                     as every browser does.
     text            appended to the top (next to the text before it,
                     if that is the top's last child).

   The head and the body: <html>, <head> and <body> are made at once,
   the head on the stack. Head material (Dtd.is_head_element: title,
   meta, link, base, style, script) goes in the head while the body has
   not started; anything else -- a tag, or text that is not only
   spaces -- starts the body (pops the head, pushes the body). Later
   <html> and <body> tags only give their attributes (<body
   bgcolor=white>); </body> and </html> are ignored, so what follows
   them still lands in the body, as in every browser. Comments and the
   doctype are dropped. A newline right after <pre> is dropped too (the
   spec's: "<pre>\n  x" starts with the two spaces).

   Where ours and the WHATWG's differ: **misnested formatting**.

     <b><i>x</b>y</i>
       ours (and Mothra's): </b> pops i and b       b(i("x")), "y"
       the WHATWG's adoption agency algorithm:    b(i("x")), i("y")

   Every browser shows y in italics; reproducing it is notes_browser.md's
   first exercise.

   **Foreign content**: inside an <svg> (a picture in the page, as
   pages put their icons), XML's rules instead: an element closes only
   at its end tag or its "/>" (<path d="..."/>), and no HTML rule closes
   anything -- so the picture's tree is whole for Box_layout and Svg.
   (The spec's MathML, and its adjusting of SVG's attribute names'
   case, not done: attributes stay lower case, as Svg reads them.)

   Also not done: tables' implied <tbody> and the
   "foster parenting" of what is misplaced in a table (phase 10's),
   forms' rules, <frameset>, and the quirks of the spec's 23 insertion
   modes beyond the three above (before the body, in it, after it).

   Reference: WHATWG HTML, 13.2.6 "Tree construction" -- the stack of
   open elements, "has an element in scope", "generate
   implied end tags", the "in body" insertion mode; MMM's html_eval.ml,
   which does the same from its DTD; html5lib, the reference parser
   the tests' trees were compared with by hand. *)

(* the tree of a page's tokens: its root is always <html>, with a
 * <head> and a <body> *)
val parse : Html_lexer.token list -> Dom.element

(* Html_lexer.tokenize, then parse *)
val of_string : string -> Dom.element
