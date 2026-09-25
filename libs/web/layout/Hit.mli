(* Hit: from a point on the page to what is drawn there -- the link a
   click follows -- and from a #fragment's name to where it is.

   Layout goes from the tree to places; a click goes back, from a
   place to the tree. **Hit testing** walks the boxes the layout made,
   down to the line whose top and bottom hold the point, and along it
   to the fragment whose left and right do; the fragment's look knows
   the link it is in (Looks.t.link: the href of the nearest <a href>
   around it, inherited like a colour). The page's coordinates are the
   layout's (x right, y down from the page's top); the app adds the
   scroll and turns its y over.

     <p>the <a href=recipes.html>recipes</a>   (the tests' metrics: a
                                               character 10, a space 10)
     click at (x 60, y 25)   the line from 19.2 to 31.2 -> its fragments
                             "the" 8..38, "recipes" 48..118 <- here
                             -> its link: "recipes.html"

   A link of several words is several fragments with spaces between
   them; a point in a space between two fragments of the same link is
   in the link too (Mosaic underlined the spaces and made them
   clickable), a space between two links is not.

   Browsers do the same, the other way up: each box knows its element,
   and the element its ancestors, so a click finds the <a> by walking up
   the tree (and the event bubbles up it, for scripts). Ours finds the
   link in the look because a look is all a fragment keeps.

   The other lookup, **anchors**: "#people" in a URL names a place in
   the page -- an <a name="people"> (HTML 2.0), or any element's
   id="people" (HTML 4) -- and the browser scrolls to it. The layout
   keeps each anchor on its line (a block's id is the block itself), so
   the place is the line's top, or the block's.

   Reference: Web Browser Engineering, chapter 7 ("Handling Buttons and
   Links"); WHATWG HTML, "Scrolling to a fragment" (which looks for an
   id in the whole page before an <a name>; ours takes the first of
   either). *)

(* the href of the link at a point of the page, if any *)
val link_at : Html_layout.box -> x:float -> y:float -> string option

(* where the page's anchor named so is (its y), if it has one: the
 * first in the page, an element's id or an <a name> *)
val anchor : Html_layout.box -> string -> float option
