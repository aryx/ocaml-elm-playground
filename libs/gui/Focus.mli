(* Who gets the keys (notes_gui.md section 6).
 *
 * A click can say which widget it means -- it has a position, and the
 * hit test answers. A key press cannot: "e" belongs to whichever
 * field the person last pointed at, and that is the whole of what
 * focus is. Every application's keyboard experience is this, a ring
 * drawn around the focused thing, and the order Tab walks in.
 *
 *     click on a field      ->  it has the focus, its caret shows
 *     type                  ->  the characters go there and nowhere else
 *     Tab                   ->  the next one in order takes it
 *     Shift-Tab             ->  the previous one
 *     click on the backdrop ->  nobody has it
 *
 * The **tab order** is the interesting part here. A retained toolkit
 * has a tree, so it walks it -- and then needs an escape hatch when
 * the tree's order is not the reading order (the web's [tabindex],
 * and the accessibility bugs that come of getting it wrong). In
 * immediate mode there is no tree: the widgets are simply *asked for*
 * in an order, each frame, and that order is the tab order, for free
 * and in the source where you can see it.
 *
 * With one wrinkle worth knowing, because it is the price of having
 * no tree: when Tab arrives, this frame's order does not exist yet --
 * the widgets have not been asked for. So the walk uses the *previous*
 * frame's order, which is the same order unless the interface changed
 * shape in that very frame, and then Tab lands one widget off, once.
 *
 * Worked example, three fields asked for in the order A, B, C:
 *
 *   focus     Tab      Shift-Tab
 *   (none)    A        C           (nobody yet: Tab takes the first)
 *   A         B        C           (and Shift-Tab wraps round)
 *   C         A        B
 *)

(* who has the keys, and the order the widgets were asked for *)
type t

(* nobody has the focus *)
val none : t

(* start a frame: this frame's order starts empty, and the order Tab
 * walks is the one the last frame saw *)
val frame : t -> t

(* [saw id t]: a widget that can take the keys was asked for; its
 * place in the tab order is where it was asked *)
val saw : Widget.id -> t -> t

val has : Widget.id -> t -> bool
val give : Widget.id -> t -> t
val clear : t -> t

(* Tab and Shift-Tab: the next (previous) widget of the last frame's
 * order, wrapping round; with nobody focused, the first (last) *)
val next : t -> t
val previous : t -> t
