(* Browser_draw: a laid-out page (Html_layout) as the playground's
 * shapes -- the letters by Hershey's pen, the pictures by their pixels,
 * the rules and the list markers, the form controls in Motif's look
 * (Mosaic's and Netscape's toolkit on X), and, for an inspector, the
 * boxes outlined.
 *
 * The page's coordinates are the layout's, x right and y down from the
 * page's top; the shapes here are the same turned over (y up, a line
 * below the top negative), so that a browser moves the whole page into
 * its window and scrolls it with one move. Each thing drawn comes with
 * its top and bottom on the page ([drawn]), so that a frame shows only
 * what is in the window (culling).
 *
 * What the drawing needs to know that the layout does not, the browser
 * gives: which links were visited (purple), which pictures have come,
 * each control's value (the browser's, never the page's tree), and
 * which field has the keys (its caret). *)

(* things drawn, each with its top and bottom on the page *)
type drawn = (float * float * Playground.shape) list

(* a rectangle's outline, [t] (1) thick, its top-left at (x, y) of the
 * page, [w] by [h] *)
val frame : ?t:float -> Playground.color -> float -> float -> float -> float -> Playground.shape

(* Motif's two bevels, at (x, top), w by h: a raised thing (a button)
 * lit from the top left, a sunken one (a field) the other way *)
val raised : float -> float -> float -> float -> Playground.shape list

val sunken : float -> float -> float -> float -> Playground.shape list

(* a fragment's shapes: a word's letters (a link's in blue, purple if
 * [visited]), a picture ([picture_of] its src: arrived, the room kept,
 * the broken image; a link's framed in its colour); a control's are
 * [control_shapes]' *)
val glyphs :
  ?visited:(string -> bool) -> ?picture_of:(string -> Browser_picture.t option) -> Html_layout.fragment -> Playground.shape list

(* the whole page but its controls: every line, rule and marker *)
val draw : visited:(string -> bool) -> picture_of:(string -> Browser_picture.t option) -> Html_layout.box -> drawn

(* a control with its [value], its caret if [focused] *)
val control_shapes :
  value:(Dom.element -> Forms.value) -> focused:bool -> Html_layout.fragment -> Html_layout.control -> Playground.shape list

(* every control of the page, drawn with its value (every frame: they
 * change as one types, where the rest is drawn once a layout) *)
val controls_drawn : value:(Dom.element -> Forms.value) -> focus:Dom.element option -> Html_layout.box -> drawn

(* the layout's boxes outlined, as a browser's inspector does: blocks
 * blue, the anonymous boxes of inline content green, their lines grey *)
val outlines : Html_layout.box -> drawn
