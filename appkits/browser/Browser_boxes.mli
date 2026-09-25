(* Browser_boxes: a page laid out by the box model (Box_layout) as the
 * playground's shapes -- TinyChrome's drawing, where Browser_draw is
 * the teaching browsers'.
 *
 * CSS's painting order, simplified (CSS 2.1 appendix E): each box's
 * background, then its border, then what is inside it -- its lines'
 * words and pictures (Browser_draw.glyphs, the same pen), its list
 * marker, its children in order; the absolute boxes come last, being
 * the page's last children. A colour with transparency is mixed with
 * white (the page under it, most of the time).
 *
 * Not drawn: border styles (every border solid), rounded corners,
 * shadows, background images, overflow's clip. *)

(* the page's shapes, each with its top and bottom on the page (y down,
 * turned over: Browser_draw.drawn) *)
val draw :
  visited:(string -> bool) -> picture_of:(string -> Browser_picture.t option) -> Box_layout.box -> Browser_draw.drawn

(* a CSS colour as the playground's, its transparency mixed with white;
 * None if fully transparent *)
val color : Css_values.color -> Playground.color option
