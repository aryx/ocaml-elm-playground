(* Browser_boxes: a page laid out by the box model (Box_layout) as the
 * playground's shapes -- TinyChrome's drawing, where Browser_draw is
 * the teaching browsers'.
 *
 * CSS's painting order, simplified (CSS 2.1 appendix E): each box's
 * background, then its border, then what is inside it -- its inline
 * elements' backgrounds and borders, its lines' words and pictures
 * (Browser_draw.glyphs, the same pen), its list marker, its children
 * in order; the absolute boxes come last, being
 * the page's last children. A colour with transparency is mixed with
 * white (the page under it, most of the time).
 *
 * A box whose overflow is not visible (hidden, and here auto and
 * scroll, a box that would scroll) clips what it holds -- by what is
 * drawn, not by pixels (the playground has no clipping): a background
 * or a border cut to it, a line or a word not wholly inside left out.
 * What screen readers are given in a 1-pixel box is so hidden, and a
 * table of contents taller than its column cut at its bottom.
 *
 * A background-image is drawn once its picture has come, at its own
 * size at the box's top left, or shrunk to fit the box if larger:
 * background-size, -position and -repeat read as that.
 *
 * A mask-image (Wikipedia's icons: an SVG, the box's background
 * showing through its shape in currentColor) is drawn as its picture
 * tinted with the background's colour, its alpha kept, fitted to the
 * box and centred -- what a mask of one colour gives.
 *
 * Not drawn: border styles (every border solid), rounded corners,
 * shadows, gradients. *)

(* the page's shapes, each with its top and bottom on the page (y down,
 * turned over: Browser_draw.drawn) *)
val draw :
  visited:(string -> bool) -> picture_of:(string -> Browser_picture.t option) -> Box_layout.box -> Browser_draw.drawn

(* a CSS colour as the playground's, its transparency mixed with white;
 * None if fully transparent *)
val color : Css_values.color -> Playground.color option
