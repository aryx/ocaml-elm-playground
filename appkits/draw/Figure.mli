(* One object of a drawing: a line, a rectangle, an oval, a piece of
 * text -- or a group of them, which is an object too.
 *
 * This is what separates a *drawing* program from a paint program
 * (appkits/paint): MacPaint's rectangle is gone once it is drawn, only
 * the dots it left remain; MacDraw's (1984) is still a rectangle,
 * which can be clicked, moved, resized, filled differently, sent
 * behind another. Bitmap against objects is still the difference
 * between Photoshop and Illustrator.
 *
 * Coordinates are the playground's: y up. A box is two corners,
 * always in order (x0 <= x1, y0 <= y1). Three ideas, one per group of
 * functions below:
 *
 * - **Hit testing** answers "did this click land on me?", and its
 *   subtlety is MacDraw's own: a *filled* shape is hit anywhere inside
 *   it, a *hollow* one only near its outline -- a click in the middle
 *   of an unfilled rectangle goes through to whatever is behind it,
 *   because nothing of the rectangle is there.
 *
 * - **Resizing is an affine map**: a figure is fitted to a new box by
 *   mapping its old bounds onto the new ones,
 *
 *     x' = nx0 + (x - x0) * (nx1 - nx0) / (x1 - x0)      (and y alike)
 *
 *   applied to every point it has. So a group, resized, scales
 *   everything in it, however deep -- the transform goes down the tree.
 *   (Pen widths and text sizes are not scaled, as in MacDraw.)
 *
 * - **Handles**: the eight squares round a selected figure's bounds
 *   (four corners, four sides), or a line's two ends; dragging one
 *   moves that corner, side or end, the opposite one staying put. *)

(*****************************************************************************)
(* {1 Figures and their boxes} *)
(*****************************************************************************)

type point = float * float
type box = { x0 : float; y0 : float; x1 : float; y1 : float }

(* the box with two opposite corners, whichever two *)
val box : point -> point -> box

(* [fill]: a grey from 0 (black) to 1 (white), or None for hollow;
 * [pen]: the outline's width *)
type style = { fill : float option; pen : float }

type t =
  | Line of point * point * style
  | Rect of box * style
  | Oval of box * style
  | Text of box * string * float (* its box, the text, its size *)
  | Group of t list

val bounds : t -> box

(* the box round two boxes *)
val union : box -> box -> box

(*****************************************************************************)
(* {1 Hit testing} *)
(*****************************************************************************)

(* [hit ~tolerance figure point]: whether the point is on the figure --
 * inside if it is filled (text always is), within [tolerance] of its
 * outline if it is hollow, within [tolerance] of a line *)
val hit : tolerance:float -> t -> point -> bool

(*****************************************************************************)
(* {1 Moving and resizing} *)
(*****************************************************************************)

val translate : float -> float -> t -> t

(* [fit box figure]: the figure resized so that its bounds are [box] *)
val fit : box -> t -> t

(*****************************************************************************)
(* {1 Handles} *)
(*****************************************************************************)

(* the handles, in order: a line's two ends; else the corners from the
 * top-left clockwise, then the sides top, right, bottom, left *)
val handles : t -> point list

(* [drag_handle figure i point]: handle [i] moved to [point]; a side
 * dragged past its opposite one makes the box the other way round (it
 * is not mirrored: the corners are sorted again) *)
val drag_handle : t -> int -> point -> t

(*****************************************************************************)
(* {1 Restyling} *)
(*****************************************************************************)

(* every style in it changed, a group's children included *)
val restyle : (style -> style) -> t -> t
