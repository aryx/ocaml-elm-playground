(* What MacPaint's tools put into a picture: a dot, a line, a brush
 * stroke, a rectangle, an oval -- every one of them in a pattern, so
 * that black is [Pattern.solid], white [Pattern.white], and the
 * eraser is a brush painting white. All of them in place, on a bitmap
 * nobody else holds (see Bitmap.change); all clipped to the picture.
 *
 * Corners are dots and inclusive: a rectangle from (2, 1) to (5, 3) is
 * 4 dots wide and 3 high, as a person dragging from one dot to another
 * expects -- whichever corner the drag started from. *)

(* [dot b p x y]: one dot, black where the pattern is *)
val dot : Bitmap.t -> Pattern.t -> int -> int -> unit

(* [line_dots (x0, y0) (x1, y1)]: the dots of a line, from its first end
 * to its last, by Bresenham's algorithm (Jack Bresenham, IBM, 1965):
 * one dot per step along the longer axis, and a step along the other
 * whenever the accumulated error says the true line has moved half a
 * dot away -- in integers only, no division, which is why a plotter
 * could do it and why everything still does. From (0, 0) to (5, 2):
 *
 *   ##....       (0,0) (1,0)
 *   ..##..       (2,1) (3,1)
 *   ....##       (4,2) (5,2)
 *)
val line_dots : int * int -> int * int -> (int * int) list

(* [stroke b ~brush p a z]: the brush put down at every dot of the line
 * from [a] to [z] -- how a drag becomes a continuous stroke though the
 * mouse is only seen once a frame, a dozen dots apart when it moves
 * fast *)
val stroke : Bitmap.t -> brush:(int * int) list -> Pattern.t -> int * int -> int * int -> unit

(* brushes, as the dots they cover around the mouse *)
val pencil : (int * int) list (* one dot *)
val round : int -> (int * int) list (* a disc of that radius *)
val square : int -> (int * int) list (* a square of that side *)

(* the rectangle between two corners: its outline, or all of it *)
val frame_rect : Bitmap.t -> Pattern.t -> int * int -> int * int -> unit
val fill_rect : Bitmap.t -> Pattern.t -> int * int -> int * int -> unit

(* The oval inscribed in the rectangle between two corners: all the
 * dots whose centres are inside the ellipse, and for the outline, those
 * of them with a neighbour (left, right, up, down) outside -- the
 * outline is then exactly the edge of the filled oval, so a frame and
 * a fill of the same oval always meet, with no gap and no dot outside
 * the frame. *)
val frame_oval : Bitmap.t -> Pattern.t -> int * int -> int * int -> unit
val fill_oval : Bitmap.t -> Pattern.t -> int * int -> int * int -> unit
