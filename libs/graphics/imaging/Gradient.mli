(* Gradient: a colour fading into another, the gradient tool.

   Drag from a point to another: each pixel's place along that line,
   its projection on it, from 0 at the start to 1 at the end, clamped
   before and after, decides its mix of the two colours (Photoshop's
   foreground and background):

       t = ((p - a) . (b - a)) / |b - a|^2

   Worked example (in the tests): from (0, 0) to (10, 0), black to
   white: the pixel at x 5 (its centre 5.5) is 140, 55% of the way. *)

val linear : ?selection:Mask.t -> float * float -> float * float -> int * int * int -> int * int * int -> Pixels.image -> Pixels.image
