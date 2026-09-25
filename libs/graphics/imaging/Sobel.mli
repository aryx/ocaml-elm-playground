(* Sobel: where the picture changes, Photoshop's Find Edges.

   An edge is where the values change quickly, a large gradient. Irwin
   Sobel's operator (1968) measures it with two kernels, the change
   across (x) and down (y), each smoothed along the other direction:

       gx:  -1  0  1        gy:  -1 -2 -1
            -2  0  2              0  0  0
            -1  0  1              1  2  1

   and the edge's strength is the length of (gx, gy). Find Edges draws
   it dark on white, as a pencil sketch: 255 less the strength, per
   channel.

   Worked example (in the tests): an image black on its left half and
   white on its right: on the column where it changes, gx = 4 x 255,
   the edge at full strength (0, black), and far from it nothing (255,
   white). *)

(* the gradient's length at (x, y) in channel c, unclamped *)
val magnitude : Pixels.image -> int -> int -> int -> float

val find_edges : Pixels.image -> Pixels.image
