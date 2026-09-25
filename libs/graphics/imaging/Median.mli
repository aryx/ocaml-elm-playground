(* Median: each value replaced by the middle one of its neighbourhood.

   Not a convolution: no weights, a sort. In a square of (2 r + 1)
   squared pixels, the median is the value half of them are below: a
   speck of dust, one bright pixel among dark ones, never becomes the
   median, so it vanishes -- where a blur would smear it -- and an edge,
   half one value and half the other, stays sharp. Photoshop's Noise >
   Median.

   Worked example (in the tests): a 3 by 3 image all 10 but 255 in the
   middle (a speck): with radius 1, the middle becomes 10. *)

val median : radius:int -> Pixels.image -> Pixels.image
