(* Gaussian: the blur of a lens out of focus, and the sharpening made
   from it.

   A box blur (Convolve.blur) gives each neighbour the same weight, and
   its squares show; a lens spreads a point of light as a bell, heavier
   in the middle, fading with the distance: Gauss's function,
   e^(-d^2 / 2 sigma^2). Its radius, Photoshop's "Radius" in Gaussian
   Blur, is sigma: most of the bell (99.7%) is within 3 sigma, so the
   kernel stops there.

   The bell of two dimensions is the product of two bells of one,
   along x and along y, so the blur is a row pass then a column pass
   (Convolve.separable): a radius of 10 reads 2 x 61 neighbours a pixel
   instead of 61 x 61.

   Unsharp masking, the darkroom's trick (a blurred negative, the
   "unsharp mask", sandwiched with the photograph): the difference
   between the picture and its blur is its detail; add [amount] of it
   back, and the edges stand out. [threshold] leaves alone the
   differences smaller than it: a sky's grain is not sharpened.

   Worked example (in the tests): the kernel of sigma 1 has 7 weights
   (3 sigma each side), symmetric, summing to 1, the middle one 0.399
   (1 over the square root of 2 pi). *)

val kernel : float -> float array

(* [blur ~radius img]: radius is sigma, in pixels *)
val blur : radius:float -> Pixels.image -> Pixels.image

(* [unsharp ~amount ~radius ~threshold img]: amount in percent (Photoshop's
   default 50 to 500), threshold 0 to 255 *)
val unsharp : amount:float -> radius:float -> threshold:int -> Pixels.image -> Pixels.image
