(* Convolve: a pixel's new value from its neighbours, weighted.

   A kernel is a small square of weights, centred on the pixel: the new
   value is the sum of the neighbours' values times their weights,
   divided by [divisor], plus [offset]. Photoshop's Filter > Other >
   Custom shows exactly this grid, and its Sharpen, Blur and Emboss are
   kernels:

       blur          sharpen           emboss
       1 1 1          0 -1  0          -1 -1  0
       1 1 1  / 9    -1  5 -1          -1  0  1
       1 1 1          0 -1  0           0  1  1   + 128

   Blur averages; sharpen adds to the pixel how much it differs from
   its neighbours (5 = 1 + 4 times the difference); emboss keeps only
   the difference along one diagonal, around a middle grey, and the
   picture seems carved. Beyond the image's border the nearest pixel is
   read again (the edges clamped).

   Worked example (in the tests): a 3 by 3 image all 10 but 100 in the
   middle: blurred, the middle becomes (8 x 10 + 100) / 9 = 20;
   sharpened, 5 x 100 - 4 x 10 = 460, clamped to 255.

   A kernel of n by n costs n squared multiplications a pixel; one
   that is a column times a row -- a "separable" one, like the
   Gaussian -- is applied as a row then a column, 2 n (Gaussian.mli). *)

type kernel = { size : int; weights : int array; divisor : int; offset : int }

(* [apply k img]: every channel but alpha *)
val apply : kernel -> Pixels.image -> Pixels.image

(* [separable weights img]: a 1D kernel along each row, then each
   column (weights summing to 1) *)
val separable : float array -> Pixels.image -> Pixels.image

val blur : kernel
val blur_more : kernel
val sharpen : kernel
val sharpen_more : kernel
val emboss : kernel
