(* Lut: point operations, a pixel's new value from its old one alone.

   When the new value of a channel depends only on its old value -- not
   on where the pixel is, nor on its neighbours -- the whole operation
   is a table of 256 entries, a "look-up table", computed once and read
   for each pixel. Photoshop's Image > Adjust menu is mostly these, and
   each is a curve drawn from old values (across) to new ones (up):

       255 |        ____          levels: black 50, white 200
           |       /              (the values between stretched to
           |      /                the whole range, those outside
           |     /                 clipped to black and white)
         0 |____/_________
           0   50    200  255

   Worked example (in the tests): levels ~black:50 ~white:200
   ~gamma:1. gives 0 for 50 and below, 255 for 200 and above, and 128
   for 125, halfway. A gamma above 1 lifts the middle and keeps the
   ends: gamma 2. sends 125 to 180 (255 times the square root of one
   half, 180.3).

   Tables compose: two adjustments are one table, [compose f g] being g
   after f -- how Photoshop previewed a chain of them at the price of
   one. *)

type t = int array (* 256 entries, each 0 to 255 *)

val identity : t
val invert : t

(* [posterize n]: [n] levels per channel, 2 to 255 *)
val posterize : int -> t

(* [brightness_contrast ~brightness ~contrast]: each from -100 to 100;
   brightness moves every value up or down, contrast pulls them from
   (or towards) the middle grey, 128 *)
val brightness_contrast : brightness:int -> contrast:int -> t

(* [levels ~black ~white ~gamma]: the Levels dialog's three input
   sliders *)
val levels : black:int -> white:int -> gamma:float -> t

(* [curves points]: a curve through (input, output) points, 0 to 255,
   the ends (0, 0) and (255, 255) unless given: a monotone cubic
   (Fritsch and Carlson, 1980), smooth through each point and never
   overshooting between them -- a curve that rises stays rising *)
val curves : (int * int) list -> t

(* [compose f g]: g after f *)
val compose : t -> t -> t

(* the same table on red, green and blue; alpha kept *)
val apply : t -> Pixels.image -> Pixels.image

(* a table per channel: Levels' red, green and blue *)
val apply_rgb : t -> t -> t -> Pixels.image -> Pixels.image

(* [threshold level img]: black or white, by luminance -- the one of
   these that is not per channel *)
val threshold : int -> Pixels.image -> Pixels.image

(* grey by luminance: Desaturate *)
val desaturate : Pixels.image -> Pixels.image
