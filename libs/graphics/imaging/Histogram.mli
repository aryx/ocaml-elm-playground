(* Histogram: how many pixels have each value.

   For each channel, and for the luminance, an array of 256 counts: a
   picture's tones at a glance, what the Levels dialog draws above its
   sliders. A dark picture is a histogram crowded to the left; one
   with no pure black or white leaves both ends empty -- which is what
   Auto Levels looks for, and stretches away.

   Worked example (in the tests): a 2 by 2 image, pixels black, white,
   white and pure red (255, 0, 0): red counts 1 at 0 and 3 at 255;
   luminance counts 1 at 0, 1 at 76 (red's, 299 per thousand of 255)
   and 2 at 255. *)

type t = { red : int array; green : int array; blue : int array; luminance : int array }

val compute : Pixels.image -> t

(* [auto_levels img]: Levels' black and white points, where the
   luminance histogram's darkest and brightest [clip] (0.5% by
   default) of the pixels begin: the tones stretched to the full range,
   a few outliers let go *)
val auto_levels : ?clip:float -> t -> int * int
