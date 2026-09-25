(* Scale: a picture made larger or smaller, Image Size.

   A pixel of the new picture, at (x, y), comes from the point of the
   old one it falls on, (x + 0.5) x old / new - 0.5 (pixel centres
   matched, not corners), which is almost never on a pixel of the old:
   what to read there is interpolation, three ways, each smoother and
   dearer than the one before.

   - nearest: the closest old pixel. Blocks when enlarging, jagged
     edges when shrinking; but no colour that was not there (the pixel
     artist's choice).
   - bilinear: the four around it, weighted by how near each is: a
     straight ramp between them. Soft.
   - bicubic: the sixteen around it, a cubic curve through them (Robert
     Keys's, 1981, a = -1/2): smooth, and sharper than bilinear, the
     curve slightly overshooting at edges -- Photoshop's default.

   Worked example (in the tests): a 2 by 1 image, 0 then 200, made 4
   by 1 bilinearly: its pixels fall at -0.25, 0.25, 0.75 and 1.25 in
   the old one, so 0, 50, 150, 200 (the ends clamped). *)

type method_ = Nearest | Bilinear | Bicubic

val resize : method_ -> width:int -> height:int -> Pixels.image -> Pixels.image

val flip_horizontal : Pixels.image -> Pixels.image
val flip_vertical : Pixels.image -> Pixels.image

(* a quarter turn clockwise: the width and height swapped *)
val rotate_90 : Pixels.image -> Pixels.image
