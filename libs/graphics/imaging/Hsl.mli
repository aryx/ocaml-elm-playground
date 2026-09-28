(* Hsl: colour as hue, saturation and lightness.

   Red, green and blue are how a screen makes a colour, not how a
   person names one. HSL (Alvy Ray Smith's HSV, 1978, and its HSL
   twin) turns the colour cube on its black-to-white diagonal:

   - hue, the angle around it, 0 to 360: red at 0, yellow 60, green
     120, cyan 180, blue 240, magenta 300;
   - saturation, how far from the grey axis, 0 to 1;
   - lightness, how far along it, 0 black to 1 white, pure colours at
     one half.

   So "a little more saturated" or "bluer" is one number moved, which
   is Photoshop's Hue/Saturation dialog: all the pixels' hues turned by
   the same angle, their saturations and lightnesses pushed.

   Worked example (in the tests): pure red (255, 0, 0) is hue 0,
   saturation 1, lightness 0.5; its hue turned by 120 degrees, pure
   green (0, 255, 0). *)

(* 0 to 255 each, to hue (degrees), saturation and lightness (0 to 1) *)
val of_rgb : int -> int -> int -> float * float * float
val to_rgb : float -> float -> float -> int * int * int

(* [hue_saturation ~hue ~saturation ~lightness img]: hue in degrees
   (-180 to 180), saturation and lightness from -100 to 100, as the
   dialog's sliders *)
val hue_saturation : hue:float -> saturation:float -> lightness:float -> Pixels.image -> Pixels.image
