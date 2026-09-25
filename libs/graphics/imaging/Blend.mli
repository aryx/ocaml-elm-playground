(* Blend: how a layer's colour meets the colour under it.

   Photoshop 3.0 (1994) gave each layer a mode: the function B that
   takes the backdrop's colour b and the layer's s, each channel from
   0 to 1, and gives what the layer shows there. Most work a channel at
   a time ("separable"):

     Normal       s                   the layer covers
     Multiply     b s                 darker: white is neutral, like two
                                      slides projected through each other
     Screen       1 - (1 - b)(1 - s)  lighter: black is neutral, like two
                                      projectors on one screen
     Overlay      Multiply where the backdrop is dark (b <= 1/2, doubled),
                  Screen where it is light: contrast, the mid-tones kept
     Hard Light   Overlay with the two swapped: decided by the layer
     Darken, Lighten   the smaller, the larger
     Difference   |b - s|             black where they agree

   and one does not: Color keeps the layer's hue and saturation and the
   backdrop's lightness (Hsl.mli) -- a black and white photograph
   tinted by a coloured layer, as colourists painted them.

   Worked example (in the tests): a mid grey, 128 (0.502), over the
   same grey: Multiply gives 64, a quarter; Screen 192, three
   quarters; Difference 0.

   How the result is then mixed with the backdrop by the layer's alpha
   is Layers.mli's. *)

type mode = Normal | Multiply | Screen | Overlay | Hard_light | Darken | Lighten | Difference | Color

val modes : mode list
val name : mode -> string

(* [blend mode (br, bg, bb) (sr, sg, sb)]: each channel 0 to 1 *)
val blend : mode -> float * float * float -> float * float * float -> float * float * float
