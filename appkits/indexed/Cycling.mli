(* Cycling: the palette turning, Deluxe Paint's colour cycling.

   A range of the palette, colours [low] to [high], turns: at each step
   every colour of the range moves to the next number, the last one
   going back to the first. Nothing in the picture changes -- its dots
   still hold the same numbers -- but what those numbers look like
   does. Paint a waterfall in stripes of the range's colours, a band
   of each, and the stripes run down it; a fire, and it flickers. On
   the Amiga it cost nothing: the chip read the palette at each frame
   anyway. It was the animation of every Amiga and PC game of the
   time, and Mark Ferrari's scenes for LucasArts made it art.

   Worked example (in the tests): a range from 2 to 4 over colours A, B,
   C: after one step, 2 shows C, 3 shows A, 4 shows B; after three,
   the palette as it was. A range with [reverse] turns the other way. *)

(* [palette_at palette ranges seconds]: the palette as it shows after
   [seconds] of cycling, the active ranges turned at their rates *)
val palette_at : (int * int * int) array -> Ilbm.range list -> float -> (int * int * int) array

(* [turn palette range steps]: one range turned [steps] times *)
val turn : (int * int * int) array -> Ilbm.range -> int -> (int * int * int) array
