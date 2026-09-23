(* Motion: motion estimation -- an encoder's search for where each
 * macroblock came from.

   The standard fixes what a decoder does with a vector (Mpeg1.mli),
   not how an encoder finds one: that is where encoders compete. For
   each 16 x 16 macroblock of the frame being coded, look in the
   reference frame for the square most like it, by the **sum of absolute
   differences** (SAD) of their 256 brightnesses -- the cheapest measure
   of "alike", and good enough.

   **Full search** tries every whole-pixel offset in a window, (2r + 1)^2
   of them: for r = 16, 1,089 candidates, 278,784 subtractions a
   macroblock -- the encoder's whole cost, and the answer is the best
   there is (in the window, by SAD). **Logarithmic search** (the
   three-step search of Koga et al., 1981, for r = 7: steps 4, 2, 1)
   tries the 9 points of a square around the best so far, halves the
   step, and again: 25 candidates for r = 7 instead of 225, betting that
   the SAD gets smaller towards its minimum -- true for smooth motion,
   false when the window holds two look-alikes, where it can settle in
   the wrong valley.

   Either way, the best whole-pixel offset is then refined by half a
   pixel (Mpeg1.mli: its 8 neighbours at half a pixel, averaged as the
   decoder will): 8 more candidates. Every candidate stays inside the
   reference (MPEG-1's vectors may not point out of it).

   Worked example (the tests'): a smooth picture moved 3 pixels right
   and 2 down finds, for a macroblock inside it, the vector (-6, -4) in
   half pixels -- where its pixels came from -- and a SAD of 0; full
   search within 7 pixels in 225 whole-pixel candidates, the
   logarithmic one in 25, then 8 half-pixel ones each. The same moved
   **noise**: full search finds it again; the logarithmic one doesn't
   -- noise has no valley leading to the answer, and its first coarse
   step, 4 pixels off, sees nothing better than anywhere else. *)

(* a plane of brightness, row by row *)
type plane = { bytes : Bytes.t; stride : int; rows : int }

type search = Full | Logarithmic

(* [sad cur reference ~x ~y v]: the SAD of [cur]'s 16 x 16 square at
 * (x, y) and [reference]'s moved by [v] half pixels *)
val sad : plane -> plane -> x:int -> y:int -> int * int -> int

(* [estimate search ~range cur reference ~x ~y]: the best vector (half
 * pixels) for [cur]'s macroblock at (x, y), whole-pixel offsets within
 * [range], its SAD, and the candidates tried *)
val estimate : search -> range:int -> plane -> plane -> x:int -> y:int -> (int * int) * int * int
