(* Scheme_image: HtDP's images as values, 2htdp/image's.

   How to Design Programs (Felleisen, Findler, Flatt and Krishnamurthi,
   2001; its second edition, 2018) starts with pictures, not numbers:
   (circle 10 "solid" "red") is a value, printed in DrScheme's
   Interactions as the red disc itself, and images compose --
   (beside (circle 10 "solid" "red") (square 20 "outline" "blue")) --
   the way numbers add. A child's first programs draw.

   An image here is its description, a tree, and what it knows is its
   size; drawing it is the host's (TinyDrScheme draws it with the
   Playground's Bigbang way, playground/ways/Bigbang.mli, whose
   combinators are the same). So the language stays pure text and
   numbers, and a test can ask an image's width.

       beside a b     side by side, centered vertically
       above a b      one over the other, centered horizontally
       overlay a b    a on top of b, their centers together
       place-image a x y scene
                      a's center at (x, y) of the scene, from its
                      top-left corner, y going down; cut to the scene

   A text's width is estimated from its length, as the Bigbang way's
   is: the host's font is not the language's to measure. *)

type mode = Solid | Outline

type t =
  | Circle of float * mode * string (* radius, mode, colour name *)
  | Ellipse of float * float * mode * string
  | Rectangle of float * float * mode * string
  | Triangle of float * mode * string (* equilateral, its side *)
  | Text of string * float * string (* the string, its size, the colour *)
  | Scene of float * float (* empty-scene: white, framed *)
  | Beside of t * t
  | Above of t * t
  | Overlay of t * t
  | Place of t * float * float * t

val width : t -> float
val height : t -> float

(* the expression that makes it, (circle 10 "solid" "red"): how the
   teaching languages print an image in text, the stepper's *)
val to_string : t -> string
