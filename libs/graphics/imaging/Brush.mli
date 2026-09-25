(* Brush: paint laid down as dabs along the mouse's path.

   A brush is a disc: fully opaque inside [hardness] of its radius,
   fading to nothing at its edge (a smooth step), so a soft brush
   (hardness 0) is a bell and a hard one (1) a coin:

       hardness 1         hardness 0.5        hardness 0
       ___________          _______               _
      |           |        /       \            /   \
      |           |      _/         \_       __/     \__

   A stroke is dabs every quarter of the brush's diameter along the
   path (Photoshop's default spacing, 25%): closer and the paint piles
   up for nothing, further and the dabs show as beads.

   The dabs of one stroke are gathered into the stroke's own mask, each
   pixel keeping the most any dab gave it, and the paint goes through
   that mask once, at the stroke's opacity: a stroke at 50% is 50%
   everywhere, even where its dabs overlap. The airbrush is the other
   way: each dab lays its paint over the last, at a small [flow], and
   holding it in place darkens -- a spray can.

   The other tools are dabs of something else than a colour: the eraser
   paints the background colour; the rubber stamp paints the picture
   itself, taken at an offset (Photoshop 1.0's clone: Option-click the
   source, then paint); smudge drags the colours along, each dab mixing
   in what was under the one before.

   Worked example (in the tests): a hard brush of radius 3 dabbed once
   at (5.5, 5.5) in red on white: the pixel under its centre is red,
   one 5 away white; a stroke from (0, 5) to (20, 5) has dabs 1.5 apart
   (a quarter of 6). *)

type t = { radius : float; hardness : float; opacity : float }

(* how much of a dab covers a pixel at distance d from its centre, 0 to 1 *)
val coverage : t -> float -> float

(* the dabs' centres from one point to the next, the first included *)
val spacing : t -> float * float -> float * float -> (float * float) list

(* [stroke_mask brush w h points]: the stroke's own mask, the most of
   its dabs at each pixel *)
val stroke_mask : t -> int -> int -> (float * float) list -> Mask.t

(* [paint brush colour ?selection img points]: the stroke laid down,
   through the selection if there is one *)
val paint : t -> int * int * int -> ?selection:Mask.t -> Pixels.image -> (float * float) list -> Pixels.image

(* [airbrush brush colour ~flow img points]: each dab over the last *)
val airbrush : t -> int * int * int -> flow:float -> ?selection:Mask.t -> Pixels.image -> (float * float) list -> Pixels.image

(* [stamp brush ~source ~offset ?selection img points]: the rubber
   stamp -- the pixels of [source] at (x + dx, y + dy) painted at (x, y) *)
val stamp : t -> source:Pixels.image -> offset:int * int -> ?selection:Mask.t -> Pixels.image -> (float * float) list -> Pixels.image

(* [smudge brush ~strength img points]: strength 0 to 1 *)
val smudge : t -> strength:float -> ?selection:Mask.t -> Pixels.image -> (float * float) list -> Pixels.image
