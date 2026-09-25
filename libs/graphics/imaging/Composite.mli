(* Composite: the new pixels and the old, mixed by a mask.

   An editor computes its operation on the whole picture (or the
   selection's bounds), then keeps it only where the selection says:

       result = before + (after - before) x mask / 255

   a straight line from the old value to the new, 0 all old, 255 all
   new: a feathered selection's edge fades the change in.

   Worked example (in the tests): before 100, after 200, mask 64: 125
   (a quarter of the way, 64/255 of 100 rounded). *)

val through : Mask.t -> before:Pixels.image -> after:Pixels.image -> Pixels.image

(* [fill mask (r, g, b) img]: the colour poured through the mask, Edit >
   Fill and the paint bucket's *)
val fill : Mask.t -> int * int * int -> Pixels.image -> Pixels.image
