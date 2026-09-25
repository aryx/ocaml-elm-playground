(* Add_noise: grain, as film has it.

   Each channel of each pixel moved by a random amount, up to [amount]
   either way (uniform), or the same amount on the three channels
   ([monochrome]: grey grain rather than coloured specks). The numbers
   come from a seed (Lehmer, libs/random), so the same seed gives the
   same grain: a test can check it, and an undo followed by a redo
   gives back the same picture. Photoshop's Noise > Add Noise. *)

val add : ?monochrome:bool -> amount:int -> seed:int -> Pixels.image -> Pixels.image
