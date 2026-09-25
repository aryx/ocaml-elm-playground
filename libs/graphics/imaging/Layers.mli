(* Layers: a picture as a stack of pictures, flattened for the screen.

   Photoshop 3.0's layers: each a whole picture with its own
   transparency (the alpha of each pixel), an opacity for the whole
   layer, a blend mode (Blend.mli), and whether it is shown. What the
   screen shows is the stack flattened, from the bottom up, each layer
   composited over what is under it -- Thomas Porter and Tom Duff's
   "over" (1984), with the blend mode inside it as the W3C's
   Compositing and Blending specification writes it:

     as = the layer's alpha x its opacity
     cs' = (1 - ab) cs + ab B(cb, cs)        the blend, where there is
                                              a backdrop to blend with
     ao = as + ab (1 - as)
     co = (as cs' + (1 - as) ab cb) / ao

   so a layer at 50% over an opaque backdrop is half its blend and half
   the backdrop, and over nothing (ab = 0) it is itself.

   Worked example (in the tests): an opaque red layer at 50% opacity,
   Normal, over opaque white: (255, 128, 128) -- half red, half white. *)

type layer = { name : string; image : Rgba_image.t; opacity : float; mode : Blend.mode; visible : bool }

val make : ?opacity:float -> ?mode:Blend.mode -> string -> Rgba_image.t -> layer

(* a layer of nothing: every pixel transparent *)
val transparent : string -> int -> int -> layer

(* [flatten layers]: the bottom layer first; one layer shown as it is,
   Normal and at 100%, is its own image (nothing computed) *)
val flatten : layer list -> Rgba_image.t
