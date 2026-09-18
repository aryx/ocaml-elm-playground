(* Draws a list of Playground shapes into a framebuffer, using only the
 * from-scratch algorithms of playground/raster/ -- the software
 * rasterizer's counterpart of playground/native/Shape_render_native.ml,
 * which asks Cairo to do the same job. *)
val render : Framebuffer.t -> Playground.shape list -> unit
