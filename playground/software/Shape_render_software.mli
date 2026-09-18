(* Draws a list of Playground shapes into a framebuffer, using only the
 * from-scratch algorithms of playground/raster/ -- the software
 * rasterizer's counterpart of playground/native/Shape_render_native.ml,
 * which asks Cairo to do the same job. *)

(* Rendering features that can be turned off, to see what each one
 * contributes (see the keys in Playground_platform.ml) *)
type options = {
  (* false: no Porter-Duff blending; a faded shape is either drawn
   * fully opaque (alpha > 0) or not at all (alpha = 0) *)
  alpha_blending : bool;
}

(* everything on *)
val default_options : options

val render : ?options:options -> Framebuffer.t -> Playground.shape list -> unit
