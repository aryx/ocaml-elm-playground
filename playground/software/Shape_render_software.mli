(* Draws a list of Playground shapes into a framebuffer, using only the
 * from-scratch algorithms of graphics/ -- the software
 * rasterizer's counterpart of playground/native/Shape_render_native.ml,
 * which asks Cairo to do the same job. *)

(* Rendering features that can be turned on or off, to see what each
 * one does (see the keys in Playground_platform.ml) *)
type options = {
  (* false: no Porter-Duff blending; a faded shape is either drawn
   * fully opaque (alpha > 0) or not at all (alpha = 0) *)
  alpha_blending : bool;
  (* true: draw every shape as the axis-aligned box around it instead
   * of its real outline *)
  bounding_boxes : bool;
  (* true: draw only the outlines, with Line (Bresenham), instead of
   * filling; shows the polygons circles and ovals became *)
  wireframe : bool;
  (* images: true = bilinear filtering (smooth), false = nearest pixel
   * (blocky), see Blit *)
  bilinear : bool;
  (* true: smooth edges, pixels partly covered drawn partly transparent
   * (Fill.polygons_aa, Line.draw_aa); false: all-or-nothing pixels,
   * "jaggies" *)
  antialiasing : bool;
}

(* blending on, real outlines, filled, bilinear, antialiased *)
val default_options : options

val render : ?options:options -> Framebuffer.t -> Playground.shape list -> unit
