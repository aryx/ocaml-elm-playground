(* Draws a list of Playground shapes into a framebuffer, using only the
 * from-scratch algorithms of graphics/ -- the software
 * rasterizer's counterpart of playground/platforms/native/Shape_render_native.ml,
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

(* [render ?scale fb shapes]: the shapes drawn into [fb], the playground's
 * (0, 0) at its center; [scale] (default 1) pixels per playground unit,
 * e.g. 1/3 to draw a 1000-wide window into a 334-wide framebuffer (see
 * Pixelate) *)
val render : ?options:options -> ?scale:float -> Framebuffer.t -> Playground.shape list -> unit

(* [render_region ~window:(width, height) ~origin:(x0, y0) fb shapes]:
 * the part of the [width] x [height] window that [fb] covers, from the
 * window's pixel (x0, y0): the same pixels as [render] on the whole
 * window, cropped, for less work when only a part is needed *)
val render_region :
  ?options:options -> window:int * int -> origin:int * int -> Framebuffer.t -> Playground.shape list -> unit

(* [pixel_bounds ~width ~height shapes]: the box, (x0, y0, x1, y1) in
 * pixels of a [width] x [height] window (x1, y1 excluded, clipped to
 * the window), outside of which [render] paints nothing -- a little
 * larger than what it paints, for antialiased edges and pens; None if
 * nothing is drawn. E.g. for the OpenGL backend's HUD, to redraw and
 * upload only that part of the window when the HUD changes. *)
val pixel_bounds : width:int -> height:int -> Playground.shape list -> (int * int * int * int) option
