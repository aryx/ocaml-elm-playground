(* Recovering transparency from two opaque renders: "matting".
 *
 * A Framebuffer has no transparency: every pixel is opaque, and
 * drawing with some [alpha] blends into what's already there (see
 * Framebuffer.blend). That's all a screen needs. But sometimes a
 * drawing must be kept *separate* from its background, with its
 * transparency, to be composited later onto something else: e.g. the
 * OpenGL backend's HUD, drawn by our 2D software rasterizer, then
 * blended by the GPU over a 3D scene the CPU never sees.
 *
 * The trick: draw it twice, over black and over white. A pixel of
 * color c (one channel, 0..255) and opacity a gives, by Porter-Duff
 * "over" (Framebuffer.blend):
 *
 *   over black:  B = a*c + (1-a)*0   = a*c
 *   over white:  W = a*c + (1-a)*255
 *
 * so  W - B = (1-a)*255, i.e.  a = 1 - (W - B)/255,  and B itself is
 * a*c, the color already multiplied by its alpha ("premultiplied"),
 * exactly what a GPU's (ONE, ONE_MINUS_SRC_ALPHA) blending wants.
 * For example, a half-transparent red (a = 0.5), in bytes (blend
 * rounds 127.5 to 128):
 *
 *    over black   over white    W - B          alpha            premultiplied
 *   (128, 0, 0)  (255,128,128)  (127,128,128)  255 - 128 = 127  (128, 0, 0)
 *
 * i.e. alpha 127/255, 0.5 up to rounding (the 3 channels' differences
 * are averaged).
 *
 * Nothing drawn: B = 0 and W = 255, a = 0. Opaque: B = W = c, a = 1.
 * Exact for antialiased edges and faded shapes too, as long as the
 * drawing only paints "over" (it does: fill_span, plot, blend).
 *
 * Reference: Alvy Ray Smith, James F. Blinn, "Blue Screen Matting",
 * SIGGRAPH '96, section 5, "triangulation matting": the same object
 * shot against two known backgrounds. For premultiplied alpha: Thomas
 * Porter, Tom Duff, "Compositing Digital Images", SIGGRAPH '84. *)

type rgba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* [premultiplied_rgba ~width ~height draw]: what [draw] paints on a
 * [width] x [height] framebuffer, with its transparency: 4 bytes per
 * pixel, red, green, blue premultiplied by alpha, then alpha, row by
 * row from the top. [draw] is called twice (over black, then white). *)
val premultiplied_rgba : width:int -> height:int -> (Framebuffer.t -> unit) -> rgba
