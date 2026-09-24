(* The debug keys' help ("h" in the software backends, 2D and 3D): a
 * translucent panel in the top-left corner of the frame, one line per
 * key, what it does and its current state, e.g.
 *
 *   +--------------------------------------+
 *   | h  this help                         |
 *   | m  shading: phong                    |
 *   | b  backface culling: on              |
 *   | ...                                  |
 *   +--------------------------------------+
 *
 * drawn like any Playground [words], by Shape_render_software. *)

(* [draw fb keys]: the panel for [keys], each a key and its line *)
val draw : Framebuffer.t -> (string * string) list -> unit
