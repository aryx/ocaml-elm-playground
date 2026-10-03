(* Gl_textures: the OpenGL backend's textures. Loading and decoding one
 * (a file, a URL, an embedded picture; cached, preloaded) is
 * Texture_decode's, shared with the software rasterizer; what is here is
 * the one new step, the decoded pixels uploaded to the GPU, once per
 * source, the texture's id kept.
 *
 * Uploaded nearest-neighbour, as the software rasterizer samples, for a
 * fair comparison (the drawing sets the filter again from the rendering
 * hints); clamped at the edges. A texture that cannot be loaded is the
 * 1 by 1 magenta one, the other backends' convention.
 *)

(* the GL texture of a source, uploaded the first time it is asked for;
 * the magenta one if it cannot be loaded. Needs a current GL context *)
val get_or_create_gl_texture : string -> int
