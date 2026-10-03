(* Gl_shaders: the OpenGL backend's GLSL programs (3.30 core), their
 * sources and how they are compiled.
 *
 * Two programs. The scene's: the vertex shader places a vertex (one
 * matrix, model-view-projection) and passes on its normal, colour and
 * texture coordinates; the fragment shader lights each pixel, with the
 * same formula and the same constants as the software rasterizer
 * (Lighting.brightness_of_normal: one directional light, an ambient
 * floor), for a fair comparison side by side. What differs is where it
 * runs: once per pixel, on a normal the hardware interpolated, which is
 * Phong shading for free. A uniform switches the shading (none, flat,
 * smooth); flat needs no per-face normals in the vertex data, the face's
 * normal being the cross product of how the position changes across the
 * pixel (dFdx, dFdy).
 *
 * The HUD's: a rectangle covering the window, textured with the HUD's
 * image, its vertices given directly in the window's -1..1.
 *
 * OpenGL reports a compile or link error only through a status to check
 * and a log to fetch: unchecked, a GLSL typo is a blank window.
 * [link_program] checks both and fails with the log.
 *)

val vertex_shader_source : string
val fragment_shader_source : string
val hud_vertex_shader_source : string
val hud_fragment_shader_source : string

(* the two shaders compiled and linked into a program, its id; raises
 * Failure with OpenGL's own message on an error. Needs a current GL
 * context *)
val link_program : vertex_source:string -> fragment_source:string -> int

(* [n] int32s for OpenGL to write into: how it returns an id or a status
 * (glGenTextures, glGetShaderiv...) *)
val int32_bigarray1 : int -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
