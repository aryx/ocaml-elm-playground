(* Webgl_shaders: the WebGL backend's GLSL programs, the OpenGL
 * backend's two shaders (Gl_shaders, whose comments tell the lighting)
 * written in WebGL 1's dialect, GLSL ES 1.00: attribute and varying for
 * in and out, gl_FragColor for an out variable, texture2D for texture,
 * and a float precision the fragment shader must choose itself.
 *
 * Like OpenGL, WebGL reports a compile or link error only through a
 * status to check and a log to fetch; [link_program] checks both.
 *)

open Js_of_ocaml

val vertex_shader_source : string

(* [derivatives]: whether the browser has the OES_standard_derivatives
 * extension. Flat shading needs its dFdx and dFdy (core in OpenGL's
 * GLSL, an extension here); without it, flat falls back to the vertex
 * normals, that is to smooth, which differs only on curved shapes *)
val fragment_shader_source : derivatives:bool -> string

(* the fragment shader's uShading: 0 no lighting, 1 flat, 2 smooth *)
val shading_code : Playground3d.shading -> int

(* the two shaders compiled and linked; raises Failure with WebGL's own
 * message on an error *)
val link_program : WebGL.renderingContext Js.t -> vertex_source:string -> fragment_source:string -> WebGL.program Js.t
