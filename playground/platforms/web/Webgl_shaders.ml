(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Webgl_shaders.mli *)

open Js_of_ocaml

(*****************************************************************************)
(* Shaders *)
(*****************************************************************************)
(* GLSL ES 1.00, WebGL 1's, rather than OpenGL's GLSL 3.30: attribute/
 * varying instead of in/out, gl_FragColor instead of an out variable,
 * texture2D instead of texture, and a float precision the fragment
 * shader must choose itself (the vertex shader has highp by default).
 * Otherwise the same two programs as the OpenGL backend's; see its
 * comments for the lighting and the flat-shading trick. *)

let vertex_shader_source =
  "attribute vec3 aPos;\n\
   attribute vec3 aNormal;\n\
   attribute vec3 aColor;\n\
   attribute vec2 aUv;\n\
   varying vec3 vNormal;\n\
   varying vec3 vColor;\n\
   varying vec2 vUv;\n\
   varying vec3 vPos;\n\
   uniform mat4 uMVP;\n\
   void main() {\n\
  \  gl_Position = uMVP * vec4(aPos, 1.0);\n\
  \  vPos = aPos;\n\
  \  vNormal = aNormal;\n\
  \  vColor = aColor;\n\
  \  vUv = aUv;\n\
   }\n"

(* [derivatives]: whether the OES_standard_derivatives extension is
 * there. Flat shading needs its dFdx/dFdy (core in OpenGL's GLSL 3.30,
 * an extension in WebGL 1); without it, Flat falls back to the vertex
 * normals, i.e. to Smooth, which only differs on curved shapes.
 *
 * highp when the GPU has it (nearly all do): mediump is only required
 * to have 10 bits of mantissa, too coarse for dFdx of positions.
 *
 * uAmbient is a uniform rather than a literal, so Lighting.ambient is
 * the only place the value is written. *)
let fragment_shader_source ~(derivatives : bool) : string =
  let flat_normal = if derivatives then "normalize(cross(dFdx(vPos), dFdy(vPos)))" else "normalize(vNormal)" in
  (if derivatives then "#extension GL_OES_standard_derivatives : enable\n" else "")
  ^ "#ifdef GL_FRAGMENT_PRECISION_HIGH\n\
     precision highp float;\n\
     #else\n\
     precision mediump float;\n\
     #endif\n\
     varying vec3 vNormal;\n\
     varying vec3 vColor;\n\
     varying vec2 vUv;\n\
     varying vec3 vPos;\n\
     uniform vec3 uLightDir;\n\
     uniform float uAmbient;\n\
     uniform bool uUseTexture;\n\
     uniform sampler2D uTexture;\n\
     uniform int uShading;\n\
     void main() {\n\
    \  vec3 n = uShading == 1 ? "
  ^ flat_normal
  ^ " : normalize(vNormal);\n\
    \  float lit = max(dot(n, uLightDir), 0.0);\n\
    \  float brightness = uShading == 0 ? 1.0 : uAmbient + (1.0 - uAmbient) * lit;\n\
    \  vec3 baseColor = uUseTexture ? texture2D(uTexture, vUv).rgb : vColor;\n\
    \  gl_FragColor = vec4(baseColor * brightness, 1.0);\n\
     }\n"

(* the fragment shader's uShading *)
let shading_code (s : Playground3d.shading) : int = match s with No_lighting -> 0 | Flat -> 1 | Smooth -> 2


(* Like OpenGL, WebGL reports a shader compile or link error only through
 * a status to check and a log to fetch: without these checks, a GLSL
 * typo is a silently blank canvas. *)
let compile_shader (gl : WebGL.renderingContext Js.t) (kind : WebGL.shaderType) (source : string) :
    WebGL.shader Js.t =
  let shader = gl##createShader kind in
  gl##shaderSource shader (Js.string source);
  gl##compileShader shader;
  if not (Js.to_bool (gl##getShaderParameter shader gl##._COMPILE_STATUS_)) then
    failwith (Printf.sprintf "WebGL shader compile error:\n%s" (Js.to_string (gl##getShaderInfoLog shader)));
  shader

let link_program (gl : WebGL.renderingContext Js.t) ~(vertex_source : string) ~(fragment_source : string) :
    WebGL.program Js.t =
  let vs = compile_shader gl gl##._VERTEX_SHADER_ vertex_source in
  let fs = compile_shader gl gl##._FRAGMENT_SHADER_ fragment_source in
  let program = gl##createProgram in
  gl##attachShader program vs;
  gl##attachShader program fs;
  gl##linkProgram program;
  if not (Js.to_bool (gl##getProgramParameter program gl##._LINK_STATUS_)) then
    failwith (Printf.sprintf "WebGL program link error:\n%s" (Js.to_string (gl##getProgramInfoLog program)));
  gl##deleteShader vs;
  gl##deleteShader fs;
  program
