(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Gl_shaders.mli *)

module Gl = Tgl3.Gl

(*****************************************************************************)
(* Shaders *)
(*****************************************************************************)
(* The fragment shader's lighting formula is deliberately byte-for-byte
 * the same as Lighting.brightness_of_normal in
 * graphics/3d/Lighting.ml (same directional
 * "sun" light, same ambient floor) -- the point of this backend is a
 * fair comparison, not a different look. The one real difference:
 * this runs once per PIXEL, on every one of the GPU's cores in
 * parallel, given a per-pixel *interpolated* normal that the hardware
 * rasterizer produces for free -- i.e. this is genuine Phong shading,
 * not native's Gouraud/Phong approximation built by hand on top of a
 * software triangle-fill loop (see notes_3d_shading.md). *)

let vertex_shader_source =
  "#version 330 core\n\
   layout (location = 0) in vec3 aPos;\n\
   layout (location = 1) in vec3 aNormal;\n\
   layout (location = 2) in vec3 aColor;\n\
   layout (location = 3) in vec2 aUv;\n\
   out vec3 vNormal;\n\
   out vec3 vColor;\n\
   out vec2 vUv;\n\
   out vec3 vPos;\n\
   uniform mat4 uMVP;\n\
   void main() {\n\
  \  gl_Position = uMVP * vec4(aPos, 1.0);\n\
  \  vPos = aPos;\n\
  \  vNormal = aNormal;\n\
  \  vColor = aColor;\n\
  \  vUv = aUv;\n\
   }\n"

(* claude: no v-flip here, on purpose. glTexImage2D's row 0 (below,
 * always Texture_decode's own unmodified top-to-bottom buffer) becomes
 * texture coordinate v=0 -- mechanically the same "v=0 is the image's
 * first/top row" rule this project's own UV convention already uses
 * (see textured_quad's doc comment and native's sample_texture, which
 * reads the decoded row 0 directly at v=0 too). Verified empirically
 * against TexturedCube3d.exe's checker pattern (see plan_opengl.md's
 * verification notes) rather than assumed -- OpenGL's "textures are
 * upside down" folklore is real for some pipelines, but only when a
 * flip gets introduced elsewhere (e.g. a bottom-up image loader); it
 * doesn't apply here. *)
(* claude: uShading is Playground3d.rendering's shading (0 = no lighting,
 * 1 = flat, 2 = smooth, see shading_code), switchable at runtime with
 * "m". Flat shading needs one normal per *face*, but the vertices only
 * carry per-vertex normals (a sphere's are smooth); instead of new
 * vertex data, the classic trick: dFdx/dFdy are how much vPos changes
 * from this pixel to the next one right/up, i.e. two vectors lying in
 * the face's plane, so their cross product is the face's normal --
 * pointing towards the camera, as a visible face's outward normal
 * does. *)
let fragment_shader_source =
  "#version 330 core\n\
   in vec3 vNormal;\n\
   in vec3 vColor;\n\
   in vec2 vUv;\n\
   in vec3 vPos;\n\
   out vec4 FragColor;\n\
   uniform vec3 uLightDir;\n\
   uniform bool uUseTexture;\n\
   uniform sampler2D uTexture;\n\
   uniform int uShading;\n\
   const float ambient = 0.25;\n\
   void main() {\n\
  \  vec3 n = uShading == 1 ? normalize(cross(dFdx(vPos), dFdy(vPos))) : normalize(vNormal);\n\
  \  float lit = max(dot(n, uLightDir), 0.0);\n\
  \  float brightness = uShading == 0 ? 1.0 : ambient + (1.0 - ambient) * lit;\n\
  \  vec3 baseColor = uUseTexture ? texture(uTexture, vUv).rgb : vColor;\n\
  \  FragColor = vec4(baseColor * brightness, 1.0);\n\
   }\n"

(* claude: the HUD's two shaders (see the HUD section in run_app3d): a
 * rectangle covering the whole window, textured with the HUD's image.
 * No matrix: the vertices are given directly in normalized device
 * coordinates, -1..1, the window's left..right and bottom..top. *)
let hud_vertex_shader_source =
  "#version 330 core\n\
   layout (location = 0) in vec2 aPos;\n\
   layout (location = 1) in vec2 aUv;\n\
   out vec2 vUv;\n\
   void main() {\n\
  \  gl_Position = vec4(aPos, 0.0, 1.0);\n\
  \  vUv = aUv;\n\
   }\n"

let hud_fragment_shader_source =
  "#version 330 core\n\
   in vec2 vUv;\n\
   out vec4 FragColor;\n\
   uniform sampler2D uHud;\n\
   void main() {\n\
  \  FragColor = texture(uHud, vUv);\n\
   }\n"

let int32_bigarray1 (n : int) : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t =
  Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout n

(*****************************************************************************)
(* Shader compilation *)
(*****************************************************************************)

(* claude: OpenGL reports shader/program errors via a status flag you
 * have to check explicitly (get_*iv) plus a separate call
 * (get_*_info_log) to fetch the actual human-readable message -- a
 * compile/link failure never raises or crashes on its own, it just
 * silently produces a shader/program that does nothing. Skipping this
 * check would turn every GLSL typo into a confusing blank window
 * instead of a compiler error message. *)
let compile_shader (kind : Gl.enum) (source : string) : int =
  let shader = Gl.create_shader kind in
  Gl.shader_source shader source;
  Gl.compile_shader shader;
  let status = int32_bigarray1 1 in
  Gl.get_shaderiv shader Gl.compile_status status;
  if Int32.to_int status.{0} = 0 then begin
    let log = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4096 in
    Gl.get_shader_info_log shader 4096 None log;
    failwith (Printf.sprintf "OpenGL shader compile error:\n%s" (Gl.string_of_bigarray log))
  end;
  shader

let link_program ~(vertex_source : string) ~(fragment_source : string) : int =
  let vs = compile_shader Gl.vertex_shader vertex_source in
  let fs = compile_shader Gl.fragment_shader fragment_source in
  let program = Gl.create_program () in
  Gl.attach_shader program vs;
  Gl.attach_shader program fs;
  Gl.link_program program;
  let status = int32_bigarray1 1 in
  Gl.get_programiv program Gl.link_status status;
  if Int32.to_int status.{0} = 0 then begin
    let log = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4096 in
    Gl.get_program_info_log program 4096 None log;
    failwith (Printf.sprintf "OpenGL program link error:\n%s" (Gl.string_of_bigarray log))
  end;
  (* claude: a linked program keeps its own copy of the compiled
   * shaders' code -- the standalone shader objects aren't needed once
   * linked, so they can be deleted right away (a GL idiom, not
   * specific to this project). *)
  Gl.delete_shader vs;
  Gl.delete_shader fs;
  program
