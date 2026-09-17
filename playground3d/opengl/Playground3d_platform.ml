(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Tsdl
module Gl = Tgl3.Gl

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A third backend for the 3D playground, alongside the native
 * from-scratch software rasterizer and the web (SVG) one: hands the
 * same shape3d/camera scenes to a real GPU via OpenGL 3.3 core,
 * through tgls (thin bindings, same author/family as tsdl). See
 * docs/claude_notes/plan_opengl.md for the full design and the
 * one-for-one comparison against what the software rasterizer
 * hand-rolls (z-buffer, backface culling, perspective-correct
 * interpolation, the rasterizer loop itself) versus what a GPU does
 * instead (a one-line hardware feature, or nothing at all).
 *
 * PHASE 2 of that plan, not yet the real thing: this file only proves
 * the tsdl (window/GL-context) + tgls (OpenGL calls) + GLSL (shader
 * compile/link) pipeline works end to end, by drawing one hardcoded
 * triangle every frame -- [run_app3d] does not look at its [app3d]
 * argument's shapes/camera at all yet. Phase 3 wires in the real
 * Playground3d.shape3d/camera pipeline (a Mat4 module, vertex buffers
 * built from flatten_faces, a real fragment shader doing Phong
 * lighting). *)

let ( let* ) o f =
  match o with
  | Error (`Msg msg) -> failwith (Printf.sprintf "TSDL error: %s" msg)
  | Ok x -> f x

(*****************************************************************************)
(* Shader compilation *)
(*****************************************************************************)
(* Every OpenGL "how do I turn a shape into pixels" question from here
 * down is answered in GLSL (compiled and run by the GPU driver, not
 * OCaml) rather than by the hand-written OCaml the native rasterizer
 * uses -- this is the one place that OCaml/GLSL boundary is crossed. *)

let int32_bigarray1 (n : int) : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t =
  Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout n

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

(*****************************************************************************)
(* Phase 2 placeholder: one hardcoded triangle *)
(*****************************************************************************)
(* Position (2 floats) + color (3 floats) per vertex, interleaved --
 * the vertex shader passes color straight through to the fragment
 * shader, which blends it smoothly across the triangle (this
 * per-pixel interpolation of a vertex attribute is done by fixed
 * hardware, unconditionally, for any "out"/"in" variable -- the exact
 * "perspective-correct interpolation is free on a GPU" point
 * plan_opengl.md's comparison table makes, though this 2D-clip-space
 * triangle has no actual perspective to correct for yet). *)
let vertex_shader_source =
  "#version 330 core\n\
   layout (location = 0) in vec2 aPos;\n\
   layout (location = 1) in vec3 aColor;\n\
   out vec3 vColor;\n\
   void main() {\n\
  \  gl_Position = vec4(aPos, 0.0, 1.0);\n\
  \  vColor = aColor;\n\
   }\n"

let fragment_shader_source =
  "#version 330 core\n\
   in vec3 vColor;\n\
   out vec4 FragColor;\n\
   void main() {\n\
  \  FragColor = vec4(vColor, 1.0);\n\
   }\n"

let triangle_vertices =
  [| (* x; y; r; g; b *)
     0.0; 0.5; 1.0; 0.0; 0.0;
    -0.5; -0.5; 0.0; 1.0; 0.0;
     0.5; -0.5; 0.0; 0.0; 1.0
  |]

let floats_per_vertex = 5
let float_bytes = 4

let preload_texture (_src : string) : unit =
  (* claude: textures are deferred (see plan_opengl.md's "Scope for
   * v1") -- Texture_native's download/decode code will be reusable
   * as-is when that phase happens, only the "upload to the GPU +
   * sample in the shader" half is new. *)
  ()

let run_app3d (_app3d : ('model, 'msg) Playground3d.app3d) : unit =
  let sx = int_of_float Playground.default_width in
  let sy = int_of_float Playground.default_height in

  let* () = Sdl.init Sdl.Init.(video + events) in
  (* claude: these 3 attributes must be set *before* the window is
   * created -- SDL bakes the requested GL profile/version into the
   * pixel format it picks for the window, unlike e.g. a window's
   * title which can be changed anytime after creation. *)
  let* () = Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core in
  let* () = Sdl.gl_set_attribute Sdl.Gl.context_major_version 3 in
  let* () = Sdl.gl_set_attribute Sdl.Gl.context_minor_version 3 in
  let* sdl_window =
    Sdl.create_window ~w:sx ~h:sy "Playground3D (OpenGL)" Sdl.Window.(opengl + shown)
  in
  let* gl_context = Sdl.gl_create_context sdl_window in
  let* () = Sdl.gl_make_current sdl_window gl_context in

  let program = link_program ~vertex_source:vertex_shader_source ~fragment_source:fragment_shader_source in

  let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout triangle_vertices in
  (* claude: a VAO ("vertex array object") remembers a set of
   * vertex_attrib_pointer/enable_vertex_attrib_array calls, so they
   * don't have to be reissued every frame -- bind it once here, bind
   * the same one again in [draw] before the actual draw call. A VBO
   * ("vertex buffer object") is the GPU-side memory the vertex data
   * itself lives in; buffer_data below is the actual upload. *)
  let vao = int32_bigarray1 1 in
  Gl.gen_vertex_arrays 1 vao;
  Gl.bind_vertex_array (Int32.to_int vao.{0});

  let vbo = int32_bigarray1 1 in
  Gl.gen_buffers 1 vbo;
  Gl.bind_buffer Gl.array_buffer (Int32.to_int vbo.{0});
  Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size vertex_data) (Some vertex_data) Gl.static_draw;

  let stride = floats_per_vertex * float_bytes in
  Gl.vertex_attrib_pointer 0 2 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 1 3 Gl.float false stride (`Offset (2 * float_bytes));
  Gl.enable_vertex_attrib_array 1;

  Gl.viewport 0 0 sx sy;

  let on_key_press (_ : string) : unit = () in
  let draw (_computer : Playground.computer) (() : unit) : unit =
    Gl.clear_color 1.0 1.0 1.0 1.0;
    Gl.clear Gl.color_buffer_bit;
    Gl.use_program program;
    Gl.bind_vertex_array (Int32.to_int vao.{0});
    Gl.draw_arrays Gl.triangles 0 3
  in
  let present () = Sdl.gl_swap_window sdl_window in
  Native_loop.run ~sdl_window ~sx ~sy ~title_prefix:"Playground3D (OpenGL)" ~on_key_press
    ~init:(fun () -> ()) ~update:(fun _computer () -> ()) ~view:(fun _computer () -> ()) ~draw ~present
