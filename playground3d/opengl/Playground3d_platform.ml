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
 * PHASE 3 of that plan: the real thing, not just a hardcoded triangle
 * (see git history for Phase 2's proof-of-pipeline version). Every
 * shape3d in the scene is flattened into a (position, normal, color)
 * vertex buffer once per frame (naive -- no per-shape VAO/VBO caching
 * yet, matching the plan's stated v1 simplification), and a single
 * GLSL fragment shader does real per-pixel Phong lighting using the
 * exact same light_dir/ambient constants as the native rasterizer's
 * brightness_of_normal, for a fair side-by-side comparison. Still out
 * of scope, per the plan: textures, flat/Gouraud shading modes (Phong
 * is the natural, "free on a GPU" one to start with), wireframe,
 * painter's-algorithm mode (a hardware z-buffer makes it moot), and a
 * HUD. *)

let ( let* ) o f =
  match o with
  | Error (`Msg msg) -> failwith (Printf.sprintf "TSDL error: %s" msg)
  | Ok x -> f x

(*****************************************************************************)
(* Vec3 (duplicated from Playground3d.ml, which keeps it private -- see
 * the identical comment in playground3d/native/Playground3d_platform.ml) *)
(*****************************************************************************)

type vec3 = float * float * float

let sub ((ax, ay, az) : vec3) ((bx, by, bz) : vec3) : vec3 = (ax -. bx, ay -. by, az -. bz)
let dot ((ax, ay, az) : vec3) ((bx, by, bz) : vec3) : float = (ax *. bx) +. (ay *. by) +. (az *. bz)

let cross ((ax, ay, az) : vec3) ((bx, by, bz) : vec3) : vec3 =
  ((ay *. bz) -. (az *. by), (az *. bx) -. (ax *. bz), (ax *. by) -. (ay *. bx))

let normalize (v : vec3) : vec3 =
  let n = sqrt (dot v v) in
  if n = 0. then v
  else
    let (x, y, z) = v in
    (x /. n, y /. n, z /. n)

let up_hint : vec3 = (0., 1., 0.)

let rgb_of_color (color : Playground.color) : int * int * int =
  match color with
  | Color.Rgb (r, g, b) -> (r, g, b)
  | Color.Hex s ->
      let s = String.lowercase_ascii s in
      let component i = int_of_string ("0x" ^ String.sub s i 2) in
      (component 1, component 3, component 5)

(*****************************************************************************)
(* Mat4: the one genuinely new piece of math this backend needs that
 * the software rasterizer doesn't -- see plan_opengl.md's comparison
 * table. The rasterizer projects one point at a time with a few plain
 * scalar formulas (view_space + project_vertex); a GPU vertex shader
 * instead expects a single 4x4 "model-view-projection" matrix per
 * draw call, uploaded once, that it then applies to every vertex
 * itself, in parallel. A row-major float array of 16 elements --
 * uniform_matrix4fv's [transpose] argument (set to true below) tells
 * OpenGL to transpose it into the column-major layout it actually
 * wants internally, so this code never has to think in column-major. *)
(*****************************************************************************)

(* [look_at eye target] builds a view matrix using the exact same
 * right/up/forward basis as the native rasterizer's view_space (same
 * up_hint, same "which way is the camera pointing" derivation) --
 * V * point = (dot (point - eye) right, dot (point - eye) up,
 * dot (point - eye) forward), i.e. the same view-space coordinates
 * view_space computes, just packaged as a matrix a GPU can apply. *)
let look_at ~(eye : vec3) ~(target : vec3) : float array =
  let forward = normalize (sub target eye) in
  let right = normalize (cross forward up_hint) in
  let up = cross right forward in
  let (rx, ry, rz) = right and (ux, uy, uz) = up and (fx, fy, fz) = forward in
  [| rx; ry; rz; -.(dot right eye); ux; uy; uz; -.(dot up eye); fx; fy; fz; -.(dot forward eye); 0.; 0.; 0.; 1. |]

(* [perspective ~fov_degrees ~aspect ~near ~far]: the exact same
 * f = 1/tan(fov/2), x scaled by f/aspect, y scaled by f formulas as
 * project_vertex's ndc_x/ndc_y (see that function's comment) -- same
 * fov/near/far camera field, same on-screen framing, on both
 * backends. The z row (derived from "NDC z must be -1 at [near] and
 * +1 at [far], for a view-space z that's positive in front of the
 * camera, matching look_at's convention above") is new: the software
 * rasterizer never needs to remap depth into any particular range, it
 * only ever directly compares raw view-space z values against each
 * other in its own hand-rolled zbuffer; a GPU's hardware depth test
 * expects normalized device coordinates instead. *)
let perspective ~(fov_degrees : float) ~(aspect : float) ~(near : float) ~(far : float) : float array =
  let fov_rad = fov_degrees *. Float.pi /. 180. in
  let f = 1. /. tan (fov_rad /. 2.) in
  let a = (far +. near) /. (far -. near) in
  let b = -2. *. far *. near /. (far -. near) in
  [| f /. aspect; 0.; 0.; 0.; 0.; f; 0.; 0.; 0.; 0.; a; b; 0.; 0.; 1.; 0. |]

(* row-major 4x4 * 4x4 -- [mat4_mul a b] then applied to a point means
 * "apply b first, then a" (standard matrix composition), so
 * [mat4_mul projection view] is the usual "view, then project" order. *)
let mat4_mul (a : float array) (b : float array) : float array =
  Array.init 16 (fun idx ->
      let r = idx / 4 and c = idx mod 4 in
      let sum = ref 0. in
      for k = 0 to 3 do
        sum := !sum +. (a.((r * 4) + k) *. b.((k * 4) + c))
      done;
      !sum)

(*****************************************************************************)
(* Flattening a shape3d tree into a (position, normal, color) vertex list *)
(*****************************************************************************)
(* Deliberately much simpler than the native rasterizer's flatten_faces
 * (playground3d/native/Playground3d_platform.ml): no material/texture
 * distinction (textures are deferred, see plan_opengl.md's Scope), no
 * separate "material" type at all -- a color is just baked directly
 * into every vertex here, since that's the vertex attribute a GPU
 * fragment shader can use with zero extra code. *)

let face_normal (points : vec3 list) : vec3 =
  match points with
  | p0 :: p1 :: p2 :: _ -> normalize (cross (sub p1 p0) (sub p2 p0))
  | _ -> failwith "polygon3d needs at least 3 points"

(* fan-triangulate a (convex, e.g. a cube face or a plane) polygon:
 * (p0,p1,p2), (p0,p2,p3), (p0,p3,p4), ... -- same as native's, and
 * genuinely polymorphic (works on plain points or (point, normal)
 * pairs alike), so SmoothPolygon3d below reuses it directly. *)
let rec fan_triangles = function
  | p0 :: p1 :: p2 :: rest -> (p0, p1, p2) :: fan_triangles (p0 :: p2 :: rest)
  | _ -> []

let placeholder_texture_color = Playground.gray

(* every triangle's 3 corners, each already carrying its own
 * (position, normal, color) -- a Polygon3d/TexturedPolygon3d face
 * repeats the same winding-based face_normal/color at all 3 of a
 * triangle's corners (flat shading across that face, uniform within
 * it but still smoothly lit pixel by pixel against the light, via the
 * fragment shader's own dot product -- see the fragment shader
 * source below); a SmoothPolygon3d face already has its own distinct
 * normal per point (see Playground3d.sphere), which is what makes
 * Phong lighting actually look smooth/curved across it. *)
let rec collect_vertices (shape : Playground3d.shape3d) : (vec3 * vec3 * Playground.color) list =
  match shape.form with
  | Polygon3d (color, points) ->
      let normal = face_normal points in
      fan_triangles points |> List.concat_map (fun (a, b, c) -> [ (a, normal, color); (b, normal, color); (c, normal, color) ])
  | TexturedPolygon3d (_src, points) ->
      let bare = List.map fst points in
      let normal = face_normal bare in
      fan_triangles bare
      |> List.concat_map (fun (a, b, c) ->
             [ (a, normal, placeholder_texture_color); (b, normal, placeholder_texture_color);
               (c, normal, placeholder_texture_color) ])
  | SmoothPolygon3d (color, points) ->
      fan_triangles points
      |> List.concat_map (fun ((pa, na), (pb, nb), (pc, nc)) -> [ (pa, na, color); (pb, nb, color); (pc, nc, color) ])
  | Group3d shapes -> List.concat_map collect_vertices shapes

let floats_per_vertex = 9 (* position (3) + normal (3) + color (3) *)

(* claude: rebuilds and re-uploads the WHOLE scene's vertex data every
 * frame -- no per-shape caching of a GPU-side buffer across frames.
 * Stated up front as a known v1 simplification (see plan_opengl.md):
 * matches this project's "measure before optimizing" convention
 * (notes_3d_opti.md) rather than assuming caching is needed before an
 * FPS counter actually says so. *)
let vertex_floats_of_shapes (shapes : Playground3d.shape3d list) : float array * int =
  let vertices = List.concat_map collect_vertices shapes in
  let vertex_count = List.length vertices in
  let data = Array.make (vertex_count * floats_per_vertex) 0. in
  List.iteri
    (fun i ((px, py, pz), (nx, ny, nz), color) ->
      let (r, g, b) = rgb_of_color color in
      let base = i * floats_per_vertex in
      data.(base) <- px;
      data.(base + 1) <- py;
      data.(base + 2) <- pz;
      data.(base + 3) <- nx;
      data.(base + 4) <- ny;
      data.(base + 5) <- nz;
      data.(base + 6) <- float_of_int r /. 255.;
      data.(base + 7) <- float_of_int g /. 255.;
      data.(base + 8) <- float_of_int b /. 255.)
    vertices;
  (data, vertex_count)

(*****************************************************************************)
(* Shaders *)
(*****************************************************************************)
(* The fragment shader's lighting formula is deliberately byte-for-byte
 * the same as brightness_of_normal in
 * playground3d/native/Playground3d_platform.ml (same directional
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
   out vec3 vNormal;\n\
   out vec3 vColor;\n\
   uniform mat4 uMVP;\n\
   void main() {\n\
  \  gl_Position = uMVP * vec4(aPos, 1.0);\n\
  \  vNormal = aNormal;\n\
  \  vColor = aColor;\n\
   }\n"

let fragment_shader_source =
  "#version 330 core\n\
   in vec3 vNormal;\n\
   in vec3 vColor;\n\
   out vec4 FragColor;\n\
   uniform vec3 uLightDir;\n\
   const float ambient = 0.25;\n\
   void main() {\n\
  \  vec3 n = normalize(vNormal);\n\
  \  float lit = max(dot(n, uLightDir), 0.0);\n\
  \  float brightness = ambient + (1.0 - ambient) * lit;\n\
  \  FragColor = vec4(vColor * brightness, 1.0);\n\
   }\n"

(* claude: byte-for-byte the same light_dir as
 * playground3d/native/Playground3d_platform.ml's -- computed here in
 * OCaml (with the exact same normalize function) and uploaded as a
 * uniform, rather than re-typed as a GLSL literal, so there's no risk
 * of a copy-paste/rounding mismatch between the two backends' "same
 * light" claim. *)
let light_dir : vec3 = normalize (1., 1.3, 0.6)

(*****************************************************************************)
(* Shader compilation *)
(*****************************************************************************)

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
(* Run app *)
(*****************************************************************************)

let preload_texture (_src : string) : unit =
  (* claude: textures are deferred (see plan_opengl.md's "Scope for
   * v1") -- Texture_native's download/decode code will be reusable
   * as-is when that phase happens, only the "upload to the GPU +
   * sample in the shader" half is new. *)
  ()

let run_app3d (app3d : ('model, 'msg) Playground3d.app3d) : unit =
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
  let mvp_location = Gl.get_uniform_location program "uMVP" in
  let light_dir_location = Gl.get_uniform_location program "uLightDir" in
  Gl.use_program program;
  let (lx, ly, lz) = light_dir in
  Gl.uniform3f light_dir_location lx ly lz;

  (* claude: the z-buffer and backface culling the native rasterizer
   * hand-rolls (its own zbuffer array + per-pixel compare;
   * dot-product-against-the-camera per face, see notes_3d.md sections
   * 5-6) are each a single hardware feature toggle here -- see
   * plan_opengl.md's comparison table. Enabled once, unconditionally,
   * at startup: this backend doesn't (yet, if ever) expose the
   * native rasterizer's "b"/"z" runtime toggles -- see the plan's
   * Scope section for why. *)
  Gl.enable Gl.depth_test;
  Gl.enable Gl.cull_face_enum;
  Gl.cull_face Gl.back;

  let vao = int32_bigarray1 1 in
  Gl.gen_vertex_arrays 1 vao;
  Gl.bind_vertex_array (Int32.to_int vao.{0});

  let vbo = int32_bigarray1 1 in
  Gl.gen_buffers 1 vbo;
  Gl.bind_buffer Gl.array_buffer (Int32.to_int vbo.{0});

  (* claude: the attribute layout (3 floats position, 3 floats normal,
   * 3 floats color, interleaved) is fixed for the lifetime of this
   * VAO/VBO pair even though the actual vertex DATA is re-uploaded
   * every frame (see vertex_floats_of_shapes) -- so these pointers only
   * need setting up once here, not per frame. *)
  let stride = floats_per_vertex * 4 (* bytes per float *) in
  Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 1 3 Gl.float false stride (`Offset (3 * 4));
  Gl.enable_vertex_attrib_array 1;
  Gl.vertex_attrib_pointer 2 3 Gl.float false stride (`Offset (6 * 4));
  Gl.enable_vertex_attrib_array 2;

  Gl.viewport 0 0 sx sy;

  let on_key_press (_ : string) : unit = () in
  let draw (_computer : Playground.computer) ((camera, shapes) : Playground3d.camera * Playground3d.shape3d list) :
      unit =
    let (data, vertex_count) = vertex_floats_of_shapes shapes in
    let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout data in
    Gl.bind_buffer Gl.array_buffer (Int32.to_int vbo.{0});
    Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size vertex_data) (Some vertex_data) Gl.dynamic_draw;

    let aspect = float_of_int sx /. float_of_int sy in
    let view = look_at ~eye:camera.eye ~target:camera.target in
    let projection = perspective ~fov_degrees:camera.fov ~aspect ~near:camera.near ~far:camera.far in
    let mvp = mat4_mul projection view in
    let mvp_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout mvp in

    Gl.clear_color 1.0 1.0 1.0 1.0;
    Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
    Gl.use_program program;
    Gl.uniform_matrix4fv mvp_location 1 true mvp_data;
    Gl.bind_vertex_array (Int32.to_int vao.{0});
    Gl.draw_arrays Gl.triangles 0 vertex_count
  in
  let present () = Sdl.gl_swap_window sdl_window in
  Native_loop.run ~sdl_window ~sx ~sy ~title_prefix:"Playground3D (OpenGL)" ~on_key_press
    ~init:(Playground3d.init3d app3d) ~update:(Playground3d.update3d app3d) ~view:(Playground3d.view3d app3d) ~draw
    ~present
