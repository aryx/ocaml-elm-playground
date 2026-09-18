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
 * PHASES 3-5 of that plan (see git history for Phase 2's
 * proof-of-pipeline version): every shape3d in the scene is flattened
 * into per-material (position, normal, color, uv) vertex buffers once
 * per frame (naive -- no per-shape VAO/VBO caching yet, matching the
 * plan's stated v1 simplification), and a single GLSL fragment shader
 * does real per-pixel Phong lighting using the exact same
 * light_dir/ambient constants as the native rasterizer's
 * brightness_of_normal, for a fair side-by-side comparison, plus real
 * texture sampling and a wireframe mode. Still out of scope, per the
 * plan: flat/Gouraud shading modes (Phong is the natural, "free on a
 * GPU" one), a painter's-algorithm mode (a hardware z-buffer makes it
 * moot), and a HUD. *)

let ( let* ) o f =
  match o with
  | Error (`Msg msg) -> failwith (Printf.sprintf "TSDL error: %s" msg)
  | Ok x -> f x

(* claude: the GL-independent half of this backend (the shape3d ->
 * per-material vertex list flattening, light_dir) is in
 * playground3d/Gpu_scene.ml, shared with the planned WebGL backend
 * (docs/claude_notes/plan_webgl.md, Phase 1), and the camera matrices
 * in graphics/3d/geometry/Mat4.ml. What's here is only what talks to
 * OpenGL itself. *)

(*****************************************************************************)
(* Shaders *)
(*****************************************************************************)
(* The fragment shader's lighting formula is deliberately byte-for-byte
 * the same as brightness_of_normal in
 * playground3d/software/Playground3d_platform.ml (same directional
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
 * always stb_image's own unmodified top-to-bottom buffer) becomes
 * texture coordinate v=0 -- mechanically the same "v=0 is the image's
 * first/top row" rule this project's own UV convention already uses
 * (see textured_quad's doc comment and native's sample_texture, which
 * reads stb_image's row 0 directly at v=0 too). Verified empirically
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

let int32_bigarray1 (n : int) : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t =
  Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout n

(*****************************************************************************)
(* Wireframe (pluggable: "f" to toggle at runtime, see on_key_press
 * below -- same key as native's own wireframe toggle) *)
(*****************************************************************************)
(* A single hardware feature toggle, unlike native's
 * draw_triangle_wireframe (a whole hand-written DDA line-drawer) --
 * see plan_opengl.md's comparison table. *)

type render_mode = Filled | Wireframe

let render_mode : render_mode ref = ref Filled
let cycle_render_mode () = render_mode := (match !render_mode with Filled -> Wireframe | Wireframe -> Filled)

(*****************************************************************************)
(* Rendering hints (Playground3d.rendering), switchable at runtime *)
(*****************************************************************************)
(* claude: the starting values come from run_app3d's ?rendering; keys
 * like the native rasterizer's: "m" shading, "b" backface culling, "i"
 * texture filtering. Each is a uniform or a GL setting applied every
 * frame. *)

let shading : Playground3d.shading ref = ref Playground3d.Smooth

let cycle_shading () =
  shading := (match !shading with No_lighting -> Flat | Flat -> Smooth | Smooth -> No_lighting)

(* the fragment shader's uShading *)
let shading_code (s : Playground3d.shading) : int = match s with No_lighting -> 0 | Flat -> 1 | Smooth -> 2

let backface_culling = ref true
let smooth_textures = ref true

(*****************************************************************************)
(* Textures *)
(*****************************************************************************)
(* Loading (a local file path or an http(s) URL, with caching and a
 * preload queue) is entirely reused from graphics/images/Texture_decode,
 * shared with the software rasterizer (see plan_opengl.md's Scope).
 * The only new piece is uploading the
 * decoded pixel buffer to the GPU (once per distinct src, cached
 * below by this module) and sampling it in the fragment shader
 * instead of sample_texture's hand-written nearest-neighbor lookup --
 * nearest-neighbor here too (Gl.nearest), for a fair comparison
 * rather than free bilinear filtering that would look different;
 * switching to Gl.linear is a one-line, GPU-only upgrade if ever
 * wanted, unlike native's own "well-known easy upgrade" bilinear note
 * in notes_3d.md section 9. *)

(* claude: only 3- or 4-channel images are handled correctly (mirrors
 * native's sample_texture's identical limitation, see its own doc
 * comment) -- a 1- or 2-channel (grayscale[+alpha]) image is uploaded
 * as if it were RGB, which misreads its bytes, but "no real texture
 * image is likely to be" that. *)
let gl_format_of_channels (channels : int) : Gl.enum = if channels = 4 then Gl.rgba else Gl.rgb

let upload_texture ~(width : int) ~(height : int) ~(format : Gl.enum)
    (data : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t) : int =
  let id = int32_bigarray1 1 in
  Gl.gen_textures 1 id;
  let tex = Int32.to_int id.{0} in
  Gl.bind_texture Gl.texture_2d tex;
  (* claude: glTexImage2D otherwise assumes rows are padded to a
   * multiple of 4 bytes -- true for most image dimensions but not
   * guaranteed (e.g. a 1-pixel-wide, 3-channel-per-pixel texture is 3
   * bytes/row); this disables that assumption so any width works. *)
  Gl.pixel_storei Gl.unpack_alignment 1;
  Gl.tex_image2d Gl.texture_2d 0 format width height 0 format Gl.unsigned_byte (`Data data);
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
  tex

(* same magenta convention as native's missing_texture_color *)
let missing_texture_gl_id : int Lazy.t =
  lazy
    (let pixel = Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout [| 255; 0; 255 |] in
     upload_texture ~width:1 ~height:1 ~format:Gl.rgb pixel)

let gl_texture_cache : (string, int) Hashtbl.t = Hashtbl.create 8

let get_or_create_gl_texture (src : string) : int =
  match Hashtbl.find_opt gl_texture_cache src with
  | Some id -> id
  | None ->
      let id =
        match Texture_decode.load src with
        | None -> Lazy.force missing_texture_gl_id
        | Some (img : Stb_image.int8 Stb_image.t) ->
            upload_texture ~width:img.width ~height:img.height ~format:(gl_format_of_channels img.channels) img.data
      in
      Hashtbl.add gl_texture_cache src id;
      id

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

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

let preload_texture : string -> unit = Texture_decode.preload

let run_app3d ?(rendering = Playground3d.default_rendering) (app3d : ('model, 'msg) Playground3d.app3d) :
    unit =
  shading := rendering.shading;
  backface_culling := rendering.backface_culling;
  smooth_textures := rendering.smooth_textures;
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

  (* claude: bugfix -- without this, compile_shader intermittently
   * failed with a GLSL syntax error pointing at garbage that was never
   * in vertex_shader_source/fragment_shader_source (confirmed by
   * eprintf-dumping the exact string passed to Gl.shader_source right
   * before the call: it was always the correct, uncorrupted source).
   * Reproduced reliably (10/10) on games3d/StarCollector3d.exe
   * specifically -- a scene with more shapes/allocation before this
   * point than examples3d/Cubes3d.exe or Spheres3d.exe, which never
   * triggered it -- and, tellingly, adding *any* extra allocation
   * (even an unrelated Printf.eprintf) right before this call made it
   * disappear just as reliably. That points at a GC-timing-sensitive
   * memory-safety bug in tgls/ctypes-foreign's glShaderSource binding
   * (tgl3.ml: `let src = allocate string src in shader_source sh 1 src
   * null` -- a pointer-to-a-pointer marshaling pattern that is a
   * known-tricky case for ctypes' GC-safety guarantees), not a bug in
   * this project's own code. Forcing a full major GC right before the
   * shader-compile calls flushes any pending finalizers/compactions
   * first, which reliably avoids the race (10/10 clean runs) --
   * a real, principled mitigation for this failure mode, not a
   * superstitious "just add a sleep". Root-causing/fixing tgls itself
   * is out of scope here. *)
  Gc.full_major ();
  let program = link_program ~vertex_source:vertex_shader_source ~fragment_source:fragment_shader_source in
  let mvp_location = Gl.get_uniform_location program "uMVP" in
  let light_dir_location = Gl.get_uniform_location program "uLightDir" in
  let use_texture_location = Gl.get_uniform_location program "uUseTexture" in
  let texture_location = Gl.get_uniform_location program "uTexture" in
  let shading_location = Gl.get_uniform_location program "uShading" in
  Gl.use_program program;
  let (lx, ly, lz) = Gpu_scene.light_dir in
  Gl.uniform3f light_dir_location lx ly lz;
  (* claude: uTexture always samples GL_TEXTURE0 -- there's only ever
   * one texture bound at a time (one draw call per material, see
   * group_by_material), so a single fixed texture unit is enough; a
   * scene needing several textures visible at once in one draw call
   * (multi-texturing) would need more units, not needed here. *)
  Gl.uniform1i texture_location 0;
  Gl.active_texture Gl.texture0;

  (* claude: the z-buffer and backface culling the native rasterizer
   * hand-rolls (its own zbuffer array + per-pixel compare;
   * dot-product-against-the-camera per face, see notes_3d.md sections
   * 5-6) are each a single hardware feature toggle here -- see
   * plan_opengl.md's comparison table. Enabled once, unconditionally,
   * at startup: this backend doesn't (yet, if ever) expose the
   * native rasterizer's "b"/"z" runtime toggles -- see the plan's
   * Scope section for why. *)
  Gl.enable Gl.depth_test;
  (* claude: which faces to cull, when culling is on (see
   * backface_culling, applied every frame in [draw]) *)
  Gl.cull_face Gl.back;

  let vao = int32_bigarray1 1 in
  Gl.gen_vertex_arrays 1 vao;
  Gl.bind_vertex_array (Int32.to_int vao.{0});

  let vbo = int32_bigarray1 1 in
  Gl.gen_buffers 1 vbo;
  Gl.bind_buffer Gl.array_buffer (Int32.to_int vbo.{0});

  (* claude: the attribute layout (3 floats position, 3 floats normal,
   * 3 floats color, 2 floats uv, interleaved) is fixed for the
   * lifetime of this VAO/VBO pair even though the actual vertex DATA
   * is re-uploaded every frame (see vertex_floats_of_group) -- so
   * these pointers only need setting up once here, not per frame. *)
  let stride = Gpu_scene.floats_per_vertex * 4 (* bytes per float *) in
  Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 1 3 Gl.float false stride (`Offset (3 * 4));
  Gl.enable_vertex_attrib_array 1;
  Gl.vertex_attrib_pointer 2 3 Gl.float false stride (`Offset (6 * 4));
  Gl.enable_vertex_attrib_array 2;
  Gl.vertex_attrib_pointer 3 2 Gl.float false stride (`Offset (9 * 4));
  Gl.enable_vertex_attrib_array 3;

  Gl.viewport 0 0 sx sy;

  let on_key_press (str : string) : unit =
    if str = "f" then cycle_render_mode ();
    if str = "m" then cycle_shading ();
    if str = "b" then backface_culling := not !backface_culling;
    if str = "i" then smooth_textures := not !smooth_textures
  in
  let draw_group ((material, vertices) : Gpu_scene.material * Gpu_scene.vertex_data list) : unit =
    let (data, vertex_count) = Gpu_scene.vertex_floats_of_group vertices in
    if vertex_count > 0 then begin
      let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout data in
      Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size vertex_data) (Some vertex_data) Gl.dynamic_draw;
      (match material with
      | Flat -> Gl.uniform1i use_texture_location 0
      | Textured src ->
          Gl.uniform1i use_texture_location 1;
          Gl.bind_texture Gl.texture_2d (get_or_create_gl_texture src);
          (* claude: smooth_textures: the GPU's bilinear filtering, or
           * nearest texel *)
          let filter = if !smooth_textures then Gl.linear else Gl.nearest in
          Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter filter;
          Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter filter);
      Gl.draw_arrays Gl.triangles 0 vertex_count
    end
  in
  let draw (_computer : Playground.computer) ((camera, shapes) : Playground3d.camera * Playground3d.shape3d list) :
      unit =
    let aspect = float_of_int sx /. float_of_int sy in
    let view = Mat4.look_at ~eye:camera.eye ~target:camera.target in
    let projection = Mat4.perspective ~fov_degrees:camera.fov ~aspect ~near:camera.near ~far:camera.far in
    let mvp = Mat4.mul projection view in
    let mvp_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout mvp in

    Gl.clear_color 1.0 1.0 1.0 1.0;
    Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
    Gl.use_program program;
    Gl.uniform_matrix4fv mvp_location 1 true mvp_data;
    Gl.bind_vertex_array (Int32.to_int vao.{0});
    Gl.bind_buffer Gl.array_buffer (Int32.to_int vbo.{0});
    Gl.polygon_mode Gl.front_and_back (match !render_mode with Filled -> Gl.fill | Wireframe -> Gl.line);
    Gl.uniform1i shading_location (shading_code !shading);
    if !backface_culling then Gl.enable Gl.cull_face_enum else Gl.disable Gl.cull_face_enum;
    Gpu_scene.group_by_material shapes |> List.iter draw_group
  in
  let present () = Sdl.gl_swap_window sdl_window in
  Native_loop.run ~sdl_window ~sx ~sy ~title_prefix:"Playground3D (OpenGL)" ~on_key_press
    ~init:(Playground3d.init3d app3d) ~update:(Playground3d.update3d app3d) ~view:(Playground3d.view3d app3d) ~draw
    ~present
