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
 * per frame, except for Playground3d.cached3d shapes, whose buffers
 * stay on the GPU from frame to frame (see Mesh_cache and
 * docs/claude_notes/plan_opengl_perf.md), and a single GLSL fragment shader
 * does real per-pixel Phong lighting using the exact same
 * light_dir/ambient constants as the native rasterizer (both from
 * graphics/3d/Lighting.ml), for a fair side-by-side comparison, plus real
 * texture sampling and a wireframe mode. Still out of scope, per the
 * plan: flat/Gouraud shading modes (Phong is the natural, "free on a
 * GPU" one), a painter's-algorithm mode (a hardware z-buffer makes it
 * moot). The HUD is drawn on the CPU and blended over the scene, see
 * the HUD section in run_app3d. *)

let ( let* ) o f =
  match o with
  | Error (`Msg msg) -> failwith (Printf.sprintf "TSDL error: %s" msg)
  | Ok x -> f x

(* claude: the GL-independent half of this backend (the shape3d ->
 * per-material vertex list flattening, light_dir) is in
 * Gpu_scene.ml, shared with the WebGL backend
 * (plan_webgl.md, Phase 1), and the camera matrices
 * in graphics/3d/geometry/Mat4.ml. What's here is only what talks to
 * OpenGL itself. *)

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

(* claude: "o": keep the GPU buffers of Playground3d.cached3d shapes
 * from frame to frame (see Mesh_cache), or rebuild and re-upload them
 * every frame like groups. The same key as the software backend's
 * optimizations (Opti), for the same idea: the simple code or the
 * optimized one. Not a rendering hint: it changes the speed, never the
 * picture. *)
let use_cache = ref true

(* claude: "u": draw the HUD (Playground3d.hud) or not, to tell at once
 * whether a slow frame is the scene's fault or the HUD's (drawn on the
 * CPU, see draw_hud). *)
let show_hud = ref true

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

let preload_texture : string -> unit = Texture_decode.preload

let run_app3d ?(rendering = Playground3d.default_rendering) ?capture_mouse ?flags ?network
    (app3d : ('model, 'msg) Playground3d.app3d) : unit =
  (* claude: tinybox taking the app for a preview (Playground3d.capture3d) *)
  match !Playground3d.capture3d with
  | Some give -> give (Playground3d.Any_app3d app3d) rendering
  | None ->
  Option.iter Download.grant network;
  (* claude: Multiplayer3d's net=host, net=join and net=relay, as the 2D
   * platform's run_app *)
  Transport.set_connect Connect.connect;
  (* claude: -v, -debug, and the -fixed-time/-keys/-dump-frame flags (see
   * Native_loop_3d) *)
  Native_loop_3d.parse_cli_and_setup_logging ();
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
  (* claude: a window that can change size (-size, -fullscreen,
   * Alt+Enter, dragged), the sx by sy picture scaled to fit, centred, as
   * the Cairo platform's (Native_loop_2d.scale) *)
  let w0, h0, full = Native_loop_2d.window_start ~sx ~sy in
  let* sdl_window =
    Sdl.create_window ~w:w0 ~h:h0 "Playground3D (OpenGL)" Sdl.Window.(opengl + shown + resizable)
  in
  if full then Native_loop_2d.toggle_fullscreen sdl_window;
  (* claude: the picture's square in the window, in GL's coordinates (y
   * up): every viewport, the clear and the dump go through it *)
  let frame = ref (0, 0, sx, sy) in
  let set_frame (w, h) =
    let k = Native_loop_2d.scale ~sx ~sy (w, h) in
    let fw = int_of_float (Float.round (k *. float_of_int sx)) and fh = int_of_float (Float.round (k *. float_of_int sy)) in
    frame := ((w - fw) / 2, (h - fh) / 2, fw, fh)
  in
  set_frame (Sdl.get_window_size sdl_window);
  let frame_viewport () = let fx, fy, fw, fh = !frame in Gl.viewport fx fy fw fh in
  let* gl_context = Sdl.gl_create_context sdl_window in
  let* () = Sdl.gl_make_current sdl_window gl_context in

  (* claude: bugfix -- without this, compile_shader intermittently
   * failed with a GLSL syntax error pointing at garbage that was never
   * in vertex_shader_source/fragment_shader_source (confirmed by
   * eprintf-dumping the exact string passed to Gl.shader_source right
   * before the call: it was always the correct, uncorrupted source).
   * Reproduced reliably (10/10) on StarCollector3d.exe
   * specifically -- a scene with more shapes/allocation before this
   * point than Cubes3d.exe or Spheres3d.exe, which never
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
  let program = Gl_shaders.link_program ~vertex_source:Gl_shaders.vertex_shader_source ~fragment_source:Gl_shaders.fragment_shader_source in
  (* claude: the same precaution before the HUD's shaders *)
  Gc.full_major ();
  let hud_program = Gl_shaders.link_program ~vertex_source:Gl_shaders.hud_vertex_shader_source ~fragment_source:Gl_shaders.hud_fragment_shader_source in
  Gl.use_program hud_program;
  (* texture unit 0, like the scene's uTexture *)
  Gl.uniform1i (Gl.get_uniform_location hud_program "uHud") 0;
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

  (* claude: a VAO/VBO pair: the VBO holds the vertex data, the VAO
   * remembers how to read it. One pair for the whole scene, re-uploaded
   * every frame, plus one per material of each cached3d (see Meshes
   * below), uploaded once. *)
  let create_vao_vbo () =
    let vao = Gl_shaders.int32_bigarray1 1 in
    Gl.gen_vertex_arrays 1 vao;
    Gl.bind_vertex_array (Int32.to_int vao.{0});

    let vbo = Gl_shaders.int32_bigarray1 1 in
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
    (vao, vbo)
  in
  let (vao, vbo) = create_vao_vbo () in

  (* claude: HUD -- the 2D shapes over the 3D scene (Playground3d.hud).
   * The other backends draw them with their 2D renderer: straight onto
   * the finished frame (software), or in an <svg> over the canvas
   * (WebGL). Here the frame is on the GPU, so the HUD is drawn by the
   * CPU into an image of its own, with transparency, then given to the
   * GPU as a texture over a rectangle covering the window, blended
   * over the scene:
   *
   *   HUD shapes --Hud_render (Shape_render_software), twice (Matting)--> RGBA image
   *     --tex_image2d--> texture --hud program, blending--> window
   *
   * Only when the shapes change: the image stays in the texture, and on
   * other frames drawing the HUD is a single draw call. *)
  let hud_vao = Gl_shaders.int32_bigarray1 1 in
  Gl.gen_vertex_arrays 1 hud_vao;
  Gl.bind_vertex_array (Int32.to_int hud_vao.{0});
  let hud_vbo = Gl_shaders.int32_bigarray1 1 in
  Gl.gen_buffers 1 hud_vbo;
  Gl.bind_buffer Gl.array_buffer (Int32.to_int hud_vbo.{0});
  (* two triangles covering the window, each vertex its (x, y), in
   * normalized device coordinates (-1 left or bottom, 1 right or top),
   * and (u, v) in the image (v = 0 its top row, at the window's top) *)
  let quad =
    [| -1.; 1.; 0.; 0.; -1.; -1.; 0.; 1.; 1.; -1.; 1.; 1.; -1.; 1.; 0.; 0.; 1.; -1.; 1.; 1.; 1.; 1.; 1.; 0. |]
  in
  let quad_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout quad in
  Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size quad_data) (Some quad_data) Gl.static_draw;
  Gl.vertex_attrib_pointer 0 2 Gl.float false 16 (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 1 2 Gl.float false 16 (`Offset 8);
  Gl.enable_vertex_attrib_array 1;
  let hud_texture =
    let id = Gl_shaders.int32_bigarray1 1 in
    Gl.gen_textures 1 id;
    let tex = Int32.to_int id.{0} in
    Gl.bind_texture Gl.texture_2d tex;
    (* one image pixel per window pixel: no filtering needed *)
    Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
    (* claude: the whole window, transparent to start with: after that,
     * only the parts the HUD's shapes cover are redrawn, see draw_hud *)
    let empty = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (sx * sy * 4) in
    Bigarray.Array1.fill empty 0;
    Gl.pixel_storei Gl.unpack_alignment 1;
    Gl.tex_image2d Gl.texture_2d 0 Gl.rgba sx sy 0 Gl.rgba Gl.unsigned_byte (`Data empty);
    tex
  in
  (* the shapes whose image is in hud_texture *)
  let hud_in_texture : Playground.shape list option ref = ref None in
  let draw_hud (shapes : Playground.shape list) : unit =
    Gl.bind_texture Gl.texture_2d hud_texture;
    if !hud_in_texture <> Some shapes then begin
      (* claude: bugfix, a hitch at every change of the HUD: redrawing
       * it all took ~20ms on a 1000x1000 window, nearly all of it the
       * matting's pass over every pixel (the 2 renders of a few words:
       * under 1ms) and the upload of the whole texture -- a skipped
       * frame each time "PRESS SPACE" blinked on a title, and on nearly
       * every frame for a HUD always changing (TinyVirtuaRacing's
       * speed and time: 25 fps instead of 60). Now only what changed
       * is redrawn: the shapes that appeared or disappeared since the
       * last time (a blinking "PRESS SPACE", a new speed), the box
       * around them (the old ones' included, to erase them), and in it
       * only the shapes touching it -- the big title above isn't
       * rendered again when the line below blinks, and big antialiased
       * words are the renderer's costliest shapes. That box is matted
       * and uploaded (tex_sub_image2d): a few thousand pixels, not a
       * million. *)
      let old = Option.value !hud_in_texture ~default:[] in
      let changed = List.filter (fun s -> not (List.mem s old)) shapes @ List.filter (fun s -> not (List.mem s shapes)) old in
      (* claude: optimization, the same bugfix again: one box per place
       * that changed, not one box around them all. A HUD changing in
       * two far corners every frame (TinyDoom3d's timer on the
       * left, its minimap's player on the right) made that box most of
       * the window's bottom: 40-75ms every few frames, felt as a choppy
       * walk while the fps in the title stayed high. Each changed
       * shape's box, merged with the others it overlaps (two boxes
       * overlapping would redraw their shared pixels twice, and the
       * shapes in them once per box). The simple version, one box:
       *
       *   (match Hud_render.pixel_bounds ~width:sx ~height:sy changed with
       *   | None -> ()
       *   | Some (x0, y0, x1, y1) -> ... (the same redraw of the box as below))
       *)
      let overlap (a0, b0, a1, b1) (c0, d0, c1, d1) = a0 < c1 && c0 < a1 && b0 < d1 && d0 < b1 in
      let union (a0, b0, a1, b1) (c0, d0, c1, d1) = (min a0 c0, min b0 d0, max a1 c1, max b1 d1) in
      let rec merge boxes =
        match boxes with
        | [] -> []
        | b :: rest -> (
            match List.partition (overlap b) rest with
            | [], _ -> b :: merge rest
            | hits, others -> merge (List.fold_left union b hits :: others))
      in
      let boxes = merge (List.filter_map (fun s -> Hud_render.pixel_bounds ~width:sx ~height:sy [ s ]) changed) in
      Logs.debug (fun m ->
          m "hud: %s redrawn" (String.concat ", " (List.map (fun (x0, y0, x1, y1) -> Printf.sprintf "%dx%d at (%d, %d)" (x1 - x0) (y1 - y0) x0 y0) boxes)));
      boxes
      |> List.iter (fun (x0, y0, x1, y1) ->
          let w = x1 - x0 and h = y1 - y0 in
          let touching (s : Playground.shape) =
            match Hud_render.pixel_bounds ~width:sx ~height:sy [ s ] with
            | Some (a0, b0, a1, b1) -> a0 < x1 && x0 < a1 && b0 < y1 && y0 < b1
            | None -> false
          in
          let visible = List.filter touching shapes in
          let rgba =
            Matting.premultiplied_rgba ~width:w ~height:h (fun fb ->
                Hud_render.render_region ~window:(sx, sy) ~origin:(x0, y0) fb visible)
          in
          Gl.pixel_storei Gl.unpack_alignment 1;
          (* the image's rows from the top, as the texture's (see the
           * quad's texture coordinates) *)
          Gl.tex_sub_image2d Gl.texture_2d 0 x0 y0 w h Gl.rgba Gl.unsigned_byte (`Data rgba));
      hud_in_texture := Some shapes
    end;
    Gl.use_program hud_program;
    (* over everything, both sides, filled: none of the scene's
     * settings apply to a flat overlay *)
    Gl.disable Gl.depth_test;
    Gl.disable Gl.cull_face_enum;
    Gl.polygon_mode Gl.front_and_back Gl.fill;
    (* "over", for premultiplied colors (see Matting): result = hud +
     * (1 - hud's alpha) * scene *)
    Gl.enable Gl.blend;
    Gl.blend_func Gl.one Gl.one_minus_src_alpha;
    Gl.bind_vertex_array (Int32.to_int hud_vao.{0});
    Gl.draw_arrays Gl.triangles 0 6;
    Gl.disable Gl.blend;
    Gl.enable Gl.depth_test
  in

  frame_viewport ();

  let on_key_press (str : string) : unit =
    if str = "f" then cycle_render_mode ();
    if str = "m" then cycle_shading ();
    if str = "b" then backface_culling := not !backface_culling;
    if str = "i" then smooth_textures := not !smooth_textures;
    if str = "o" then use_cache := not !use_cache;
    if str = "u" then show_hud := not !show_hud
  in
  let use_material (material : Gpu_scene.material) : unit =
    match material with
    | Flat -> Gl.uniform1i use_texture_location 0
    | Textured src ->
        Gl.uniform1i use_texture_location 1;
        Gl.bind_texture Gl.texture_2d (Gl_textures.get_or_create_gl_texture src);
        (* claude: smooth_textures: the GPU's bilinear filtering, or
         * nearest texel *)
        let filter = if !smooth_textures then Gl.linear else Gl.nearest in
        Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter filter;
        Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter filter
  in
  (* claude: for the -debug stats line, see draw *)
  let draw_calls = ref 0 and vertices_uploaded = ref 0 in
  let draw_group ((material, vertices) : Gpu_scene.material * Gpu_scene.vertex_data list) : unit =
    let (data, vertex_count) = Gpu_scene.vertex_floats_of_group vertices in
    if vertex_count > 0 then begin
      let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout data in
      Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size vertex_data) (Some vertex_data) Gl.dynamic_draw;
      use_material material;
      Gl.draw_arrays Gl.triangles 0 vertex_count;
      incr draw_calls;
      vertices_uploaded := !vertices_uploaded + vertex_count
    end
  in
  (* claude: Meshes -- the GPU side of Playground3d.cached3d (see
   * Mesh_cache): each material group of a cached3d uploaded once, into
   * its own VAO/VBO pair, with static_draw (a hint to the driver that
   * the data won't change, so it can keep it in GPU memory); on later
   * frames, only draw_arrays again. *)
  let build_mesh (c : Playground3d.cached) () : (Gpu_scene.material * int * int * int) list =
    Gpu_scene.group_by_material [ c.content ]
    |> List.filter (fun (_, vertices) -> vertices <> [])
    |> List.map (fun (material, vertices) ->
           let (data, vertex_count) = Gpu_scene.vertex_floats_of_group vertices in
           let (vao, vbo) = create_vao_vbo () in
           let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout data in
           Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size vertex_data) (Some vertex_data) Gl.static_draw;
           vertices_uploaded := !vertices_uploaded + vertex_count;
           (material, Int32.to_int vao.{0}, Int32.to_int vbo.{0}, vertex_count))
  in
  let draw_mesh (mesh : (Gpu_scene.material * int * int * int) list) : unit =
    mesh
    |> List.iter (fun (material, vao, _vbo, vertex_count) ->
           Gl.bind_vertex_array vao;
           use_material material;
           Gl.draw_arrays Gl.triangles 0 vertex_count;
           incr draw_calls)
  in
  let free_mesh (mesh : (Gpu_scene.material * int * int * int) list) : unit =
    mesh
    |> List.iter (fun (_material, vao, vbo, _vertex_count) ->
           Gl.delete_buffers 1 (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [| Int32.of_int vbo |]);
           Gl.delete_vertex_arrays 1 (Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [| Int32.of_int vao |]))
  in
  let meshes = Mesh_cache.create () in
  (* one view, in its rectangle of the window (all of it but in a split
   * screen, Playground3d.split3d): GL's viewport maps the scene into
   * it, the scissor keeps the clear of its depth inside it, and the
   * projection gets its aspect *)
  let draw_view (v : Playground3d.view) : unit =
    let camera = v.camera and shapes = v.shapes in
    let px f n = int_of_float (Float.round (f *. float_of_int n)) in
    let fx, fy, fw, fh = !frame in
    let vx = px v.area.x fw and vy = px v.area.y fh in
    let vw = px (v.area.x +. v.area.w) fw - vx and vh = px (v.area.y +. v.area.h) fh - vy in
    let vx = fx + vx and vy = fy + vy in
    Gl.viewport vx vy vw vh;
    Gl.enable Gl.scissor_test;
    Gl.scissor vx vy vw vh;
    Gl.clear Gl.depth_buffer_bit;
    Gl.disable Gl.scissor_test;
    let aspect = float_of_int vw /. float_of_int (max 1 vh) in
    let view = Mat4.look_at ~up:camera.up ~eye:camera.eye ~target:camera.target () in
    let projection = (if camera.ortho > 0. then Mat4.orthographic ~height:camera.ortho ~aspect ~near:camera.near ~far:camera.far
       else Mat4.perspective ~fov_degrees:camera.fov ~aspect ~near:camera.near ~far:camera.far) in
    let mvp = Mat4.mul projection view in
    let mvp_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout mvp in
    Gl.use_program program;
    Gl.uniform_matrix4fv mvp_location 1 true mvp_data;
    Gl.bind_vertex_array (Int32.to_int vao.{0});
    Gl.bind_buffer Gl.array_buffer (Int32.to_int vbo.{0});
    Gl.polygon_mode Gl.front_and_back (match !render_mode with Filled -> Gl.fill | Wireframe -> Gl.line);
    Gl.uniform1i shading_location (shading_code !shading);
    if !backface_culling then Gl.enable Gl.cull_face_enum else Gl.disable Gl.cull_face_enum;
    (* claude: the cached3d shapes are set aside (without the cache, "o",
     * they're flattened with the rest, like groups), the rest drawn as
     * before, then the cached ones from their meshes *)
    let cached = ref [] in
    let on_cached = if !use_cache then Some (fun c -> cached := c :: !cached) else None in
    Gpu_scene.group_by_material ?on_cached shapes |> List.iter draw_group;
    List.rev !cached
    |> List.iter (fun (c : Playground3d.cached) -> draw_mesh (Mesh_cache.find_or_build meshes c.id (build_mesh c)))
  in
  let draw (computer : Playground.computer) (views : Playground3d.view list) : unit =
    (* claude: black round the picture's square, white in it *)
    let fx, fy, fw, fh = !frame in
    Gl.clear_color 0.0 0.0 0.0 1.0;
    Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
    Gl.enable Gl.scissor_test;
    Gl.scissor fx fy fw fh;
    Gl.clear_color 1.0 1.0 1.0 1.0;
    Gl.clear Gl.color_buffer_bit;
    Gl.disable Gl.scissor_test;
    frame_viewport ();
    draw_calls := 0;
    vertices_uploaded := 0;
    List.iter draw_view views;
    frame_viewport ();
    (* once all the views are drawn: a mesh one view didn't use may be
     * another's *)
    Mesh_cache.sweep meshes ~free:free_mesh;
    (match if !show_hud then Playground3d.views_hud computer.screen views else [] with
    | [] -> ()
    | hud_shapes -> draw_hud hud_shapes);
    let s = Mesh_cache.stats meshes in
    Logs.debug (fun m ->
        m "draw: %d draw calls, %d vertices uploaded; cached meshes: %d live, %d built, %d freed%s" !draw_calls
          !vertices_uploaded s.live s.built s.freed
          (if !use_cache then "" else " (cache off)"))
  in
  let present () = Sdl.gl_swap_window sdl_window in
  (* claude: -dump-frame (see Native_loop_3d): the frame as a PPM or a
   * PNG (Native_loop_2d.write_frame), like the software backend's, read
   * back from the GPU before [present]; OpenGL's rows go bottom to top,
   * a picture file's top to bottom. The pixels depend on the GPU and
   * its driver: only compare frames from the same machine (e.g. with
   * and without -keys o). *)
  let dump_frame file =
    (* claude: the picture's square, at the size it is drawn *)
    let fx, fy, sx, sy = !frame in
    let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (sx * sy * 3) in
    Gl.pixel_storei Gl.pack_alignment 1;
    Gl.read_pixels fx fy sx sy Gl.rgb Gl.unsigned_byte (`Data pixels);
    Native_loop_2d.write_frame ~width:sx ~height:sy
      (fun x y ->
        let i = ((((sy - 1 - y) * sx) + x) * 3) in
        (pixels.{i} lsl 16) lor (pixels.{i + 1} lsl 8) lor pixels.{i + 2})
      file
  in
  Native_loop_3d.run ~sdl_window ~sx ~sy ~title_prefix:"Playground3D (OpenGL)" ~on_key_press
    ~init:(Playground3d.init3d app3d) ~update:(Playground3d.update3d app3d) ~view:(Playground3d.views3d app3d) ~draw
    ~present ~dump_frame ?capture_mouse ?flags ~on_resize:(fun w h -> set_frame (w, h)) ()
