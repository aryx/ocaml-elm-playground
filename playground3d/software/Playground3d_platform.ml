(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Tsdl

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Native backend of the 3D playground: unlike playground/native (Cairo
 * drawing 2D vector paths), this is a real, from-scratch software
 * rasterizer. SDL is used only for the window, the event loop, and
 * presenting the final image (by writing straight into the window
 * surface's own pixel buffer, the same trick playground/native uses,
 * just without Cairo in front of it): the triangle rasterization, the
 * z-buffer depth test, and the perspective projection are all
 * hand-written OCaml, in graphics/3d/ (one module per idea, see
 * Triangle.mli to start); this file turns the Playground's shapes and
 * camera into their inputs, and runs the window.
 *
 * First version, deliberately simple: single flat color per triangle
 * (alpha/fade3d is not honored here, unlike the web backend -- true
 * alpha blending would need back-to-front ordering, which the z-buffer
 * approach doesn't give us for free), no near-plane clipping (a
 * triangle with any vertex behind the near plane is dropped whole,
 * rather than clipped into visible sub-triangles), linear (not
 * perspective-correct) depth interpolation across a triangle. Good
 * enough for the modest scenes this library targets so far; revisit if
 * needed.
 *)
open Playground3d

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let rgb_of_color (color : Playground.color) : int * int * int =
  match color with
  | Color.Rgb (r, g, b) -> (r, g, b)
  | Color.Hex s ->
      let s = String.lowercase_ascii s in
      let component i = int_of_string ("0x" ^ String.sub s i 2) in
      (component 1, component 3, component 5)

(*****************************************************************************)
(* Shading *)
(*****************************************************************************)
(* Pluggable: edit shading_mode's initial value below, or press "m" at
 * runtime to cycle through modes while a game is running (a debug
 * toggle in the same spirit as e.g. Quake's r_drawflat console
 * variable, or a "wireframe view" hotkey -- see the key_down handling
 * in run_app3d). The 4 modes, and where they differ, are in
 * graphics/3d/Shading.mli. *)

(* claude: a ref, not a plain constant, so it can be changed at
 * runtime (see cycle_shading_mode and the "m" key below) -- the same
 * kind of debug toggle many game engines/games expose (e.g. Quake's
 * r_drawflat console variable, or a "wireframe view" hotkey), handy
 * for comparing shading modes side by side without restarting. *)
let shading_mode : Shading.mode ref = ref Shading.Flat_shading

let cycle_shading_mode () =
  shading_mode :=
    (match !shading_mode with
    | Shading.Flat_color -> Shading.Flat_shading
    | Flat_shading -> Gouraud
    | Gouraud -> Phong
    | Phong -> Flat_color)

let scale_channel (c : int) (brightness : float) : int = int_of_float (float_of_int c *. brightness)

(*****************************************************************************)
(* Textures *)
(*****************************************************************************)
(* Real per-pixel texture sampling -- this is the one thing the web
 * backend (see Playground3d.placeholder_texture_color) can't do, since
 * it has no per-pixel access to anything. Loading (a local file path or
 * an http(s) URL, with caching and a preload queue) lives in
 * graphics/images/Texture_decode.ml, the same split as
 * playground/native's Playground_platform.ml and
 * graphics/images/Image_decode.ml.
 *
 * The sampling itself (nearest, bilinear) is graphics/3d/Texture's. *)

(* a bright, unmistakable "this texture failed to load" color -- the
 * same convention (a magenta/checkerboard placeholder) many game
 * engines use, rather than silently falling back to something that
 * could be mistaken for an intentional color *)
let missing_texture_color = Playground.rgb 255 0 255

(* claude: Playground3d.rendering's smooth_textures (the starting value
 * comes from run_app3d's ?rendering), "i" to toggle at runtime *)
let smooth_textures : bool ref = ref true

(* claude: Texture_decode gives RGBA textures, whatever the file's
 * channels (Rgba.of_stb_image; not Stb_image.load ~channels:4, which
 * the pinned binding gets wrong, see Rgba.mli), with no offset and no
 * padding between rows: exactly Texture.image's layout *)
let texture_of_stb_image (img : Stb_image.int8 Stb_image.t) : Texture.image =
  assert (img.channels = 4 && img.offset = 0 && img.stride = img.width * 4);
  { width = img.width; height = img.height; rgba = img.data }

let sample_texture (img : Texture.image) ~(u : float) ~(v : float) : int * int * int =
  if !smooth_textures then Texture.sample_bilinear img ~u ~v else Texture.sample_nearest img ~u ~v

(*****************************************************************************)
(* Projection (with depth, for the z-buffer -- see Playground3d.project
 * for the depth-less 2D version used by the web backend) *)
(*****************************************************************************)

(* claude: the view and perspective steps are graphics/3d/geometry/Camera's,
 * and a 3D point to a rasterizer-ready vertex (its pixel, and its depth
 * and texture coordinates in both the forms Interpolate needs) is
 * graphics/3d/Project's *)
let camera_of (camera : Playground3d.camera) : Camera.t =
  { eye = camera.eye; target = camera.target; fov = camera.fov; near = camera.near; far = camera.far }

(*****************************************************************************)
(* Perspective-correct vs linear interpolation (pluggable: "p" to
 * toggle at runtime, see key_down below) *)
(*****************************************************************************)
(* The two modes, and why the obvious one is wrong, are in
 * graphics/3d/Interpolate.mli. *)

let interpolation_mode : Interpolate.mode ref = ref Interpolate.Perspective_correct

let cycle_interpolation_mode () =
  interpolation_mode :=
    (match !interpolation_mode with Interpolate.Perspective_correct -> Interpolate.Linear | Linear -> Perspective_correct)

(*****************************************************************************)
(* Flatten + backface cull *)
(*****************************************************************************)

type material = Flat of Playground.color | Textured of string

(* claude: Newell's method, robust to repeated points like a sphere's
 * pole; see Vec3.face_normal for the bug the obvious formula caused
 * (spheres drawn "cut" at the top), explained at length *)
let face_normal = Vec3.face_normal

(* every leaf face's points, tagged with (uv, normal) -- a flat
 * Polygon3d/TexturedPolygon3d face repeats the SAME winding-based
 * face_normal at every one of its points (which is exactly what makes
 * flat_shading uniform across a face: see graphics/3d/Shading.ml), while a
 * SmoothPolygon3d face already has its own distinct normal per point. *)
let rec flatten_faces (shape : Playground3d.shape3d) :
    (material * (Vec3.t * (float * float) * Vec3.t) list) list =
  match shape.form with
  | Polygon3d (color, points) ->
      let normal = face_normal points in
      [ (Flat color, List.map (fun p -> (p, (0., 0.), normal)) points) ]
  | TexturedPolygon3d (src, points) ->
      let normal = face_normal (List.map fst points) in
      [ (Textured src, List.map (fun (p, uv) -> (p, uv, normal)) points) ]
  | SmoothPolygon3d (color, points) -> [ (Flat color, List.map (fun (p, n) -> (p, (0., 0.), n)) points) ]
  | Hud _ -> [] (* collected separately by Playground3d.collect_hud_shapes, contributes no geometry *)
  | Group3d shapes -> List.concat_map flatten_faces shapes

(* fan-triangulate a (convex, e.g. a cube face or a plane) polygon:
 * (p0,p1,p2), (p0,p2,p3), (p0,p3,p4), ... *)
let rec fan_triangles = function
  | p0 :: p1 :: p2 :: rest -> (p0, p1, p2) :: fan_triangles (p0 :: p2 :: rest)
  | _ -> []

(*****************************************************************************)
(* Z-buffer vs painter's algorithm, filled vs wireframe *)
(*****************************************************************************)
(* The triangles themselves are drawn by graphics/3d/Triangle.ml, with or
 * without a z-buffer (see graphics/3d/Zbuffer.mli and Painter.mli) *)

type visibility = Z_buffer | Painters_algorithm

(* claude: "z" to toggle at runtime, see key_down below; run
 * examples3d/PaintersAlgorithmFail3d.ml to actually see the difference
 * this makes (see graphics/3d/Painter.mli) *)
let visibility_mode : visibility ref = ref Z_buffer

let cycle_visibility_mode () =
  visibility_mode := (match !visibility_mode with Z_buffer -> Painters_algorithm | Painters_algorithm -> Z_buffer)

(* claude: "f" to toggle at runtime: filled triangles (Triangle.fill), or
 * only their edges (Triangle.outline) *)
type render_mode = Filled | Wireframe

let render_mode : render_mode ref = ref Filled
let cycle_render_mode () = render_mode := (match !render_mode with Filled -> Wireframe | Wireframe -> Filled)

(*****************************************************************************)
(* Render one frame *)
(*****************************************************************************)

(* the [fill] closure for a material, resolving a pixel's final 0xRRGGBB
 * color from its interpolated (u, v) and its brightness: a constant
 * closure for a flat-colored face, a texture sample for a textured one.
 * Computed once per face, not once
 * per pixel, except for the actual texture sampling (genuinely
 * per-pixel, since the color varies across the face) and the
 * [brightness] scaling (genuinely per-pixel too now, for gouraud/phong
 * -- see graphics/3d/Shading.ml, which is what actually computes it; this
 * function doesn't know or care which shading mode produced it). *)
let fill_of_material (material : material) : u:float -> v:float -> brightness:float -> int =
  let shade (r, g, b) ~brightness : int =
    (scale_channel r brightness lsl 16) lor (scale_channel g brightness lsl 8) lor scale_channel b brightness
  in
  match material with
  | Flat color ->
      let (r, g, b) = rgb_of_color color in
      fun ~u:_ ~v:_ ~brightness -> shade (r, g, b) ~brightness
  | Textured src -> (
      match Texture_decode.load src with
      | Some img ->
          let img = texture_of_stb_image img in
          fun ~u ~v ~brightness -> shade (sample_texture img ~u ~v) ~brightness
      | None ->
          let (r, g, b) = rgb_of_color missing_texture_color in
          fun ~u:_ ~v:_ ~brightness -> shade (r, g, b) ~brightness)

(* claude: pluggable, "b" to toggle at runtime (see key_down below) --
 * off is the simplest possible code (draw every face regardless of
 * which way it points), on is backface culling as described in
 * notes_3d.md section 5 and graphics/3d/Cull.mli. Toggling it live in
 * *filled* mode (render_mode = Filled, the default) shows NO visual
 * difference at all, only a performance one (watch the fps counter);
 * switch to wireframe first ("f") to actually *see* culling do
 * something -- Cull.mli explains why. *)
let backface_culling_enabled = ref true

let render_shape3d (fb : Framebuffer.t) (zbuffer : Zbuffer.t) (camera : Playground3d.camera)
    (shape : Playground3d.shape3d) : unit =
  let sx = fb.width and sy = fb.height in
  let view_camera = camera_of camera in
  let faces = flatten_faces shape in
  (* claude: only in Painters_algorithm mode -- Triangle.fill without a
   * z-buffer has no per-pixel depth test at all, so *draw order* is the only
   * thing that determines what ends up on top; sorting faces
   * farthest-from-the-camera-first here, so nearer faces are drawn
   * later and end up covering farther ones, is what makes that mode
   * look right at all (still not correct for intersecting/cyclically-
   * overlapping geometry -- see graphics/3d/Painter.mli). The
   * z-buffer mode needs no such sort: its per-pixel depth test makes
   * the result correct regardless of draw order. *)
  let faces =
    match !visibility_mode with
    | Z_buffer -> faces
    | Painters_algorithm ->
        faces |> Painter.sort_far_to_near ~eye:camera.eye (fun (_, points) -> List.map (fun (p, _uv, _n) -> p) points)
  in
  faces
  |> List.iter (fun (material, points) ->
         let bare_points = List.map (fun (p, _uv, _n) -> p) points in
         if (not !backface_culling_enabled) || Cull.faces_camera ~eye:camera.eye bare_points then begin
           let fill = fill_of_material material in
           let projected =
             fan_triangles points
             |> List.map (fun (pa, pb, pc) ->
                    ( Project.vertex view_camera ~width:sx ~height:sy pa,
                      Project.vertex view_camera ~width:sx ~height:sy pb,
                      Project.vertex view_camera ~width:sx ~height:sy pc ))
           in
           match !render_mode with
           | Wireframe ->
               (* one representative, unlit color for the whole face
                * (sampled at the texture's center for a textured one)
                * -- wireframe mode draws bare edges, not shaded pixels,
                * so brightness is always 1. here regardless of
                * shading_mode. *)
               let rgb = fill ~u:0.5 ~v:0.5 ~brightness:1. in
               projected
               |> List.iter (function Some v0, Some v1, Some v2 -> Triangle.outline fb ~rgb v0 v1 v2 | _ -> ())
           | Filled ->
               projected
               |> List.iter (function
                    | Some v0, Some v1, Some v2 ->
                        let zbuffer = match !visibility_mode with Z_buffer -> Some zbuffer | Painters_algorithm -> None in
                        Triangle.fill fb ~zbuffer ~interpolation:!interpolation_mode ~shading:!shading_mode ~color:fill v0 v1
                          v2
                    | _ -> (* a vertex is behind the near plane: drop the whole
                            * triangle rather than clip it -- see the module
                            * doc comment above *)
                        ())
         end)

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)
(* The SDL event loop/Playground.computer bookkeeping/frame-pacing itself
 * lives in Native_loop now, shared with a future OpenGL backend (see
 * docs/claude_notes/plan_opengl.md) -- this module only creates the
 * window/pixel buffer and supplies the [draw] callback that turns a
 * (camera, shape3d list) into pixels via the software rasterizer above. *)
open Native_loop

let preload_texture = Texture_decode.preload

let run_app3d ?(rendering = Playground3d.default_rendering) (app3d : ('model, 'msg) Playground3d.app3d) :
    unit =
  (* claude: -v, -debug, and the -fixed-time/-keys/-dump-frame flags (see
   * Native_loop) *)
  Native_loop.parse_cli_and_setup_logging ();
  (* claude: the app's choices are the starting values of the modes
   * below; the debug keys can still change them (e.g. "m" also cycles
   * through Gouraud, which the portable hints don't name) *)
  shading_mode :=
    (match rendering.shading with
    | No_lighting -> Shading.Flat_color
    | Flat -> Shading.Flat_shading
    | Smooth -> Shading.Phong);
  backface_culling_enabled := rendering.backface_culling;
  smooth_textures := rendering.smooth_textures;
  let sx = int_of_float Playground.default_width in
  let sy = int_of_float Playground.default_height in

  let* () = Sdl.init Sdl.Init.(video + events) in
  let* sdl_window =
    Sdl.create_window ~w:sx ~h:sy "Playground3D (software rasterizer)" Sdl.Window.shown
  in
  let* window_surface = Sdl.get_window_surface sdl_window in

  let pixels = Sdl.get_surface_pixels window_surface Bigarray.int32 in
  assert (Bigarray.Array1.dim pixels = sx * sy);
  (* claude: a 2D *view* onto the same underlying memory as [pixels]
   * (Bigarray.reshape shares data, it doesn't copy): drawing into [fb]
   * *is* drawing into the window, like the 2D software backend; and
   * Cairo.Image.create_for_data32, for the HUD pass below, wants the
   * same Array2.t *)
  let pixels_2d = Bigarray.reshape_2 (Bigarray.genarray_of_array1 pixels) sy sx in
  let fb = Framebuffer.of_pixels pixels_2d in
  (* claude: a Framebuffer's pixels are 0xAARRGGBB, 8 bits per channel;
   * better a clear error than wrong colors on a window that isn't *)
  let* masks = Sdl.pixel_format_enum_to_masks (Sdl.get_surface_format_enum window_surface) in
  (match masks with
  | 32, 0xFF0000l, 0xFF00l, 0xFFl, _ -> ()
  | _ -> failwith "the window's pixels are not 32-bit xRGB, which this backend needs");

  (* claude: unlike playground/native's run_app, no "Loading..." message
   * first -- this rasterizer has no text-drawing capability at all, so
   * the window just stays whatever the OS shows it as (typically blank)
   * until the first frame is ready. Fine for now (a texture-heavy game
   * should mostly preload_texture everything it needs up front anyway,
   * making this a short, one-time pause), revisit if it's ever
   * noticeable. *)
  Texture_decode.load_queued ();

  let zbuffer = Zbuffer.create ~width:sx ~height:sy in

  (* debug toggles for comparing rendering strategies live, see each
   * one's own doc comment above: "m" shading mode, "b" backface
   * culling, "f" wireframe/filled, "z" painter's algorithm/z-buffer,
   * "p" perspective-correct/linear interpolation, "i" bilinear/nearest
   * texture filtering *)
  let on_key_press str =
    if str = "m" then cycle_shading_mode ();
    if str = "b" then backface_culling_enabled := not !backface_culling_enabled;
    if str = "f" then cycle_render_mode ();
    if str = "z" then cycle_visibility_mode ();
    if str = "p" then cycle_interpolation_mode ();
    if str = "i" then smooth_textures := not !smooth_textures
  in
  let draw (_computer : Playground.computer) ((camera, shapes) : Playground3d.camera * Playground3d.shape3d list)
      : unit =
    Framebuffer.clear fb ~rgb:0xFFFFFF;
    Zbuffer.clear zbuffer;
    let group = Playground3d.group3d shapes in
    render_shape3d fb zbuffer camera group;
    (* claude: a HUD pass, once the 3D scene above is fully rasterized
     * into [pixels] for this frame -- reuses the exact same trick the
     * 2D backend already uses (Cairo.Image.create_for_data32 pointed
     * directly at an SDL window surface's own pixel Bigarray), just
     * applied as an extra pass on top instead of the only pass. No
     * Cairo.paint/clear here (unlike the 2D backend's per-frame reset)
     * -- this must only add pixels on top, never erase the 3D frame
     * underneath. See docs/claude_notes/done/plan_hud.md. *)
    match Playground3d.collect_hud_shapes group with
    | [] -> ()
    | hud_shapes ->
        let surface = Cairo.Image.create_for_data32 ~w:sx ~h:sy pixels_2d in
        let cr = Cairo.create surface in
        Cairo.identity_matrix cr;
        Cairo.translate cr (float_of_int sx /. 2.) (float_of_int sy /. 2.);
        Shape_render_native.render cr hud_shapes;
        Cairo.Surface.flush surface
  in
  let present () =
    let* () = Sdl.update_window_surface sdl_window in
    ()
  in
  (* claude: -dump-frame (see Native_loop): the frame as a binary PPM
   * image, the simplest image format there is (a header, then r, g, b
   * bytes for each pixel) *)
  let dump_frame file =
    let oc = open_out_bin file in
    Printf.fprintf oc "P6\n%d %d\n255\n" sx sy;
    for y = 0 to sy - 1 do
      for x = 0 to sx - 1 do
        let rgb = Framebuffer.get_rgb fb ~x ~y in
        output_byte oc ((rgb lsr 16) land 0xFF);
        output_byte oc ((rgb lsr 8) land 0xFF);
        output_byte oc (rgb land 0xFF)
      done
    done;
    close_out oc
  in
  Native_loop.run ~sdl_window ~sx ~sy ~title_prefix:"Playground3D" ~on_key_press
    ~init:(Playground3d.init3d app3d) ~update:(Playground3d.update3d app3d) ~view:(Playground3d.view3d app3d)
    ~draw ~present ~dump_frame ()
