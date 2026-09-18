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
 * hand-written OCaml, in graphics/3d/ (one module per idea; see
 * Render.mli for the pipeline and a reading order);
 * Shape3d_render_software turns the Playground's shapes and camera
 * into their inputs, and this file runs the window.
 *
 * Deliberately simple: alpha/fade3d is not honored here, unlike the
 * web backend -- true alpha blending would need back-to-front
 * ordering, which the z-buffer approach doesn't give us for free; and
 * no near-plane clipping (a triangle with any vertex behind the near
 * plane is dropped whole, rather than clipped into visible
 * sub-triangles). Good enough for the modest scenes this library
 * targets so far; revisit if needed.
 *)
open Playground3d

(*****************************************************************************)
(* Debug keys *)
(*****************************************************************************)
(* Keys to turn rendering features on and off while any example or game
 * runs, to see what each one does, and compare rendering strategies
 * side by side without restarting -- the same kind of debug toggle many
 * game engines/games expose (e.g. Quake's r_drawflat console variable,
 * or a "wireframe view" hotkey); the 2D software backend has its own
 * (playground/software/Playground_platform.ml). The window title shows
 * their current state. Avoid the keys games use: arrows, w/a/s/d,
 * space.
 *
 *  - "m": the shading mode, cycling through flat color (no lighting),
 *    flat, Gouraud, Phong (see graphics/3d/Shading.mli); Gouraud and
 *    Phong only differ from flat on curved shapes (spheres)
 *  - "b": backface culling on/off (see graphics/3d/Cull.mli): no visual
 *    difference in filled mode, only a performance one (watch the
 *    fps); switch to wireframe first to actually *see* it
 *  - "f": wireframe, only the triangles' edges
 *  - "z": the z-buffer or the painter's algorithm (see
 *    graphics/3d/Painter.mli); try examples3d/PaintersAlgorithmFail3d
 *  - "p": perspective-correct or linear interpolation (see
 *    graphics/3d/Interpolate.mli); try examples3d/TexturedCube3d
 *  - "i": texture filtering, bilinear or nearest texel
 *  - "o": optimizations on/off, i.e. the original simple code instead
 *    of the optimized one (see graphics/core/Opti.mli); watch the fps
 *  - "x": the pixel magnifier (graphics/2d/Magnifier), following the
 *    mouse ("z" is taken)
 *)

let options = ref Render.default_options
let magnifier = ref false

let on_key_press (key : string) =
  let o = !options in
  match key with
  | "m" ->
      options :=
        {
          o with
          shading =
            (match o.shading with
            | Shading.Flat_color -> Shading.Flat_shading
            | Flat_shading -> Gouraud
            | Gouraud -> Phong
            | Phong -> Flat_color);
        }
  | "b" -> options := { o with backface_culling = not o.backface_culling }
  | "f" -> options := { o with wireframe = not o.wireframe }
  | "z" ->
      options :=
        { o with visibility = (match o.visibility with Z_buffer -> Painters_algorithm | Painters_algorithm -> Z_buffer) }
  | "p" ->
      options :=
        {
          o with
          interpolation =
            (match o.interpolation with
            | Interpolate.Perspective_correct -> Interpolate.Linear
            | Linear -> Perspective_correct);
        }
  | "i" -> options := { o with bilinear = not o.bilinear }
  | "o" -> Opti.enabled := not !Opti.enabled
  | "x" -> magnifier := not !magnifier
  | _ -> ()

(* e.g. "m:phong b:cull=on f:wire=off z:zbuffer p:perspective
 * i:bilinear o:opti=on x:zoom=off" *)
let title_keys () =
  let o = !options in
  let on_off b = if b then "on" else "off" in
  Printf.sprintf "m:%s b:cull=%s f:wire=%s z:%s p:%s i:%s o:opti=%s x:zoom=%s"
    (match o.shading with
    | Shading.Flat_color -> "nolight"
    | Flat_shading -> "flat"
    | Gouraud -> "gouraud"
    | Phong -> "phong")
    (on_off o.backface_culling) (on_off o.wireframe)
    (match o.visibility with Z_buffer -> "zbuffer" | Painters_algorithm -> "painter")
    (match o.interpolation with Interpolate.Perspective_correct -> "perspective" | Linear -> "linear")
    (if o.bilinear then "bilinear" else "nearest")
    (on_off !Opti.enabled) (on_off !magnifier)

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)
(* The SDL event loop/Playground.computer bookkeeping/frame-pacing itself
 * lives in Native_loop, shared with the OpenGL backend (see
 * docs/claude_notes/plan_opengl.md) -- this module only creates the
 * window/pixel buffer and supplies the [draw] callback that turns a
 * (camera, shape3d list) into pixels via Shape3d_render_software. *)
open Native_loop

let preload_texture = Texture_decode.preload

let run_app3d ?(rendering = Playground3d.default_rendering) (app3d : ('model, 'msg) Playground3d.app3d) :
    unit =
  (* claude: -v, -debug, and the -fixed-time/-keys/-dump-frame flags (see
   * Native_loop) *)
  Native_loop.parse_cli_and_setup_logging ();
  (* claude: the app's choices are the starting values of the options;
   * the debug keys can still change them (e.g. "m" also cycles through
   * Gouraud, which the portable hints don't name) *)
  options :=
    {
      !options with
      shading =
        (match rendering.shading with
        | No_lighting -> Shading.Flat_color
        | Flat -> Shading.Flat_shading
        | Smooth -> Shading.Phong);
      backface_culling = rendering.backface_culling;
      bilinear = rendering.smooth_textures;
    };
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
   * first -- the window just stays whatever the OS shows it as
   * (typically blank) until the first frame is ready. Fine for now (a
   * texture-heavy game should mostly preload_texture everything it
   * needs up front anyway, making this a short, one-time pause),
   * revisit if it's ever noticeable. *)
  Texture_decode.load_queued ();

  let zbuffer = Zbuffer.create ~width:sx ~height:sy in

  let draw (_computer : Playground.computer) ((camera, shapes) : Playground3d.camera * Playground3d.shape3d list)
      : unit =
    Framebuffer.clear fb ~rgb:0xFFFFFF;
    let group = Playground3d.group3d shapes in
    Shape3d_render_software.render ~options:!options fb zbuffer camera group;
    (* claude: a HUD pass, once the 3D scene above is fully rasterized
     * into [pixels] for this frame -- reuses the exact same trick the
     * 2D backend already uses (Cairo.Image.create_for_data32 pointed
     * directly at an SDL window surface's own pixel Bigarray), just
     * applied as an extra pass on top instead of the only pass. No
     * Cairo.paint/clear here (unlike the 2D backend's per-frame reset)
     * -- this must only add pixels on top, never erase the 3D frame
     * underneath. See docs/claude_notes/done/plan_hud.md. *)
    (match Playground3d.collect_hud_shapes group with
    | [] -> ()
    | hud_shapes ->
        let surface = Cairo.Image.create_for_data32 ~w:sx ~h:sy pixels_2d in
        let cr = Cairo.create surface in
        Cairo.identity_matrix cr;
        Cairo.translate cr (float_of_int sx /. 2.) (float_of_int sy /. 2.);
        Shape_render_native.render cr hud_shapes;
        Cairo.Surface.flush surface);
    if !magnifier then begin
      (* SDL keeps track of the mouse position, in window pixels *)
      let (_buttons, (mx, my)) = Sdl.get_mouse_state () in
      Magnifier.draw fb ~cx:mx ~cy:my
    end
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
    ~draw ~present ~dump_frame ~title_keys ()
