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
(* Native backend of the 3D playground: unlike playground/platforms/native (Cairo
 * drawing 2D vector paths), this is a real, from-scratch software
 * rasterizer. SDL is used only for the window, the event loop, and
 * presenting the final image (by writing straight into the window
 * surface's own pixel buffer, the same trick playground/platforms/native uses,
 * just without Cairo in front of it): the triangle rasterization, the
 * z-buffer depth test, and the perspective projection are all
 * hand-written OCaml, in graphics/3d/ (one module per idea; see
 * Render.mli for the pipeline and a reading order);
 * Shape3d_render_software turns the Playground's shapes and camera
 * into their inputs, and this file runs the window.
 *
 * Deliberately simple: alpha/fade3d is not honored here, unlike the
 * SVG backend -- true alpha blending would need back-to-front
 * ordering, which the z-buffer approach doesn't give us for free.
 * Good enough for the modest scenes this library targets so far;
 * revisit if needed. (Triangles crossing the near plane are clipped,
 * see Clip.mli and the "c" key below.)
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
 * (playground/platforms/software/Playground_platform.ml). Only with the
 * -debug-keys flag (see Native_loop_3d), so that without it a game can use
 * any key. The window title shows their current state. Avoid the keys
 * games use most: arrows, w/a/s/d, space.
 *
 *  - "m": the shading mode, cycling through flat color (no lighting),
 *    flat, Gouraud, Phong (see graphics/3d/Shading.mli); Gouraud and
 *    Phong only differ from flat on curved shapes (spheres)
 *  - "b": backface culling on/off (see graphics/3d/Cull.mli): no visual
 *    difference in filled mode, only a performance one (watch the
 *    fps); switch to wireframe first to actually *see* it
 *  - "f": wireframe, only the triangles' edges
 *  - "z": the z-buffer or the painter's algorithm (see
 *    graphics/3d/Painter.mli); try PaintersAlgorithmFail3d
 *  - "p": perspective-correct or linear interpolation (see
 *    graphics/3d/Interpolate.mli); try TexturedCube3d
 *  - "i": texture filtering, bilinear or nearest texel
 *  - "t": the top-left fill rule, or the epsilon (see
 *    graphics/3d/Triangle.mli); with the magnifier on a shared edge
 *  - "c": near-plane clipping on/off (see graphics/3d/Clip.mli): off,
 *    the triangles going behind the camera vanish; try Corridor3d
 *  - "o": optimizations on/off, i.e. the original simple code instead
 *    of the optimized one (see graphics/core/Opti.mli); watch the fps
 *  - "x": the pixel magnifier (graphics/2d/Magnifier), following the
 *    mouse ("z" is taken)
 *  - "r": the resolution, full, then a half, a third, a quarter, each
 *    pixel shown as a 2x2, 3x3, 4x4 block (graphics/core/Pixelate):
 *    faster (a z-buffer test, shading, texturing per pixel: about k^2
 *    times less of them), and the look of 320x200 games
 *  - "y": the renderer, this rasterizer or the ray tracer
 *    (graphics/3d/raytrace/, plan_raytracing_teaching.md): the same
 *    scene, drawn the other way, by each of the ray tracer's
 *    algorithms in turn (Raytrace.algorithms: ray casting, then
 *    Lambert's light, then shadow rays...), and last the shadow acne
 *    bug, the latest one with no epsilon; slow, so try it with "r"
 *    first. Also -raytrace, the latest from the first frame
 *  - "v": versus, the frame split down the middle, the rasterizer's on
 *    the left and the ray tracer's on the right (the algorithm "y"
 *    chose, or the latest), with the time each took; with "r", or it
 *    is slow
 *  - "h": this list, with each key's state, over the frame
 *  - Ctrl + any of them: the debug key alone, not given to the game
 *    (playground/platforms/software/Help_overlay)
 *)

let options = ref Render.default_options
let magnifier = ref false
let help = ref false
(* claude: "y", the ray tracer instead of the rasterizer: None, the
 * rasterizer, or the ray tracer's options, cycled through by "y" *)
let renderers : Raytrace.options option list =
  (None :: List.map (fun algorithm -> Some { Raytrace.default_options with algorithm }) Raytrace.algorithms)
  @ [ Some { Raytrace.default_options with epsilon = 0. } ]

let renderer = ref 0
let raytraced () : Raytrace.options option =
  (* claude: -rt-brute, -rt-samples, -rt-bounces *)
  Option.map
    (fun (o : Raytrace.options) ->
      { o with
        acceleration = (if Native_loop_3d.raytrace_brute_force () then Brute_force else o.acceleration);
        samples = Native_loop_3d.raytrace_samples ();
        depth = Native_loop_3d.raytrace_bounces () })
    (List.nth renderers !renderer)

(* claude: "v", the split view, and the two times it measured, in ms *)
let split = ref false
let split_times : (float * float) option ref = ref None

let renderer_name () : string =
  match raytraced () with
  | None -> "the rasterizer"
  | Some { algorithm; epsilon; _ } ->
      "the ray tracer, " ^ Raytrace.name algorithm ^ if epsilon = 0. then " (epsilon 0: acne)" else ""

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
  | "c" -> options := { o with clipping = not o.clipping }
  | "t" ->
      options := { o with fill_rule = (match o.fill_rule with Triangle.Epsilon -> Triangle.Top_left | Top_left -> Epsilon) }
  | "o" -> Opti.enabled := not !Opti.enabled
  | "x" -> magnifier := not !magnifier
  | "r" -> Pixelate.next ()
  | "y" -> renderer := (!renderer + 1) mod List.length renderers
  | "v" -> split := not !split
  | "h" -> help := not !help
  | _ -> ()

(* e.g. "m:phong b:cull=on f:wire=off z:zbuffer p:perspective
 * i:bilinear c:clip=on t:epsilon o:opti=on x:zoom=off y:raster h:help" *)
let title_keys () =
  let o = !options in
  let on_off b = if b then "on" else "off" in
  Printf.sprintf "m:%s b:cull=%s f:wire=%s z:%s p:%s i:%s c:clip=%s t:%s o:opti=%s x:zoom=%s r:%s y:%s h:help"
    (match o.shading with
    | Shading.Flat_color -> "nolight"
    | Flat_shading -> "flat"
    | Gouraud -> "gouraud"
    | Phong -> "phong")
    (on_off o.backface_culling) (on_off o.wireframe)
    (match o.visibility with Z_buffer -> "zbuffer" | Painters_algorithm -> "painter")
    (match o.interpolation with Interpolate.Perspective_correct -> "perspective" | Linear -> "linear")
    (if o.bilinear then "bilinear" else "nearest")
    (on_off o.clipping)
    (match o.fill_rule with Triangle.Epsilon -> "epsilon" | Top_left -> "topleft")
    (on_off !Opti.enabled) (on_off !magnifier)
    (Pixelate.name ~width:(int_of_float Playground.default_width) ~height:(int_of_float Playground.default_height))
    (match raytraced () with None -> "raster" | Some _ -> "raytrace" ^ string_of_int !renderer)

(* the same, one line per key, for "h" (Help_overlay) *)
let help_lines () =
  let o = !options in
  let on_off b = if b then "on" else "off" in
  [
    ("h", "this help");
    ( "m",
      "shading: "
      ^
      match o.shading with
      | Shading.Flat_color -> "no lighting"
      | Flat_shading -> "flat"
      | Gouraud -> "Gouraud"
      | Phong -> "Phong" );
    ("b", "backface culling: " ^ on_off o.backface_culling);
    ("f", "wireframe: " ^ on_off o.wireframe);
    ("z", "visibility: " ^ match o.visibility with Z_buffer -> "z-buffer" | Painters_algorithm -> "painter's algorithm");
    ( "p",
      "interpolation: "
      ^ match o.interpolation with Interpolate.Perspective_correct -> "perspective-correct" | Linear -> "linear" );
    ("i", "texture filtering: " ^ if o.bilinear then "bilinear" else "nearest");
    ("c", "near-plane clipping: " ^ on_off o.clipping);
    ("t", "fill rule: " ^ match o.fill_rule with Triangle.Epsilon -> "epsilon" | Top_left -> "top-left");
    ("o", "optimizations: " ^ on_off !Opti.enabled);
    ( "r",
      "resolution: "
      ^ Pixelate.name ~width:(int_of_float Playground.default_width) ~height:(int_of_float Playground.default_height) );
    ("x", "pixel magnifier, following the mouse: " ^ on_off !magnifier);
    ("y", "renderer: " ^ renderer_name ());
    ("v", "the rasterizer and the ray tracer side by side: " ^ on_off !split);
    ("Ctrl", "+ a key: that key's debug action only, not the game's");
    ("Q", "quit");
  ]

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)
(* The SDL event loop/Playground.computer bookkeeping/frame-pacing itself
 * lives in Native_loop_3d, shared with the OpenGL backend (see
 * docs/claude_notes/plan_opengl.md) -- this module only creates the
 * window/pixel buffer and supplies the [draw] callback that turns a
 * (camera, shape3d list) into pixels via Shape3d_render_software. *)
open Native_loop_3d

let preload_texture = Texture_decode.preload

let run_app3d ?(rendering = Playground3d.default_rendering) ?capture_mouse ?flags ?network
    (app3d : ('model, 'msg) Playground3d.app3d) : unit =
  Option.iter Download.grant network;
  (* claude: Multiplayer3d's net=host, net=join and net=relay, as the 2D
   * platform's run_app *)
  Transport.set_connect Connect.connect;
  (* claude: -v, -debug, and the -fixed-time/-keys/-dump-frame flags (see
   * Native_loop_3d) *)
  Native_loop_3d.parse_cli_and_setup_logging ();
  if Native_loop_3d.raytrace_at_start () then renderer := List.length Raytrace.algorithms;
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
   * *is* drawing into the window, like the 2D software backend *)
  let pixels_2d = Bigarray.reshape_2 (Bigarray.genarray_of_array1 pixels) sy sx in
  let fb = Framebuffer.of_pixels pixels_2d in
  (* claude: a Framebuffer's pixels are 0xAARRGGBB, 8 bits per channel;
   * better a clear error than wrong colors on a window that isn't *)
  let* masks = Sdl.pixel_format_enum_to_masks (Sdl.get_surface_format_enum window_surface) in
  (match masks with
  | 32, 0xFF0000l, 0xFF00l, 0xFFl, _ -> ()
  | _ -> failwith "the window's pixels are not 32-bit xRGB, which this backend needs");

  (* claude: unlike playground/platforms/native's run_app, no "Loading..." message
   * first -- the window just stays whatever the OS shows it as
   * (typically blank) until the first frame is ready. Fine for now (a
   * texture-heavy game should mostly preload_texture everything it
   * needs up front anyway, making this a short, one-time pause),
   * revisit if it's ever noticeable. *)
  Texture_decode.load_queued ();

  (* the z-buffer, the size of the framebuffer drawn into (smaller with
   * "r"), made again when that size changes *)
  let zbuffer = ref (sx, sy, Zbuffer.create ~width:sx ~height:sy) in
  let zbuffer_for (fb : Framebuffer.t) : Zbuffer.t =
    let w, h, zb = !zbuffer in
    if w = fb.width && h = fb.height then zb
    else begin
      let zb = Zbuffer.create ~width:fb.width ~height:fb.height in
      zbuffer := (fb.width, fb.height, zb);
      zb
    end
  in

  (* claude: a split screen's views (Playground3d.split3d) are each
   * drawn in a framebuffer of their own size -- the camera then gets
   * the aspect of its rectangle for free -- and copied into theirs, a
   * row at a time; one for each size, made once *)
  (* claude: the scene of a view, by the renderer "y" chose *)
  let render_scene (fb : Framebuffer.t) (zb : Zbuffer.t) (v : Playground3d.view) : unit =
    let shape = Playground3d.group3d v.shapes in
    let raster () = Shape3d_render_software.render ~options:!options fb zb v.camera shape in
    let ray_trace ?from_x rt = Shape3d_render_software.raytrace ~options:rt ~bilinear:!options.bilinear ?from_x fb v.camera shape in
    if !split then begin
      (* claude: the whole frame rasterized, then its right half ray
       * traced over it: the two renderers on the same frame, timed *)
      let rt = match raytraced () with Some rt -> rt | None -> { Raytrace.default_options with samples = Native_loop_3d.raytrace_samples () } in
      let t0 = Unix.gettimeofday () in
      raster ();
      let t1 = Unix.gettimeofday () in
      ray_trace ~from_x:(fb.width / 2) rt;
      let t2 = Unix.gettimeofday () in
      (* the ray tracer did half the pixels: twice that for the frame *)
      split_times := Some ((t1 -. t0) *. 1000., 2. *. (t2 -. t1) *. 1000.);
      for y = 0 to fb.height - 1 do
        Framebuffer.plot fb ~x:(fb.width / 2) ~y ~rgb:0 ~alpha:1.
      done
    end
    else match raytraced () with Some rt -> ray_trace rt | None -> raster ()
  in
  let view_buffers : (int * int, Framebuffer.t * Zbuffer.t) Hashtbl.t = Hashtbl.create 4 in
  let view_buffer (w : int) (h : int) : Framebuffer.t * Zbuffer.t =
    match Hashtbl.find_opt view_buffers (w, h) with
    | Some b -> b
    | None ->
        let b = (Framebuffer.create ~width:w ~height:h, Zbuffer.create ~width:w ~height:h) in
        Hashtbl.replace view_buffers (w, h) b;
        b
  in
  let draw_view (fb : Framebuffer.t) (v : Playground3d.view) : unit =
    let x0 = int_of_float (Float.round (v.area.x *. float_of_int fb.width)) in
    let x1 = int_of_float (Float.round ((v.area.x +. v.area.w) *. float_of_int fb.width)) in
    (* the framebuffer's rows go down, the area's y up *)
    let y0 = int_of_float (Float.round ((1. -. v.area.y -. v.area.h) *. float_of_int fb.height)) in
    let y1 = int_of_float (Float.round ((1. -. v.area.y) *. float_of_int fb.height)) in
    let w = x1 - x0 and h = y1 - y0 in
    if w > 0 && h > 0 then begin
      let sub, zb = view_buffer w h in
      Framebuffer.clear sub ~rgb:0xFFFFFF;
      render_scene sub zb v;
      for r = 0 to h - 1 do
        Bigarray.Array1.blit (Bigarray.Array2.slice_left sub.pixels r)
          (Bigarray.Array1.sub (Bigarray.Array2.slice_left fb.pixels (y0 + r)) x0 w)
      done
    end
  in
  (* claude: a frame's scene and HUD into [fb], whatever its size (the
   * window's, "r"'s smaller one, or -dump-size's) *)
  let render_frame (fb : Framebuffer.t) (zb : Zbuffer.t) (computer : Playground.computer) (views : Playground3d.view list)
      ~(hud : bool) ~(hud_options : Shape_render_software.options) ~(scale : float) : unit =
    Framebuffer.clear fb ~rgb:0xFFFFFF;
    (match views with
    | [ v ] when v.area = Playground3d.whole -> render_scene fb zb v
    | views -> List.iter (draw_view fb) views);
    (* claude: a HUD pass, once the 3D scene above is fully rasterized
     * into [fb] for this frame: the 2D shapes drawn on top by the 2D
     * software rasterizer (playground/platforms/software/Shape_render_software,
     * graphics/2d/), into the same framebuffer. No clear here (unlike
     * the 2D backend's per-frame one) -- this must only add pixels on
     * top, never erase the 3D frame underneath. See
     * docs/claude_notes/done/plan_hud.md. *)
    if hud then
      match Playground3d.views_hud computer.screen views with
      | [] -> ()
      | hud_shapes -> Shape_render_software.render ~options:hud_options ~scale fb hud_shapes
  in
  (* the last frame, for -dump-size to make again *)
  let last_frame : (Playground.computer * Playground3d.view list) option ref = ref None in
  let draw (computer : Playground.computer) (views : Playground3d.view list) : unit =
    last_frame := Some (computer, views);
    (* at the resolution of "r", the scene and its HUD, then blown up
     * (Pixelate); the HUD without antialiasing then, as in the 2D
     * backend: big pixels, no seams *)
    let hud_options =
      { Shape_render_software.default_options with antialiasing = !Pixelate.factor = 1 }
    in
    Pixelate.draw fb (fun fb ~scale ->
        render_frame fb (zbuffer_for fb) computer views ~hud:true ~hud_options ~scale);
    (* claude: the split view's two times; not measured, as far as
     * anyone reading a golden frame can tell, under -fixed-time *)
    (if !split then
       let ms t = if Native_loop_3d.deterministic () then "(not timed: -fixed-time)" else Printf.sprintf "%.0f ms" t in
       match !split_times with
       | Some (raster, rt) ->
           let name = match raytraced () with Some _ -> renderer_name () | None -> "the ray tracer, " ^ Raytrace.name Raytrace.latest in
           Help_overlay.draw fb [ ("left", "the rasterizer: " ^ ms raster); ("right", name ^ ": " ^ ms rt) ]
       | None -> ());
    if !help then Help_overlay.draw fb (help_lines ());
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
  (* claude: -dump-frame (see Native_loop_3d): the frame as a PPM or a
   * PNG (Native_loop_2d.write_frame) *)
  let dump_frame file =
    match (Native_loop_3d.dump_size (), Native_loop_3d.dump_hud (), !last_frame) with
    | None, true, _ | _, _, None -> Native_loop_2d.write_frame ~width:sx ~height:sy (fun x y -> Framebuffer.get_rgb fb ~x ~y) file
    | size, hud, Some (computer, views) ->
        (* claude: -dump-size, -no-hud: the frame made again offscreen,
         * at its own size, its HUD scaled to it *)
        let w, h = Option.value size ~default:(sx, sy) in
        let big = Framebuffer.create ~width:w ~height:h in
        render_frame big (Zbuffer.create ~width:w ~height:h) computer views ~hud
          ~hud_options:Shape_render_software.default_options ~scale:(float_of_int w /. float_of_int sx);
        Native_loop_2d.write_frame ~width:w ~height:h (fun x y -> Framebuffer.get_rgb big ~x ~y) file
  in
  Native_loop_3d.run ~sdl_window ~sx ~sy ~title_prefix:"Playground3D" ~on_key_press
    ~init:(Playground3d.init3d app3d) ~update:(Playground3d.update3d app3d) ~view:(Playground3d.views3d app3d)
    ~draw ~present ~dump_frame
    ?title_keys:(if Native_loop_3d.debug_keys_enabled () then Some title_keys else None)
    ?capture_mouse ?flags ()
