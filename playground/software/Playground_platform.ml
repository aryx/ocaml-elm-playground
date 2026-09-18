(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Software-rendering backend of Playground: every pixel is computed by
 * our own code (Shape_render_software, on top of playground/raster/),
 * and SDL is only used to open a window, get input events, and show the
 * pixels. See docs/claude_notes/plan_software_2d.md.
 *
 * The trick that makes this simple: SDL gives us the window's pixels as
 * a plain array in memory (Native_loop_2d.create_window), and a
 * Framebuffer is just a view of such an array, so drawing into the
 * framebuffer *is* drawing into the window; Native_loop_2d.present then
 * asks SDL to show it.
 *)

let title = "Playground (software rasterizer)"

(*****************************************************************************)
(* Debug keys *)
(*****************************************************************************)
(* Keys to turn rendering features on and off while any example or game
 * runs, to see what each one does -- like playground3d/software/'s
 * "m"/"b"/"f"/"z"/"p". The window title shows their current state.
 * Avoid the keys games use: arrows, w/a/s/d, space.
 *
 *  - "t": transparency (Porter-Duff alpha blending) on/off; try
 *    examples/Mouse.exe, whose circle fades while the button is down
 *  - "b": bounding boxes instead of the real outlines (the first
 *    version of this backend drew only those)
 *  - "f": wireframe, outlines only (Bresenham lines, midpoint circles)
 *  - "z": the pixel magnifier (Magnifier), following the mouse
 *)

let options = ref Shape_render_software.default_options
let magnifier = ref false

let on_key_press (key : string) =
  match key with
  | "t" -> options := { !options with alpha_blending = not !options.alpha_blending }
  | "b" -> options := { !options with bounding_boxes = not !options.bounding_boxes }
  | "f" -> options := { !options with wireframe = not !options.wireframe }
  | "z" -> magnifier := not !magnifier
  | _ -> ()

(* e.g. "Playground (software rasterizer) -- 60 fps -- t:alpha=on
 * b:boxes=off f:wire=off z:zoom=off" *)
let window_title ~fps =
  let on_off b = if b then "on" else "off" in
  Printf.sprintf "%s -- %.0f fps -- t:alpha=%s b:boxes=%s f:wire=%s z:zoom=%s" title fps
    (on_off !options.alpha_blending) (on_off !options.bounding_boxes)
    (on_off !options.wireframe) (on_off !magnifier)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let preload_image = Image_decode.preload

let run_app (app : _ Playground.app) =
  Native_loop_2d.parse_cli_and_setup_logging ();
  let sx = int_of_float Playground.default_width in
  let sy = int_of_float Playground.default_height in

  let (sdl_window, pixels) = Native_loop_2d.create_window ~title ~sx ~sy in
  let fb = Framebuffer.of_pixels pixels in

  (* TODO: a "Loading..." message, once we can draw text (phase 5);
   * until then the window just stays white while images download *)
  Native_loop_2d.present sdl_window;
  ignore (Image_decode.load_queued () : string list);

  let draw ~fps shapes =
    Framebuffer.clear fb ~rgb:0xFFFFFF;
    Shape_render_software.render ~options:!options fb shapes;
    if !magnifier then begin
      (* SDL keeps track of the mouse position, in window pixels *)
      let (_buttons, (mx, my)) = Tsdl.Sdl.get_mouse_state () in
      Magnifier.draw fb ~cx:mx ~cy:my
    end;
    (* in the window title until we can draw text (phase 5) *)
    Tsdl.Sdl.set_window_title sdl_window (window_title ~fps)
  in
  Native_loop_2d.run ~sdl_window ~sx ~sy ~draw ~on_key_press
    ~init:app.init ~update:app.update ~subscriptions:app.subscriptions ~view:app.view
