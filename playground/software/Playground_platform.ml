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
    Shape_render_software.render fb shapes;
    (* in the window title until we can draw text (phase 5) *)
    Tsdl.Sdl.set_window_title sdl_window (Printf.sprintf "%s -- %.0f fps" title fps)
  in
  Native_loop_2d.run ~sdl_window ~sx ~sy ~draw
    ~init:app.init ~update:app.update ~subscriptions:app.subscriptions ~view:app.view
