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
 * our own code (Shape_render_software, on top of graphics/),
 * and SDL is only used to open a window, get input events, and show the
 * pixels. See docs/claude_notes/done/plan_software_2d.md.
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
 * runs, to see what each one does -- like the 3D software backend's
 * "m"/"b"/"f"/"z"/"p". Only with the -debug-keys flag (see
 * Native_loop_2d), so that without it a game can use any key. The
 * window title shows their current state. Avoid the keys games use
 * most: arrows, w/a/s/d, space.
 *
 *  - "t": transparency (Porter-Duff alpha blending) on/off; try
 *    examples/Mouse.exe, whose circle fades while the button is down
 *  - "b": bounding boxes instead of the real outlines (the first
 *    version of this backend drew only those)
 *  - "f": wireframe, outlines only (Bresenham lines, midpoint circles)
 *  - "i": image filtering, bilinear or nearest pixel; try
 *    examples/Turtle.exe with the magnifier on the turtle
 *  - "n": antialiasing on/off; with the magnifier on any edge
 *  - "o": optimizations on/off, i.e. the original simple code instead
 *    of the optimized one (see Opti); watch the fps
 *  - "z": the pixel magnifier (Magnifier), following the mouse
 *  - "l": band-limited oscillators on/off (audio/Oscillator.mli), the
 *    sounds' aliases back; hear it on examples/AudioPiano.exe's high
 *    notes, see it with "v"
 *  - "v": the sound, seen: an oscilloscope, then a spectrum, then off
 *    (Audio_debug); try games/platform/TinyMario.exe (its music) or
 *    examples/AudioPiano.exe (space: the waveforms' harmonics)
 *  - "r": the resolution, full, then a half, a third, a quarter, each
 *    pixel shown as a 2x2, 3x3, 4x4 block (Pixelate): faster (less
 *    per-pixel work), and the look of the old low-resolution games
 *  - "h": this list, with each key's state, over the frame
 *  - Ctrl + any of them: the debug key alone, not given to the game
 *    (for a game that uses the key itself: AudioPiano's "h")
 *    (Help_overlay)
 *)

let options = ref Shape_render_software.default_options
let magnifier = ref false
let help = ref false
let audio_view = ref Audio_debug.Off

let on_key_press (key : string) =
  match key with
  | "t" -> options := { !options with alpha_blending = not !options.alpha_blending }
  | "b" -> options := { !options with bounding_boxes = not !options.bounding_boxes }
  | "f" -> options := { !options with wireframe = not !options.wireframe }
  | "i" -> options := { !options with bilinear = not !options.bilinear }
  | "n" -> options := { !options with antialiasing = not !options.antialiasing }
  | "o" -> Opti.enabled := not !Opti.enabled
  | "l" -> Synth.band_limited := not !Synth.band_limited
  | "z" -> magnifier := not !magnifier
  | "v" -> audio_view := Audio_debug.next !audio_view
  | "r" -> Pixelate.next ()
  | "h" -> help := not !help
  | _ -> ()

(* e.g. "Playground (software rasterizer) -- 60 fps -- t:alpha=on
 * b:boxes=off f:wire=off i:bilinear n:aa=on o:opti=on z:zoom=off
 * h:help" *)
let window_title ~fps =
  let on_off b = if b then "on" else "off" in
  if not (Native_loop_2d.debug_keys_enabled ()) then Printf.sprintf "%s -- %.0f fps" title fps
  else
    Printf.sprintf "%s -- %.0f fps -- t:alpha=%s b:boxes=%s f:wire=%s i:%s n:aa=%s o:opti=%s l:bandlimit=%s z:zoom=%s v:%s r:%s h:help"
      title fps (on_off !options.alpha_blending) (on_off !options.bounding_boxes)
      (on_off !options.wireframe)
      (if !options.bilinear then "bilinear" else "nearest")
      (on_off !options.antialiasing) (on_off !Opti.enabled) (on_off !Synth.band_limited) (on_off !magnifier) (Audio_debug.name !audio_view)
      (Pixelate.name ~width:(int_of_float Playground.default_width) ~height:(int_of_float Playground.default_height))

(* the same, one line per key, for "h" (Help_overlay) *)
let help_lines () =
  let on_off b = if b then "on" else "off" in
  let o = !options in
  [
    ("h", "this help");
    ("t", "transparency (alpha blending): " ^ on_off o.alpha_blending);
    ("b", "bounding boxes instead of shapes: " ^ on_off o.bounding_boxes);
    ("f", "wireframe: " ^ on_off o.wireframe);
    ("i", "image filtering: " ^ if o.bilinear then "bilinear" else "nearest");
    ("n", "antialiasing: " ^ on_off o.antialiasing);
    ("o", "optimizations: " ^ on_off !Opti.enabled);
    ("l", "band-limited oscillators (no aliases): " ^ on_off !Synth.band_limited);
    ("z", "pixel magnifier, following the mouse: " ^ on_off !magnifier);
    ("v", "the sound, seen: " ^ Audio_debug.name !audio_view);
    ( "r",
      "resolution: "
      ^ Pixelate.name ~width:(int_of_float Playground.default_width) ~height:(int_of_float Playground.default_height) );
    ("Ctrl", "+ a key: that key's debug action only, not the game's");
    ("Q", "quit");
  ]

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let preload_image = Image_decode.preload

(* Text on top of the app's shapes, as ordinary Playground shapes, so
 * it's drawn like any [words], always with the default options *)
let overlay fb (shapes : Playground.shape list) =
  Shape_render_software.render ~options:Shape_render_software.default_options fb shapes

(* e.g. "1000x1000 -- 60 fps", left-aligned at the bottom left, where
 * the Cairo backend puts it, with its baseline 5% above the bottom:
 * words are centered on their position, so move right by half their
 * width, and up by the 9 font units from Hershey's middle (y = 0) to
 * its baseline (y = 9) *)
let fps_counter (fb : Framebuffer.t) ~fps : Playground.shape =
  let w = float fb.width and h = float fb.height in
  let text = Printf.sprintf "%gx%g -- %.0f fps" w h fps in
  let _strokes, width = Hershey.layout text in
  let unit = Playground.words_font_size /. Hershey.units_per_em in
  Playground.words Playground.black text
  |> Playground.move (-.(0.45 *. w) +. (width *. unit /. 2.)) (-.(0.45 *. h) +. (9. *. unit))

let flags () : Playground.flags = Playground.flags_of_strings (Native_loop_2d.app_args ())

(* claude: documents, in a directory (native_common/Store); the
 * capability is the caller's proof it may, see the .mli *)
let store (_ : < Cap.open_out; .. >) name bytes = Store.store name bytes
let fetch (_ : < Cap.open_in; .. >) name = Store.fetch name
let stored (_ : < Cap.readdir; .. >) = Store.stored ()
let export (_ : < Cap.open_out; .. >) name bytes = Store.export name bytes

(* claude: Audio.loop_from's files: a local path read, a URL downloaded
 * (curl, blocking: Download.local_file) *)
let fetch_file (source : string) (k : string option -> unit) : unit =
  match In_channel.with_open_bin (Download.local_file ~prefix:"audio" source) In_channel.input_all with
  | bytes -> k (Some bytes)
  | exception e ->
      Logs.warn (fun m -> m "can't get %s: %s" source (Printexc.to_string e));
      k None

let run_app ?(rendering = Playground.default_rendering) ?(flags = []) (app : _ Playground.app) =
  Audio.set_fetcher fetch_file;
  (* the app's choices are the starting values; the keys can change them *)
  options :=
    { !options with antialiasing = rendering.antialiasing; bilinear = rendering.smooth_images };
  Native_loop_2d.parse_cli_and_setup_logging ();
  let sx = int_of_float Playground.default_width in
  let sy = int_of_float Playground.default_height in

  let (sdl_window, pixels) = Native_loop_2d.create_window ~title ~sx ~sy in
  let fb = Framebuffer.of_pixels pixels in

  (* show something right away while images download *)
  overlay fb [ Playground.words Playground.black "Loading..." ];
  Native_loop_2d.present sdl_window;
  ignore (Image_decode.load_queued () : string list);

  let draw ~fps shapes =
    (* at the resolution of "r", then blown up (Pixelate); what follows,
     * the debug views, at the window's. Big pixels without antialiasing:
     * the retro look, and without the seams where two shapes share an
     * edge between pixels (each covering it partly), which a 3x3 block
     * makes plain to see. *)
    let options = if !Pixelate.factor > 1 then { !options with antialiasing = false } else !options in
    Pixelate.draw fb (fun fb ~scale ->
        Framebuffer.clear fb ~rgb:0xFFFFFF;
        Shape_render_software.render ~options ~scale fb shapes);
    overlay fb [ fps_counter fb ~fps ];
    overlay fb (Audio_debug.shapes !audio_view (Playground.to_screen (float fb.width) (float fb.height)));
    if !help then Help_overlay.draw fb (help_lines ());
    if !magnifier then begin
      (* SDL keeps track of the mouse position, in window pixels *)
      let (_buttons, (mx, my)) = Tsdl.Sdl.get_mouse_state () in
      Magnifier.draw fb ~cx:mx ~cy:my
    end;
    (* the keys and their state *)
    Tsdl.Sdl.set_window_title sdl_window (window_title ~fps)
  in
  Native_loop_2d.run ~sdl_window ~sx ~sy ~draw ~on_key_press ~dump_frame:(Native_loop_2d.dump_ppm pixels)
    ~pull_audio:(fun n -> let s = Audio.pull n in Audio_debug.record s; s) ~dump_audio:Wav.write
    ~init:(fun () -> app.init flags) ~update:app.update ~subscriptions:app.subscriptions ~view:app.view
