open Basics

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Native backend of Playground using Cairo and SDL.
 *
 * history:
 *  - use Graphics, but no keydown/keyup
 *  - use ocaml-SDL, but initialy lack example to work with Cairo
 *  - use TSDL+cairo
 *
 * The SDL loop is in Native_loop_2d, shared with other SDL backends.
 *)

(*****************************************************************************)
(* Render (independent of Playground) *)
(*****************************************************************************)
(* The actual shape-drawing code (render_shape and everything it calls)
 * now lives in Shape_render_native, a plain sibling module, so it's
 * usable from a second, independent caller too (the 3D software
 * backend's HUD overlay pass -- see docs/claude_notes/plan_hud.md). *)

let debug_coordinates cr ~sx ~sy =
  let (x0,y0) = Cairo.device_to_user cr 0. 0. in
  let (xmax, ymax) = Cairo.device_to_user cr (float sx) (float sy) in
  Logs.debug (fun m -> m "device 0,0 => %.1f %.1f, device %d,%d => %.1f %.1f"
    x0 y0 sx sy xmax ymax)

(*****************************************************************************)
(* FPS (using Cairo) *)
(*****************************************************************************)
(* claude: the counting itself is in Native_loop_2d.Fps *)

let draw_fps cr width height fps =
  Cairo.set_source_rgba cr 0. 0. 0. 1.;
  Cairo.move_to cr (0.05 *. width) (0.95 *. height);
  Cairo.show_text cr (Printf.sprintf "%gx%g -- %.0f fps" width height fps)

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

(* claude: preload_image just queues -- see Image_decode.preload -- so it
 * has no ordering dependency on anything and is safe to call anytime,
 * including before run_app has even started (examples/Mario.ml calls it
 * at module init, before run_app). run_app is what actually downloads
 * the queue (via Image_native.load_queued), once it has parsed argv, set
 * up logging, and created its window -- so the window is visible and
 * -v/-debug output makes sense before any blocking network call
 * happens. *)
let preload_image = Image_native.preload

let flags () : Playground.flags = Playground.flags_of_strings (Native_loop_2d.app_args ())

let utc_offset (Playground.Time t) : int = Native_loop_2d.utc_offset t

(* claude: see Playground_platform.mli; set by run_app's draw, each frame,
 * to the scale it draws the picture with *)
let ratio = ref 1.
let pixel_ratio () : float = !ratio

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

let run_app ?(rendering = Playground.default_rendering) ?(flags = []) ?network ?(window = Playground.default_window) app =
  let { Playground.screen_size = screen; follows_window = screen_follows_window; platform_keys; skip_same_view } = window in
  (* claude: tinybox taking the app for a preview (Playground.capture) *)
  match !Playground.capture with
  | Some give -> give (Playground.Any_app app)
  | None ->
  Option.iter Download.grant network;
  Audio.set_fetcher fetch_file;
  (* claude: Multiplayer's net=host and net=join (UDP), net=relay
   * (WebSocket), and Universe's worlds *)
  Transport.set_connect Connect.connect;
  Transport.set_tunnel Tls_tunnel.connect;
  Transport.set_tls (fun caps ~host ~port -> Tls_client.connect_lines caps ~host ~port);
  Native_loop_2d.parse_cli_and_setup_logging ();
  let sx, sy = match screen with Some wh -> wh | None -> (int_of_float Playground.default_width, int_of_float Playground.default_height) in
  (* claude: [screen_follows_window]: the program's screen is the window
   * (in points), whatever its size: the picture's size is then the
   * window's last one, kept here for [draw] *)
  let followed = ref (sx, sy) in

  (* claude: a window that can change size (-size, -fullscreen,
   * Alt+Enter, dragged), the program's sx by sy picture scaled to fit it,
   * centred, black bars round it (Native_loop_2d.scale); the program's
   * screen stays sx by sy -- or, with [screen_follows_window], is the
   * window, drawn 1 to 1 *)
  let (sdl_window, pixels0) =
    Native_loop_2d.create_window ~resizable:true ~title:"Playground using SDL+Cairo" ~sx ~sy () in

  (* Create a Cairo surface to write on the pixels, made again when the
   * window changes size (the pixels are then new) *)
  let make_surface (pixels : Native_loop_2d.pixels) =
    let w = Bigarray.Array2.dim2 pixels and h = Bigarray.Array2.dim1 pixels in
    let sdl_surface = Cairo.Image.create_for_data32 ~w ~h pixels in
    let cr = Cairo.create sdl_surface in
    Cairo.identity_matrix cr;
    (* claude: Playground.rendering's antialiasing, for shapes and text
     * (set once per surface: save/restore below keep it) *)
    if not rendering.antialiasing then begin
      Cairo.set_antialias cr Cairo.ANTIALIAS_NONE;
      let font_options = Cairo.Font_options.create () in
      Cairo.Font_options.set_antialias font_options Cairo.ANTIALIAS_NONE;
      Cairo.Font_options.set cr font_options
    end;
    (pixels, sdl_surface, cr, (w, h))
  in
  let current = ref (make_surface pixels0) in
  let (_, _, cr, _) = !current in
  debug_coordinates cr ~sx ~sy;

  (* claude: show a "Loading..." message right away, then run any queued
   * preload_image downloads -- without this the window doesn't show
   * anything until preloading finishes, which looks like the app just
   * hung for a few seconds with no feedback. *)
  let (_, _, cr, (w, h)) = !current in
  Cairo.set_source_rgba cr 0. 0. 0. 1.;
  Cairo.move_to cr (float w /. 2. -. 40.) (float h /. 2.);
  Cairo.show_text cr "Loading...";
  Native_loop_2d.present sdl_window;
  Image_native.load_queued ();

  let draw ~fps shapes =
    let (_, sdl_surface, cr, (w, h)) = !current in
    (* claude: the window's own size as the picture's: the scale is then
     * only the display's density (2 on a Retina), and no bar is left *)
    let sx, sy = if screen_follows_window then !followed else (sx, sy) in
    let k = Native_loop_2d.scale ~sx ~sy (w, h) in
    ratio := k;
    Cairo.save cr;

    (* claude: black round the picture, when the window isn't sx by sy *)
    if (w, h) <> (sx, sy) then begin
      Cairo.set_source_rgba cr 0. 0. 0. 1.;
      Cairo.paint cr
    end;
    (* reset the surface content: the picture's square, nothing drawn
     * outside it (a program may draw beyond its screen) *)
    Cairo.identity_matrix cr;
    Cairo.rectangle cr ((float w -. (k *. float sx)) /. 2.) ((float h -. (k *. float sy)) /. 2.)
      ~w:(k *. float sx) ~h:(k *. float sy);
    Cairo.clip cr;
    Cairo.set_source_rgba cr 1. 1. 1. 1.;
    Cairo.paint cr;

    (* elm-convetion: set the origin (0, 0) in the center of the surface *)
    Cairo.translate cr (float w / 2.) (float h / 2.);
    Cairo.scale cr k k;
    (*debug_coordinates cr ~sx ~sy;*)

    Shape_render_native.render ~smooth_images:rendering.smooth_images cr shapes;

    Cairo.restore cr;
    draw_fps cr (float w) (float h) fps;

    (* Don't forget to flush the surface before using its content. *)
    Cairo.Surface.flush sdl_surface
  in
  let (app : _ Playground.app) = app in
  (* claude: -debug-keys is parsed by the shared loop, but this backend
   * has no debug keys: say where they are rather than silently ignore *)
  if Native_loop_2d.debug_keys_enabled () then
    prerr_endline
      "-debug-keys: no debug keys in the Cairo backend; they're in the software one, e.g. examples/software/AudioPiano.exe";
  (* claude: threads=on, the commands' blocking calls on threads *)
  let threads = List.assoc_opt "threads" flags = Some "on" in
  Native_loop_2d.run ~platform_keys ~follow_window:screen_follows_window ~skip_same_view ~threads ~sdl_window ~sx ~sy ~draw ~on_key_press:(fun _key -> ())
    ~on_resize:
      (Some
         (fun w h ->
           followed := (w, h);
           current := make_surface (Native_loop_2d.window_pixels sdl_window)))
    ~dump_frame:(fun file -> let (pixels, _, _, _) = !current in Native_loop_2d.dump_pixels pixels file)
    ~pull_audio:(fun n -> let s = Audio.pull n in (s.left, s.right))
    ~dump_audio:(fun file (left, right) -> Wav.write_stereo file { left; right })
    ~audio_latency:Audio.set_latency
    ~init:(fun () ->
      let model, cmd = app.init flags in
      (* claude: a screen other than the default, said to the program
       * before its first frame, through its own subscriptions *)
      match screen with
      | None -> (model, cmd)
      | Some (w, h) -> (
          match Sub.event_to_msgopt (Sub.EResized (w, h)) (app.subscriptions model) with
          | Some msg ->
              let model, cmd2 = app.update msg model in
              (model, Cmd.batch [ cmd; cmd2 ])
          | None -> (model, cmd)))
    ~update:app.update ~subscriptions:app.subscriptions ~view:app.view
