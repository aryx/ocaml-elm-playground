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

let run_app ?(rendering = Playground.default_rendering) ?(flags = []) app =
  Audio.set_fetcher fetch_file;
  Native_loop_2d.parse_cli_and_setup_logging ();
  let sx = int_of_float Playground.default_width in
  let sy = int_of_float Playground.default_height in

  let (sdl_window, pixels) =
    Native_loop_2d.create_window ~title:"Playground using SDL+Cairo" ~sx ~sy in

  (* Create a Cairo surface to write on the pixels *)
  let sdl_surface =
    Cairo.Image.create_for_data32 ~w:sx ~h:sy pixels
  in
  let cr = Cairo.create sdl_surface in

  Cairo.identity_matrix cr;
  debug_coordinates cr ~sx ~sy;

  (* claude: Playground.rendering's antialiasing, for shapes and text
   * (set once: save/restore below keep it) *)
  if not rendering.antialiasing then begin
    Cairo.set_antialias cr Cairo.ANTIALIAS_NONE;
    let font_options = Cairo.Font_options.create () in
    Cairo.Font_options.set_antialias font_options Cairo.ANTIALIAS_NONE;
    Cairo.Font_options.set cr font_options
  end;

  (* claude: show a "Loading..." message right away, then run any queued
   * preload_image downloads -- without this the window doesn't show
   * anything until preloading finishes, which looks like the app just
   * hung for a few seconds with no feedback. *)
  Cairo.set_source_rgba cr 0. 0. 0. 1.;
  Cairo.move_to cr (float sx /. 2. -. 40.) (float sy /. 2.);
  Cairo.show_text cr "Loading...";
  Native_loop_2d.present sdl_window;
  Image_native.load_queued ();

  let draw ~fps shapes =
    Cairo.save cr;

    (* reset the surface content *)
    Cairo.set_source_rgba cr 1. 1. 1. 1.;
    Cairo.paint cr;

    (* elm-convetion: set the origin (0, 0) in the center of the surface *)
    Cairo.identity_matrix cr;
    Cairo.translate cr (float sx / 2.) (float sy / 2.);
    (*debug_coordinates cr ~sx ~sy;*)

    Shape_render_native.render ~smooth_images:rendering.smooth_images cr shapes;

    Cairo.restore cr;
    draw_fps cr (float sx) (float sy) fps;

    (* Don't forget to flush the surface before using its content. *)
    Cairo.Surface.flush sdl_surface
  in
  let (app : _ Playground.app) = app in
  (* claude: -debug-keys is parsed by the shared loop, but this backend
   * has no debug keys: say where they are rather than silently ignore *)
  if Native_loop_2d.debug_keys_enabled () then
    prerr_endline
      "-debug-keys: no debug keys in the Cairo backend; they're in the software one, e.g. examples/software/AudioPiano.exe";
  Native_loop_2d.run ~sdl_window ~sx ~sy ~draw ~on_key_press:(fun _key -> ())
    ~dump_frame:(Native_loop_2d.dump_ppm pixels)
    ~pull_audio:(fun n -> let s = Audio.pull n in (s.left, s.right))
    ~dump_audio:(fun file (left, right) -> Wav.write_stereo file { left; right })
    ~init:(fun () -> app.init flags) ~update:app.update ~subscriptions:app.subscriptions ~view:app.view
