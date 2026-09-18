open Basics
module E = Sub
open Tsdl

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Native backend of Playground using Cairo and SDL.
 *
 * history:
 *  - use Graphics, but no keydown/keyup
 *  - use ocaml-SDL, but initialy lack example to work with Cairo
 *  - use TSDL+cairo
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* was in my Commom.ml before, could move in a core/Regexp_.ml *)

let spf = Printf.sprintf

(*****************************************************************************)
(* Render (independent of Playground) *)
(*****************************************************************************)
(* The actual shape-drawing code (render_shape and everything it calls)
 * now lives in Shape_render_native, a plain sibling module, so it's
 * usable from a second, independent caller too (playground3d/software/'s
 * HUD overlay pass -- see docs/claude_notes/plan_hud.md). *)

(* Cairo (0,0) is at the top left of the screen, y down; Elm's
 * convention (y up) is the opposite -- see the identical convert in
 * Shape_render_native, duplicated here (a one-line helper, not worth
 * exposing from that module's minimal render-only interface) since
 * mouse-event coordinates need the same conversion. *)
let convert (x, y) = (x, -.y)

let debug_coordinates cr ~sx ~sy =
  let (x0,y0) = Cairo.device_to_user cr 0. 0. in
  let (xmax, ymax) = Cairo.device_to_user cr (float sx) (float sy) in
  Logs.debug (fun m -> m "device 0,0 => %.1f %.1f, device %d,%d => %.1f %.1f"
    x0 y0 sx sy xmax ymax)

(*****************************************************************************)
(* FPS (using Cairo) *)
(*****************************************************************************)

module Fps = struct
(* was in cairo/examples/graphics_demo.ml *)
let lastfps = ref (Unix.gettimeofday ())
let frames = ref 0
let fps = ref 0.

let update_fps () =
  let t = Unix.gettimeofday () in
  let dt = t -. !lastfps in
  if dt > 0.5 then (
    fps := float !frames /. dt;
    frames := 0;
    lastfps := t
  );
  incr frames

let draw_fps cr width height =
  Cairo.set_source_rgba cr 0. 0. 0. 1.;
  Cairo.move_to cr (0.05 *. width) (0.95 *. height);
  Cairo.show_text cr (Printf.sprintf "%gx%g -- %.0f fps" width height !fps)
end

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

(* The tsdl library is a heavy user of Result, which is annoying
 * to check at every calls; fortunately OCaml 4.08 allow to define
 * monadic operators to remove some boilerplate!
 *)
let (let*) o f =
  match o with
  | Error (`Msg msg) ->
      failwith (spf "TSDL error: %s" msg)
  | Ok x -> f x

let scancode_to_keystring = function
 | "Left" -> "ArrowLeft"
 | "Right" -> "ArrowRight"
 | "Up" -> "ArrowUp"
 | "Down" -> "ArrowDown"

 | "Q" -> exit 0
 | s -> String.lowercase_ascii s

(* claude: preload_image just queues -- see Image_native.preload -- so it
 * has no ordering dependency on anything and is safe to call anytime,
 * including before run_app has even started (examples/Mario.ml calls it
 * at module init, before run_app). run_app is what actually downloads
 * the queue (via Image_native.load_queued), once it has parsed argv, set
 * up logging, and created its window -- so the window is visible and
 * -v/-debug output makes sense before any blocking network call
 * happens. *)
let preload_image = Image_native.preload

(* claude: generic -v/-verbose/-debug/-quiet handling for every native
 * example/game, so individual examples don't each need their own
 * Arg.parse boilerplate. Without a reporter installed, Logs.xxx calls
 * anywhere in the program are silently dropped (nothing else in this
 * codebase installs one), so this is also what makes the Image_native.ml
 * Logs.info calls (e.g. "loading image ...", visible with -v) actually
 * show up. *)
let parse_cli_and_setup_logging () =
  let level = ref (Some Logs.Warning) in
  let cli_flags = [
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " debug mode";
    "-quiet", Arg.Unit (fun () -> level := None),
    " quiet mode";
  ] in
  Arg.parse cli_flags
    (fun s -> raise (Arg.Bad (spf "don't know what to do with %s" s)))
    (spf "usage: %s [-v|-verbose|-debug|-quiet]" Sys.argv.(0));
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level !level

let run_app app =
  parse_cli_and_setup_logging ();
  let sx = int_of_float Playground.default_width in
  let sy = int_of_float Playground.default_height in

  let* () = Sdl.init Sdl.Init.(video + events) in
  let* sdl_window = Sdl.create_window ~w:sx ~h:sy "Playground using SDL+Cairo"
    Sdl.Window.shown in
  let sdl_event = Sdl.Event.create () in

  let* window_surface = Sdl.get_window_surface sdl_window in

  let pixels = Sdl.get_surface_pixels window_surface Bigarray.int32 in
  assert (Bigarray.Array1.dim pixels = sx *.. sy);

  (* less? need that? *)
  Bigarray.Array1.fill pixels 0xFFFFFFFFl ;
  let pixels =
    try
      let genarray = Bigarray.genarray_of_array1 pixels in
      Bigarray.reshape_2 genarray sy sx
    with _ ->
      let len = Bigarray.Array1.dim pixels in
      failwith (spf
        "Error while reshaping pixel array of length %d to screen size %d x %d"
        len sx sy)
  in
  (* Create a Cairo surface to write on the pixels *)
  let sdl_surface =
    Cairo.Image.create_for_data32 ~w:sx ~h:sy pixels
  in
  let cr = Cairo.create sdl_surface in

  Cairo.identity_matrix cr;
  debug_coordinates cr ~sx ~sy;

  (* claude: show a "Loading..." message right away, then run any queued
   * preload_image downloads -- without this the window doesn't show
   * anything until preloading finishes, which looks like the app just
   * hung for a few seconds with no feedback. *)
  Cairo.set_source_rgba cr 0. 0. 0. 1.;
  Cairo.move_to cr (float sx /. 2. -. 40.) (float sy /. 2.);
  Cairo.show_text cr "Loading...";
  let* () = Sdl.update_window_surface sdl_window in
  Image_native.load_queued ();

  let initmodel, _cmdsTODO = app.Playground.init () in
  let model = ref initmodel in

  (* claude: the loop below has no vsync (we blit to a plain SDL window
   * surface, not an accelerated/vsync'd renderer), so without this cap it
   * free-runs at several hundred fps. Playground.game's update functions
   * (e.g., examples/Mario.ml) use a fixed per-tick dt inherited from the
   * original Elm code, which assumes browser's requestAnimationFrame's
   * ~60Hz pacing (see playground/web/Playground_platform.ml's
   * animation_frame, which re-schedules itself via
   * Window.request_animation_frame); an uncapped native loop breaks that
   * assumption and makes games run several times too fast. *)
  let target_fps = 60. in
  let target_frame_time = 1. /. target_fps in

  (* typing "Q" will cause an 'exit 0' that will exit the loop *)
  while true do
    let frame_start = Unix.gettimeofday () in
    Cairo.save cr;

    (* reset the surface content *)
    Cairo.set_source_rgba cr 1. 1. 1. 1.;
    Cairo.paint cr;

    (* elm-convetion: set the origin (0, 0) in the center of the surface *)
    Cairo.identity_matrix cr;
    Cairo.translate cr (float sx / 2.) (float sy / 2.);
    (*debug_coordinates cr ~sx ~sy;*)

    (* one frame *)
    let apply_playground_event pevent =
      let subs = app.Playground.subscriptions !model in
      match E.event_to_msgopt pevent subs with
      | None -> ()
      | Some msg ->
        let newmodel, _cmds = app.Playground.update msg !model in
        model := newmodel
    in

    (* claude: drain the *whole* pending SDL event queue every frame,
     * instead of at most one event, and always additionally deliver a
     * Tick below. Playground.game's update_memory (e.g. Mario's physics)
     * only runs on Tick, not on KeyChanged (see Playground.game_update);
     * with only one SDL event consumed per loop iteration, a burst of
     * queued input events (e.g. OS key-repeat while holding an arrow key)
     * used to starve Tick delivery for several frames in a row, which
     * showed up as the game visibly slowing down while a key was held. *)
    let rec drain_sdl_events () =
      if Sdl.poll_event (Some sdl_event) then begin
        let event_type = Sdl.Event.get sdl_event Sdl.Event.typ in
        (match event_type with
        | x when x = Sdl.Event.mouse_motion ->
          let x = Sdl.Event.(get sdl_event mouse_motion_x) in
          let y = Sdl.Event.(get sdl_event mouse_motion_y) in
          let (x, y) = Cairo.device_to_user cr (float x) (float y) in
          let (x, y) = convert (x, y) in
          apply_playground_event (E.EMouseMove (int_of_float x, int_of_float y))

        | x when x = Sdl.Event.mouse_button_down ->
          apply_playground_event (E.EMouseButton true)

        | x when x = Sdl.Event.mouse_button_up ->
          apply_playground_event (E.EMouseButton false)

        | x when x = Sdl.Event.key_down ->
          let key = Sdl.(get_key_name Event.(get sdl_event keyboard_keycode)) in
          let str = scancode_to_keystring key in
          apply_playground_event (E.EKeyChanged (true, str))

        | x when x = Sdl.Event.key_up ->
          let key = Sdl.(get_key_name Event.(get sdl_event keyboard_keycode)) in
          let str = scancode_to_keystring key in
          apply_playground_event (E.EKeyChanged (false, str))

        (* claude: SDL posts this both when the (only, here) window's
         * close button is clicked, and -- on Unix -- when the process
         * receives SIGINT/SIGTERM, which SDL's own signal handler
         * intercepts and turns into this event instead of the default
         * "terminate the process" behavior. Without handling it, both
         * the close button and e.g. `kill`/Ctrl-C appeared to do
         * nothing: the event was received but silently ignored below. *)
        | x when x = Sdl.Event.quit -> exit 0

        (* other SDL event types (window resize/expose/...): ignored *)
        | _ -> ()
        );
        drain_sdl_events ()
      end
    in
    drain_sdl_events ();
    apply_playground_event (E.ETick (Unix.gettimeofday ()));

    let shapes = app.Playground.view !model in
    Shape_render_native.render cr shapes;

    Cairo.restore cr;
    Fps.draw_fps cr (float sx) (float sy);

    (* Don't forget to flush the surface before using its content. *)
    Cairo.Surface.flush sdl_surface;
    let* () = Sdl.update_window_surface sdl_window in

    (* Update our fps counter. *)
    Fps.update_fps ();

    let elapsed = Unix.gettimeofday () -. frame_start in
    if elapsed < target_frame_time
    then Unix.sleepf (target_frame_time -. elapsed);
  done
