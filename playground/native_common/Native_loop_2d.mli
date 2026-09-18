(* The SDL window/event loop shared by the SDL-based 2D playground
 * backends (playground/native/, and the planned playground/software/ --
 * see docs/claude_notes/done/plan_software_2d.md). Each backend only supplies
 * how a Playground.shape list becomes pixels in the window's pixel
 * array. *)

(* [Ok x -> f x], [Error (`Msg msg) -> failwith msg], for Tsdl calls *)
val ( let* ) : ('a, [ `Msg of string ]) result -> ('a -> 'b) -> 'b

(* -v/-verbose/-debug/-quiet, and installs a Logs reporter; also
 * -uncapped (no 60 fps pacing), -debug-keys (see [debug_keys_enabled])
 * and, for reproducible frames (see
 * tests/2d/Golden_frames.ml), -fixed-time t (the app's clock stays at
 * t), -keys k (the debug keys k pressed, through [run]'s
 * [on_key_press], before the first frame), -dump-frame n file (after
 * drawing frame n, counted from 1, [run] calls its [dump_frame file],
 * then exits; the fps given to [draw] is then 0, and mouse and
 * keyboard are ignored), and -script s (game keys held over given
 * frames, see Input_script); the arguments without a dash are the app's
 * (see [app_args]). Parses once: later calls do nothing. *)
val parse_cli_and_setup_logging : unit -> unit

(* The command line's arguments without a dash (and not an option's
 * value), in order, e.g. ["level=5"; "fast"]: the app's own, for
 * Playground_platform.flags. Parses the command line if not done yet
 * (so it can be called before run_app, which calls
 * [parse_cli_and_setup_logging]). *)
val app_args : unit -> string list

(* -debug-keys was given: [run] calls its [on_key_press] for the
 * backend's debug keys. Off by default, so that all keys go to the app
 * only (a game may use "f" or "h" itself); -keys still presses its keys
 * either way. *)
val debug_keys_enabled : unit -> bool

(* Tsdl's key names to Playground's ("Left" -> "ArrowLeft", ...);
 * "Q" quits immediately. *)
val scancode_to_keystring : string -> string

(* The window surface's pixels, (sy, sx)-shaped, one 0xAARRGGBB int32
 * per pixel (SDL's window surface format on the platforms we run on,
 * which is also Cairo's ARGB32). Whatever is written there shows up on
 * [present]. *)
type pixels = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t

(* Sdl.init + a shown window of size [sx] x [sy], filled with white. *)
val create_window : title:string -> sx:int -> sy:int -> Tsdl.Sdl.window * pixels

(* Copy the window surface's pixels to the screen. *)
val present : Tsdl.Sdl.window -> unit

(* [dump_ppm pixels file]: the pixels as a binary PPM image, the usual
 * [dump_frame] for [run] *)
val dump_ppm : pixels -> string -> unit

(* [run ~sdl_window ~sx ~sy ~init ~update ~subscriptions ~view ~draw]
 * runs an app forever (like Playground_platform.run_app, it never
 * returns: "Q" or the window's close button call [exit]): each frame,
 * drains the SDL events into the app's msgs (via [subscriptions]) plus
 * a Tick, calls [draw ~fps v] with [v] the app's [view] (the backend
 * must have put the pixels in the window surface when it returns),
 * [present]s, and paces to 60fps. Mouse positions are already in Elm's
 * coordinates (origin at the window's center, y up). Each physical key
 * press (not the repeats while it's held) also calls [on_key_press] with
 * the key's name, e.g. "t", for backend-specific debug toggles; the app
 * still gets the key too.
 *
 * The arguments are the fields of a ('model, 'msg) Playground.app
 * ('view = Playground.shape list), passed one by one because this
 * library can't depend on elm_playground: a backend that
 * (implements elm_playground) can't also reach that same virtual
 * library through one of its dependencies -- dune forbids it (same
 * reason playground3d/native_common/Native_loop.mli is generic). *)
val run :
  sdl_window:Tsdl.Sdl.window ->
  sx:int ->
  sy:int ->
  init:(unit -> 'model * 'msg Cmd.t) ->
  update:('msg -> 'model -> 'model * 'msg Cmd.t) ->
  subscriptions:('model -> 'msg Sub.t) ->
  view:('model -> 'view) ->
  draw:(fps:float -> 'view -> unit) ->
  on_key_press:(string -> unit) ->
  dump_frame:(string -> unit) ->
  unit
