(* The SDL window/event loop shared by the SDL-based 2D playground
 * backends (playground/platforms/native/, and the planned
 * playground/platforms/software/ --
 * see docs/claude_notes/done/plan_software_2d.md). Each backend only supplies
 * how a Playground.shape list becomes pixels in the window's pixel
 * array. *)

(* [Ok x -> f x], [Error (`Msg msg) -> failwith msg], for Tsdl calls *)
val ( let* ) : ('a, [ `Msg of string ]) result -> ('a -> 'b) -> 'b

(* -v/-verbose/-debug/-quiet, and installs a Logs reporter; also
 * -uncapped (no 60 fps pacing), -debug-keys (see [debug_keys_enabled])
 * and, for reproducible frames (see
 * Scenes_2d.ml), -fixed-time t (the app's clock stays at
 * t), -keys k (the debug keys k pressed, through [run]'s
 * [on_key_press], before the first frame), -dump-frame n file (after
 * drawing frame n, counted from 1, [run] calls its [dump_frame file],
 * then exits; the fps given to [draw] is then 0, and mouse and
 * keyboard are ignored), -dump-audio file (with -dump-frame, the sound
 * of those frames as a WAV), and -script s (game keys held over given
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
 * backend's debug keys. Off by default; -keys still presses its keys
 * either way. With it, Ctrl + a key is the debug key, and not the
 * app's (Ctrl-h: the help); a plain key is the app's alone, always (a
 * game may use "f" or "h" itself, a field takes any letter). False
 * once [run] was told [~platform_keys:false]. *)
val debug_keys_enabled : unit -> bool

(* The local clocks' minutes ahead of UTC at [t], seconds since the
 * epoch (Playground_platform.utc_offset); 0 under -fixed-time. *)
val utc_offset : float -> int

(* Tsdl's key names to Playground's ("Left" -> "ArrowLeft", ...), the
 * others lowercased ("Q" -> "q": quitting is Ctrl+Q, [run]'s). *)
val scancode_to_keystring : string -> string

(* The window surface's pixels, (sy, sx)-shaped, one 0xAARRGGBB int32
 * per pixel (SDL's window surface format on the platforms we run on,
 * which is also Cairo's ARGB32). Whatever is written there shows up on
 * [present]. *)
type pixels = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t

(* Sdl.init + a shown window of size [sx] x [sy], filled with white.
 * claude: [resizable] (default false), a window that can change size,
 * the picture scaled to fit it ([run ~on_resize:(Some ...)]): then -size WxH gives
 * its size at the start, and -fullscreen starts it in full screen; the
 * pixels are the window's, whatever its size, at the display's own
 * resolution (on a Retina display, two pixels per point: the pixels
 * twice the window's size), but for -dump-frame. *)
val create_window : ?resizable:bool -> title:string -> sx:int -> sy:int -> unit -> Tsdl.Sdl.window * pixels

(* claude: -size WxH and -fullscreen, for Native_loop_3d, which parses
 * the same command line; and what they asked for, for a platform that
 * makes its window itself (OpenGL's): its size and full screen at the
 * start, [sx] by [sy] if no -size *)
val set_window_size : string -> unit
val set_fullscreen : unit -> unit
val window_start : sx:int -> sy:int -> int * int * bool

(* claude: full screen, or back to a window (Alt+Enter) *)
val toggle_fullscreen : Tsdl.Sdl.window -> unit

(* claude: the window surface's pixels at the window's current size, a
 * new surface after a resize (the old pixels are then not to be used) *)
val window_pixels : Tsdl.Sdl.window -> pixels

(* claude: [scale ~sx ~sy (w, h)]: how much a picture of [sx] by [sy]
 * is enlarged to fit whole in a window of [w] by [h], centred, the
 * rest black bars (a letterbox): min (w / sx) (h / sy) *)
val scale : sx:int -> sy:int -> int * int -> float

(* Copy the window surface's pixels to the screen. *)
val present : Tsdl.Sdl.window -> unit

(* [write_frame ~width ~height rgb file]: the frame whose pixel (x, y) is
 * [rgb x y] (0xRRGGBB), as a PNG if [file] ends in .png (graphics/
 * images/png/Png.mli), else as a binary PPM, the simplest image format
 * there is (a header, then r, g, b bytes for each pixel), which the
 * golden frame tests read. What every native platform's -dump-frame
 * writes. *)
val write_frame : width:int -> height:int -> (int -> int -> int) -> string -> unit

(* [dump_pixels pixels file]: the window's pixels by [write_frame], the
 * usual [dump_frame] for [run] *)
val dump_pixels : pixels -> string -> unit

(*****************************************************************************)
(* {1 The sound card} *)
(*****************************************************************************)
(* 44,100 samples a second, 735 a frame; SDL's queue kept about three
   frames (50 ms) ahead of what the card has played, topped up each
   frame by what it used, so that the two clocks never drift apart
   (audio/Mixer.mli). Exposed for playground3d's loop, which plays the
   same way. *)

val frame_samples : int
val queue_ahead : int

(* the card, opened and started; None, with a warning, if there is none *)
val open_audio : unit -> Tsdl.Sdl.audio_device_id option

(* [latency queued]: the seconds before a sample queued behind [queued]
 * others leaves SDL: those, and the device's own buffer (1024 samples,
 * 23 ms). With the queue kept 3 frames ahead and topped up each frame,
 * [queued] is 2 to 3 frames when a frame's sound goes in: 56 to 73 ms. *)
val latency : int -> float

(* [queue_samples device (left, right)]: queued as 16-bit, clipped,
 * the two channels interleaved *)
val queue_samples : Tsdl.Sdl.audio_device_id -> float array * float array -> unit

(* [run ~sdl_window ~sx ~sy ~init ~update ~subscriptions ~view ~draw
 * ... ~pull_audio ~dump_audio]
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
 * The sound: [pull_audio n] gives the next [n] samples of what's
 * playing (Audio.pull), left and right, at 44,100 a second, which
 * [run] queues for
 * SDL's audio device, kept about 3 frames (50 ms) ahead; with
 * -dump-frame, no device, exactly a frame's worth (735) pulled each
 * frame, and with -dump-audio file, [dump_audio file samples] writes
 * them all at the end (the platform's Wav.write: this library doesn't
 * know audio/). No device (SDL can't open one): silence, and a warning.
 * Each time it queues, [audio_latency] is told the frame's [latency]
 * (Audio.set_latency).
 *
 * The arguments are the fields of a ('model, 'msg) Playground.app
 * ('view = Playground.shape list), passed one by one because this
 * library can't depend on elm_playground: a backend that
 * (implements elm_playground) can't also reach that same virtual
 * library through one of its dependencies -- dune forbids it (same
 * reason Native_loop_3d.mli is generic).
 *
 * With [threads] (the flag threads=on), the
 * commands' blocking calls are made on a pool of threads (Commands.mli).
 *
 * claude: [on_resize] (Some, for a window made [~resizable]): the window can
 * change size -- dragged, Alt+Enter's full screen, -size, -fullscreen --
 * and [on_resize w h] is called, once before the first frame if it is
 * not [sx] by [sy] and after each change, for the platform to take the
 * new [window_pixels] and draw the [sx] by [sy] picture scaled by
 * [scale], centred; mouse positions are mapped back through that scale.
 * Alt+Enter is then the platform's (full screen or not), not the app's.
 * None: the window stays [sx] by [sy].
 *
 * claude: [follow_window] (with [on_resize]): the program's screen is
 * the window itself rather than a picture of [sx] by [sy] scaled into
 * it -- an application's window, a browser's, whose content is laid out
 * again at its new size. The program is told the size before its first
 * frame and after each change (Sub.on_resize), and the mouse is not
 * scaled back; drawing at that size is the platform's. *)
(* [platform_keys]: Ctrl+Q quits, and with -debug-keys Ctrl + a key is
 * a debug key. False, every key is the app's, and -debug-keys and
 * -keys do nothing (Playground.window's platform_keys). *)
(* claude: [skip_same_view]: a frame whose view is physically
 * the one drawn last is not drawn nor presented again, unless an SDL
 * event came since or the window changed size
 * (Playground.window's skip_same_view); -uncapped and
 * -debug-keys draw every frame. *)
val run :
  platform_keys:bool ->
  follow_window:bool ->
  skip_same_view:bool ->
  on_resize:(int -> int -> unit) option ->
  threads:bool ->
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
  pull_audio:(int -> float array * float array) ->
  dump_audio:(string -> float array * float array -> unit) ->
  audio_latency:(float -> unit) ->
  unit
