(* The SDL window/event loop shared by the SDL-based 2D playground
 * backends (playground/native/, and the planned playground/software/ --
 * see docs/claude_notes/plan_software_2d.md). Each backend only supplies
 * how a Playground.shape list becomes pixels in the window's pixel
 * array. *)

(* [Ok x -> f x], [Error (`Msg msg) -> failwith msg], for Tsdl calls *)
val ( let* ) : ('a, [ `Msg of string ]) result -> ('a -> 'b) -> 'b

(* -v/-verbose/-debug/-quiet, and installs a Logs reporter *)
val parse_cli_and_setup_logging : unit -> unit

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

(* [run ~sdl_window ~sx ~sy ~init ~update ~subscriptions ~view ~draw]
 * runs an app forever (like Playground_platform.run_app, it never
 * returns: "Q" or the window's close button call [exit]): each frame,
 * drains the SDL events into the app's msgs (via [subscriptions]) plus
 * a Tick, calls [draw ~fps v] with [v] the app's [view] (the backend
 * must have put the pixels in the window surface when it returns),
 * [present]s, and paces to 60fps. Mouse positions are already in Elm's
 * coordinates (origin at the window's center, y up).
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
  unit
