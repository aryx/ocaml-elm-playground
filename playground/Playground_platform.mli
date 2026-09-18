(* claude: [rendering] (default: Playground.default_rendering) sets how
 * to draw, see Playground.rendering; [flags] (default: none) are given
 * to the app's init, and so end up in computer.flags, see
 * Playground.flags *)
val run_app:
  ?rendering:Playground.rendering -> ?flags:Playground.flags -> ('a, 'b) Playground.app -> unit

(* The parameters the program was started with (see Playground.flags):
 * natively, the command line's arguments without a dash, name=value or
 * name; on the web, the page's URL parameters, ?name=value&name. The
 * one impure step of flags, visible in a program's main:
 *   let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
 *)
val flags: unit -> Playground.flags

(* Load (and cache) an image url ahead of time, e.g. for all the sprite
 * variants a game will need, so that [Playground.image]/[run_app] never
 * has to load one lazily mid-game. On the web backend this starts an
 * asynchronous download and keeps the image in the browser's memory
 * cache, so switching a sprite to it later does not flicker; on the
 * native backend this blocks until the image is
 * downloaded and decoded, which is fine to do once up front but would
 * freeze the render loop if done lazily on first use. *)
val preload_image: string -> unit
