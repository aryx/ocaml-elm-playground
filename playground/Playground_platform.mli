(* claude: [rendering] (default: Playground.default_rendering) sets how
 * to draw, see Playground.rendering *)
val run_app:
  ?rendering:Playground.rendering -> ('a, 'b) Playground.app -> unit

(* Load (and cache) an image url ahead of time, e.g. for all the sprite
 * variants a game will need, so that [Playground.image]/[run_app] never
 * has to load one lazily mid-game. On the web backend this starts an
 * asynchronous download and keeps the image in the browser's memory
 * cache, so switching a sprite to it later does not flicker; on the
 * native backend this blocks until the image is
 * downloaded and decoded, which is fine to do once up front but would
 * freeze the render loop if done lazily on first use. *)
val preload_image: string -> unit
