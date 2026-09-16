val run_app:
  ('a, 'b) Playground.app -> unit

(* Load (and cache) an image url ahead of time, e.g. for all the sprite
 * variants a game will need, so that [Playground.image]/[run_app] never
 * has to load one lazily mid-game. On the web backend this is a no-op
 * (the browser already loads/caches <img>/<svg:image> asynchronously on
 * its own); on the native backend this blocks until the image is
 * downloaded and decoded, which is fine to do once up front but would
 * freeze the render loop if done lazily on first use. *)
val preload_image: string -> unit
