(* Load an external image (e.g., a URL passed to [Playground.image]) into a
 * Cairo surface, caching the result (or the failure) by url so repeated
 * calls don't re-download/re-decode every frame. Returns [None] (after
 * logging a warning) rather than raising, so a broken/unreachable image
 * doesn't crash the whole app. This blocks on a cache miss -- callers
 * that can't afford to block (e.g. the render loop, mid-game) should
 * ensure the url was already warmed via [preload]. *)
val surface_of_url : string -> Cairo.Surface.t option

(* claude: same as [surface_of_url], but for an animated GIF (for which
 * [surface_of_url] returns only the first frame), returns the frame to
 * display at [time] (in seconds, e.g., Unix.gettimeofday ()); the
 * animation loops forever, like in browsers. *)
val surface_of_url_at : time:float -> string -> Cairo.Surface.t option

(* Queue an image url to be loaded ahead of time (so a later
 * [surface_of_url] call for it returns immediately); doesn't touch the
 * network itself, so it's safe to call anytime. [load_queued] actually
 * downloads/decodes/caches every queued url, synchronously. *)
val preload : string -> unit
val load_queued : unit -> unit
