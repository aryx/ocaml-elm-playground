(* Load an external image (e.g., a URL passed to [Playground.image]) into a
 * Cairo surface, caching the result (or the failure) by url so repeated
 * calls don't re-download/re-decode every frame. Returns [None] (after
 * logging a warning) rather than raising, so a broken/unreachable image
 * doesn't crash the whole app. *)
val surface_of_url : string -> Cairo.Surface.t option
