(* Load a texture (a src passed to [Playground3d.textured_quad]/
 * [textured_cube]) into a decoded pixel buffer, caching the result (or
 * the failure) by src so repeated calls don't re-download/re-decode
 * every frame. Accepts either a local file path or an http(s) URL (the
 * latter downloaded via curl, like Image_decode.image_of_url does for
 * 2D's [Playground.image]). Returns [None] (after printing a warning)
 * rather than raising, so a broken/unreachable texture doesn't crash
 * the whole app. This blocks on a cache miss -- callers that can't
 * afford to block (e.g. the render loop, mid-game) should ensure the
 * src was already warmed via [preload]. *)
val load : string -> Stb_image.int8 Stb_image.t option

(* Queue a texture src to be loaded ahead of time (so a later [load]
 * call for it returns immediately); doesn't touch the network itself,
 * so it's safe to call anytime. [load_queued] actually
 * downloads/decodes/caches every queued src, synchronously. *)
val preload : string -> unit
val load_queued : unit -> unit
