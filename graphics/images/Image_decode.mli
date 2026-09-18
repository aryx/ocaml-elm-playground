(* claude: Cairo-free image loading shared by the SDL-based 2D backends.
 *
 * An image is stb_image's decoded buffer: width x height pixels,
 * row-major, top-to-bottom, 4 bytes per pixel in R, G, B, A order,
 * straight (non-premultiplied) alpha. *)
type image = Stb_image.int8 Stb_image.t

(* Load an external image (e.g., a URL passed to [Playground.image]),
 * caching the result (or the failure) by url so repeated calls don't
 * re-download/re-decode every frame. Returns [None] (after logging a
 * warning) rather than raising, so a broken/unreachable image doesn't
 * crash the whole app. This blocks on a cache miss -- callers that
 * can't afford to block (e.g. the render loop, mid-game) should ensure
 * the url was already warmed via [preload]. *)
val image_of_url : string -> image option

(* The frames of an animated GIF, each with its display duration in
 * seconds; [duration] is the sum of those. *)
type 'a animation = { frames : ('a * float) array; duration : float }

(* [None] if [src] is not an animated GIF (or could not be loaded or
 * parsed, in which case [image_of_url]'s first frame is all there is). *)
val animation_of_url : string -> image animation option

(* Convert every frame once, e.g. to a backend-specific surface. *)
val map_animation : ('a -> 'b) -> 'a animation -> 'b animation

(* The frame to display at [time] (in seconds, e.g.,
 * Unix.gettimeofday ()); the animation loops forever, like in
 * browsers. *)
val frame_at : time:float -> 'a animation -> 'a

(* Same as [image_of_url], but for an animated GIF (for which
 * [image_of_url] returns only the first frame), returns the frame to
 * display at [time]. *)
val image_of_url_at : time:float -> string -> image option

(* Queue an image url to be loaded ahead of time (so a later
 * [image_of_url] call for it returns immediately); doesn't touch the
 * network itself, so it's safe to call anytime. [load_queued] actually
 * downloads/decodes/caches every queued url, synchronously, and returns
 * them, so a backend can also convert them ahead of time. *)
val preload : string -> unit
val load_queued : unit -> string list
