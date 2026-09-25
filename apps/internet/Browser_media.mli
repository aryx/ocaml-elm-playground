(* Browser_media: TinyChrome's players -- what plays in a page's <video>
 * and <audio> (plan_tiny_chrome.md C9).
 *
 * The page's side is the engine's: Box_layout makes each a box of its
 * width= and height=, Browser_boxes draws it black, the tab fetches its
 * file (Browser_tab.media_sources). What plays in it is here, over
 * TinyMediaPlayer's library (media_player's Media): the bytes recognized
 * by their magic numbers and opened -- an .mpg's MPEG-1 and MP2, an
 * AVI's Motion JPEG and PCM, FLC, Y4M, an animated GIF, and MP3, MP2 or
 * WAV for an <audio> -- and a **player** per file: paused on its first
 * frame (a thumbnail, what a video site's index shows), playing from a
 * click on it or from autoplay, looped with loop.
 *
 * **The clock**: with a sound, the sound's -- one deck, like
 * TinyMediaPlayer's, whose samples the mixer pulls (Audio.instrument),
 * the picture the frame at the samples played; a movie without sound
 * follows the browser's frame clock. A player drawn with controls= (an
 * <audio>'s always) has a bar: play or pause, how far along, the time.
 *
 * Not done: seeking by the bar, the volume, muted, the elements' methods
 * for scripts (play(), currentTime), full screen, subtitles. *)

(* the players' frames and controls over their boxes, in the page's
 * coordinates (Browser_draw.drawn); [media url] a file's bytes if the
 * tab has them, [now] the browser's clock (seconds); a player with
 * autoplay starts here the first time its file is had *)
val draw : now:float -> media:(string -> string option) -> Browser_page.t -> Browser_draw.drawn

(* a click on a player (its element): played, or paused; whether it was
 * one (else the page's link around it is followed) *)
val click : now:float -> media:(string -> string option) -> Browser_page.t -> Dom.element -> bool

(* the deck's instrument, asked for at each frame (the mixer keeps it) *)
val install : unit -> unit
