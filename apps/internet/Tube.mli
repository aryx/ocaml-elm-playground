(* Tube: TinyTube, a video site of our own in TinyChrome's built-in site
 * -- what a video site asks of a browser, without the real one
 * (plan_tiny_chrome.md, "Video and sound": YouTube's streams are H.264
 * and AV1, its pages megabytes of script, its terms against us).
 *
 *   about:tube         the index: a thumbnail per clip (a <video> paused
 *                      on its first frame) in a grid of flexbox
 *   about:tube-N       a clip's page: its player (<video controls
 *                      autoplay>), its title and description, the next
 *                      ones beside it
 *   about:clip/NAME    the files, TinyMediaPlayer's own (Our_media): one
 *                      clip -- a ball and a square, filmed by graphics/2d
 *                      -- in the containers and codecs this repository
 *                      reads (MPEG-1 with its MP2 in an .mpg, Motion
 *                      JPEG with PCM in an AVI, FLC, raw Y4M, an animated
 *                      GIF), and a tune as MP3 for an <audio>
 *
 * so the golden frames fetch nothing and change for no one else. *)

(* about:NAME's bytes and type, for the names above *)
val about : string -> (string * string) option
