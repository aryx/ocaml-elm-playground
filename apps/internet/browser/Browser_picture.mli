(* Browser_picture: a page's picture, as a browser keeps it -- waiting
 * for its turn, arrived and decoded, or not to be had -- and its bytes
 * decoded by what they say they are.
 *
 * The first bytes of a file say its format better than a server's
 * Content-Type does (a .gif served as text/plain still starts with
 * GIF8): the formats' magic numbers, as TinyMediaPlayer's Media.sniff
 * reads them for every format.
 *
 *   GIF8      GIF (1987)
 *   \x89PNG   PNG (1996)
 *   \xFF\xD8  JPEG (1992)
 *   <svg      SVG (2001), text (after <?xml ...?> perhaps): drawn at its
 *             own size into pixels (graphics/images/svg's Svg)
 *
 * Decoded by our own readers (graphics/images/: Gif, Png, Jpeg, Svg), pure
 * OCaml, so a browser running in a browser decodes them too. *)

type t = Waiting | Arrived of Rgba_image.t | Broken

(* the bytes decoded: Arrived, or Broken for what is none of the three
 * or does not decode *)
val decode : string -> t

(* the size a picture that could not be had takes: the broken image's *)
val broken_size : float

(* its size, for the layout, once known: its pixels', or the broken
 * image's *)
val size : t -> (float * float) option
