(* Text drawn from Hershey's own strokes, in a look -- how the two word
 * processors here get bold, italic, underline and strike out of a
 * playground whose [words] knows only a colour and a string.
 *
 * A Hershey glyph (graphics/font, 1967) is a few pen strokes, so
 * drawing one ourselves is drawing its strokes -- each a thin
 * rectangle -- and a look is then a change to the *pen*, not a second
 * font:
 *
 *   bold       a thicker pen (Hershey's own duplex and triplex faces
 *              are the same letters drawn with more strokes: the
 *              plotter's way to be bold)
 *   italic     the strokes' points sheared right as they go up, a
 *              fifth of their height -- a slant, which is what an
 *              oblique face is (a true italic is drawn differently)
 *   underline  a rule below the baseline, and strike one through the
 *              middle, as a typewriter's backspace-and-overstrike did
 *
 * and because the same glyph data gives the widths ([metrics]) and the
 * strokes, what is laid out is exactly what is drawn: the caret goes
 * between two letters where they really meet, which is what the rest
 * of the toolkit, measuring with an average width, could not promise.
 *
 * Shared by TinyBravo and TinyWord. *)

(* the width of a character in a look: Hershey's, scaled to its size *)
val metrics : Page.metrics

(* [glyph color look ch ~x ~baseline]: [ch] drawn in [look], its left
 * edge at [x] and its baseline at [baseline], in the playground's
 * coordinates (y up) *)
val glyph :
  Playground.color -> Style.t -> string -> x:float -> baseline:float -> Playground.shape list
