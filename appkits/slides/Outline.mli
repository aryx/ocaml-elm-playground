(* A presentation written as an outline: the way PowerPoint (Robert
 * Gaskins and Dennis Austin, Forethought, 1987) let a talk be typed
 * before it was drawn -- the ideas first, in order, as plain indented
 * text, and the slides made from them.
 *
 * A line against the left edge is a slide's title; an indented line is
 * a point on that slide, one level deeper for every two spaces (a tab
 * counts as two); blank lines are nothing:
 *
 *   Why paint programs          slide 1: "Why paint programs"
 *     Pictures are dots           a point, level 1
 *       and dots are bits         a point, level 2
 *   The bucket                  slide 2: "The bucket"
 *     A mask, then the pattern    a point, level 1
 *
 * So the outline is the presentation, and the slides are a view of it,
 * computed again whenever it changes: nothing about a slide's text is
 * kept anywhere else. Points before the first title go on a first
 * slide with no title, rather than being lost. *)

type slide = {
  title : string;
  (* each point with its level, 1 for the first indentation *)
  points : (int * string) list;
}

val parse : string -> slide list

(* [slide_at text offset]: the number (from 0) of the slide the
 * character at [offset] belongs to -- for showing, beside the outline,
 * the slide being typed *)
val slide_at : string -> int -> int

(* [start_of text n]: where slide [n]'s title line starts, the length
 * of [text] if there is no slide [n] -- where a new slide put after
 * slide [n - 1] goes *)
val start_of : string -> int -> int

(* the outline of some slides: two spaces per level, so that [parse]
 * gives them back *)
val to_text : slide list -> string
