(* A text with looks, laid out: where every character sits on the page,
 * and -- the half that makes it WYSIWYG -- the way back, from a point
 * on the page to a place in the text.
 *
 *   caret_at     offset 12  ->  (x, baseline, height)   to draw the caret
 *   offset_at    a click    ->  offset 12               to put it there
 *
 * Both are needed because what is edited is not what is shown: the
 * text is a sequence of characters, the page is lines of glyphs of
 * different sizes, broken where the words stopped fitting. Bravo
 * (Xerox PARC, 1974) was the first editor where the two were the same
 * picture, and keeping them the same picture after every keystroke --
 * relaying out, putting the caret back where the person expects it --
 * is the part of a word processor that is harder than it looks.
 *
 * The page's coordinates are the typesetter's: x from the left edge of
 * the text to the right, y from its top *downwards*, since a page is
 * read top to bottom. (The playground's y is up; the caller turns the
 * page over when it draws it.)
 *
 * Lines are broken greedily, word by word -- what Bravo did and what
 * Word does -- and a word too long for the line is broken where it
 * reaches the edge. A newline ends its line and is itself a glyph of
 * no width, so that the caret can sit just before it, at the end of
 * the line it ends.
 *
 * It never sees a font. The width of a character in a look is the
 * caller's [metrics], which is how this stays a library the web
 * backend can use: the applications give Hershey's real widths, the
 * tests give ten units per character, which a person can check.
 *
 * Worked example, "ab cd ef" with every character 10 wide, a line 50
 * wide, plain looks of size 16 (a line 16 * 1.4 = 22.4 high, its
 * baseline 16 below its top):
 *
 *   line 1, top 0:     a(0) b(10) space(20) c(30) d(40)     -- "ab cd "
 *   line 2, top 22.4:  e(0) f(10)                           -- "ef"
 *
 * the space after "cd" does not fit and is not drawn (a space at the
 * end of a line is not part of the picture), and the caret at offset
 * 6, between the space and "e", is at the start of line 2. *)

(* how wide a character is, in a look: the caller's font *)
type metrics = Style.t -> string -> float

(* Where a line's slack goes -- the room between its last letter and
 * the edge:
 *
 *   Left     all of it on the right: ragged right, a typewriter's
 *   Center   half on each side
 *   Right    all of it on the left
 *   Justify  shared between the line's spaces, so that both edges are
 *            straight -- except on the last line of a paragraph,
 *            which is set Left, since stretching three words across
 *            the page is worse than a short line
 *
 * Justified here is justification of greedy lines: the breaks are
 * made as Left makes them, and only then stretched. Choosing the
 * breaks *for* justification is Knuth and Plass's idea (appkits/
 * typeset, and examples/TypesetParagraph), which Word never used. *)
type align = Left | Center | Right | Justify

type glyph = {
  offset : int; (* where in the text *)
  text : string; (* the character; "\n" for the end of a line *)
  style : Style.t;
  x : float; (* its left edge *)
  baseline : float; (* its line's baseline, from the top of the page *)
  advance : float; (* how far the next one starts *)
}

(* a line: where it is (from the top of the page), how tall, its
 * baseline, its glyphs, and the offsets it covers, [first, stop) --
 * what Flow cuts a text into columns by *)
type line = { top : float; height : float; baseline : float; cells : glyph list; first : int; stop : int }

type t

(* [layout ?align ~metrics ~width text]: [text] set in lines of at
 * most [width], aligned [Left] unless said otherwise. For example,
 * "ab cd" in a line 60 wide (every character 10) has 10 of slack:
 * Center starts it at x = 5, Right at 10, and Justify widens its one
 * space from 10 to 20 -- except that a one-line paragraph is its own
 * last line, and so stays Left. *)
val layout : ?align:align -> metrics:metrics -> width:float -> Rich.t -> t

(* every glyph, in order *)
val glyphs : t -> glyph list

(* the lines, top to bottom *)
val lines : t -> line list

(* how tall the whole page is *)
val height : t -> float

(* [caret_at page offset]: where the caret before the character at
 * [offset] goes -- its x, its line's baseline, and its line's height *)
val caret_at : t -> int -> float * float * float

(* [offset_at page (x, y)]: the place in the text nearest to a point on
 * the page, for a click *)
val offset_at : t -> float * float -> int
