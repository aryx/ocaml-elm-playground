(* Text with a vector font: Hershey's "Roman simplex", from 1967. Each
 * letter is a few lines drawn with a pen (it was designed for pen
 * plotters and vector displays), so drawing text needs nothing more
 * than drawing lines: Line for thin text, Stroke for thick text.
 *
 * Each glyph is stored as text, one line per glyph, in the "JHF"
 * format: every coordinate is one character, its distance from the
 * letter 'R' ("R" is 0, "S" is 1, "Q" is -1, ...), and " R" lifts the
 * pen. After an 8-character header (a number and the count of pairs),
 * the first pair is the glyph's left and right side, then come the
 * points. For "A":
 *
 *   12345  9I[RFJ[ RRFZ[ RMTWT
 *           ^^ left = I = -9, right = [ = +9: 18 units wide
 *             ^^^^ RF (0,-12) to J[ (-8,9): the left leg
 *                  ^^ pen up
 *                    ^^^^ RF (0,-12) to Z[ (8,9): the right leg
 *                          ^^^^^^^ MT (-5,2) to WT (5,2): the bar
 *
 *               (0,-12)            y goes down: the top of capital
 *                  /\              letters is at -12, the baseline
 *                 /  \             at 9, and y = 0 is about half way
 *         (-5,2) /----\ (5,2)      (the middle of the "em" square,
 *               /      \           see [units_per_em])
 *        (-8,9)          (8,9)
 *
 * The font data, fonts/futural.jhf, and its (free) use conditions:
 * see fonts/README.md.
 *
 * Reference: A. V. Hershey, "Calligraphy for Computers", NWL Report
 * No. 2101, U.S. Naval Weapons Laboratory, Dahlgren, Virginia, 1967. *)

type glyph = {
  (* the glyph's extent: it takes right - left units of space *)
  left : int;
  right : int;
  (* the pen's paths, each a list of points *)
  strokes : (int * int) list list;
}

(* Decode the part of a JHF line after its 8-character header, e.g.
 * "I[RFJ[ RRFZ[ RMTWT" for "A" *)
val decode_glyph : string -> glyph

(* The glyph for a character of the font (ASCII 32 to 127); '?' for
 * the others *)
val glyph : char -> glyph

(* The size of the font's "em" square, in font units: text of font
 * size s is drawn scaled by s / units_per_em. *)
val units_per_em : float

(* [layout str]: the strokes of the whole string, in font units, glyph
 * after glyph starting at x = 0 (y as in the glyphs), and the total
 * width. *)
val layout : string -> (float * float) list list * float
