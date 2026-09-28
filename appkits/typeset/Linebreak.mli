(* Where to break a paragraph into lines -- the one real algorithm in
 * a word processor (notes_gui.md section 7).
 *
 * The obvious way is **greedy**: put words on the line until the next
 * one does not fit, break, and carry on. Every browser does it, most
 * editors do it, and it is fast and never looks back -- which is its
 * flaw. A greedy break is the best for *this* line and blind to what
 * it does to the next, so a paragraph set greedily and then justified
 * gets lines whose spaces are stretched far apart next to lines that
 * are tight: the "rivers" of white a typesetter's eye goes straight
 * to.
 *
 * Donald Knuth and Michael Plass ("Breaking Paragraphs into Lines",
 * Software -- Practice and Experience, 1981) score the *whole
 * paragraph* instead, and pick the set of breaks with the best score
 * by dynamic programming. It is why a paragraph of TeX looks the way
 * it does, and it is these pages' best example of an algorithm that
 * is not about speed at all but about taste, made precise.
 *
 * The model is theirs, simplified:
 *
 *   box    a word, with a width that does not change
 *   glue   a space between words: a natural width, and how far it may
 *          stretch and shrink
 *
 * and a line is scored by how much its glue had to give:
 *
 *   ratio r    = (measure - natural) / (spaces * stretch)    too short
 *              = (measure - natural) / (spaces * shrink)     too long
 *   badness    = 100 |r|^3            (0 for a perfect line; a line
 *                                      that shrinks past r = -1 cannot
 *                                      be set at all)
 *   demerits   = (10 + badness)^2     (the 10 is TeX's \linepenalty:
 *                                      a line costs something even when
 *                                      it is perfect, so fewer lines
 *                                      win a tie)
 *
 * The paragraph's score is the sum of its lines' demerits, and the
 * last line is free: its glue stretches as far as it likes (TeX's
 * \parfillskip), because a short last line is how a paragraph ends.
 *
 * Squaring and cubing is the whole of the taste: one line stretched
 * twice as far costs eight times the badness, so two moderately loose
 * lines beat one tight line and one very loose one -- which is the
 * even colour a reader sees and cannot name.
 *
 * Worked example, "aaa bb cc ddddd ee ff gggg", every letter one
 * unit wide, a measure of 10, a space of 1 that may stretch by 1 and
 * shrink by 0.5:
 *
 *   greedy                      ratio  badness   demerits
 *     aaa bb cc                  0.5     12.5       506.25
 *     ddddd ee                   2.0    800.0   656100.00   <- one space
 *     ff gggg                    0.0      0.0       100.00      stretched
 *                                               ---------      to 3
 *                                               656706.25
 *   optimal
 *     aaa bb cc                  0.5     12.5       506.25
 *     ddddd ee ff               -1.0    100.0     12100.00   <- two spaces
 *     gggg                       0.0      0.0       100.00      shrunk
 *                                               ---------      to 0.5
 *                                                12706.25
 *
 * Greedy put "ff" on the last line because it fit there, and left
 * "ddddd ee" alone with 2 units to fill and one space to fill them
 * with. The optimal breaker saw that taking "ff" up a line and
 * shrinking its two spaces costs far less than that one river --
 * fifty times less, over the paragraph. Line 2 by hand: 5 + 2 + 2
 * letters and 2 spaces is 11, one too many; 2 spaces times 0.5 of
 * shrink is 1, so the ratio is -1, exactly the most a line may
 * shrink.
 *
 * The algorithm is a shortest path. For each place a line could end,
 * the best score of a paragraph ending there is the best, over every
 * place the line could have started, of the best score up to that
 * start plus this one line's demerits:
 *
 *   best(j) = min over i < j of  best(i) + demerits(words i .. j-1)
 *
 * which is O(n^2) as written here. TeX keeps only the starts that can
 * still reach -- its "active nodes" -- and is close to linear; the
 * answer is the same.
 *
 * What it deliberately does not do: hyphenation (TeX's other half,
 * Liang's 1983 patterns, which gives the breaker more places to
 * choose from and is why TeX's lines rarely need to stretch far),
 * penalties for two hyphens in a row or for a tight line next to a
 * loose one (the "fitness classes"), and widths that are anything but
 * the caller's numbers -- this module never sees a font. *)

(* a word, and how wide it is -- in whatever unit the caller measures *)
type word = { text : string; width : float }

type params = {
  (* how wide a line is *)
  measure : float;
  (* a space: its natural width, and how far it may stretch and shrink *)
  space : float;
  stretch : float;
  shrink : float;
}

(* a line: the words [first] to [last] (inclusive), how far its spaces
 * gave (the ratio above; 0 on the last line, which is set at its
 * natural width), and what it cost *)
type line = { first : int; last : int; ratio : float; demerits : float }

(* the two ways, the simple one first *)
val greedy : params -> word array -> line list
val optimal : params -> word array -> line list

(* the paragraph's score: the sum of its lines' demerits. [optimal]'s
 * is never more than [greedy]'s, since greedy's breaks are one of the
 * sets the optimal one chose among -- the property the tests check on
 * paragraphs nobody wrote by hand. *)
val total : line list -> float

(* how far apart the words of a line are set, when it is justified:
 * the natural space, stretched or shrunk by the line's ratio *)
val spacing : params -> line -> float

(* the line's badness: 100 |ratio|^3, so that a view can show which
 * lines are the loose ones *)
val badness : line -> float
