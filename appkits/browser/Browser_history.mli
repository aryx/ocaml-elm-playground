(* Browser_history: Back and Forward, as two stacks -- the pages behind
 * and the pages ahead (notes_browser.md section 9).
 *
 * Visiting a page pushes the current one behind and **empties what was
 * ahead**; Back moves the current one ahead and takes the last one
 * behind; Forward the other way. Worked example (the tests'):
 *
 *   visit A, B, C     behind [B; A]   current C   ahead []
 *   Back              behind [A]      current B   ahead [C]
 *   visit D           behind [B; A]   current D   ahead []    C is gone
 *
 * That last line surprises everyone once, and every browser does it
 * (MMM calls the lost branch "obsolete"; Mothra keeps no Forward, but
 * a list of its last 64 pages). What an entry is -- a URL, the page
 * kept whole, its scroll -- is the browser's: the stacks don't look. *)

type 'a t = { behind : 'a list; (* the last visited first *) ahead : 'a list }

val empty : 'a t

(* [visit current h]: [current] left for another page *)
val visit : 'a -> 'a t -> 'a t

(* [back current h]: the page to go back to, and the history with
 * [current] ahead; None at the start *)
val back : 'a -> 'a t -> ('a * 'a t) option

(* [forward current h]: the other way *)
val forward : 'a -> 'a t -> ('a * 'a t) option
