(* A text with looks: the characters, and which of them are bold.
 *
 * The characters are gui/Text_edit's piece table -- the structure
 * Bravo introduced (Lampson and Simonyi, Xerox PARC, 1974) and Word
 * inherited through Simonyi -- with its caret and selection. The looks
 * are a second table beside it, of **runs**: stretches of characters
 * that all look the same, each stored once.
 *
 *   "The quick brown fox"
 *    |---|-----------|---|
 *    plain    bold    plain       3 runs, not 19 styles
 *
 * Every edit is then surgery on two tables at once, and it is the
 * same surgery the piece table does -- split at a position, keep what
 * is on either side:
 *
 *   restyle "brown fox" italic
 *    |---|------|-----|----|
 *    plain  bold  bold  italic       "quick " stays bold, "brown"
 *                +ital              becomes both, " fox" is
 *                                   split off the plain run
 *
 * and after every edit, two neighbours that have come to look the
 * same are merged, so the table stays as short as the text's looks
 * really are.
 *
 * Two rules every word processor has, and that are easy to get wrong:
 *
 *   - **what you type looks like what is before it.** Typing inside a
 *     bold word types bold; at the very start of the text, it takes
 *     the look of what comes after; typing over a selection, the look
 *     of its first character.
 *   - **a look set with nothing selected is for what you type next.**
 *     Press bold with the caret in plain text and nothing changes on
 *     the screen, but the next characters come out bold. That pending
 *     look is the "typing style", and moving the caret forgets it.
 *
 * A text with looks is a value, like everything else here, so undo is
 * appkits/document/Undo over it: the old characters and the old runs
 * together, kept -- not the command pattern's "and put the looks
 * back", which is where the bugs of rich text editors have always
 * lived.
 *
 * What it deliberately does not have: paragraph looks (alignment,
 * indents -- a paragraph is only text between newlines here), named
 * styles ("Heading 1", which is a look with a name and an
 * inheritance), and fonts: a look is weight, slant, rules and size,
 * because the stroke font it will be drawn in has one face. *)

type t

(* a text in one look ([Style.plain] if none is said) *)
val of_string : ?style:Style.t -> string -> t

val to_string : t -> string
val length : t -> int

(* the characters, with their caret and selection *)
val edit : t -> Text_edit.t

(* (start, length, look), in order, covering the text exactly, no two
 * neighbours alike -- the worked example above as a list *)
val runs : t -> (int * int * Style.t) list

(* the look of the character starting at byte [i] *)
val style_at : t -> int -> Style.t

(*****************************************************************************)
(* {1 The caret and the selection} *)
(*****************************************************************************)
(* As Text_edit's -- and moving the caret forgets a pending look. *)

val caret : t -> int
val range : t -> int * int
val at : int -> t -> t
val select : anchor:int -> caret:int -> t -> t
val to_ : int -> t -> t

(*****************************************************************************)
(* {1 Editing} *)
(*****************************************************************************)

(* [insert s t]: [s] at the caret, replacing the selection, in the
 * typing style *)
val insert : string -> t -> t

val delete_backward : t -> t
val delete_forward : t -> t

(*****************************************************************************)
(* {1 Looks} *)
(*****************************************************************************)

(* [restyle f t]: [f] applied to the look of every character of the
 * selection -- or, with nothing selected, to the typing style, so
 * that it is what the next characters typed will look like *)
val restyle : (Style.t -> Style.t) -> t -> t

(* what the next character typed will look like *)
val typing_style : t -> Style.t
