(* A text as numbers: tokens (notes_ai_learning.md section 11).
 *
 * A model computes with numbers, so the first thing done to a text is
 * to cut it into pieces and number the pieces. The pieces are
 * *tokens*, and the simplest are the characters themselves: collect
 * those the text uses, sort them, count from 1.
 *
 *     the names' 26 letters:    a  b  c  ...  z
 *                               1  2  3  ...  26
 *
 * Token 0 is kept for something no text contains: the *boundary*,
 * where a piece of text starts and where it ends. A model that is to
 * write names has to know how names begin and when to stop, and both
 * are a token like the others:
 *
 *     "emma"    is    0  5  13  13  1  0        ( . e m m a . )
 *
 * so "which letter starts a name" is "what follows 0", and "is the
 * name over" is "is the next token 0". One number does both jobs
 * (makemore writes it "."; a GPT calls it BOS or end-of-text).
 *
 * The *vocabulary* is how many tokens there are: 27 here. It is the
 * size of everything downstream -- the side of [Bigram]'s table, the
 * number of scores a network gives.
 *
 * References: Andrej Karpathy, makemore, 2022 (this numbering);
 * Claude Shannon, "A Mathematical Theory of Communication", 1948,
 * whose examples of English made letter by letter are section 11's. *)

(* the characters, in order: token i + 1 is the i-th *)
type t

(* the characters a text uses, but for the newlines that separate its
 * words *)
val of_text : string -> t

(* how many tokens, the boundary counted *)
val size : t -> int

(* the boundary's token, 0 *)
val boundary : int

(* a word's tokens, without boundaries. A character the tokenizer
 * never saw raises [Not_found]. *)
val encode : t -> string -> int list

(* a word between its two boundaries: 0 ... 0 *)
val bounded : t -> string -> int list

(* back to text; a boundary is written "." *)
val decode : t -> int list -> string
val char : t -> int -> char

(* a text's words, one a line, the empty lines dropped *)
val words : string -> string list
