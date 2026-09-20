(* A string as the person sees it: characters, and the cells they sit
 * in -- which are two different things from the bytes it is made of.
 *
 * A character is not a byte. The playground hands a field whatever
 * the keyboard layout produced (Playground.keyboard's [typed]), and
 * an accented letter is two bytes of UTF-8:
 *
 *   "caf\xc3\xa9"   4 characters, 5 bytes
 *                        ^^^^^^^^ one character, in two bytes: the
 *                                 second has 10 as its top bits, which
 *                                 is what marks a continuation
 *
 * so backspace, which deletes one *character*, must step back over
 * the whole sequence, or leave half a letter behind.
 *
 * A cell is the second idea: a field here lays its text out one
 * character to a fixed width, like a terminal, rather than at the
 * widths the stroke font really draws. That is what lets a click say
 * exactly where the caret goes, and a caret say exactly where it is,
 * with no font to ask (Widget.text_width is an average, not a
 * measurement). *)

(* [prev_char s i], [next_char s i]: the byte index a character before
 * or after [i], never inside one, never outside the string *)
val prev_char : string -> int -> int
val next_char : string -> int -> int

(* [chars s]: the characters of [s], each one still a string (a cell
 * holds a character, not a byte). ["caf\xc3\xa9"] gives four. *)
val chars : string -> string list

(* [column s i]: how many characters come before the byte index [i] --
 * which cell the caret is in *)
val column : string -> int -> int

(* [byte_of_column s col]: the other way round, for a click *)
val byte_of_column : string -> int -> int

(* [edit ~typed ~pressed text caret]: the text and the caret after one
 * frame of typing -- the characters the platform says were typed
 * inserted at the caret, and the keys that produce no character at
 * all (Backspace, Delete, the arrows, Home, End) doing what they do.
 *
 * It is here rather than in a widget because every architecture needs
 * it and none of them should differ in it: what a field does with a
 * key press is not an architectural question (notes_gui.md section
 * 4). [pressed] answers "did this key go down at this frame", which
 * *is* one -- each architecture knows where it keeps the frame
 * before. *)
val edit : typed:string -> pressed:(string -> bool) -> string -> int -> string * int
