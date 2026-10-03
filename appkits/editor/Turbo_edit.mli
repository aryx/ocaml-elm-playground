(* Turbo_edit: the IDE's editor, the text and the window on it.

   The text is an array of lines, never changed in place: a key gives a
   new model, and a text that is another array is a text that changed
   (which is how the IDE knows to reset the program being debugged).

   The keys are Turbo's, which were WordStar's: the arrows or Ctrl-E,
   Ctrl-X, Ctrl-S, Ctrl-D; Ctrl-A and Ctrl-F a word; Home, End, PgUp,
   PgDn; Ctrl-Y deletes a line; Insert toggles overwriting; Enter keeps
   the line's indentation (autoindent). The cursor may sit past a
   line's end, which is padded when something is typed there. *)

open Turbo_model

(* The text *)

val nlines : model -> int

(* a line, from 0 *)
val line : model -> int -> string

(* the whole text, a newline after each line: what the compiler reads
   and what is saved *)
val text : model -> string

(* a letter, a digit or an underscore: a word's characters, for Ctrl-A
   and Ctrl-F, the word to watch, the reserved words' colour *)
val is_word : char -> bool

(* a new file's name, until saved as another *)
val noname : string

(* [load m file]: the file of the model's disk open in the editor, the
   cursor at its top; what belonged to the text before (its code, the
   error, the program started, the breakpoints) forgotten. An empty
   text if the disk has no such file *)
val load : model -> string -> model

(* The window *)

(* the columns of text in the window *)
val text_cols : int

(* its lines: 20, less the Watches window's when there are watches *)
val text_rows : model -> int

(* the Watches window's height: none without a watch, else a line each
   and its frame, 8 at most *)
val watch_rows : model -> int

(* the cursor kept in the text, and the window moved to follow it *)
val follow : model -> model

(* The keys *)

(* a key in the editor: the cursor moved or the text changed *)
val edit_key : model -> string -> model

(* [find m pattern]: the cursor on the next place the text has it,
   whatever its case, from after the cursor to the end of the text; a
   box saying so if there is none. The pattern kept, for Search again *)
val find : model -> string -> model
