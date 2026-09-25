(* TinyVi: vi in a terminal, an editor whose commands are a language.

   Bill Joy wrote vi at Berkeley in 1976, on top of his ex, itself on
   top of ed (Ken Thompson, 1969), the line editor of Unix: ex's
   commands are still there, after a ":". His terminal was a Lear
   Siegler ADM-3A, whose h j k l keys had arrows printed on them and
   whose Escape key sat where Tab is now -- which is why vi moves with
   h j k l and leaves insert mode with Escape.

   Two ideas make vi:

   - **Modes.** A key means a command in normal mode and itself in
     insert mode (i, a, o to enter; Escape to leave). A 300-baud
     terminal made that economical: one key a command, no Control
     held, and nothing redrawn but what changed.

   - **Commands are sentences.** An operator (d delete, c change, y
     yank) followed by a motion (w a word, $ the end of the line, j a
     line down, f, and a character, to that character), each with an
     optional count, the counts multiplying:

         d w        delete a word            2 d 3 w   delete six
         c $        change to the end        d d       delete a line
         y 2 j      yank three lines         d f ,     up to the comma

     so n operators and m motions give n times m commands, learnt as
     n + m -- the grammar is parse_command's. And "." repeats the
     last change, whatever it was, sentence and inserted text included:
     its keys are kept and typed again.

   Undo is vi's, one level: u undoes the last change, and u again
   undoes the undo. (vim, 1991, keeps a tree of them.)

   The lines are an array of strings, as ed kept them, a change making
   a new array: an ed-descended editor thinks in lines (:5 goes to one,
   dd deletes one), where Emacs thinks in characters (Tui_emacs.mli).

   The keys, in normal mode: h j k l (or the arrows), w b e, 0 ^ $, gg
   G, f F t T and a character, x X, dd D, cc C, s S, yy Y, p P, J, r and
   a character, ~, u, ., i a I A o O, / ? n N to search, C-f C-b C-d
   C-u to page, ZZ to save and quit; in insert mode, Backspace, Enter,
   Escape. After a ":", ex's commands: :w [file], :q, :q!, :wq, :x, :e
   file, :N (a line), :$, :s/old/new/ and :%s/old/new/g (with literal
   strings, not ed's regular expressions: an exercise), :d, :set
   number and nonumber. *)

type model

(* the disk it starts with: README (the keys) and poem.txt *)
val disk : (string * string) list

val program : model Tui.program

(* for the tests: the lines, the cursor (from 0), the status line, the
   mode's name ("normal", "insert", "command"), and the disk *)
val lines : model -> string list
val cursor : model -> int * int
val message : model -> string
val mode : model -> string
val file : model -> string -> string option

(* [parse_command keys]: the grammar of normal mode, on the keys typed
   so far: "d" and "2d" are Incomplete, "dw" a count and a sentence,
   "dz" Invalid *)
type parsed = Incomplete | Invalid | Complete of int option * string list

val parse_command : string list -> parsed
