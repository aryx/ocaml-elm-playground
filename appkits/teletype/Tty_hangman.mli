(* Hangman, as the teletype played it: guess the word a letter at a
   time, before the man is hanged.

   The game is older than computers (a Victorian pencil-and-paper game);
   David Ahl's "101 BASIC Computer Games" (1973) has it as a listing of
   a hundred lines, a gallows of ten parts drawn in a 12 by 12 array of
   characters. This one keeps six parts and the words of a child's
   spelling list.

   The lesson is in its [turn]: it reads like the BASIC listing -- print
   the gallows, ask, check, go round again -- with no model, no update
   and no view. Every [let*] is a place where the program may wait (for
   a line, for a random number), and Talk.mli explains how it waits
   without stopping: what comes after [ask] is a function of the
   answer.

   Run by examples/TeletypeHangman.ml alone, and by TinyTerminal's
   shell as its command hangman. *)

val program : unit Talk.talk

(* the words, one picked at random *)
val words : string array
