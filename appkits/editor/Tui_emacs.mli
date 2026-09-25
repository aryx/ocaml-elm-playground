(* TinyEmacs: Emacs in a terminal, a Lisp machine with a screen.

   Emacs began as macros for TECO (Richard Stallman, Guy Steele and
   others, MIT, 1976: Editor MACroS), and became a Lisp program with
   Multics Emacs (Bernard Greenberg, 1978) and GNU Emacs (Stallman,
   1985). Its idea: an editor whose every command is a function of a
   programming language the user has too, so that extending it is
   writing more of it. Here, the language is libs/languages/lisp; the
   core that Lisp drives is Emacs_editor.mli; most commands and all key
   bindings are Emacs_simple.mli's Lisp. This module is what is left:
   the command loop (keyboard.c) and the display (xdisp.c).

   The command loop, for each key:

       key --> in the minibuffer? its text  (or C-g, RET, TAB)
           --> in isearch? the search string
           --> else added to the keys typed: "\C-x" then "\C-f"
                 |
                 +-- a binding of global-map, exactly: its command
                 +-- the start of one (C-x): wait for the next key
                 +-- a character: self-insert-command
                 +-- neither: "C-x C-z is undefined"

       command --> its (interactive "SPEC") read: "p" the prefix
                   argument, "r" the region, "sFind: " a string asked
                   in the minibuffer -- the command waiting meanwhile,
                   as a continuation: the arguments read so far and
                   the codes left
               --> called, last-command and this-command set around
                   it, a Boundary on the undo list before it (but not
                   between two characters typed: one undo takes back a
                   run of typing), its error shown in the echo area

   The screen is the VT100's 24 lines: 22 of the buffer, the mode line
   in reverse video (the buffer's name, ** once modified, the line,
   the mode), and the echo area, which is also the minibuffer.

   The keys: those of Emacs, C-f C-b C-n C-p, M-f, C-a C-e, C-v M-v,
   M-< M->, C-d DEL, C-k C-y M-y, C-SPC C-w M-w, C-/ (undo), C-s C-r,
   C-x C-f, C-x C-s, C-x b, C-x C-b, M-x (TAB completes), C-x C-e and,
   in *scratch*, C-j to evaluate Lisp, M-: to type some, C-h k and C-h f
   (Emacs documents itself: every command's first string), C-u (4
   times), C-g to cancel, C-x C-c to quit. Meta is Alt, or Escape then
   the key, as on a terminal without a Meta key. *)

type model

(* the disk it starts with: .emacs (loaded at start, as yours is),
   TUTORIAL and notes.txt *)
val disk : (string * string) list

val program : model Tui.program

(* for the tests: the current buffer's name, text and point (from 1),
   the echo area, and what the disk holds *)
val buffer_name : model -> string
val text : model -> string
val point : model -> int
val message : model -> string
val file : model -> string -> string option
