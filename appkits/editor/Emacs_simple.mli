(* Emacs_simple: most of TinyEmacs, written in its own Lisp.

   In GNU Emacs, C gives the primitives (Emacs_editor.mli) and Lisp
   the editor: simple.el has kill-line, yank, next-line, undo's
   commands; subr.el, lisp.el and bindings.el the rest -- the key
   bindings included, a table a user changes from .emacs as the
   authors did. So here: the global keymap is a Lisp variable, an
   association list from key sequences (the bytes the terminal sends,
   written "\C-x\C-f") to commands, and global-set-key a three-line
   function.

   What makes a function a command is its (interactive ...) form: the
   command loop (Tui_emacs.mli) reads it to know what to ask for --
   "p" the prefix argument, "r" the region's two ends, "s" a string
   read in the minibuffer -- then calls it. The same function called
   from Lisp takes its arguments like any other.

   Worth reading, as examples of the Lisp: next-line (its goal column
   remembered across C-n's, through last-command), kill-region (C-k's
   appended in the ring when they follow each other, through
   last-command again), eval-buffer (reading forms until end-of-file,
   caught by condition-case), and describe-key (the keymap looked up
   by Lisp itself). *)

(* the Lisp, loaded by Emacs_editor.create *)
val text : string
