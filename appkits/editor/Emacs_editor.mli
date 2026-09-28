(* Emacs_editor: the editor Emacs Lisp runs in -- the buffers, and the
   primitives that change them.

   GNU Emacs (Richard Stallman, 1985) is two programs: a core in C that
   knows buffers, the screen and the keyboard, and everything else in
   Lisp, down to what most keys do. The core's functions are the
   *primitives* (point, insert, delete-region, search-forward...), and a
   command is a Lisp function over them. This module is the core
   (buffer.c, insdel.c, editfns.c, undo.c, fileio.c), the primitives
   given to Lisp_eval as its host's functions; Emacs_simple.mli is the
   Lisp, and Tui_emacs.mli the command loop and the display.

       key --keymap--> command (Lisp) --calls--> primitives --> buffer
                                                               |
                                 redisplay <-------------------+

   What a buffer has:
   - its text, a Gap_buffer, and **point**, where the cursor is, and
     the **mark**, the other end of the region (C-SPC sets it, C-w kills
     from it to point);
   - its **undo list**: each change recorded as what undoes it, not as
     a copy of the text -- (Inserted (beg, end)) is undone by deleting
     beg..end, (Deleted (pos, text)) by inserting text at pos -- and a
     Boundary between commands. Undo walks back that list, and its own
     changes are recorded like any others, so after anything else, undo
     undoes the undoing: redo without a redo command, Emacs's model
     (undo.c and simple.el's primitive-undo);
   - the file it visits, and whether it was modified since.

   And the editor has the buffers, the **kill ring** (what C-k and C-w
   took, the newest first; C-y yanks it, M-y the one before), the echo
   area's message, and a disk: files as a list of names and texts, in
   memory, so that the same program runs in a browser -- a real
   filesystem through a Cap is an exercise.

   Positions in Lisp count from 1, as in Emacs: point-min is 1, the
   text's first character between 1 and 2. *)

type undo = Inserted of int * int | Deleted of int * string | Boundary

type buffer = {
  name : string;
  text : Gap_buffer.t;
  point : int; (* from 0 *)
  mark : int option;
  file : string option;
  modified : bool;
  undo : undo list; (* the newest first *)
  undoing : undo list option; (* while undos follow each other, where the next goes on from *)
  top : int; (* the first line the window shows *)
  mode : string; (* the major mode's name, for the mode line *)
}

(* what a command asks the command loop to do after it: the commands
   that read keys themselves, which only the loop can give them *)
type request =
  | Call_interactively of Lisp.t (* a command run as from a key: M-x's *)
  | Isearch of bool (* incremental search, forward or not: C-s, C-r *)
  | Universal_argument (* C-u: the next command's prefix argument *)

type t = {
  buffers : buffer list; (* the current one first *)
  disk : (string * string) list;
  kill_ring : string list;
  message : string; (* the echo area *)
  quit : bool;
  request : request option;
}

(* the evaluator over the editor *)
type lisp = t Lisp_eval.state

(* the lines of text the window has: the screen's 24 rows, less the
   mode line and the echo area *)
val window_height : int

val current : t -> buffer
val set_current : buffer -> t -> t
val make_buffer : ?file:string -> string -> string -> buffer

(* an editor with *scratch* and [disk], and Lisp over it: the
   primitives, then [simple] (Emacs_simple's Lisp) loaded *)
val create : disk:(string * string) list -> simple:string -> lisp

(* [insert_at b pos s] and [delete_range b i j]: a change, recorded on
   the undo list, point and mark following the text they were in *)
val insert_at : buffer -> int -> string -> buffer
val delete_range : buffer -> int -> int -> buffer

(* the Boundary between two commands on the current buffer's undo list,
   unless one is there already *)
val boundary : t -> t

(* the line [pos] is on, from 0, and where a line starts *)
val line_of : buffer -> int -> int
val line_start : buffer -> int -> int

(* [search b s ~forward ~bound]: from point, where [s] ends (forward)
   or starts (backward), not past [bound] *)
val search : buffer -> string -> forward:bool -> bound:int option -> int option

(* how a key sequence is written: "C-x C-f", "M-x", "RET" *)
val key_description : string -> string
