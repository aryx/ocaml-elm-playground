(* Turbo_menus: the IDE's commands. Turbo Pascal's menu bar (File,
   Search, Run, Compile, Debug, Help) as data, and what each item does.

   One function, [act], is every command: a function key and a menu's
   item both end there, so that a desktop keeping F9 or Alt-F5 for
   itself takes nothing away, every command being in the menus too.

   A command that needs a word (a file's name, a text to find, a line's
   number, an expression to watch) opens an input dialog; its keys are
   [input_key]'s, and Enter does what the dialog was for. *)

open Turbo_model

(* the bar: each menu's name and its items *)
val menus : (string * item list) list

(* a command done: the model after it (a file opened or saved, a dialog
   or a box shown, the program compiled, run or stepped, ...) *)
val act : model -> action -> model

(* The keys of a menu and of a dialog *)

(* [menu_key m bar item key] with the menu [bar] open on its [item]: the
   arrows move (left and right to the neighbouring menus), Enter or an
   item's hot letter does it, Alt and a letter opens another menu,
   Escape closes *)
val menu_key : model -> int -> int -> string -> model

(* [input_key m (title, label, text, purpose) key] in an input dialog:
   a character typed, Backspace, Escape leaving it, Enter doing what it
   is for with the text *)
val input_key : model -> string * string * string * purpose -> string -> model

(* Alt and a letter, as a terminal sends it (Escape, then the letter):
   the letter, in lower case *)
val alt_letter : string -> char option

(* the menu a letter opens (Alt-F: File), its index in the bar *)
val menu_of_letter : char -> int option

(* the floppy's files, sorted: the Open dialog's list *)
val files : model -> string list
