(* The File menu every application shares: New, Open..., Save, Save
 * As..., Export -- and the two dialogs behind them (plan_io.md).
 *
 * A document is saved as a value, with appkits/document/Saved:
 * Marshal behind a line naming the application and a version, so that
 * opening a file of another kind, or of an older build, says so
 * rather than crashing. What is saved is the application's *data*
 * ('d): its sheet, its text, its bitmap -- never a part of a compound
 * document, which is a record of functions, only each part's kind and
 * saved text (the registry reads them back).
 *
 * Where it goes is Playground_platform's store, and the authority to
 * go there is a capability, [caps], which the application's main gets
 * from Cap.main and hands to its update: an application that does not
 * is one that cannot save, and its type says so.
 *
 * The menu is immediate mode like the rest of the toolkit: [command]
 * when an item is chosen, and, while a dialog is up ([busy]), [dialog]
 * every frame instead of the application's own input, and [view]
 * under Gui.draw's shapes. *)

(*****************************************************************************)
(* {1 Setting up} *)
(*****************************************************************************)

type caps = < Cap.open_in ; Cap.open_out ; Cap.readdir >

(* what an application's files are: the line its documents start with
   ("TinyExcel 1" -- the number goes up when the saved type changes),
   and the extension of their names (".sheet") *)
type kind = { magic : string; extension : string }

(* the document's name, if it has one yet; the dialog showing, if any;
   and what the last command did, for a status line *)
type t

val start : t

(*****************************************************************************)
(* {1 The menu} *)
(*****************************************************************************)

(* the menu, its title first *)
val items : string list

(* what the application has to do after a command or a dialog *)
type 'd result = Nothing | New | Opened of 'd

(* [command caps kind ~current item t]: File > [item]. Save writes at
   once to the document's name (or asks for one); Open and Save As put
   up their dialog. [current] gives the data to save, asked only when
   saving. *)
val command : caps -> kind -> current:(unit -> 'd) -> string -> t -> t * 'd result

(* the menu itself, this frame, in a menu bar at [box]: [Gui.menu_in],
   and [command] with what was chosen; [?items] for a menu with fewer
   of them *)
val menu_in : ?items:string list -> caps -> kind -> Playground.computer -> Widget.box -> current:(unit -> 'd) -> t -> t * 'd result

(* HyperCard's way, which had no Save: [autosave caps kind ~current t]
   writes the document to its name, if it has one yet -- to be called
   when it has changed *)
val autosave : caps -> kind -> current:(unit -> 'd) -> t -> t

(*****************************************************************************)
(* {1 The dialogs} *)
(*****************************************************************************)

(* is a dialog up? the application's own input waits while it is *)
val busy : t -> bool

(* the dialog, this frame: the name typed (Enter saves, Escape
   cancels), or the documents of this kind listed to open one *)
val dialog : caps -> kind -> Playground.computer -> current:(unit -> 'd) -> t -> t * 'd result

(* the dialog's panel and name field, to draw before Gui.draw () *)
val view : t -> Playground.shape list

(*****************************************************************************)
(* {1 The name and the status line} *)
(*****************************************************************************)

(* the document's name, or "untitled" *)
val title : t -> string

(* what the last command did: "saved budget.sheet, 1204 bytes", "not a
   TinyExcel document", ... *)
val said : t -> string
