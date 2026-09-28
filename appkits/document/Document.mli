(* A document is a value: what it holds, where it lives, and whether
 * it has changed since it was last saved (notes_gui.md section 8).
 *
 *   type 'a t = { content : 'a; path : string option; saved : 'a }
 *
 * That is the whole structure, and every line of it is a decision:
 *
 *   - **an edit returns a new document**, so undo is a list of them
 *     (Undo.mli) and nothing is ever lost by accident;
 *   - **the version last saved is kept beside the current one**, so
 *     "is there anything to save?" is [content != saved] -- a
 *     *pointer* comparison, instant whatever the document's size.
 *     An editor that changes its document in place cannot do that: it
 *     has to keep a flag and remember to set it in every place that
 *     edits, which is the same bug as the label in Retained.mli;
 *   - and the same comparison makes the star in the title go out by
 *     itself when you undo back to the version you saved, which most
 *     editors get wrong.
 *
 * That last one has a catch worth knowing, and it is why [create]
 * takes an [?equal]. Pointer comparison is exact and free when going
 * back to a version means *the old value itself* -- which is what a
 * list of past documents gives you. But a structure that rebuilds a
 * version rather than keeping it (gui/Text_edit's undo rebuilds its
 * record) hands back something equal but not identical, and then the
 * star stays lit. Give [equal] and it goes out; the cost is whatever
 * comparing two documents costs.
 *
 * Saving itself is not here. Writing bytes is the backend's business
 * (and the browser has no files at all); what a document knows is
 * whether it *needs* saving, and what to call itself. *)

type 'a t

(* [create ?path ?equal content]: a document holding [content], saved
 * as it stands (nothing to save yet) *)
val create : ?path:string -> ?equal:('a -> 'a -> bool) -> 'a -> 'a t

val content : 'a t -> 'a
val path : 'a t -> string option
val with_path : string -> 'a t -> 'a t

(* [edit f doc]: a new document, holding [f]'s answer *)
val edit : ('a -> 'a) -> 'a t -> 'a t

(* [put content doc]: the same, when the caller already has the new
 * content (a widget usually does) *)
val put : 'a -> 'a t -> 'a t

(* has it changed since it was last saved? *)
val dirty : 'a t -> bool

(* [mark_saved doc]: what it holds now is what is on disk. (The
 * writing is the caller's; this is the bookkeeping.) *)
val mark_saved : 'a t -> 'a t

(* what to put in a title bar: the file's name, or "untitled", with a
 * star while there is something to save *)
val title : 'a t -> string
