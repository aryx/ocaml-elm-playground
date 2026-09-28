(* Nls_doc: a document as NLS kept it (Douglas Engelbart's
 * Augmentation Research Center, SRI, 1968) -- a tree of statements.
 *
 * Every text program before it, and most after, held a document as a
 * string of characters. NLS held it as a **tree**: a statement is a
 * paragraph, and it can have statements below it, as an outline does.
 * Its number says where it is, levels alternating digits and letters:
 *
 *   1  Shopping list
 *      1a  produce
 *          1a1  apples
 *          1a2  bananas
 *      1b  dairy
 *   2  Things to do
 *
 * So the structure is something you can edit, not just see: a
 * **branch** (a statement and everything below it) is moved, copied,
 * deleted as one, and a **view** shows only the first levels, the
 * outline of the document, without changing it ([visible]).
 *
 * Numbers change when a branch moves -- 1b above becomes 1a when
 * produce goes elsewhere -- so NLS gave each statement a second,
 * permanent name, its SID (statement identifier), never reused; and a
 * statement could be given a name of its own, a word in parentheses
 * at its start, "(shop) Shopping list". A link, written <shop> or <1b>,
 * finds its statement by name or by number ([find]); the program
 * holds on to SIDs. The web's links are the same idea, forty years of
 * broken ones later: a URL is a number, not a SID.
 *
 * Worked example, checked by the tests: the list above with produce's
 * branch moved after dairy -- dairy becomes 1a, produce 1b, apples
 * 1b1, and each keeps its SID. *)

type statement = { sid : int; text : string; children : statement list }
type t = { statements : statement list; next_sid : int }

(* from an indented outline: each line's depth (0 at the top) and
 * text, in reading order; a line deeper than the one before by more
 * than one is taken as one deeper *)
val of_outline : (int * string) list -> t

val get : t -> int -> statement option

(* the statement's number, "2a1"; None for an unknown SID *)
val number : t -> int -> string option

(* the name a statement gives itself, "(shop) Shopping list" -> shop *)
val name_of : string -> string option

(* a link's target: a statement's name (any case), else a number *)
val find : t -> string -> int option

(* where a statement goes, next to a target: after it, as its first
 * child (down a level), or after its parent (up a level; after the
 * target at the top) *)
type where = After | Down | Up

(* the new statement's SID too *)
val insert : t -> target:int -> where -> string -> t * int

val set_text : t -> int -> string -> t

(* the statement and its branch *)
val delete : t -> int -> t

(* a branch taken from where it is to a place next to [target]; None
 * when the target is inside the branch, which would put it inside
 * itself *)
val move : t -> int -> target:int -> where -> t option

(* the same, the branch copied with new SIDs *)
val copy : t -> int -> target:int -> where -> t option

(* the statements a view shows, in reading order, with their depth:
 * all of them, or the first [levels] levels *)
val visible : t -> levels:int option -> (statement * int) list

(* the links in a statement's text: where each starts, where it ends
 * (after its >) and what it names *)
val links : string -> (int * int * string) list

(* the word at a place in a text, or the next one after a space: its
 * start and its end *)
val word_at : string -> int -> (int * int) option
