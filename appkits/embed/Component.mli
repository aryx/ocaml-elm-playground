(* A part of a document: something that can be put in a document
 * without the document knowing what it is. A spreadsheet in a letter, a
 * picture in a spreadsheet, a letter in a picture -- the idea of the
 * Andrew Toolkit's "insets" (CMU, 1988), of Microsoft's OLE (1991-93),
 * and of OpenDoc's "parts" (Apple and IBM, 1994-97), where it was the
 * whole architecture: no applications, only documents made of parts,
 * and the part you click on is the editor.
 *
 * All a document needs from a part is a handful of things it can do,
 * so a part is those things -- a record of functions, each closing
 * over the part's own state, whatever that is:
 *
 *   height    how tall I am at this width        the document lays out
 *   draw      myself into this rectangle         the document draws
 *   input     the mouse and keys, when I am      the document hands
 *             active, in this rectangle           them over
 *   menu      my commands, to show while I am    the document's menu
 *   command   active -- and doing one              bar changes
 *   save      myself, as text                    the document saves
 *
 * What COM did with an interface, a registry and reference counts, and
 * a Java component with a class, OCaml does with a record: [input] and
 * [command] return the part changed, a new record closing over the new
 * state, so a part is a value, and a document made of them is a value
 * too -- which is what gives the document its undo for nothing.
 *
 * The one thing a document cannot get from a part is the part itself,
 * back from the text it saved: that needs code for its kind, found by
 * name -- the [registry], OLE's CLSIDs in the Windows registry as an
 * association list. And a kind nobody here knows is not an error:
 * it becomes a [placeholder], which shows what it is, and saves back
 * exactly the text it was loaded from -- the rule that a document must
 * survive passing through a program that cannot read all of it. *)

type part = {
  (* the name of its kind, which [registry] loads it by *)
  kind : string;
  height : float -> float;
  (* [~active]: whether it is the one being edited -- a caret, a
     selection, only show then *)
  draw : Widget.box -> active:bool -> Playground.shape list;
  input : Playground.computer -> Widget.box -> part;
  (* its menu's title, then its items; [] for none *)
  menu : string list;
  command : string -> part;
  save : unit -> string;
}

(* a way to read each kind back: its name, and a loader *)
type registry = (string * (string -> part)) list

(* [load registry ~kind text]: the part [text] was saved from, or a
 * placeholder when no loader is known for [kind] *)
val load : registry -> kind:string -> string -> part

(* a part that cannot be shown here, kept whole *)
val placeholder : kind:string -> string -> part
