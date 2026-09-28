(* Ps_machine: PostScript run, one object at a time.
 *
 * PostScript (John Warnock and Chuck Geschke, Adobe, 1984) described
 * a page as a **program**: the printer ran it to find out what to
 * print. Before it, a printer was sent characters, or a bitmap, or a
 * device's own escape codes; after it, the same file printed on a
 * 300-dpi LaserWriter (1985) and a 2,540-dpi Linotronic, because the
 * program says where things are in points and the printer's
 * interpreter decides the pixels. Desktop publishing is that
 * sentence, and PDF (1993) is PostScript with the programming taken
 * out.
 *
 * The machine has three stacks, and they are the whole language:
 *
 *   operands      the data: 3 4 add leaves 7 here
 *   dictionaries  the names: /size 72 def puts size in the dictionary
 *                 on top; a name is looked up from the top down, so
 *                 begin and end make scopes
 *   execution     what is being run: the program's text, a procedure
 *                 and how far into it, a loop and how many turns left
 *
 * A procedure, { dup mul }, is an array marked executable. Met in the
 * program it is only pushed -- data, like a number -- and it runs when
 * a name that holds it is executed, or when exec, if, ifelse, repeat,
 * for, loop or forall are given it. So control structures are not
 * syntax but operators taking procedures, as Smalltalk's ifTrue: takes
 * a block:
 *
 *   x 0 lt { x neg } { x } ifelse     the absolute value of x
 *
 * Arrays, strings and dictionaries are shared, not copied: /a [1 2]
 * def a 0 9 put changes the array every name for it sees, as in the
 * real machine -- so a [t] is not a snapshot to go back to, and the
 * machine only steps forward.
 *
 * What it paints goes on the current page as [paint]s, in device
 * space (points, 72 to the inch, y up from the bottom-left corner of
 * a US Letter page, 612 by 792), for its caller to draw; showpage
 * ends a page. Text is shown in the host's letters, stroked.
 *
 * The errors are the LaserWriter's, as it printed them:
 * %%[ Error: undefined; OffendingCommand: sqaure ]%%
 *
 * Reference: Adobe Systems, "PostScript Language Reference Manual"
 * (the Red Book, 1985) and "PostScript Language Tutorial and Cookbook"
 * (the Blue Book, 1985). *)

type value =
  | Int of int
  | Real of float
  | Bool of bool
  | Name of string (* executable: looked up and run *)
  | Literal of string (* /name: the name itself *)
  | String of string
  | Array of array_
  | Dict of (string, value) Hashtbl.t
  | Operator of string
  | Font of float (* its size *)
  | Mark
  | Null (* what a new array holds *)

(* the items, where each came from in the program's text (none for an
 * array built by [ ]), and whether it is a procedure *)
and array_ = { items : value array; spans : (int * int) array; exec : bool }

(* what fill and stroke leave on the page: polylines in device space,
 * each with whether it is closed, the colour, and how *)
type paint = { lines : ((float * float) list * bool) list; rgb : float * float * float; how : how }
and how = Fill | Stroke of float (* the width, in device points *)

(* the letters: a character's strokes and its advance, in ems (a
 * letter 1 high is about an em), y up from the baseline *)
type host = { glyph : char -> (float * float) list list * float }

type status = Running | Done | Failed of string
type t

(* a machine about to run a program's text; with no host, letters are
 * boxes *)
val start : ?host:host -> string -> t

(* one object executed: a token of the text or of a procedure, or a
 * loop's turn *)
val step : t -> t

(* steps until the program ends, fails, or [budget] steps are taken *)
val run : ?budget:int -> t -> t

val status : t -> status

(* the page being painted, first paint first, and the pages finished
 * by showpage *)
val page : t -> paint list
val pages : t -> paint list list

(* the operand stack, top first, each as == prints it *)
val stack : t -> string list

(* what =, == and pstack printed, first line first *)
val output : t -> string list

(* where in the program's text the last object executed came from *)
val span : t -> (int * int) option

val steps : t -> int

(* a value as == prints it: 3, 2.5, /name, (text), {dup mul}, [1 2] *)
val show : value -> string
