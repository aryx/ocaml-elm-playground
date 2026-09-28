(* Scratch's blocks: what each one says, what shape it has, and the
 * program they make (Scratch, Mitchel Resnick, John Maloney, Natalie
 * Rusk and the Lifelong Kindergarten group, MIT Media Lab, 2007; this
 * is Scratch 2's set, 2013, less the sounds).
 *
 * A Scratch program has no syntax errors because it has no syntax to
 * get wrong: blocks snap together only where they fit, and the shape
 * says where. A stack block has a notch above and a tab below; a hat
 * (an event) has nothing above, it starts a script; a cap (forever,
 * stop) nothing below; a C block has mouths that hold stacks; a
 * reporter is round and fits a round or square slot, a predicate (a
 * boolean) is pointed and fits only a pointed one:
 *
 *     .-----------------.
 *    (  when flag clicked  )     a hat: nothing goes above it
 *    |_____    ____________|
 *          \__/                  one block's tab, the next one's notch
 *    |  move (10) steps    |     a stack block
 *    |_____    ____________|
 *    |  repeat (10)        |     a C block, its mouth holding a stack
 *    |   |  turn right (15) degrees  |
 *    |___|___________________|
 *
 *    (x position)   <touching [edge v]?>     a reporter, a predicate
 *
 * So a block is described once, here, by a template -- "move %n
 * steps", "if %b then|else" -- where %n is a number slot, %s a text
 * slot, %m a menu, %b a boolean slot, and a | starts the line after a
 * mouth. Everything else reads this table: the text's parser and
 * printer (Scratch_text), the runtime (Scratch_run, by the opcode),
 * the palette and the editor's layout (appkits/blocks). Scratch 3 is
 * built the same way, its blocks a JSON table of opcodes and
 * arguments; the opcodes here are its names.
 *
 * The program is a tree: a block is an opcode, its arguments (a
 * literal typed in the slot, or a reporter block dropped on it) and
 * its mouths' stacks. A variable is the reporter "data_variable" whose
 * one argument is its name.
 *
 * Snap! (Jens Monig and Brian Harvey, Berkeley, 2011; BYOB, "Build
 * Your Own Blocks", 2008) is Scratch with what Scratch left out on
 * purpose, and its blocks are here too ([snap_specs]):
 *
 * - blocks of one's own: a script under a "define" hat whose template
 *   names the parameters ("factorial %n", n the parameter), a command,
 *   a reporter ("report") or a predicate. A call is the block
 *   "custom:reporter:factorial %s" -- its op is its kind and its
 *   template, so a call needs no table to be read or drawn, as a Snap!
 *   project's XML names a custom block by its spec;
 * - rings, the lambda: a reporter, a predicate or a script with a grey
 *   ring round it is not run but reported, a value to call, to run, to
 *   pass to map -- a Ring or a Command_ring in a slot of kind %r, %p
 *   or %c, which comes with an empty ring in it;
 * - lists, first class: in a variable, in a list, reported by a
 *   custom block. *)

type category = Motion | Looks | Events | Control | Sensing | Operators | Variables | Pen | Lists | Other

type shape =
  | Hat (* starts a script *)
  | Stack
  | Cap (* ends one: nothing below *)
  | C_block (* mouths; blocks below *)
  | C_cap (* mouths, nothing below: forever *)
  | Reporter (* round, a value *)
  | Predicate (* pointed, a boolean *)
  | Ring (* Snap!'s ring round a reporter or a predicate: a function *)
  | Command_ring (* round a script: a procedure *)

(* a template's parts; a slot with its default; a ring slot comes with
   the empty ring of that op in it *)
type part = Word of string | Num of string | Text of string | Menu of string | Bool | Lambda of string

type spec = {
  op : string;
  category : category;
  shape : shape;
  lines : part list list; (* a C block: one line, then one after each mouth but the last *)
}

type arg = Lit of string | Block of block
and block = { op : string; args : arg list; mouths : block list list }

(* a stack of blocks where it lies in the scripts area *)
type script = { x : float; y : float; blocks : block list }

(* Scratch's blocks, Snap!'s, and the two palettes' categories *)
val specs : spec list
val snap_specs : spec list
val categories : category list
val snap_categories : category list
val category_name : category -> string

(* the spec of an opcode, a custom block's read from its op; the
   variable reporter has one, a menu slot for its name. Not_found for
   an op neither Scratch nor Snap! has *)
val spec : string -> spec

(* [custom_op kind template]: the op of the block a "define" hat of that
   kind ("command", "reporter", "predicate") and template ("factorial
   %n") defines, "custom:reporter:factorial %s"; the template's
   parameters, ["n"] *)
val custom_op : string -> string -> string
val params : string -> string list

(* the slots of a spec, in order, which are its block's arguments *)
val slots : spec -> part list

(* a block as the palette gives it: its slots' defaults, empty mouths *)
val make : string -> block

val variable : string -> block
