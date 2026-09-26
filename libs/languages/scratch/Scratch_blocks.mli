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
 * one argument is its name. *)

type category = Motion | Looks | Events | Control | Sensing | Operators | Variables | Pen

type shape =
  | Hat (* starts a script *)
  | Stack
  | Cap (* ends one: nothing below *)
  | C_block (* mouths; blocks below *)
  | C_cap (* mouths, nothing below: forever *)
  | Reporter (* round, a value *)
  | Predicate (* pointed, a boolean *)

(* a template's parts; a slot with its default *)
type part = Word of string | Num of string | Text of string | Menu of string | Bool

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

val specs : spec list
val categories : category list
val category_name : category -> string

(* the spec of an opcode; the variable reporter has one, a menu slot
   for its name *)
val spec : string -> spec

(* the slots of a spec, in order, which are its block's arguments *)
val slots : spec -> part list

(* a block as the palette gives it: its slots' defaults, empty mouths *)
val make : string -> block

val variable : string -> block
