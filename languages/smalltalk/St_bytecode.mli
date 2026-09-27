(* St_bytecode: the Blue Book's instruction set, and a CompiledMethod.

   A method runs as bytecodes on a stack machine, one byte each
   (a few take a byte or two more). The set is the Blue Book's, chapter
   28, byte for byte:

     0-15     push receiver variable #iiii
     16-31    push temporary location #iiii
     32-63    push literal constant #iiiii
     64-95    push literal variable #iiiii (an Association's value)
     96-103   pop and store receiver variable #iii
     104-111  pop and store temporary location #iii
     112-119  push self, true, false, nil, -1, 0, 1, 2
     120-123  return self, true, false, nil from the method
     124      return stack top from the method (from its home, in a block)
     125      return stack top from the block, to its caller
     128      push, extended: jjkkkkkk (jj: receiver variable,
              temporary, literal constant, literal variable; k the index)
     129      store, extended (jj: receiver variable, temporary, -, literal variable)
     130      pop and store, extended
     131      send, extended: iiijjjjj (i arguments, selector literal j)
     132      send, double extended: then a byte of arguments, a byte of literal
     133      send to super, extended; 134 double extended
     135      pop stack top
     136      duplicate stack top
     137      push the active context (thisContext)
     144-151  jump forward 1-8
     152-159  pop and jump forward 1-8 on false
     160-167  jump (iii - 4) * 256 + next byte, backwards too
     168-171  pop and jump on true, ii * 256 + next byte
     172-175  pop and jump on false, ii * 256 + next byte
     176-191  send + - < > <= >= = ~= * / \\ @ bitShift: // bitAnd: bitOr:
     192-207  send at: at:put: size next nextPut: atEnd == class
              blockCopy: value value: do: new new: x y
     208-255  send literal selector #iiii with 0, 1 or 2 arguments

   The 32 "special selectors" of 176-207 cost one byte and no literal;
   for the arithmetic ones on SmallIntegers the interpreter does not
   even look the method up (St_interp.mli).

   A CompiledMethod is an object with fields and bytes: its first field
   the **header** (a SmallInteger: the primitive's number, the numbers
   of arguments and temporaries, the frame's size), then the
   **literals** (constants, selectors, Associations for globals), then
   -- ours, not the Blue Book's, which kept them in the sources file --
   a trailer: the selector, the class, the source text, the pc map
   (where each send's bytecode is, and the text it came from) and the
   temporaries' names, for the debugger.

   Worked example, the Blue Book's (chapter 26): Rectangle's

     center
         ^origin + corner / 2

   is 0 1 176 119 185 124: push origin (receiver variable 0), push
   corner (1), send +, push 2, send /, return the top. *)

type oop = St_memory.oop

val special_selectors : string array (* the 32, in order *)

(*****************************************************************************)
(* The header *)
(*****************************************************************************)

type header = { primitive : int; num_args : int; num_temps : int (* the arguments included *); frame_size : int }

val encode_header : header -> int
val decode_header : int -> header

(*****************************************************************************)
(* A CompiledMethod *)
(*****************************************************************************)

val trailer_size : int

val new_method :
  St_memory.t ->
  header:header ->
  literals:oop array ->
  bytecodes:Bytes.t ->
  selector:oop ->
  cls:oop ->
  source:string ->
  pcmap:(int * int * int) list ->
  temp_names:string list ->
  oop

val header : St_memory.t -> oop -> header
val literals : St_memory.t -> oop -> oop array (* a copy *)
val literal : St_memory.t -> oop -> int -> oop
val bytecodes : St_memory.t -> oop -> Bytes.t
val selector : St_memory.t -> oop -> oop
val method_class : St_memory.t -> oop -> oop
val source : St_memory.t -> oop -> string

(* each send: its pc, and where its text starts and stops *)
val pcmap : St_memory.t -> oop -> (int * int * int) list

(* the temporaries' names, by index: the arguments first, then the
 * method's temporaries, then its blocks' (which are the method's too) *)
val temp_names : St_memory.t -> oop -> string list

(*****************************************************************************)
(* Reading them *)
(*****************************************************************************)

(* the instructions from pc, each "pc <bytes> meaning", literals shown
 * by [show_literal] *)
val disassemble : show_literal:(int -> string) -> Bytes.t -> (int * string) list

(* how long the instruction at pc is, in bytes *)
val length_at : Bytes.t -> int -> int

(* whether the instruction at pc is a send, and of how many arguments *)
val is_send : Bytes.t -> int -> bool
