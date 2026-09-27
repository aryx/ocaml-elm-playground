(* St_memory: the object memory, as the Blue Book's chapter 30 has it.

   Every object is an *oop* (an "object pointer"), and an oop is an
   int with two meanings, told apart by its lowest bit:

     ...value...1   a SmallInteger: the value is the oop shifted right
                    once, no object behind it (3 is the oop 7)
     ...index...0   an object: the index of its entry in the object
                    table (the entry 0 is nil, so nil is the oop 0)

   An entry of the table holds the object's class (an oop) and its
   body: its fields (oops), or its bytes (a String, a Symbol, a
   LargePositiveInteger), or a float, or -- a CompiledMethod -- both
   fields and bytes, its literals and its bytecodes.

     oop 2 -----> table[1] = { class = <oop of True>; body = Pointers [||] }
     oop 7 = the SmallInteger 3, no entry

   Why a table, when OCaml has pointers? For [become:]: "a become: b"
   exchanges the two entries, and every reference to a, anywhere,
   now reaches what was b -- in constant time, without finding the
   references. Smalltalk uses it to grow a collection in place and to
   change a class's shape under its instances. Squeak (1996) dropped
   the table for direct pointers, and pays for become: with a scan of
   the whole memory.

   SmallIntegers are 31 bits, -2^30 to 2^30 - 1, so that a tagged one
   fits a 32-bit integer on the web (js_of_ocaml's ints). Past that,
   the arithmetic primitives fail and the kernel's
   LargePositiveInteger, written in Smalltalk over bytes, takes over.

   The garbage collector is a mark and sweep over the table: from the
   roots (nil, true, false, the Smalltalk dictionary, the symbols, the
   processes, what the host holds), every reachable entry is marked,
   and the others go back on the free list. OCaml's own collector
   cannot help: to it, the table is one big array, all alive. (The
   Blue Book counted references, and marked only to reclaim cycles;
   reference counting is an exercise.) The collector runs only when
   St_interp asks, between two bytecodes, when every live oop is in the
   memory or in a root -- never from inside [alloc], which would
   free an object a primitive is still building. *)

type oop = int

type body =
  | Pointers of oop array (* the named fields, then the indexed ones *)
  | Bytes of Bytes.t
  | Float of float
  (* a CompiledMethod: the header and the literals (St_bytecode.mli),
   * and the bytecodes *)
  | Method of oop array * Bytes.t
  | Free

(* the objects the virtual machine knows by name, set by St_boot *)
type known = {
  mutable small_integer : oop;
  mutable string : oop;
  mutable symbol : oop;
  mutable array : oop;
  mutable float : oop;
  mutable character : oop;
  mutable compiled_method : oop;
  mutable method_context : oop;
  mutable block_context : oop;
  mutable message : oop;
  mutable association : oop;
  mutable point : oop;
  mutable large_positive : oop;
  mutable large_negative : oop;
  mutable metaclass : oop;
  mutable method_dictionary : oop;
  mutable true_ : oop;
  mutable false_ : oop;
  mutable smalltalk : oop; (* the SystemDictionary *)
  mutable characters : oop array; (* the 256 Characters, unique *)
  mutable special_selectors : oop array; (* the 32 of bytecodes 176-207 *)
}

type t

val create : unit -> t
val known : t -> known

(* nil is the entry 0, made by [create] (its class set by St_boot) *)
val nil : oop

(*****************************************************************************)
(* SmallIntegers *)
(*****************************************************************************)

val is_int : oop -> bool
val int_of : oop -> int
val of_int : int -> oop
val fits : int -> bool (* in SmallInteger's 31 bits *)

(*****************************************************************************)
(* Objects *)
(*****************************************************************************)

val alloc : t -> cls:oop -> body -> oop

(* the class of any oop, SmallIntegers' included *)
val class_of : t -> oop -> oop
val set_class : t -> oop -> oop -> unit
val body : t -> oop -> body
val set_body : t -> oop -> body -> unit

(* a pointer field (a CompiledMethod's literal frame counts), from 0 *)
val fetch : t -> oop -> int -> oop
val store : t -> oop -> int -> oop -> unit

(* the number of fields (pointers), bytes, or 0 *)
val size : t -> oop -> int
val fields : t -> oop -> oop array (* the fields themselves, not a copy *)

(* a String's, a Symbol's bytes as an OCaml string *)
val string_of : t -> oop -> string
val new_string : t -> string -> oop
val new_array : t -> oop array -> oop
val new_float : t -> float -> oop
val float_of : t -> oop -> float

(* the Symbol of a string, the same oop every time *)
val symbol : t -> string -> oop
val symbols : t -> (string * oop) list

(* exchange the two entries: every reference to one reaches the other *)
val become : t -> oop -> oop -> unit

(*****************************************************************************)
(* The whole memory *)
(*****************************************************************************)

(* the objects of a class (someInstance, nextInstance, allInstances) *)
val instances : t -> oop -> oop list

(* how many entries are in use *)
val live : t -> int

(* allocations since the last collection, which St_interp watches *)
val allocated : t -> int

(* mark from the roots (nil, true, false, Smalltalk, the characters,
 * the special selectors, the symbols, and [roots]), sweep the rest;
 * the number of entries freed. A context's fields are marked up to its
 * stack pointer only: above it are the stale values of a stack that
 * shrank. *)
val gc : t -> roots:oop list -> int

(* the table, for St_image: every entry in use, its index, class and
 * body; and a memory made again from them *)
val entries : t -> (int * oop * body) list
val restore : known -> (int * oop * body) list -> t
