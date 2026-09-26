(* St_interp: the Blue Book's interpreter (chapters 27 to 29).

   The machine's registers are the active context and what is cached
   from it: its method, the instruction pointer (ip), the stack
   pointer (sp), the receiver, the temporaries' home. Each cycle
   fetches a bytecode, decodes it and executes it (St_bytecode.mli).

   **Contexts are objects**, in the object memory like any other:

     MethodContext  0 sender  1 ip  2 sp  3 method  4 -  5 receiver
                    6... the arguments, the temporaries, then the stack
     BlockContext   0 caller  1 ip  2 sp  3 argument count
                    4 initial ip  5 home  6... the stack

   A send makes a new MethodContext whose sender is the active one;
   a return makes the sender active again. So the stack of calls is a
   linked list of objects, "thisContext" is one of them, and the
   debugger only has to read their fields (St_debug.mli).

   **A send**: the receiver's class, the selector, looked up up the
   superclass chain (through a cache of 1024 entries, the Blue Book's
   method cache: most sends hit it). If the method names a primitive,
   the primitive runs first (St_primitives.mli), and the method's own
   code only when it fails. The arithmetic special selectors on two
   SmallIntegers do not even look up: the bytecode does the sum. A
   selector not found sends #doesNotUnderstand: with a Message instead.

   **Returns**: "^" returns from the block's *home* method to the
   home's sender, however many contexts sit in between (a non-local
   return): "detect:" is written with it. If the home has already
   returned, #cannotReturn: is sent instead. A returned context is
   marked by a nil sender and a nil ip.

   **Processes**: a process is a chain of contexts not running. The
   host runs one at a time, for a budget of bytecodes, so that an
   endless loop never freezes the screen; a process ends when its
   bottom context returns, or is suspended: an error (Object>>error:,
   halt), a condition the debugger set, or the host itself (the user's
   interrupt). A suspended process is resumed from where it stopped.

   The collector (St_memory.mli) runs between two bytecodes, when
   enough has been allocated since the last time. *)

type oop = St_memory.oop

(* what the virtual machine asks of the world *)
type host = {
  transcript : string -> unit; (* the Transcript shows *)
  milliseconds : unit -> int; (* a clock *)
  inspect : oop -> unit; (* anObject inspect: an Inspector to open *)
  (* where the mouse is on the Display, and its buttons: 4 red (the
   * left), 2 yellow (the middle), 1 blue (the right), as Smalltalk-80
   * named them *)
  mouse : unit -> int * int * int;
}

type process_state =
  | Runnable
  | Suspended of string (* why: "Message not understood: foo", "Halt", "Step" *)
  | Finished of oop (* the bottom context returned this *)
  | Terminated

type process = {
  id : int;
  mutable top : oop; (* the context to run next, when not running *)
  mutable state : process_state;
}

type vm

(* a primitive: the vm, the number of arguments; on success it has
 * replaced the receiver and the arguments on the stack by its result *)
type primitive = vm -> int -> bool

val create : St_memory.t -> host -> vm
val memory : vm -> St_memory.t
val host : vm -> host
val set_host : vm -> host -> unit

(* the primitives, filled by St_primitives.install *)
val primitives : vm -> primitive option array

(* what the host holds, kept alive by the collector *)
val set_extra_roots : vm -> (unit -> oop list) -> unit

(*****************************************************************************)
(* Processes *)
(*****************************************************************************)

(* a new process sending [selector] to [receiver] with [args] *)
val spawn : vm -> oop -> string -> oop list -> process

(* a new process running a compiled DoIt with this receiver *)
val spawn_method : vm -> oop -> oop -> process

(* run a process for at most [budget] bytecodes; with [stop_when], it
 * is suspended with "Step" before a bytecode where the condition
 * holds (the first one excepted) *)
val run : ?stop_when:(vm -> bool) -> vm -> process -> budget:int -> unit

(* resume a suspended process: it becomes runnable where it stopped *)
val resume : process -> unit
val suspend : process -> string -> unit

(* ended for good, and forgotten: its contexts are garbage now *)
val terminate : vm -> process -> unit

(* spawn and run to the end, for the host and the tests: the answer,
 * or why it stopped *)
val call : vm -> ?budget:int -> oop -> string -> oop list -> (oop, string) result

(* [call ... "printString"], as an OCaml string *)
val print_string : vm -> oop -> string

(* evaluate a Workspace's text with a receiver, to the end *)
val evaluate : vm -> ?budget:int -> ?receiver:oop -> string -> (oop, string) result

(*****************************************************************************)
(* For the primitives *)
(*****************************************************************************)

val stack : vm -> int -> oop (* 0 the top, 1 under it... *)
val pop : vm -> int -> unit
val push : vm -> oop -> unit
val bool : vm -> bool -> oop

(* a send from a primitive (perform:), the receiver and the arguments
 * on the stack *)
val send : vm -> oop -> int -> unit

(* the context blockCopy: was sent from, the instruction pointer *)
val active_context : vm -> oop
val ip : vm -> int
val home_context : vm -> oop

(* make a context active (a block's value), saving the current one *)
val activate_context : vm -> oop -> unit

(* ask the run loop to suspend the process after this bytecode *)
val request_suspend : vm -> string -> unit

(* a garbage collection now, the registers saved first *)
val collect : vm -> unit

(* after a method is added or removed *)
val flush_cache : vm -> unit

(* the method cache's hits and misses since it was created *)
val cache_stats : vm -> int * int

(* bytecodes executed since it was created *)
val bytecodes_run : vm -> int

(* a context's field numbers *)
val c_sender : int
val c_ip : int
val c_sp : int
val c_method : int
val c_receiver : int
val c_home : int
val c_temps : int

(* for the debugger: whether a context is a block's, its method, its
 * home, its receiver *)
val is_block_context : vm -> oop -> bool
val context_method : vm -> oop -> oop
val context_home : vm -> oop -> oop
