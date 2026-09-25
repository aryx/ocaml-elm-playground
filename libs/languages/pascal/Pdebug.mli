(* Pdebug: a paused P-machine, read as Pascal.

   The machine knows words and addresses; the programmer thinks of
   lines, procedures and variables. The debugger joins the two with
   what the compiler left it (Pcode.mli's procedures and statements):

   - **where we are**: the procedure whose code holds pc, and the line
     of the statement there;
   - **the calls**: the frames, from mp along the dynamic links, each
     with the procedure its return address is in -- the call stack
     (Turbo's Ctrl-F3), recursion included:

         TRY(3)     frame 58   static link 0   dynamic link 51
         TRY(2)     frame 51   static link 0   dynamic link 44
         TRY(1)     frame 44   static link 0   dynamic link 0
         QUEENS     frame 0

     every TRY's static link is the main program's frame, where TRY is
     declared, while the dynamic links chain the calls;
   - **a variable by its name**: looked up in the procedure at pc, then
     along the *static* links, in the procedures around it, as the
     compiler resolved it -- a watch (Ctrl-F7) such as x, a[3] or
     p.x read from those words, and written as Turbo Pascal wrote it:
     an array (1,5,8,6), a boolean TRUE, a character 'A';
   - **steps**: what to stop at, as a predicate for Pmachine.resume --
     a statement's start that is on another line (F7, trace into), on
     another line in this call or its callers (F8, step over: the calls
     it makes run through), on a given line (F4, go to cursor), or at a
     breakpoint (Ctrl-F9 again).

   Reference: Jonathan B. Rosenberg, "How Debuggers Work" (Wiley,
   1996): the same pieces for a real processor, with DWARF for the
   compiler's information. *)

(* the index in the program's procedures of the one holding [pc] *)
val procedure_at : Pcode.program -> int -> int

(* the source line of the machine's next instruction *)
val line : Pcode.program -> Pmachine.machine -> int

type frame = { procedure : int; base : int; static_link : int; dynamic_link : int; return_address : int }

(* the calls, the innermost first, to the main program's *)
val frames : Pcode.program -> Pmachine.machine -> frame list

(* a frame as the call stack shows it: TRY(3) *)
val call : Pcode.program -> Pmachine.machine -> frame -> string

(* [watch program machine expression]: its value, or the reason
   there is none ("Unknown identifier: y") *)
val watch : Pcode.program -> Pmachine.machine -> string -> string

type step =
  | Trace_into (* F7 *)
  | Step_over (* F8 *)
  | To_line of int (* F4 *)
  | Continue of int list (* Ctrl-F9: to one of these lines, the breakpoints *)

(* [pause_for program step machine]: when to pause, the step starting
   where [machine] is now; at least one instruction always runs *)
val pause_for : Pcode.program -> step -> Pmachine.machine -> Pmachine.machine -> bool
