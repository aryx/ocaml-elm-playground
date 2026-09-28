(* Scheme_eval: running Scheme on a CESK machine.

   Emacs Lisp's eval (Lisp_eval.mli) is recursive: to evaluate (f (g
   x)), it calls itself on (g x), and OCaml's stack remembers what to
   do with the result. That stack is the whole difficulty of Scheme:
   call/cc must *capture* it as a value and reinstate it later, a call
   in tail position must *not* grow it, and DrScheme's Break button
   must stop a program in the middle of it. So here the stack is data,
   the continuation (Scheme.mli's [kont]), and running is a loop over
   a machine's state, one small step at a time -- Matthias Felleisen
   and Daniel Friedman's CEK machine (1986), with a Store for set!
   (the CESK machine), Felleisen being the author of DrScheme:

       Control      the expression being evaluated (with its
                    Environment), or the value just computed
       Environment  each variable's location in the store
       Store        each location's value: what set! changes
       Kontinuation what to do next with a value

       ((lambda (x) (+ x 1)) 41), a step at a time:
         Eval ((lambda ...) 41)         k = Halt
         Eval (lambda ...)              k = [app: ( ) args (41)] Halt
         Return #<procedure>            k = [app: ( ) args (41)] Halt
         Eval 41                        k = [app: (#<procedure>) ( )] Halt
         Return 41                      ...   all evaluated: apply,
         Eval (+ x 1)  env x->l0        k = Halt   store l0 -> 41
         ...
         Return 42                      k = Halt: done

   What falls out of the continuation being data:
   - call/cc is two lines: (call/cc f) calls f with the current [kont]
     wrapped as a procedure, and calling that procedure throws the
     current one away for it;
   - tail calls: a body's last expression is evaluated with the body's
     own continuation, no frame added, so a loop runs in constant
     space (R5RS requires it; C and OCaml's own stack don't give it);
   - [run] takes a number of steps, its *fuel*, and stops there: the
     host runs a budget of steps a frame, so an endless loop leaves
     the screen alive and Break is simply not running any more.

   The state is a value, as Lisp_eval's is: the store a persistent
   map, never collected (a program making a million bindings holds
   them all -- a garbage collector is an exercise).

   References: Matthias Felleisen and Daniel Friedman, "Control
   Operators, the SECD-Machine, and the lambda-Calculus" (1986);
   Felleisen, Findler and Flatt, "Semantics Engineering with PLT
   Redex" (2009), chapter 6; R5RS, "Revised^5 Report on the
   Algorithmic Language Scheme" (1998), section 3.5, "Proper tail
   recursion". *)

type state

(* a world program asked for by (big-bang ...): the first world, and
   its handlers by clause name, "to-draw", "on-tick"... in the order
   written *)
type world = { init : Scheme.t; handlers : (string * Scheme.t) list; span : Sexpr.span }

(* an error, the text at fault when known -- DrScheme paints it pink *)
type error = { message : string; at : Sexpr.span option }

type outcome =
  | Done of Scheme.t (* the expression's value *)
  | Running (* the fuel ran out first: run again *)
  | Failed of error
  | World of world (* waiting for the host to run a world, then [resume] *)

(* a machine with the built-ins (Scheme_prims.mli, call/cc, apply,
   display, random) and the prelude (Scheme_prelude.mli) defined *)
val create : unit -> state

(* [start st e]: evaluating [e] next, from nothing *)
val start : state -> Scheme.expr -> state

(* [run ~fuel st]: at most [fuel] steps (100000 by default) *)
val run : ?fuel:int -> state -> outcome * state

(* [resume st v]: the big-bang that waited returns [v], the last
   world *)
val resume : state -> Scheme.t -> state

(* [call st f args]: the host calling a procedure (a world's handler),
   to its end, with at most [fuel] steps (a million by default); what
   the machine was doing is kept for after *)
val call : ?fuel:int -> state -> Scheme.t -> Scheme.t list -> (Scheme.t, error) result * state

(* what display and newline printed since the last time, and the
   machine without it *)
val take_output : state -> string * state

(* [eval_all st text]: every form of [text], read, checked and run to
   its end, the last's value -- for the tests, and the prelude *)
val eval_all : ?fuel:int -> state -> string -> (Scheme.t, error) result * state

(* how many steps the machine has taken, since its creation *)
val steps : state -> int

(* the global variables the program defined, not the built-ins' --
   for a host listing them *)
val defined : state -> string list
