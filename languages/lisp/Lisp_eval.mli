(* Lisp_eval: running Lisp, Emacs Lisp's way.

   eval is McCarthy's (1960) as every Lisp has it, a case per kind of
   expression:

   - a number or a string is its own value;
   - a symbol is a variable: its value;
   - a list (f a b) is a call: a *special form* when f is one of the
     few the evaluator knows (if, let, setq, defun, quote, while...),
     whose arguments it evaluates as it sees fit -- (if c a b)
     evaluates only one of a and b; a *macro* when f was defined by
     defmacro, called with its arguments unevaluated and returning the
     code to evaluate instead (so when and unless are two lines of
     Lisp: Lisp_prelude.mli); otherwise a *function*, its arguments
     evaluated left to right, then applied.

   What makes it Emacs Lisp is **dynamic scope** (Emacs's only scope
   from 1985 to 2012, when lexical-binding came): a variable is looked
   up in the bindings *currently* active, whoever made them, not in
   the ones around the function's text. A let saves a variable's
   value, sets it, runs its body and puts the old value back -- Emacs's
   C calls this specbind and unbind_to, "shallow binding", and keeps
   the saved values on a stack, the specpdl:

       (defvar exact t)
       (defun search-it () (if exact "exact" "any case"))
       (let ((exact nil)) (search-it))     ==> "any case"
       (search-it)                         ==> "exact"

   search-it sees the let's binding although the let is not in its
   text. For an editor this is a feature: a command binds a variable
   that changes how every function it calls behaves, without passing
   it down -- Stallman's argument for it ("EMACS: The Extensible,
   Customizable Self-Documenting Display Editor", MIT AI Memo 519a,
   1981). The price: a function's meaning depends on who calls it, and
   there are no closures (a lambda is only a list, and its free
   variables are whatever they are when it runs).

   The evaluator is **pure**: its state -- the variables, the
   functions, the specpdl and the host's world (an editor, for Emacs)
   -- is a value, threaded through eval and returned with each result.
   A host adds functions (subrs) and special forms over its world
   (save-excursion, in an editor). An error keeps the state at the
   moment of the signal ([protect]), so the changes made before it
   stay, as in Emacs, and condition-case unwinds the specpdl to where it
   was: the lets inside undone, the setqs kept. A program that runs
   away is stopped by [fuel], a count of evaluations, where Emacs has
   C-g.

   References: John McCarthy, "Recursive Functions of Symbolic
   Expressions and Their Computation by Machine, Part I" (CACM, 1960);
   the GNU Emacs Lisp Reference Manual, "Variable Scoping"; Stefan
   Monnier and Michael Sperber, "Evolution of Emacs Lisp" (HOPL IV,
   2020). *)

(* what a built-in function raises: (error-symbol . data), as Emacs's
   signal takes them -- (error "No file"), (void-variable foo),
   (wrong-type-argument integerp "a"). The evaluator turns it into an
   Error, keeping the state it was raised in. *)
exception Signal of Lisp.t

(* [signal sym data] and [error msg], raising Signal *)
val signal : string -> Lisp.t list -> 'a
val error : string -> 'a

(* how Emacs writes a signal in the echo area: "Symbol's value as
   variable is void: foo" *)
val error_message : Lisp.t -> string

type 'h state = {
  vars : (string * Lisp.t) list; (* the value cells *)
  funs : (string * Lisp.t) list; (* the function cells: a (lambda ...) list, (macro lambda ...), or a Subr *)
  subrs : (string * 'h subr) list;
  specials : (string * 'h subr) list; (* the host's special forms, their arguments unevaluated *)
  commands : (string * Lisp.t) list; (* the interactive specs of the subrs that are commands *)
  docs : (string * string) list; (* and the subrs' documentation *)
  specpdl : (string * Lisp.t option) list; (* the values the active lets saved, innermost first *)
  host : 'h;
  fuel : int; (* evaluations left before (error "Lisp ran too long") *)
  failed : 'h state option ref; (* see [protect] *)
}

and 'h subr = 'h state -> Lisp.t list -> Lisp.t * 'h state

(* a signal escaping eval; [protect] gives the state it left *)
exception Error of Lisp.t

(* [protect st f]: f's result, or the signal that escaped it and the
   state at that moment -- the changes before the error kept, as Emacs
   keeps them. (OCaml's exceptions can't carry a value whose type is a
   variable, 'h here, so that state waits in [failed], a cell shared by
   the states of one evaluator.) *)
val protect : 'h state -> ('h state -> 'a * 'h state) -> ('a, Lisp.t) result * 'h state

(* raise a signal leaving [st], for a host's special form that caught
   one, cleaned up (save-excursion) and passes it on *)
val raise_error : 'h state -> Lisp.t -> 'a

(* an evaluator over a host, with the built-in functions (car, +,
   concat, format, mapcar...), the special forms and the prelude *)
val create : ?fuel:int -> 'h -> 'h state

(* a built-in function; with [interactive], a command too (its spec:
   "p" for the prefix argument, as (interactive "p") says) *)
val define_subr : ?interactive:string -> ?doc:string -> string -> 'h subr -> 'h state -> 'h state
val define_special : string -> 'h subr -> 'h state -> 'h state

val eval : 'h state -> Lisp.t -> Lisp.t * 'h state

(* [apply st f args]: call the function [f] (a symbol naming one, or a
   function value) on values already evaluated *)
val apply : 'h state -> Lisp.t -> Lisp.t list -> Lisp.t * 'h state

(* [progn st forms]: each in turn, the last's value *)
val progn : 'h state -> Lisp.t list -> Lisp.t * 'h state

(* [load st text]: every form of [text] evaluated, for its effects *)
val load : 'h state -> string -> 'h state

(* a variable's value (None when void), and setting one *)
val get_var : 'h state -> string -> Lisp.t option
val set_var : string -> Lisp.t -> 'h state -> 'h state

(* the function a symbol names, following aliases (fset to a symbol) *)
val function_of : 'h state -> string -> Lisp.t option

(* for a function, a symbol or a value, (interactive SPEC)'s SPEC when
   it has one: what makes a function a command, callable from a key or
   M-x *)
val interactive_spec : 'h state -> Lisp.t -> Lisp.t option

(* its documentation string, the one after its parameters *)
val documentation : 'h state -> Lisp.t -> string option
