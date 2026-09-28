(* Js_eval: a JavaScript program run, by walking its tree.

   (notes_javascript.md sections 5 to 8.) Each kind of node has its
   rule: an expression's value from its children's, a statement's
   effect. What makes a language of it is three things:

   **Scopes.** A scope is a frame of names and the frame around it
   (Js_value.scope). let, const (and var, read as let: no hoisting) add
   a name to the current frame; a name is looked for in the current
   frame, then the one around it, up to the global one, else a
   ReferenceError. A block, a call and each iteration of a for make a
   new frame -- each iteration its own copy of the loop's let, so that
   functions made in the loop each keep their own i (the spec's
   CreatePerIterationEnvironment). Function declarations are defined
   first in their block ("hoisted"), so a function can be called above
   where it is written.

   **Closures.** A function value keeps the frame it was *created* in;
   a call makes a frame for the parameters whose parent is that frame,
   not the caller's. So a function can use, and change, the variables
   of a call that has returned:

     function counter() { let n = 0; return () => { n = n + 1; return n } }
     const c = counter(); c(); c()      // 2: the n of counter's call, kept by c

   **Leaving.** A statement finishes normally or wants to leave: return
   (with a value), break, continue -- an [outcome], passed up by the
   blocks, caught by the loops and the calls. A thrown value (throw, or
   an error of the engine) is an OCaml exception (Js_value.Throw),
   because it crosses calls, the host's OCaml ones included (a callback
   throwing inside forEach).

   **this.** In o.f(), f runs with this bound to o; in f(), to
   undefined; an arrow has no this of its own, it keeps the one of
   where it was written -- why event handlers are written as arrows.

   The coercions of section 7 are here, over Js_value's conversions:
   + adds two numbers but concatenates if either side (as a primitive)
   is a string; - * / % convert both to numbers; < compares strings as
   strings, the rest as numbers; && and || give one of their operands,
   not a boolean. == is read as === (no conversion: the real ==, and
   its table, are an exercise).

   **Errors**, named as browsers name them, on the line of their
   statement: "ReferenceError: x is not defined", "TypeError: f is not
   a function", "TypeError: Cannot read properties of undefined
   (reading 'y')". And two a page must never do: recurse without end
   (RangeError: Maximum call stack size exceeded) or loop without end
   -- a page's script runs to its end before the page moves again, so
   [while (true) {}] would freeze the browser; after a budget of steps
   the engine stops it with an error, as browsers ask "A script on this
   page is busy: stop it?". *)

(* an interpreter: its global scope, what console.log prints to *)
type t

(* [create ?log ?seed ?now ()]: the built-ins defined (Js_builtins);
 * console writes to [log] (nothing by default), Math.random from [seed]
 * (1), Date's clock [now] (milliseconds since 1970: 0) *)
val create : ?log:(string -> unit) -> ?seed:int -> ?now:(unit -> float) -> unit -> t

(* a mistake, as a console shows it: "ReferenceError: x is not
 * defined", on its line; an uncaught throw of any value, "Uncaught "
 * and it *)
type error = { line : int; message : string }

(* [run t program]: its statements run in the global scope; the value
 * of the last expression statement run (undefined if none): what a
 * console prints after a line typed *)
val run : t -> Js_ast.program -> (Js_value.value, error) result

(* parse, then run *)
val eval : t -> string -> (Js_value.value, error) result

(* [call t f this args]: the host calling a script's function (an event
 * handler, a timer's) *)
val call : t -> Js_value.value -> this:Js_value.value -> Js_value.value list -> (Js_value.value, error) result

(* a global, read and defined (the host's: document, and window's) *)
val global : t -> string -> Js_value.value option
val define : t -> string -> Js_value.value -> unit

(* [steps] (10 million by default): the budget of a run or a call *)
val set_budget : t -> int -> unit
