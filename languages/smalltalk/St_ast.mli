(* St_ast: a Smalltalk-80 method or expression, as a tree.

   The whole language is five kinds of expression -- a literal, a
   variable, an assignment, a message sent, a block -- and a cascade,
   several messages to one receiver ("Transcript show: 'a'; cr").
   There is no if, no loop, no operator: "x > 0 ifTrue: [...]" is the
   message ifTrue: sent to a Boolean with a block, and "3 + 4" the
   message + sent to 3. What makes it fast anyway is the compiler's
   business (St_compile.mli, the inlined messages).

   A method is a pattern (its selector and argument names), its
   temporaries, perhaps a primitive's number, and statements; a
   statement is an expression, or a return (^). Each node keeps where
   it is in the text, [start, stop), for the compiler's errors and for
   the debugger, which highlights the message being sent. *)

type pos = int * int

type literal =
  | L_int of int
  | L_large of bool * int list (* negative?, the magnitude's bytes, least significant first *)
  | L_float of float
  | L_char of char
  | L_string of string
  | L_symbol of string
  | L_array of literal list
  | L_nil (* true, false and nil inside a literal array *)
  | L_true
  | L_false

type expr = { e : desc; pos : pos }

and desc =
  | Lit of literal
  | Var of string (* self, super, nil, true, false, thisContext included *)
  | Assign of string * expr
  | Send of expr * string * expr list
  (* a receiver, then messages to it, each with where its selector
   * starts and its arguments end *)
  | Cascade of expr * (string * expr list * pos) list
  | Block of string list * string list * stmt list (* arguments, temporaries, statements *)

and stmt = Expr of expr | Return of expr * pos

type method_ = {
  selector : string;
  args : string list;
  temps : string list;
  primitive : int option;
  body : stmt list;
}

(* how many arguments a selector takes: its colons, or one for a
 * binary selector *)
val arity : string -> int

(* an expression or a statement as the tests write it, every message
 * in parentheses: "((3 + 4) * 2)" *)
val show_expr : expr -> string
val show_stmt : stmt -> string

(* a literal as Smalltalk prints it: 3, $a, 'it''s', #foo, #(1 2) *)
val show_literal : literal -> string
