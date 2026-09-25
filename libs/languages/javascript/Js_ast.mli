(* Js_ast: a JavaScript program as a tree -- the parser's output
   (Js_parse), the interpreter's input (Js_eval).

   Expressions compute a value; statements do something, and may leave
   (return, break, continue, throw). The kinds of node, and their names
   where it is cheap, follow ESTree, the tree the JavaScript tools
   (Esprima, Acorn, Babel) agree on:

     1 + 2 * 3            Binary ("+", Number 1, Binary ("*", Number 2, Number 3))
     a.b(c)[0]            Index (Call (Member (Name a, "b"), [Name c]), Number 0)
     x => x * 2           Function { params = [x]; body = [Return (x * 2)]; arrow }
     let n = 0            Let (Let_kind, [ ("n", Some (Number 0)) ])

   A statement carries its line, which the interpreter's errors report
   ("x is not defined on line 3"); an expression does not: its
   statement's line is close enough for a program of a page.

   The printer ([expr_to_string], [stmt_to_string]) writes every
   operation in parentheses, so that a test says in one string how the
   parser grouped the operators: 1 + 2 * 3 is "(1 + (2 * 3))". *)

type expr =
  | Number of float
  | String of string
  | Bool of bool
  | Null
  | Name of string (* a variable; undefined is one, the global's *)
  | This
  | Array of expr list
  | Object of (string * expr) list (* the keys in the order written *)
  | Function of func (* function (...) {...}, or an arrow *)
  | Unary of string * expr (* - + ! typeof *)
  | Update of string * bool * expr (* ++ or --, prefix (true) or postfix, on a target *)
  | Binary of string * expr * expr (* + - * / % < > <= >= === !== == != *)
  | Logical of string * expr * expr (* && || : the right side only if needed *)
  | Assign of string * expr * expr (* = += -= *= /= %=, on a target *)
  | Conditional of expr * expr * expr (* c ? a : b *)
  | Member of expr * string (* o.x *)
  | Index of expr * expr (* o[i] *)
  | Call of expr * expr list (* f(a, b), o.m(a): a method call when f is a Member or an Index *)

(* a function: its name if it has one, its parameters, its body; an
 * arrow's expression body is [Return e]; an arrow has no this of its
 * own *)
and func = { name : string option; params : string list; body : stmt list; arrow : bool }

and stmt = { line : int; stmt : statement }

and statement =
  | Expr of expr
  | Let of let_kind * (string * expr option) list (* let a = 1, b *)
  | Function_decl of func
  | Return of expr option
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | For of stmt option * expr option * expr option * stmt (* for (init; test; update) body *)
  | For_of of let_kind * string * expr * stmt (* for (let x of xs) body *)
  | Break
  | Continue
  | Throw of expr
  | Try of stmt list * string * stmt list (* try { } catch (e) { } *)
  | Block of stmt list
  | Empty (* a lone ; *)

and let_kind = Let_kind | Const_kind | Var_kind

type program = stmt list

(* a number as JavaScript prints it: 7, not 7.0; 0.5; the shortest
 * digits that read back as the same float (0.1 + 0.2 is
 * 0.30000000000000004); NaN, Infinity, -Infinity *)
val number_to_string : float -> string

(* fully parenthesized: (1 + (2 * 3)), f((x) => (x * 2), 3) *)
val expr_to_string : expr -> string

(* one line per statement, a body's statements in brackets:
 *   Let a 1
 *   If ((b > a), Block [Expr (b = 0)], Expr (b = 1))
 *   Function f [] [Return; Expr a] *)
val stmt_to_string : stmt -> string
