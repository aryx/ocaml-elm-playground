(* Token_ml: OCaml's text as tokens, every one of them kept.

   A compiler's lexer throws away what the grammar does not need; a
   visualizer's keeps it all, since what a reader reads first is often
   a comment: here the comments are tokens, each token has its text as
   it is in the file (a string's quotes and escapes, not their value)
   and its place (line and column), so that a view can draw a file
   from its tokens alone, and the spaces are what lies between them.

   The kinds are coarse, what the lexer knows without the grammar:
   [Lident] is any lowercase name (a value, a type, a field, a
   parameter), which [Highlight_ml] then tells apart by looking
   around. *)

type kind =
  | Comment (* (* ... *), nested, to the end of the file if unclosed *)
  | Keyword (* let, match, module, let*, mod, ... *)
  | Lident (* x, _foo, list, caps *)
  | Uident (* Some, List, Cap *)
  | Label (* ~width:, ?scale, ~x (punned) *)
  | Type_var (* 'a *)
  | Int (* 42, 0xFF, 1_000, 3L *)
  | Float (* 1., 1e-3 *)
  | Char (* 'a', '\n' *)
  | String (* "...", {|...|}, {id|...|id} *)
  | Operator (* + |> :: := -> ... *)
  | Punctuation (* ( ) [ ] { } , ; ;; [@ [% *)
  | Directive (* # 1 "file" *)
  | Error (* a character OCaml does not know, an unclosed string *)

type t = {
  kind : kind;
  text : string; (* as in the file *)
  offset : int; (* its first byte's, from 0 *)
  line : int; (* its first character's, from 1 *)
  col : int; (* from 0, in bytes *)
}

val show_kind : kind -> string
