(* Token_c: C's text as tokens, every one of them kept.

   As Token_ml for OCaml: the comments are tokens, each token has its
   text as it is in the file and its place, so that a view can draw a
   file from its tokens alone.

   C has a language above C, the preprocessor's: a line starting with #
   is a directive, to its end (or further, after a backslash). Nothing
   here runs it -- no #include read, no macro expanded -- but its tokens
   say that they are in a directive ([pp]), so that the parser can read
   them apart (Parse_c: a #define's name, the branches of an #if). *)

type kind =
  | Comment (* /* ... */, // ... *)
  | Keyword (* int, struct, if, return, sizeof, ... *)
  | Ident (* x, Proc, USED: a name, whatever it names *)
  | Int (* 42, 0xFF, 10UL, 0777 *)
  | Float (* 1.5, 1e-3, .5f *)
  | Char (* 'a', '\n', L'x' *)
  | String (* "...", L"...", <u.h> after #include *)
  | Operator (* + -> ++ <<= ? : ... *)
  | Punctuation (* ( ) [ ] { } , ; *)
  | Directive (* #include, # define: a directive's # and its word *)
  | Error (* a character C does not know *)

type t = {
  kind : kind;
  text : string; (* as in the file *)
  offset : int; (* its first byte's, from 0 *)
  line : int; (* its first character's, from 1 *)
  col : int; (* from 0, in bytes *)
  pp : bool; (* on a directive's line(s), the directive included *)
}

val show_kind : kind -> string

(* NAME, N2, O_RDWR: a macro's or an enum's constant, by C's habit *)
val is_constant : string -> bool
