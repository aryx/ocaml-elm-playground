(* Lexer_ml: OCaml's text cut into tokens (Token_ml), comments included.

   Written with ocamllex (the plan's decision, plan_tinybox_codemap.md):
   OCaml's lexical grammar is a list of regular expressions, and ocamllex
   turns it into one automaton that takes the longest match -- "::" one
   token, not two ":", "let*" one keyword, "'a'" a character and "'a"
   a type variable. The rules are ocaml-light's (OCaml 1.07's), with
   what later OCaml added and this repository uses.

   Worked example (the tests'):

     let f ~x = (* hi *) M.g x "s" 'c' 'a

     Keyword let  Lident f  Label ~x  Operator =  Comment (* hi *)
     Uident M  Operator .  Lident g  Lident x  String "s"  Char 'c'
     Type_var 'a

   Never fails: a file that OCaml would reject is still cut, so that a
   view can colour it -- an unclosed comment or string runs to the end,
   a character OCaml does not know is an [Error] token. *)

val tokens : string -> Token_ml.t list
