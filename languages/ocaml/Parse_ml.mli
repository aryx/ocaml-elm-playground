(* Parse_ml: OCaml's grammar, recursive descent, for a highlighter.

   Not a compiler's parser: it builds Ast_ml, the names and what they
   are, and it does not give up on a file -- an item that does not parse
   (a construct it does not know) is skipped to the next item starting
   at its column or left of it, recorded in [skipped], and the rest of
   the file parsed. Written by hand, not ocamlyacc: a grammar the size
   of OCaml's today (labels, optional arguments, polymorphic variants,
   objects' types, local opens, let*, GADTs) fights an LALR table at
   every construct, and a yacc parser stops at the first surprise, where
   a highlighter wants all it can get.

   The precedences are OCaml's (the manual's table, ocaml-light's
   parser.mly), from the tightest: - (prefix); ** lsl lsr asr (right);
   * / % mod land lor lxor; + -; :: (right); @ ^ (right); = < > | & $
   != (the comparisons); & && (right); or || (right); ,; <- := (right);
   if; ;. An operator's precedence is its first characters', as the
   manual says. let, fun, function, match, try, if go as far right as
   they can (x >>= fun y -> ...).

   A long sequence (e1; e2; ...), a long list, a long application or a
   file's items are loops, not a recursion an element: in a browser, a
   stack a few thousand frames deep overflows (notes_mobile.md). *)

(* the tokens of a file (Lexer_ml.tokens), comments included: a name's
   [tok] is its index there *)
val parse : Token_ml.t list -> Ast_ml.file
