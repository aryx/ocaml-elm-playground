(* Parse_c: C's grammar, recursive descent, for a highlighter, with no
   preprocessor.

   As Parse_ml: not a compiler's parser, it builds Ast_c (the names and
   what they are), and it does not give up on a file: a statement that
   does not parse is skipped to its semicolon, a top-level item to the
   next one (a line starting at column 0 after a ; or a }), each
   recorded in [skipped].

   The preprocessor is not run: no #include read, no macro expanded. C
   read as it is written, which is mostly C, with these rules:

   - the directives are read apart: a #define gives its name, its
     parameters and the names of its body; an #if's first branch is
     read, its #else and #elif are not (both would give a function two
     heads); #if 0 is the other way round, and so is #ifdef __cplusplus;

   - C's grammar needs to know which names are types (T * x; is a
     declaration if T is one): a typedef's names are, and a few known
     everywhere (Plan 9's ulong and uchar, size_t, ...); for the rest,
     since the headers are not read, the shapes that can only be
     declarations are ones -- a name followed by a name (Proc p), by
     stars and then a declared name (Proc *p;, Proc** ), a name alone in
     parentheses before an operand ((Proc)x) or with stars ((Proc* )x);

   - a macro used as a statement may have no semicolon (ARGEND, a line
     ending with FOO(x)), or be a loop's head: FOO(x) { ... }, FOO(x)
     stmt;, ARGBEGIN{ ... } -- a name or a call followed by a block, or
     by a statement on the same line;

   - gcc's __attribute__((...)) and __asm__(...) are skipped.

   A long sequence of statements, of initializers or of items, or a
   long chain of operators, is a loop, not a recursion an element: a
   browser's stack is small (Parse_ml.mli). *)

(* the tokens of a file (Lexer_c.tokens), comments included: a name's
   [tok] is its index there *)
val parse : Token_c.t list -> Ast_c.file
