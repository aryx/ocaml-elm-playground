(* Highlight_c: every token of a C file given its category
   (Highlight_code's, shared by every language), the colour a code view
   draws it in.

   Two passes, as Highlight_ml: a token's kind gives a first category
   (a keyword, a number, a comment; a name in capitals a constant, by
   C's habit), then the tree (Parse_c) says what each name is -- a
   function or a global defined, a parameter, a local within its
   block, a field (declared, p->x, s.x, .x = in an initializer), a type,
   an enum's constant, a label, a #define's name and parameters. What
   did not parse keeps the first category.

   Worked example (the tests'):

     static int f(Proc *p) { int n = p->len; return n; }

     static: Keyword  int: Type  f: Def_function  Proc: Type
     p: Parameter  n: Local  p: Parameter  len: Field  n: Local *)

(* the tokens of a file, each with its category *)
val categorize : Token_c.t list -> (Token_c.t * Highlight_code.category) list

(* [src] lexed, categorized and cut into lines, ready to draw *)
val lines : string -> Highlight_code.span list array

(* claude: the same, and where the names bound in the file are (the
   parameters and locals, a #define's parameters, and the top-level
   functions, globals, typedefs, tags and macros, a definition before a
   prototype: Highlight_code.occurrence), from one parse *)
val analyze : string -> Highlight_code.span list array * Highlight_code.occurrence list
