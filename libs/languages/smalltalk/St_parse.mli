(* St_parse: Smalltalk-80's grammar, by recursive descent.

   The grammar fits in ten lines, and the parser is those ten lines
   written out (Blue Book, chapter 2 and its syntax diagrams; [x] is
   an optional x, {x} any number of them):

     method     ::= pattern [temporaries] [primitive] statements
     pattern    ::= unary | binary name | keyword name {keyword name}
     statements ::= [statement {"." statement}] ["."]
     statement  ::= "^" expression | expression
     expression ::= name ":=" expression | cascade
     cascade    ::= keywordExpr {";" message}
     keywordExpr::= binaryExpr {keyword binaryExpr}
     binaryExpr ::= unaryExpr {binarySelector unaryExpr}
     unaryExpr  ::= primary {name}
     primary    ::= literal | name | block | "(" expression ")"
     block      ::= "[" {":" name} ["|"] [temporaries] statements "]"

   The precedence is in the nesting: unary messages bind tightest, then
   binary, then keyword. And all binary selectors have the *same*
   precedence, from left to right: "3 + 4 * 2" is 14, not 11, which
   surprises everyone once (Ingalls: a language with user-defined
   operators cannot know which of them multiplies):

     3 + 4 * 2                  ((3 + 4) * 2)
     a at: i + 1 put: b sqrt    (a at: (i + 1) put: (b sqrt))

   A cascade sends several messages to the receiver of the last one:
   "Transcript show: 'a'; cr" sends show: then cr to Transcript.

   A mistake is reported where it is, with Smalltalk-80's messages
   (the Browser inserts them into the text there, as the original's
   compiler did): "Nothing more expected", "Argument expected", "]
   expected"... *)

exception Error of int * string

(* a method, as the Browser accepts it: its pattern first *)
val parse_method : string -> St_ast.method_

(* what a Workspace's "do it" runs: temporaries, then statements, as
 * the body of a method with no arguments, selector "DoIt" *)
val parse_doit : string -> St_ast.method_

(* one literal, or its absence: "#(1 $a)" *)
val parse_literal : string -> St_ast.literal option

(* the selector of a method's text, read from its pattern, without
 * the rest parsed ("at: i put: x ^x" is "at:put:") *)
val selector_of : string -> string option
