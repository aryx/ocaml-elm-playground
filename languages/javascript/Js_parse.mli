(* Js_parse: JavaScript's tokens grouped into a tree -- statements by
   recursive descent, expressions by Pratt's top-down operator
   precedence.

   (notes_javascript.md sections 2 and 3.)

   {2 Statements: recursive descent}

   A statement says what it is by its first token -- let, const, var,
   function, if, while, for, return, break, continue, throw, try, {,
   ; -- or else it is an expression. One function per statement, each
   calling the one it needs next, as Formula.mli's grammar does: the
   grammar and the code are the same shape.

   **Semicolons.** A statement ends at ";", or -- JavaScript's
   automatic semicolon insertion, the simple half kept here -- before a
   "}", at the end of the text, or before a token on a new line. So
   the lexer remembers newlines. The rule's famous trap is kept too,
   since the rule gives it: "return" alone on its line returns
   nothing, and the expression on the next line is a statement of its
   own (ECMAScript's [no LineTerminator here], which also keeps "x\n++y"
   from being "x++; y"). The other half -- a line starting with ( or [
   continuing the one before -- is an exercise.

   {2 Expressions: Pratt}

   1 + 2 * 3 is 1 + (2 * 3): multiplication binds tighter. Recursive
   descent says it with one function per level of precedence (Formula:
   an expr is terms, a term factors); JavaScript has a dozen levels.
   Vaughan Pratt's way gives each operator a number, its **binding
   power**, and one loop does them all:

     expression (min):
       left <- a prefix thing: a number, a name, ( ... ), -x, [ ... ], { ... }, a function
       while the next operator binds at least as tight as min:
         take it; right <- expression (its power + 1, or its power if right-associative)
         left <- (left op right)

     power  operators                         associativity
       1    = += -= *= /= %=                  right: a = b = 1 is a = (b = 1)
       2    ? :                               right
       3    ||                                left
       4    &&                                left
       5    === !== == !=                     left
       6    < > <= >=                         left
       7    + -                               left: 1 - 2 - 3 is (1 - 2) - 3
       8    * / %                             left
       9    prefix - + ! typeof ++ --         (prefix)
      10    postfix . [ ] ( ) ++ --           left: a.b(c)[d] is ((a.b)(c))[d]

   Left-associative: the right side is read with power + 1, so the next
   operator of the same power stops it and becomes the loop's next;
   right-associative: with the same power, so it goes on. That "+ 1" is
   the whole difference.

   An arrow function looks like something else until its "=>": "x"
   is a name, and "(a, b)" a parenthesized expression, until the arrow
   comes. The spec parses them as expressions and turns them into
   parameters afterwards ("cover grammars"); here, seeing "(" or a
   name, the parser looks ahead for the ")" and the "=>" first -- the
   tokens are all in an array, so looking ahead costs nothing.

   Worked example (the tests'):

     1 + 2 * 3 - 4          ((1 + (2 * 3)) - 4)
     a = b = c || d && e    (a = (b = (c || (d && e))))
     -x.y(1)[0]             (-(((x.y)(1))[0]))
     f(x => x * 2, 3)       (f((x) => (x * 2), 3))

   {2 Why not yacc}

   The house has no parser generator: every parser here is written by
   hand (Formula, BASIC, HyperTalk, the HTML tokenizer, CSS), and the
   author's other projects use ocamlyacc where it fits (xix's
   assemblers, in Plan 9's tradition). yacc is also the classic way to
   teach parsing: the grammar is the program, the precedences
   declarations. So why not here? Because JavaScript's grammar fights
   LALR(1) at every turn a teaching parser cares about:

   - **Arrow functions.** After "(a, b" the parser cannot know
     whether it reads a parenthesized expression (a comma expression)
     or a parameter list until it meets the "=>", arbitrarily far
     away. An LALR(1) parser decides with one token of lookahead; the
     grammar has conflicts, or grows the cover-grammar tricks that make
     it unreadable. By hand, a scan ahead in the token array.
   - **Semicolons and newlines.** "A newline may end a statement" and
     "return alone on its line returns nothing" need the parser and
     the lexer to talk: the spec defines insertion as "where the next
     token is not allowed", which yacc can only imitate with error
     productions, and [no LineTerminator here] needs the lexer to know
     the parser's state.
   - **{ at the start of a statement** is a block; anywhere else, an
     object literal: more context the tokens alone don't give.
   - **Error messages.** A teaching engine should say "expected ')' on
     line 3"; yacc says "syntax error" unless much is added by hand.

   And what real engines do: every one parses JavaScript by hand --
   V8, SpiderMonkey, JavaScriptCore, QuickJS, and the tools' parsers
   Acorn, Esprima, Babel -- recursive descent for statements and
   precedence climbing (Pratt's idea) for expressions.

   Nothing is lost for the lesson: the binding powers above *are*
   yacc's %left and %right declarations, as data --

     yacc                          Pratt (the table above)
     %right '=' PLUS_EQ ...        1, right
     %left OR                      3
     %left AND                     4
     %left '+' '-'                 7
     %left '*' '/' '%'             8
     %right UMINUS '!' TYPEOF      9 (prefix)

   -- the later a %left line, the higher its power; and "%prec UMINUS"
   is the prefix operator's own power. Where yacc shines is a language
   designed for it: Wirth's Pascal (the TinyTurboPascal of
   plan_terminal.md) or a C subset. The expressions of this parser,
   written again in ocamlyacc and compared, are an exercise
   (notes_javascript.md).

   Reference: Vaughan Pratt, "Top Down Operator Precedence" (POPL,
   1973); Douglas Crockford, "Top Down Operator Precedence" (2007);
   Robert Nystrom, "Pratt Parsers: Expression Parsing Made Easy"
   (2011) and Crafting Interpreters, chapter 17; ECMAScript, sections
   13 and 14 (expressions, statements) and 12.10 (automatic semicolon
   insertion); Stephen C. Johnson, "Yacc: Yet Another
   Compiler-Compiler" (Bell Labs, 1975). *)

(* a mistake: its line and what was expected *)
type error = { line : int; message : string }

(* the program, or its first mistake (Js_lexer's included) *)
val parse : string -> (Js_ast.program, error) result

(* one expression alone (the tests', and a console's) *)
val parse_expression : string -> (Js_ast.expr, error) result
