(* Lexer_c: C's text cut into tokens (Token_c), comments included.

   Written with ocamllex, as Lexer_ml: the longest match gives "->" one
   token, "<<=" one, "L'x'" a character. The rules are C99's and Plan 9
   C's (the same, but for a few keywords: 5c's lexer, Lexer.mll in xix's
   compiler); gcc's __attribute__ is a keyword, the parser skips what
   follows it.

   The preprocessor's lines: a # starting a line starts a directive, a
   [Directive] token (its #, spaces and word: "#define", "# if"), and
   the tokens up to the line's end are [pp], or further while a line
   ends with a backslash. After #include, <u.h> is one [String]. A #
   inside a directive (a macro's # and ##, stringizing and pasting) is
   an [Operator].

   Worked example (the tests'):

     #define N 10 /* max */
     static int f(char *s) { return s[0] == 'a'; }

     Directive #define  Ident N  Int 10  Comment /* max */ (all pp)
     Keyword static  Keyword int  Ident f  Punctuation (  Keyword char
     Operator *  Ident s  Punctuation )  Punctuation {  Keyword return
     Ident s  ...  Operator ==  Char 'a'  Punctuation ;  Punctuation }

   Never fails: an unclosed comment runs to the end of the file, an
   unclosed string or character to the end of its line, a character C
   does not know is an [Error] token. *)

val tokens : string -> Token_c.t list
