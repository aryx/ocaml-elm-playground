(* Basic_parse: a line of Tiny BASIC, read.

   Tiny BASIC (Dennis Allison, "Design Notes for Tiny BASIC", People's
   Computer Company, 1975; Li-Chen Wang's Palo Alto Tiny BASIC, Dr.
   Dobb's Journal, 1976) was BASIC cut down to fit in 2 or 3 KB of a
   hobbyist's Altair: integers only, 26 variables named A to Z, a dozen
   statements, and its interpreter published for anyone to type in --
   free software before the phrase, in answer to Bill Gates' "Open
   Letter to Hobbyists" (1976). Allison's notes give its whole grammar
   in a dozen lines, which this module follows, with Palo Alto's RND,
   ABS, REM, an implicit LET, and PRINT's ";":

     line       ::= number statement | statement
     statement  ::= PRINT item ((, | ;) item)* [, | ;]
                  | INPUT var (, var)*
                  | [LET] var = expr
                  | IF expr relop expr THEN (statement | number)
                  | GOTO expr | GOSUB expr | RETURN | END | REM text
                  | LIST | RUN | NEW | BYE
     item       ::= "string" | expr
     expr       ::= [+ | -] term ((+ | -) term)*
     term       ::= factor (('*' | '/') factor)*
     factor     ::= var | number | (expr) | RND(expr) | ABS(expr)
     relop      ::= = | <> | < | <= | > | >=
     var        ::= A | B | ... | Z

   The parser is a recursive descent, a function per rule, reading the
   line's characters directly: no tokens first, as the 1976 interpreters
   did (they had no memory for a token list), and spaces skipped
   everywhere, even inside keywords' neighbours -- IFX<5THEN10 reads as
   IF X < 5 THEN 10. The line is read in capitals, the strings' insides
   excepted, as a 1977 keyboard would type it.

   GOTO takes an expression, not just a number: GOTO 100 + 10 * N is a
   computed jump, the switch statement of 1975.

   Worked example (checked by the tests):

     "10 if x<5 then print \"small\";x"
       -> Numbered (10, If (Var 'X', Lt, Num 5,
                            Print [ (Str "small", Semi); (Expr (Var 'X'), Newline) ]))

   References: Dennis Allison, "Design Notes for Tiny BASIC", People's
   Computer Company newsletter (1975) and Dr. Dobb's Journal, vol. 1,
   no. 1 (1976); Li-Chen Wang, "Palo Alto Tiny BASIC", Dr. Dobb's
   Journal, vol. 1, no. 5 (1976). *)

(*****************************************************************************)
(* {1 The syntax} *)
(*****************************************************************************)

type op = Add | Sub | Mul | Div

type expr =
  | Num of int
  | Var of char (* 'A' to 'Z' *)
  | Neg of expr
  | Bin of op * expr * expr
  | Rnd of expr (* from 1 to the argument *)
  | Abs of expr

type relop = Eq | Ne | Lt | Le | Gt | Ge

type item = Str of string | Expr of expr

(* what follows an item in a PRINT: ";" nothing, "," the next column of
   8, and after the last item without either, the end of the line *)
type sep = Semi | Comma | Newline

type stmt =
  | Print of (item * sep) list
  | Input of char list
  | Let of char * expr
  | If of expr * relop * expr * stmt
  | Goto of expr
  | Gosub of expr
  | Return
  | End
  | Rem
  (* the commands, typed without a line number *)
  | List
  | Run
  | New
  | Bye

type line =
  | Numbered of int * stmt option (* a number alone deletes its line *)
  | Direct of stmt

(*****************************************************************************)
(* {1 Reading} *)
(*****************************************************************************)

(* the line in capitals, the strings' insides kept as typed *)
val capitals : string -> string

(* a line, or what is wrong with it *)
val parse_line : string -> (line, string) result
