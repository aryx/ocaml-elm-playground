(* Basic_parse: a line of BASIC, read.

   Two BASICs, one grammar. Tiny BASIC (Dennis Allison, "Design Notes
   for Tiny BASIC", People's Computer Company, 1975; Li-Chen Wang's
   Palo Alto Tiny BASIC, Dr. Dobb's Journal, 1976) was BASIC cut down to
   fit in 2 or 3 KB of a hobbyist's Altair: integers, 26 variables, a
   dozen statements, its interpreter published for anyone to type in --
   free software before the phrase, in answer to Bill Gates' "Open
   Letter to Hobbyists" (1976). Microsoft's BASIC, the one Gates was
   selling (Altair BASIC, 1975; Applesoft on the Apple II, 1978; the
   PET's, the TRS-80's), is the one David Ahl's "BASIC Computer Games"
   (1978) was written in: floating point, strings, arrays, FOR loops,
   several statements on a line. The grammar here is Microsoft's, of
   which Tiny BASIC's is a part; the two differ only in how they
   compute (Basic_run.mli's dialects).

     { ... } any number of times, [ ... ] once or not at all

     line       ::= [number] statement { : statement }
     statement  ::= PRINT { item | , | ; }      INPUT ["prompt";] lvalue { , lvalue }
                  | [LET] lvalue = expr         IF expr THEN (number | statement)
                  | IF expr GOTO number         GOTO expr | GOSUB expr | RETURN
                  | ON expr (GOTO | GOSUB) number { , number }
                  | FOR var = expr TO expr [STEP expr] | NEXT [var { , var }]
                  | DIM var(expr { , expr }) { , var(expr { , expr }) }
                  | DATA datum { , datum } | READ lvalue { , lvalue } | RESTORE
                  | DEF FNname(var) = expr | END | STOP | REM text
                  | LIST | RUN | NEW | BYE | FP | INT
     expr       ::= disjunctions of conjunctions of NOT of comparisons
                    of sums of products of [-] powers of atoms
     atom       ::= number | "string" | var | var(expr { , expr })
                  | function(expr { , expr }) | FNname(expr) | (expr)
     var        ::= letter { letter | digit } [$]

   An IF only guards the rest of its line: IF X > 5 THEN PRINT "BIG" :
   GOTO 100 does both or neither, so IF is read as a statement "if not,
   skip to the next line", and what follows THEN as the line's next
   statements. THEN 100 is a GOTO.

   The parser is a recursive descent, a function per rule, reading the
   line's characters: spaces are skipped everywhere, even inside
   keywords (G O T O is GOTO), and a keyword is found even inside what
   looks like a name, as Microsoft's "cruncher" did when it turned
   keywords into one-byte tokens: FORI=1TO9 is FOR I = 1 TO 9, and a
   variable named TOTAL is TO TAL, SCORE is SC OR E -- the famous
   surprise, every beginner's first ?SYNTAX ERROR. Only a name's first
   two characters count, too (SCALE and SC are one variable), as in
   every Microsoft BASIC of the time; this module keeps names whole and
   Basic_run cuts them.

   The line is read in capitals, the strings' insides excepted, as a
   1977 keyboard typed it.

   Worked example (checked by the tests):

     "10 if x<5 then print \"small\";x"
       -> Numbered (10, Some [ If (Bin (Lt, Var "X", Num 5.));
                               Print [ (Expr (Str "small"), Semi);
                                       (Expr (Var "X"), Newline) ] ])

   References: Dennis Allison, "Design Notes for Tiny BASIC", Dr. Dobb's
   Journal, vol. 1, no. 1 (1976); Li-Chen Wang, "Palo Alto Tiny BASIC",
   Dr. Dobb's Journal, vol. 1, no. 5 (1976); Apple Computer, "Applesoft
   II BASIC Programming Reference Manual" (1978); David H. Ahl, "BASIC
   Computer Games, Microcomputer Edition" (1978), its introduction on
   the dialects. *)

(*****************************************************************************)
(* {1 The syntax} *)
(*****************************************************************************)

type op =
  | Add | Sub | Mul | Div | Pow
  | Eq | Ne | Lt | Le | Gt | Ge
  | And | Or

type expr =
  | Num of float
  | Str of string
  | Var of string (* "X", "N1", "A$": a scalar *)
  | Index of string * expr list (* A(I), B$(I, J) *)
  | Call of string * expr list (* the built-in functions: INT, LEFT$... *)
  | Fn of string * expr (* FNA(X), DEF FN's *)
  | Neg of expr
  | Not of expr
  | Bin of op * expr * expr

(* TAB(n) and SPC(n) move the printing along; they exist only in PRINT *)
type item = Expr of expr | Tab of expr | Spc of expr

(* what follows an item in a PRINT: ";" nothing, "," the next column of
   16, and after the last item without either, the end of the line *)
type sep = Semi | Comma | Newline

type lvalue = Scalar of string | Elem of string * expr list

(* what DATA holds: numbers, and strings, quoted or not *)
type datum = D_num of float | D_str of string

type stmt =
  | Print of (item * sep) list
  | Input of string option * lvalue list
  | Let of lvalue * expr
  | If of expr (* false: skip the rest of the line *)
  | Goto of expr
  | Gosub of expr
  | On of expr * bool * int list (* true: GOSUB *)
  | Return
  | For of string * expr * expr * expr option
  | Next of string list
  | Dim of (string * expr list) list
  | Data of datum list
  | Read of lvalue list
  | Restore
  | Def of string * string * expr (* DEF FNA(X) = e: "A", "X", e *)
  | End
  | Stop
  | Rem
  (* the commands, typed without a line number *)
  | List
  | Run
  | New
  | Bye
  | Fp (* Applesoft *)
  | Int (* Integer BASIC, Tiny BASIC's arithmetic *)

type line =
  | Numbered of int * stmt list option (* a number alone deletes its line *)
  | Direct of stmt list

(*****************************************************************************)
(* {1 Reading} *)
(*****************************************************************************)

(* the line in capitals, the strings' insides kept as typed *)
val capitals : string -> string

(* a line, or what is wrong with it *)
val parse_line : string -> (line, string) result

(* the built-in functions' names, and whether a name is a string's (A$) *)
val functions : string list
val is_string : string -> bool
