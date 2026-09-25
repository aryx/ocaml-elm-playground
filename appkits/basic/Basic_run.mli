(* Basic_run: a BASIC program, run.

   A program is its numbered lines in order, a line its statements; the
   program counter is a line and a statement in it. Running goes down
   them, jumps at a GOTO, remembers where to come back at a GOSUB (a
   stack of return points), where to loop back at a FOR (another stack,
   each loop's variable, limit and step), where the next DATA item is,
   and the variables. Those are the whole machine, which is why a BASIC
   interpreter fitted in 4 KB.

   Two dialects, the Apple II's two BASICs (1977, 1978), switched by
   their DOS commands INT and FP:

   - Integer: Tiny BASIC's and Wozniak's Integer BASIC's arithmetic,
     integers of 16 bits: 32767 + 1 is -32768, the arithmetic wrapping
     around as the processor's did, 7 / 2 is 3; RND(N) from 1 to N;
     errors as *** NO LINE 45 ERR IN 20; PRINT's "," every 8 columns.
   - Applesoft: Microsoft's floating point, 7 / 2 is 3.5, 1 / 3 printed
     as .333333333 (nine digits, no zero before the point); RND(1)
     from 0 to 1, INT(RND(1) * 6) + 1 a die, as David Ahl's games throw
     them; errors as ?NEXT WITHOUT FOR ERROR IN 20; "," every 16.

   Both have strings (A$), arrays (DIM A(10); used without a DIM, 11
   elements, 0 to 10), FOR/NEXT, DATA/READ, DEF FN, and the functions
   INT ABS SGN SQR RND SIN COS TAN ATN EXP LOG LEN VAL ASC CHR$ STR$
   LEFT$ RIGHT$ MID$. Names count by their first two characters, SCALE
   and SC one variable, as in every Microsoft BASIC.

   The interpreter is a Teletype program (Teletype.mli), which is the
   whole trick of running BASIC on the playground: PRINT is a [Print],
   INPUT a [Read_line] whose continuation is "the rest of the program,
   from this statement on, with the variable set" -- the interpreter
   never waits, it is a value waiting. Each statement is also a [step],
   so that 10 GOTO 10 runs a few thousand statements a frame and
   Control-C can stop it, as the BREAK key did. The variables are
   mutable, one set per RUN: a continuation is taken once.

   An INPUT reads its variables from one line, commas between them,
   asking ?? for those missing and saying ?REENTER when a number isn't
   one. Its prompt, INPUT "NAME"; N$, is followed by "? ", as in
   Microsoft's BASIC-80 (Applesoft printed none).

   Left out: FRE, PEEK, POKE and CALL (the machine's memory), GET (a
   key without Enter), the graphics (GR, PLOT, HLIN), ONERR, the
   variables kept after a RUN for the commands typed next, and FOR
   loops popped by a RETURN. *)

(*****************************************************************************)
(* {1 Programs} *)
(*****************************************************************************)

type dialect = Integer | Applesoft

type program

val empty : program

(* [add p n text stmts]: line [n] set (replaced if there was one), its
   [text] what LIST shows after the number *)
val add : program -> int -> string -> Basic_parse.stmt list -> program
val remove : program -> int -> program

(* the lines, as LIST shows them: "10 PRINT X\n20 END\n" *)
val listing : program -> string

(* a listing's lines typed in, "10 PRINT X" each; the first wrong one
   refused with its message *)
val of_lines : string list -> (program, string) result

(* the text after a line's number, as LIST will show it *)
val text_of : string -> string

(*****************************************************************************)
(* {1 Running} *)
(*****************************************************************************)

(* RUN: from the first line, no variables *)
val run : dialect -> program -> unit Teletype.talk

(* statements typed without a line number, done at once: PRINT 2 + 2,
   or GOTO 100, which runs the program from line 100 *)
val direct : dialect -> program -> Basic_parse.stmt list -> unit Teletype.talk

(* a number as PRINT shows it in a dialect: 3, 3.5, .333333333, 1E+10 *)
val show_number : dialect -> float -> string
