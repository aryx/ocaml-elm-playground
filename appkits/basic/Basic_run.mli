(* Basic_run: a Tiny BASIC program, run.

   A program is its numbered lines in order; running it is a program
   counter going down them, jumping at a GOTO, remembering where to
   come back at a GOSUB (a stack of return points, the one data
   structure the language hides), and 26 variables. Everything is an
   integer of 16 bits, as on the machines of 1976: 32767 + 1 is -32768,
   the arithmetic wrapping around as the processor's did.

   The interpreter is a Teletype program (Teletype.mli), which is the
   whole trick of running BASIC on the playground: PRINT is a [Print],
   and INPUT a [Read_line] whose continuation is "the rest of the
   program, from this statement on, with the variable set" -- the
   interpreter never waits, it is a value waiting. Each statement is
   also a [step], so that 10 GOTO 10 runs a few thousand lines a frame
   and Control-C can stop it, as the BREAK key did.

   PRINT's separators: ";" joins, "," goes to the next column of 8,
   and a separator at the end keeps the line open (PRINT "NAME"; then
   INPUT N shows NAME? on one line, INPUT adding "? "). Numbers are
   printed as they are, no spaces around them (Microsoft's BASIC added
   one before, for the sign; Tiny BASIC didn't).

   An error stops the program with a message and the line it was on:
   "*** NO LINE 45 ERR IN 20" (the Apple II's Integer BASIC style,
   three stars, 1977).

   Left out: FOR/NEXT, strings, arrays, and statements after a ":" on
   one line, which is the Microsoft BASIC of the Ahl games (the second
   stage of plan_terminal.md's TinyBasic); the variables are cleared
   by each RUN and not kept for the commands typed after it. *)

(*****************************************************************************)
(* {1 Programs} *)
(*****************************************************************************)

type program

val empty : program

(* [add p n text stmt]: line [n] set (replaced if there was one), its
   [text] what LIST shows after the number *)
val add : program -> int -> string -> Basic_parse.stmt -> program
val remove : program -> int -> program

(* the lines, as LIST shows them: "10 PRINT X\n20 END\n" *)
val listing : program -> string

(* a listing's lines typed in, "10 PRINT X" each; the first wrong one
   refused with its message *)
val of_lines : string list -> (program, string) result

(*****************************************************************************)
(* {1 Running} *)
(*****************************************************************************)

(* RUN: from the first line, the variables at 0 *)
val run : program -> unit Teletype.talk

(* a statement typed without a line number, done at once: PRINT 2 + 2,
   or GOTO 100, which runs the program from line 100 *)
val direct : program -> Basic_parse.stmt -> unit Teletype.talk
