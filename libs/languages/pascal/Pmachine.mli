(* Pmachine: the P-machine, running P-code.

   The interpreter of Pcode.mli's instructions: a loop of fetch,
   decode, execute over an array of words, the stack, which holds every
   frame (Pcode.mli draws one). This is the part of Pascal-P that was
   rewritten for each new computer, and it is short: a case per
   instruction. A call, for instance, is two:

       mst d     push the mark: 0 for the result, the static link
                 (the frame d static links out from this one), the
                 dynamic link (this frame), 0 for the return address
       ...       the arguments pushed, above the mark
       cup n,l   the new frame starts n + 4 words below the top: note
                 the return address in it, make it the current frame,
                 jump to l
       ent k     the frame k words long, its variables zeroed
       ...
       retp      back: the top of the stack to the frame's start, the
                 caller's frame and address from the links

   Integers are 16 bits and wrap around (32767 + 1 is -32768), as Turbo
   Pascal's did without {$R+}; what stops a program are Turbo Pascal's
   run-time errors, with their numbers: 200 division by zero, 201 range
   check (a subrange or an array index out of bounds: chk), 202 stack
   overflow (a recursion too deep), 106 invalid numeric format (read
   given "abc" for an integer).

   The machine is a Talk program (Talk.mli), the way BASIC's
   interpreter is (Basic_run.mli): write collects characters, and a
   read with no line to read from becomes a [Read_line] whose
   continuation is the machine carrying on, the instruction that read
   done again with the line; every few thousand instructions it takes a
   [step], so a long computation runs a slice a frame and Control-C
   stops it; random(n) is a [Random]. Pascal's input is lines, and
   its rules are the Report's: read(i) skips blanks, the ends of lines
   too, then reads an integer; read(c) at the end of a line gives a
   space; readln skips what is left of the line; and each waits only
   when it has nothing left to read (the "lazy input" terminals needed:
   a program asks its question before waiting for the answer). The
   machine's state is mutable, one per run: a continuation is taken
   once. *)

(* [run program]: from its first instruction to stp, or to a run-time
   error ("Runtime error 201 at line 12: Range check error") *)
val run : Pcode.program -> unit Talk.talk

(* [execute source answers]: compiled and run on those lines typed,
   what it printed -- or the compiler's error, as Turbo Pascal wrote
   it: "Error at 3:5: ';' expected" *)
val execute : string -> string list -> string
