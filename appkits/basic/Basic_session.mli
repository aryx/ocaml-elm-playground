(* Basic_session: BASIC as a person met it in 1977, the whole
   programming environment in one prompt.

   There was no editor, no file, no compiler: a line typed with a
   number went into the program (replacing the line of that number, a
   number alone deleting it), a line without one was done at once. So
   the same prompt is the editor (10 PRINT "HI"), the calculator
   (PRINT 2 + 2) and the shell (LIST, RUN, NEW); editing is retyping
   a line, and numbering by tens leaves room to insert one between two.
   That is Dartmouth BASIC's design (John Kemeny and Thomas Kurtz,
   1964, on a time-sharing system of teletypes), kept by every home
   computer after: the Apple II's ">" prompt, the PET's READY.

   Two BASICs share the prompt, as on an Apple II with its disk: FP
   switches to Applesoft (Microsoft's floating point, the prompt a
   "]"), INT back to Integer BASIC (the prompt a ">"); the program
   stays (Apple's DOS erased it, loading the other language).

   RUN is [Teletype.spawn]: the program runs as a child of the prompt,
   so Control-C (the BREAK key) stops the program and not BASIC, which
   says *** BREAK and prompts again. BYE leaves BASIC, back to
   TinyTerminal's shell when that is where it was started.

   [guess] is a program to begin with, typed in already: Guess the
   Number, our own listing, in Integer BASIC (its halving counts on
   7 / 2 being 3), the same game as Tty_guess --
   the same questions, the same random numbers from the same seed, so
   that the two play the same game on the same answers (the tests
   check it: a differential test, an interpreter checked against a
   program written directly). *)

(* Guess the Number, in Integer BASIC: a line each, "10 PRINT ..." *)
val guess : string list

(* [session ~dialect ~program banner]: the prompt, [program] typed in
   already, until BYE *)
val session : dialect:Basic_run.dialect -> program:Basic_run.program -> string -> unit Teletype.talk
