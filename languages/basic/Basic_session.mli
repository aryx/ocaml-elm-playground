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

   RUN is [Talk.spawn]: the program runs as a child of the prompt,
   so Control-C (the BREAK key) stops the program and not BASIC, which
   says *** BREAK and prompts again. BYE leaves BASIC, back to
   TinyTerminal's shell when that is where it was started.

   The disk is Basic_disk's: CATALOG lists it, LOAD NAME and RUN NAME
   read a program from it (in its own BASIC: RUN MANDEL from Integer
   BASIC goes to Applesoft, as DOS did), SAVE NAME writes the program
   in memory to it for the rest of the session. *)

(* [session ~dialect ~program banner]: the prompt, [program] typed in
   already, until BYE *)
val session : dialect:Basic_run.dialect -> program:Basic_run.program -> string -> unit Talk.talk
