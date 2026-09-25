(* Teletype: the games of the teletype, which ask and wait, on top of
   the Playground.

   A 1973 BASIC game is written the way a conversation goes:

     10 PRINT "GUESS A LETTER";
     20 INPUT G$
     30 IF G$ = MID$(W$, I, 1) THEN ...

   INPUT stops the program until the player has typed a line, and then
   it carries on from the next statement, its place in the code
   remembered. The playground can't stop: [update] runs once a frame
   and must return at once, so a game that asks has to become a state
   machine (which question are we on? TinyHamurabi and TinyZork keep
   it in their model). This module gives the BASIC style back, without
   stopping anything: a program is a *value*, the conversation written
   down, and this module plays it.

   type 'a talk =
     | Done of 'a                         the end, with a result
     | Print of string * 'a talk          these bytes, then the rest
     | Read_line of (string -> 'a talk)   a line, and what to do with it
     | Read_key of (string -> 'a talk)    a key, no Enter needed
     | Random of int * (int -> 'a talk)   a number from 0 to n - 1
     | Spawn of unit talk * (status -> 'a talk)
                                          another program, then the rest
     | Step of (unit -> 'a talk)          a step of a long computation

   What comes after a question is a function of the answer: its
   continuation, "the rest of the program" held as a closure. The
   program never waits; it *is* waiting, as a value, until this module
   calls the function with the line typed. And [let*] (bind) writes
   the continuations for us, so the program reads like BASIC:

     BASIC:  10 PRINT "WHAT IS YOUR NAME";
             20 INPUT N$
             30 PRINT "HELLO, "; N$

     OCaml:  let* () = print "WHAT IS YOUR NAME? " in
             let* name = read_line in
             print ("HELLO, " ^ name ^ "\n")

   (see examples/TeletypeHangman.ml).

   The idea is old: Haskell 1.0 (1990) did all its input and output
   this way ("dialogue" I/O: a program is a function from responses to
   requests), before monads made it pleasant; the talk type is what is
   now called a free monad. The playground's own Cmd (an HTTP request
   as a value, done by the platform) is the same idea, for one request
   at a time. The other way to get the BASIC style is to suspend a
   real function in the middle, which OCaml 5's effects can do: [read_line
   ()] in direct style, a handler keeping the continuation. An exercise
   for when the playground moves to OCaml 5; what it would lose is
   [run] below, since an effect's continuation can be resumed only once
   and so can't be replayed from a list of answers as cheaply.

   Three things come with the value being a value:

   - **Randomness from a seed**: [Random] is a request like the others,
     answered from a seed kept by this module (the flag seed=n, 1 by
     default), so the same seed gives the same game, and a test can
     play it.
   - **Tests without a screen**: [run program answers] plays it with
     those lines typed and returns what it printed -- the sample run
     that David Ahl's book prints under each game is a test.
   - **The terminal is somebody else's**: the program writes bytes and
     reads lines; the screen is a Vt (Vt.mli) and the line editing
     Line_discipline's (Backspace, Control-U, Control-C to stop), so a
     program can print escape sequences ("\x1b[2J" clears the screen),
     and TinyTerminal can run the same program under its shell.

   A shell is a program that runs programs, and [Spawn] is how: the
   child runs until it ends, then the parent carries on, told how it
   ended. It is Unix's fork, exec and wait in one request, and it
   matters for one key: Control-C interrupts the *innermost* program
   only, as SIGINT goes to the foreground process and not to the
   shell waiting for it. So TinyTerminal's shell survives a Hangman
   interrupted, and prints its prompt again. The machine keeps the
   parents waiting on a stack, one per spawn -- the process tree,
   reduced to the one line of it a terminal sees.

   A program that computes at length without reading -- a BASIC
   interpreter running 10 GOTO 10 -- would keep the machine in one
   frame forever: no screen drawn, no Control-C read. [Step] is its
   way to say "I am still going": the machine takes some thousands of
   steps a frame, then draws, reads the keyboard, and carries on at the
   next frame, as a time-sharing system gives each program its slice.
   And what follows a step is built only when the step is taken, which
   lifts the limit below for a program that steps.

   A limit: the program is built as it runs, and a [Print] builds its
   continuation at once, so a loop that prints forever without ever
   reading or drawing a random number never returns. A teletype
   program reads; a program that doesn't, doesn't need this module.

   References: Paul Hudak et al., "Report on the Programming Language
   Haskell, version 1.0" (1990), section 7 (dialogues); David H. Ahl,
   "101 BASIC Computer Games" (Digital Equipment Corporation, 1973;
   Creative Computing, 1975). *)

(*****************************************************************************)
(* {1 Programs} *)
(*****************************************************************************)

(* how a spawned program ended *)
type status = Exited | Interrupted

type 'a talk =
  | Done of 'a
  | Print of string * 'a talk
  | Read_line of (string -> 'a talk)
  | Read_key of (string -> 'a talk)
  | Random of int * (int -> 'a talk)
  | Spawn of unit talk * (status -> 'a talk)
  | Step of (unit -> 'a talk)

(* "\n" ends a line: the tty turns it into CR LF (Line_discipline.output) *)
val print : string -> unit talk

(* the line typed, without its end of line; Enter must be pressed *)
val read_line : string talk

(* one key, as soon as it is pressed and unechoed: "a", "\r", "\x1b[A"
   for the up arrow (Vt.key) *)
val read_key : string talk

(* an integer from 0 to [n - 1] *)
val random : int -> int talk

val return : 'a -> 'a talk
val ( let* ) : 'a talk -> ('a -> 'b talk) -> 'b talk

(* [spawn child]: run it, then carry on with how it ended *)
val spawn : unit talk -> status talk

(* a step of a long computation: the machine may stop here until the
   next frame; what follows is built only when the step is taken *)
val step : unit talk

(* [ask question]: print it, then read the line; BASIC's INPUT "Q"; A$ *)
val ask : string -> string talk

(*****************************************************************************)
(* {1 Running} *)
(*****************************************************************************)

(* [run ?seed program answers]: the program played with those lines
   typed, in order, without a screen; what it printed (the \n's as
   they were, no \r added). It stops at its end, or when it reads with
   no answers left. A [Read_key] takes the next answer as its key. A
   program taking a million [step]s is stopped too. *)
val run : ?seed:int -> 'a talk -> string list -> string

(* The machine playing a program: its screen, its tty, the program at
   the point it reached. TinyTerminal's shell steps one of these. *)
type machine

(* [start ~seed ~rows ~cols program]: the program on a blank screen,
   run until it first reads *)
val start : ?baud:int -> seed:int -> rows:int -> cols:int -> unit talk -> machine

(* [input m bytes]: bytes from the keyboard: echoed, and the program
   given its line (or key) and run until it reads again. Control-C or
   Control-D ends the innermost program: the one last spawned, and
   when none is, the program itself. *)
val input : machine -> string -> machine

(* [tick m dt]: a frame, [dt] seconds after the last: a program that
   [step]s takes its next steps, and at a baud rate the printing
   catches up (at none, everything is printed at once) *)
val tick : machine -> float -> machine

val screen : machine -> Vt.t

(* whether the program is waiting for a line or a key (the cursor
   blinks), and whether it has ended *)
val reading : machine -> bool
val finished : machine -> bool

(* the bytes a Playground frame typed: [computer.keyboard.typed], and
   the named keys that went down since the last frame (Enter,
   Backspace, the arrows, Control and a letter), in Vt's bytes *)
val keyboard_bytes : Playground.computer -> before:Playground.keyboard -> string

(* the width and height [draw]'s grid of characters takes, centered on
   (0, 0): what a case drawn around it needs *)
val size : Playground.computer -> machine -> Playground.number * Playground.number

(* [draw ?paper ?capitals ?phosphor computer m]: the grid of
   characters, as large as the playground's screen lets it be, centered:
   a cell per character, in [phosphor] (green by default) on black, or
   with [paper] black on a roll of paper; in [capitals] (by default with
   [paper]) as a Teletype Model 33 printed and an Apple II showed them,
   neither having lower case *)
val draw :
  ?paper:bool -> ?capitals:bool -> ?phosphor:Playground.color -> Playground.computer -> machine -> Playground.shape list

(*****************************************************************************)
(* {1 Applications} *)
(*****************************************************************************)

type state

(* [teletype program]: the program on an 80 by 24 screen; when it
   ends, Enter runs it again (with the seed where the last run left
   it: another game). Flags: seed=n, baud=n (110: the Teletype's 10
   characters a second), paper. [view] draws the machine instead of
   [draw] (TinyTerminal: a VT100 around the screen). *)
val teletype :
  ?rows:int ->
  ?cols:int ->
  ?view:(Playground.computer -> machine -> Playground.shape list) ->
  unit talk ->
  (state Playground.game, Playground.msg) Playground.app
