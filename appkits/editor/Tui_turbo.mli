(* TinyTurboPascal: Turbo Pascal's integrated development environment,
   in text mode.

   Anders Hejlsberg's Turbo Pascal (Borland, 1983, $49.95) put the
   editor, the compiler and the program in the same 39 KB: type, press
   a key, and the program runs a second later -- or the cursor lands on
   the error, the message above it. Where programmers had edited,
   left the editor, compiled, linked and run, each a program of its own
   loaded from floppies, it was one keystroke, and it is where the
   "integrated development environment" began.

   This is its later look, Turbo Pascal 7's (1992), drawn in characters
   on the IBM PC's text screen: a grey menu bar with red hot letters, a
   blue window framed by the PC's double-line box characters (╔═╗),
   the text yellow and the reserved words white, a grey status line of
   function keys, and dialogs with a shadow. The colours are the CGA's
   sixteen, the same on every PC (Teletype.draw_screen's [pc]).

       F9 (Make), Alt-F9 (Compile)   the text compiled by
                                     Pascal_compile: a box with the
                                     lines and the code's size, or the
                                     first error in a red bar at the
                                     top of the window, the cursor on it
       Ctrl-F9 (Run)                 compiled, then run on the user
                                     screen, black, by Pmachine: its
                                     writeln and readln there; a key
                                     afterwards comes back, to the
                                     line of a run-time error if there
                                     was one; Alt-F5 shows it again
       Compile / P-code              ours, not Borland's: the program's
                                     P-code (Pcode.mli), the
                                     instructions of the cursor's line
                                     highlighted

   And its debugger, Turbo Pascal 5's (1989) and 7's, over Pdebug.mli
   and a P-machine that pauses (Pmachine.resume):

       F7, F8        trace into, step over: a line at a time, the
                     execution bar on the line to run next; F7 goes
                     into the procedures a line calls, F8 runs them
       F4            go to the cursor's line
       Ctrl-F8       a breakpoint on the cursor's line, red; Ctrl-F9
                     runs to the next one
       Ctrl-F7       a watch: an expression (x, a[i], p.x) whose value
                     the Watches window shows at each pause
       Ctrl-F3       the call stack, each frame's static and dynamic
                     links beside its call (ours: the P-machine's view)
       Ctrl-F2       reset; Ctrl-C breaks a running program where it is

   The program's screen is shown only while it writes or reads, as
   Turbo's "smart" screen swapping did: a step that prints nothing
   doesn't flash it. Editing the text resets the program.

   The editor has Turbo's keys, which were WordStar's: the arrows or
   Ctrl-E, Ctrl-X, Ctrl-S, Ctrl-D; Ctrl-A and Ctrl-F a word; Home End
   PgUp PgDn; Ctrl-Y deletes a line; Insert toggles overwriting; Enter
   keeps the indentation (autoindent). The files are the floppy of
   Pascal_disk.mli, QUEENS.PAS open to begin with. F2 saves, F3 opens,
   F10 or Alt and a letter opens a menu, Alt-X quits. A desktop often
   keeps some function keys for itself (Alt-F5, Alt-F9, Ctrl-F2): every
   command is in the menus too. *)

type model

val program : model Tui.program

(* for the tests: the text, the cursor (line and column from 0), the
   error bar, the screen's name ("edit", "menu", "dialog", "run",
   "user", "p-code"), the disk, and the execution bar's line (from 0)
   while a program is paused *)
val lines : model -> string list
val cursor : model -> int * int
val error : model -> string option
val screen : model -> string
val file : model -> string -> string option
val execution_line : model -> int option
