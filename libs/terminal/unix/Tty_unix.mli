(* Tty_unix: a Tui program in the terminal it was written for.

   The same program the Textmode way runs on the playground
   (Textmode.mli), run in xterm, the macOS Terminal, the Linux console:
   the bytes curses computes go to stdout, and the keys come from stdin.
   Three things make a terminal fit for a full-screen program, and are
   undone on the way out, even after an exception:

   - raw mode (termios: ICANON, ECHO and ISIG off): the tty's line
     discipline (Line_discipline.mli), in the kernel this time, stops
     keeping lines and echoing, and hands each key over at once --
     Control-C included, which the program sees as a key and quits on;
   - the alternate screen (ESC [ ? 1049 h, xterm's): the program draws
     on a screen of its own, and quitting brings the shell's back, as
     vi and less do (the playground's Vt ignores it, having nothing to
     go back to);
   - the cursor hidden unless the program shows it.

   The loop: wait for a key for at most a twentieth of a second
   (Unix.select), hand the keys to the program, then a tick of the time
   passed, then draw -- curses' difference, written at once.

   The terminal must be at least as large as the program's screen:
   Tui programs here are 80 by 24, the VT100's, which every terminal
   window starts at. *)

val run : < Cap.stdin ; Cap.stdout ; .. > -> 'model Tui.program -> unit
