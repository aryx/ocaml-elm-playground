(* Textmode: the full-screen programs of the terminal, on top of the
   Playground.

   A Tui program (Tui.mli) is Model-View-Update with a screen of
   characters for a view. This way runs one on the playground, and runs
   it as a terminal would: each frame the program's new screen is
   turned by curses into the bytes that changed (Curses.refresh), and
   those bytes are fed to a VT100 (Vt), which the playground draws. It
   could draw the program's screen directly, and it would look the
   same; going through the bytes is what makes the same program run
   in a real terminal unchanged (Tty_unix.mli), and what lets this
   way show, under the screen, the bytes curses sent this frame
   against a whole redraw's: the economy that made full-screen programs
   possible at 9600 baud.

   What it adds to Playground.game is small, as for the other ways:
   keys as a terminal sends them (Teletype.keyboard_bytes: "a", "\r",
   "\x1b[A"), time as ticks, and the program's end -- Enter then starts
   it again. *)

type 'model state

val textmode :
  ?phosphor:Playground.color -> 'model Tui.program -> ('model state Playground.game, Playground.msg) Playground.app
