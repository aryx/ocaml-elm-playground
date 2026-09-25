(* Tui: a full-screen program of the terminal, Model-View-Update.

   Rogue, vi, top and Norton Commander don't ask a question and wait
   for a line, as the Teletype programs do (Talk.mli): they wait for
   a key, or for time to pass, then draw the whole screen again. That
   is an event loop, which is Model-View-Update as it stands: a model,
   an update from an event, a view -- the view a Curses screen rather
   than shapes, and curses sending the difference (Curses.mli).

       event (a key, time) --update--> model --view--> Curses.t
                                                          |
                                     refresh: the bytes that changed
                                                          v
                            the playground's Vt (Textmode.mli), or a real
                            terminal (Tty_unix.mli), the same program

   The same program runs on the playground, drawn by its Vt, and in the
   terminal it was written for, xterm or the macOS Terminal: a [program]
   knows nothing of either. Bubble Tea (Charm, Go, 2020) is exactly
   this, the Elm architecture for terminals, and says so; Brick
   (Haskell, 2015) before it.

   Keys come as the bytes the terminal sends ("a", "\r", "\x1b[A" for
   the up arrow: Vt.key), a key per event; time as the seconds since the
   last tick, some twenty times a second, at most a quarter of a second
   (a longer wait is a pause, not time played). *)

type event = Key of string | Tick of float

type 'model program = {
  init : 'model;
  update : event -> 'model -> 'model;
  view : 'model -> Curses.t;
  (* whether the program is over: a real terminal gets its shell back *)
  over : 'model -> bool;
}
