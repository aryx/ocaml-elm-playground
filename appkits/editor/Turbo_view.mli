(* Turbo_view: the IDE's screen, 80 by 25 characters, Turbo Pascal 7's
   look (1992) drawn with Curses on the IBM PC's text screen:

       File  Search  Run  Compile  Debug  Help          <- grey bar, red
     ╔════════════════ QUEENS.PAS ═════════════════╗       hot letters
     ║ program Queens;                             ║    <- blue window,
     ║ var ...                                     ║       yellow text,
     ║                                             ║       white reserved
     ╚══ 3:1 ══════════════════════════════════════╝       words
     ┌───────────────── Watches ───────────────────┐
     │ i: 3                                        │    <- when there are
     └─────────────────────────────────────────────┘       watches
      F1 Help  F2 Save  F3 Open  F9 Make  F10 Menu      <- grey status

   The colours are the CGA's sixteen, the frames code page 437's box
   characters (double lines for the edit window), the dialogs and the
   open menu drawn with a shadow. The text is coloured a line at a
   time: reserved words, comments (which may run over lines), strings
   and numbers. A paused program's line is the execution bar; a
   breakpoint's line is red. The status line's keys are the debugger's
   while a program is started.

   Over the editor, by the model's mode: a menu dropped from the bar, a
   dialog, a box, the call stack, the P-code listing (the cursor's
   line's instructions highlighted); or instead of it all, the user
   screen, the program's own. *)

open Turbo_model

val view : model -> Curses.t
