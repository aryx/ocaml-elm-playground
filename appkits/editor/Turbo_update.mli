(* Turbo_update: an event of the IDE, a key or a tick. A key goes to
   whatever is on the screen: the running program (its keys are its
   own), a menu, a dialog, a box that any key closes, the P-code
   listing, or the editor. In the editor, a function key or Alt and a
   letter is a command (Turbo_menus.act); any other key is the text's
   (Turbo_edit.edit_key), and a text that changed resets the program
   being debugged. A tick, while a program runs, advances its machine
   (Turbo_debug.advance).

   Function keys without function keys: Esc then a digit is F1 to F10
   (0 for F10), Midnight Commander's convention, for the keyboards
   whose top row is the volume's and the screen's; Control and a digit
   is Control and that F key (Ctrl-9: Ctrl-F9, Run). *)

open Turbo_model

val update : Tui.event -> model -> model
