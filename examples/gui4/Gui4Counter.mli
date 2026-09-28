(* 7GUIs 1, Counter, four ways: a label and a button that counts. The
 * smallest task, and the one that shows where the count lives -- in two
 * places with callbacks, one in the other three. *)

val make : Theme.t -> Widget.box -> Gui4.architecture -> Gui4.runner
