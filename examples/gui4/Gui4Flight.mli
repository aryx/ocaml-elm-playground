(* 7GUIs 3, Flight Booker, four ways: one-way or return, two dates,
 * and a Book button that is on only when the dates make sense -- the
 * return date turned off for a one-way flight. The task is the
 * validation and its dependencies: which widget is on depends on three
 * others. With callbacks every handler has to remember to call the one
 * function that re-checks everything; in the other three the rule is
 * written once, where the view is made. *)

val make : Theme.t -> Widget.box -> Gui4.architecture -> Gui4.runner
