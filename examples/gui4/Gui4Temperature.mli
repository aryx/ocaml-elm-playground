(* 7GUIs 2, Temperature Converter, four ways: two fields, each
 * converting into the other as it is typed in, and leaving the other
 * alone while what is typed is not a number. The task is the
 * two-way dependency: with callbacks each field's handler writes the
 * other field; in the other three the two temperatures are one model,
 * and each field shows its half. *)

val make : Theme.t -> Widget.box -> Gui4.architecture -> Gui4.runner
