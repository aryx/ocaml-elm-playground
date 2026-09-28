(* 7GUIs 4, Timer, four ways: a bar filling as time passes, the
 * seconds, a slider for the duration, and Reset. The task is state
 * plus time: something changes with nobody touching anything. Each way
 * has its own answer to where the clock's tick goes -- a variable read
 * every frame, a callback of its own, a change to the model, a message
 * -- and here each runner's step is one tick, a sixtieth of a second. *)

val make : Theme.t -> Widget.box -> Gui4.architecture -> Gui4.runner
