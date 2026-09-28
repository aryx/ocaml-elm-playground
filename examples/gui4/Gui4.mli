(* What the tasks of this directory share: the four ways, and one thing
 * to run whichever way it was written -- a step per frame, the input
 * in and the paint out, and a line saying what the program believes,
 * for the tests to compare. *)

type architecture = Immediate | Callbacks | Mvc | Mvu

(* the four, in the order notes_gui.md tells them *)
val architectures : (architecture * string) list

type runner = { step : Widget.input -> Widget.paint list; summary : unit -> string }

(* how much room a label wants: its text, one row high *)
val label_size : Theme.t -> string -> float * float

(* [places panel layout]: where each slot of the layout goes in the panel *)
val places : Widget.box -> 'slot Layout.t -> 'slot -> Widget.box

(* an immediate-mode program as a runner: [frame] asks for the widgets
 * of one frame, against its own state *)
val immediate : Theme.t -> (Immediate.t -> Immediate.t) -> (Widget.input -> Widget.paint list)

(* a retained window as a runner: [before] runs first each frame (a
 * clock's tick), then the callbacks, then the paint *)
val retained : ?before:(unit -> unit) -> Theme.t -> Retained.ui -> (Widget.input -> Widget.paint list)
