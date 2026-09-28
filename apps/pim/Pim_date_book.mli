(* The Palm's Date Book: a day as a list of hours, the week's days in
   the title bar to jump to, and an event written straight on its hour
   -- tap 9:00, write, and it is there (no dialog: the Pilot's way).

   The events are Ics's, their repetitions computed by Recur for the
   day shown, as TinyCalendar's are; editing a repeating one edits its
   series. *)

type t

(* the day shown: [today] *)
val start : int -> t

val update : Palm.input -> Palm.data -> t -> Palm.data * t
val view : time:float -> Palm.data -> t -> Playground.shape list
