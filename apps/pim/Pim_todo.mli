(* The Palm's To Do list: items with a check box, a priority from 1 to 5
   and maybe a due date, sorted by priority then date -- the list as a
   plan for the day. Tap the box to check an item, its number to change
   its priority, its text to write in it; an item past its date is
   marked with a "!".

   The items are Ics's VTODOs: a list any calendar program can read. *)

type t

val start : t

val update : Palm.input -> Palm.data -> t -> Palm.data * t
val view : time:float -> today:int -> Palm.data -> t -> Playground.shape list
