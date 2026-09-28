(* The Palm's Address book: the cards sorted by last name, found as you
   write in the Look Up line -- each letter narrows it, no search
   button (the Palm's answer to a screen of 12 lines) -- a card shown
   whole, and edited field by field.

   The cards are Vcard's: an address book any other one can read. *)

type t

val start : t

val update : Palm.input -> Palm.data -> t -> Palm.data * t
val view : time:float -> Palm.data -> t -> Playground.shape list
