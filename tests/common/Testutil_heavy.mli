(* Tests that take seconds of CPU where the others take milliseconds (a
   search proving every level solvable, a robot playing a game through).
   They carry the tag "heavy", so that the test executable runs just
   them with "-t heavy"; and "make test-lite" (HEAVY=skip) skips them,
   the quick check for a change that only moves or renames things.
   "make test" runs them. *)

(* the "heavy" tag *)
val tag : Testo.Tag.t

(* [t name body]: Testo.create, with the "heavy" tag, skipped when the
   environment variable HEAVY is "skip" *)
val t : ?tags:Testo.Tag.t list -> string -> (unit -> unit Testo.Promise.t) -> Testo.t
