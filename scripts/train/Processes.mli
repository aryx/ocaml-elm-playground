(* Many processes at once, for the trainers: jobs that do not depend
 * on each other (games against oneself, the steps of several
 * learners), each in a process of its own, their results sent back.
 * A fork and a pipe a job; a job that changes something changes it in
 * its own copy. *)

(* [together jobs]: their results, in order. A job that raises, or
 * whose process is killed, fails the whole with which one and why. *)
val together : (unit -> 'a) list -> 'a list

(* [those_that_finish what jobs]: the same for jobs a run can do
 * without one of: the results of those that finished, the others said
 * on the output, with [what] they were, and left out. A run of hours
 * should not end because one process of forty-eight did. *)
val those_that_finish : string -> (unit -> 'a) list -> 'a list
