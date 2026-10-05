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

(*****************************************************************************)
(* {1 Processes that stay} *)
(*****************************************************************************)
(* [together] forks a process a job, and a fork is cheap to call and
 * dear to use (notes_ai_dark_arts.md): right for a job of seconds,
 * wrong for one of milliseconds asked a thousand times. A *helper* is
 * forked once and then asked again and again, a question down one
 * pipe and its answer back up another: the slopes of a slice of a
 * batch, for a network whose numbers change at every question. *)

type ('question, 'answer) helper

(* [helpers n answer]: [n] of them, each a copy of this process as it
 * is now, answering [answer number question] to each question until
 * dismissed *)
val helpers : int -> (int -> 'question -> 'answer) -> ('question, 'answer) helper list

(* the same question to all of them at once, and their answers, in
 * order *)
val ask_all : ('question, 'answer) helper list -> 'question -> 'answer list

val dismiss : ('question, 'answer) helper list -> unit
