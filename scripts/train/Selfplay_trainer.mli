(* What the trainers of the networks that play share: the loop of
 * Selfplay.mli, run with the two things that get more out of an hour
 * -- the games of an iteration played by many processes at once, and
 * its steps taken by several learners apart, their networks averaged
 * -- measuring as it goes and writing the weights after every
 * iteration, so that a run can be stopped and gone on with.
 *
 * train_connect4 and train_go are each a game, a network's sizes and
 * the players to be measured against, given to [run]. *)

(* how many processes play at once: 48, or the environment's WORKERS *)
val workers : int

(* [together jobs]: each in a process of its own, their results in
 * order (a fork and a pipe a job, the result marshalled back). The
 * jobs do not depend on each other, and a job that changes something
 * changes it in its own copy. *)
val together : (unit -> 'a) list -> 'a list

(* [score ~games play]: [play n] is the first player's share of game
 * number [n] (1 won, 0 lost, a half drawn), each game in a process of
 * its own; "won-drawn-lost" *)
val score : games:int -> (int -> float) -> string

type ('state, 'move) setup = {
  board : ('state, 'move) Selfplay.board;
  settings : Selfplay.settings; (* a game against itself *)
  games : int; (* an iteration's, shared among the processes *)
  remembered : int; (* the newest lessons kept *)
  (* the other ways a lesson is as good a lesson: a board in a mirror,
   * turned. Learned beside it. *)
  also : Policy_value.lesson -> Policy_value.lesson list;
  steps : int; (* a learner's, an iteration *)
  batch : int;
  (* how many learn apart, their networks averaged; 1: one learner, in
   * this process, as [Selfplay.learn] *)
  learners : int;
  (* how it does, as a line of text; asked before the first iteration
   * and every [every] after, and kept in the file's notes *)
  measure : Policy_value.t -> string;
  every : int;
  notes : (string * string) list; (* what the file says of itself *)
}

(* [run setup ~fresh ~out ~iterations ~from]: so many iterations, from
 * the network in the file [from] if given, else from [fresh ()];
 * [out] rewritten after each *)
val run :
  ('state, 'move) setup -> fresh:(unit -> Policy_value.t) -> out:string -> iterations:int -> from:string option -> unit
