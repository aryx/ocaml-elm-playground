(* A text's words, shuffled and cut in three: what a model learns
 * from, and what it is judged on (notes_ai_learning.md section 12).
 *
 * A model with enough numbers can learn its examples by heart, and
 * its loss on them says nothing about the next name it meets. So some
 * of the words are put aside before any training and never shown to
 * it: the loss on those is the one that counts.
 *
 *     the names, shuffled   |------------ 80% ------------|- 10% -|- 10% -|
 *                            learn                         held    test
 *
 * Two put aside, not one: [held] is looked at while training and
 * while choosing the network's sizes, which is a slow way of training
 * on it too; [test] is looked at once, at the end.
 *
 * The shuffle is from a seed, so that every program -- the trainer,
 * the game that later plays with what was trained, the tests -- cuts
 * at the same places. A name the game shows as "never seen" must not
 * be one the trainer saw. *)

type t = {
  learn : string list;
  held : string list;
  test : string list;
}

(* [split words]: shuffled by [seed] (42), the first 80% to learn
 * from, the next 10% held out, the rest for the end *)
val split : ?seed:int -> string list -> t

(* the split of makemore's names, by the default seed: 25,626 names,
 * 3,203 and 3,204 *)
val sizes : t -> int * int * int
