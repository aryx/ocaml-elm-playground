(* TinyBreakout played without a window, a key at a time: the game as
 * a learner meets it (Dqn.mli). The game itself is the one of
 * games/arcade, its source copied here by dune: the same rules, the
 * same frames, with its effects off (juice=off: the dry game).
 *
 * A step is four frames with the same key held, as in the Atari
 * paper: at 60 frames a second nothing needs deciding more often, and
 * the learner lives four times as much game in the same time. The
 * serve is not the learner's business: when there is no ball in play
 * (the title, a ball lost, the game over) space is pressed for it. *)

type t

(* true: the game as it is played, its effects on; false (as it is
 * learned): the dry game. To measure what a learner taught on one
 * does on the other. *)
val juice : bool ref

(* the title screen, before the first serve *)
val start : unit -> t

(* left, stay, right *)
val actions : int

(* [step e action]: four frames later; the points scored in them, and
 * whether a ball was lost (where the learner's episode ends: what
 * comes after a lost ball is not owed to what it did before) *)
val step : t -> int -> t * int * bool

(* the score of the game being played, and whether it is over (three
 * balls lost, or both walls cleared) *)
val score : t -> int
val over : t -> bool

(* the game as six numbers, for checking the learner before it is
 * given the screen: where the paddle is, where the ball is and where
 * it is going, and whether there is one. Not what the paper's learner
 * gets. *)
val numbers : t -> float array

(* what is on the screen *)
val view : t -> Playground.shape list

(* the screen as the learner is given it: 64 by 64 greys, a byte
 * each, row after row (the paper's is 84 by 84). The game drawn by
 * the software rasterizer four times as fine and [shrink]'d. [zoom] (1) draws it a little
 * larger or smaller: the learner should not come to depend on where
 * exactly a pixel falls. *)
val side : int
val screen : ?zoom:float -> t -> Bytes.t

(* any picture brought down to [side] by [side] greys by averaging the
 * square of it each one covers: the game's own window, at whatever
 * size, read the same way *)
val shrink : Framebuffer.t -> Bytes.t
