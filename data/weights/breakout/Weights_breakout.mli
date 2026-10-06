(* What a [Dqn] network learned of TinyBreakout, as the bytes of a
 * weights file: read with [Weights.of_string], then [Dqn.of_weights].
 *
 * Taught by scripts/train/train_breakout from the last four screens
 * of the game, 64 by 64 greys each, and the score, with a point taken
 * for a ball lost (the one thing that is not the Atari paper's). The
 * file's own first lines are its record, and data/weights/README.md
 * repeats them. *)

val bytes : string

(* the generator's intermediate name for it *)
val breakout_weights : string
