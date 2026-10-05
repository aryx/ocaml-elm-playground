(* What a [Policy_value] network learned of Connect 4 by playing
 * against itself ([Alphazero]), as the bytes of a weights file: read
 * with [Weights.of_string], then [Policy_value.of_weights].
 *
 * Trained by scripts/train/train_connect4: 150 iterations of 480
 * games against itself, 38 minutes on 48 processes. Against the
 * game's own alpha-beta at depth 7, over 40 games from random
 * openings: 20-2-18 with 400 playouts a move, 26-1-13 with 1,600;
 * knowing nothing it lost all twenty. The file's own first lines
 * are its record -- the iterations, what an iteration is, and how it
 * then did against the search without a network and against
 * alpha-beta -- and data/weights/README.md repeats them. *)

val bytes : string

(* the generator's intermediate name for it *)
val connect4_weights : string
