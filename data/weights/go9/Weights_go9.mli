(* What a [Policy_value] network learned of Go on 9 by 9 by playing
 * against itself ([Alphazero]), as the bytes of a weights file: read
 * with [Weights.of_string], then [Policy_value.of_weights].
 *
 * Trained by scripts/train/train_go: 96 iterations of 192 games
 * against itself, four hours on 48 processes. Against AiGo's own
 * computer, the search with 1,000 random playouts a move, over 40
 * games from random openings: 33-0-7 with 100 playouts a move,
 * 35-0-5 with 1,600, and 24-0-16 with no search at all, its policy's
 * first choice; knowing nothing it lost all twenty. The file's own
 * first lines are its record -- the iterations, what an iteration is, and how it then
 * did against the search without a network -- and
 * data/weights/README.md repeats them. *)

val bytes : string

(* the generator's intermediate name for it *)
val go9_weights : string
