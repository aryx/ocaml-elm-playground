(* What a [Policy_value] network learned of Go on 9 by 9 by playing
 * against itself ([Selfplay]), as the bytes of a weights file: read
 * with [Weights.of_string], then [Policy_value.of_weights].
 *
 * Trained by scripts/train/train_go. The file's own first lines are
 * its record -- the iterations, what an iteration is, and how it then
 * did against the search without a network -- and
 * data/weights/README.md repeats them. *)

val bytes : string

(* the generator's intermediate name for it *)
val go9_weights : string
