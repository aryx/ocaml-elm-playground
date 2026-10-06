(* What a [Policy_value] network learned of chess by playing against
 * itself, as the bytes of a weights file: read with
 * [Weights.of_string], then [Policy_value.of_weights].
 *
 * Taught by scripts/train/train_chess (Alphazero.mli's loop over the
 * boards kit's Chess). The file's own first lines are its record, and
 * data/weights/README.md repeats them. *)

val bytes : string

(* the generator's intermediate name for it *)
val chess_weights : string
