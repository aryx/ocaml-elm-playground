(* What [Ngram_mlp] learned of makemore's names, as the bytes of a
 * weights file: read with [Weights.of_string], then
 * [Ngram_mlp.of_weights].
 *
 * Trained once by scripts/train/train_names -- 60,000 batches of 32 on
 * the 80% of the names that [Corpus.split] gives to learn from, eleven
 * minutes -- to a loss of 2.328 on the names held out (the table of
 * letter pairs: 2.454). The file's own first lines say the same, with
 * the seed and the rates: they are its record, and a new training
 * rewrites them. *)

val bytes : string

(* the generator's intermediate name for it *)
val names_mlp_weights : string
