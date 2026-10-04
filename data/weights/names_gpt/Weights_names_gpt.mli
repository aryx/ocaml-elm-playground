(* What [Gpt] learned of makemore's names, as the bytes of a weights
 * file: read with [Weights.of_string], then [Gpt.of_weights].
 *
 * Trained once by scripts/train/train_names_gpt -- 30,000 names, one a
 * step, from the 80% that [Corpus.split] gives to learn from, under
 * three minutes -- to a loss of 2.219 on the names held out (the table
 * of letter pairs: 2.454; [Ngram_mlp] after eleven minutes: 2.328).
 * The file's own first lines say the same, with the seed and the
 * rate: they are its record, and a new training rewrites them. *)

val bytes : string

(* the generator's intermediate name for it *)
val names_gpt_weights : string
