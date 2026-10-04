(* Drawing from a distribution: how a model that gives probabilities
 * writes anything (notes_ai_learning.md section 11).
 *
 * A language model answers "what comes next" with a probability per
 * token. To write, pick one *at random with those odds*, append it,
 * ask again. Lay the probabilities end to end on [0, 1) and see where
 * a random number falls:
 *
 *     p     0.5         0.2     0.3
 *          |-----------|-----|-------|
 *          0          0.5   0.7      1         0.62 falls in the second
 *
 * Always taking the likeliest instead writes the same text every time,
 * and a dull one: the names all come out "an".
 *
 * [temperature] is the knob between the two. The probabilities are
 * raised to the power 1/T and made to sum to 1 again: at 1 they are
 * unchanged; below, the likely get likelier (towards always the
 * best, at 0); above, the odds even out (towards any token at all).
 *
 *     p               0.5    0.2    0.3
 *     T = 0.5         0.66   0.11   0.24      safer
 *     T = 2           0.42   0.26   0.32      wilder
 *)

(* [draw state p]: an index of [p], with [p]'s odds. [p] sums to 1. *)
val draw : Lehmer.state -> float array -> int

(* [temper t p]: [p] at temperature [t] > 0 *)
val temper : float -> float array -> float array

(* the likeliest: what temperature 0 would draw *)
val best : float array -> int
