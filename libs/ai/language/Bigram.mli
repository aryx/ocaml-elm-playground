(* What comes next, knowing only the letter before: a table of pairs,
 * counted, and then learned (notes_ai_learning.md section 11).
 *
 * The smallest language model there is. Go through the text and count
 * how often each token follows each other one ("bigram": a pair):
 *
 *     "emma"  .e  em  mm  ma  a.              after ->   .     a     e    n
 *     "ava"   .a  av  va  a.                  before .    0  4410  1531  1146
 *                                                    a 6640   556   692  5438
 *     in the 32,033 names of Makemore_names:         n 6763  2977  1359  1906
 *
 * 4,410 names start with an a, 6,763 end with an n, and "an" is in
 * 5,438 places. Divide each row by its sum and it is a probability:
 * after an a, 6640 / 33885 = 0.196 that the name ends. To write a
 * name, start at the boundary's row, draw a token with the row's
 * odds, go to that token's row, until the boundary is drawn again:
 *
 *     qusrh  vanile  mylin  jae  zanayo  waalian
 *
 * Not names, and not noise either: pairs of letters that names have.
 *
 * {1 How good: the loss}
 *
 * A model is as good as the probability it gave to what actually came
 * next. Over a whole text those multiply to a number too small to
 * write, so take logarithms and average: the mean of [-log p] over
 * every pair in the text. 0 would be certainty about every letter;
 * knowing nothing, 27 tokens alike, is log 27 = 3.296. On the names:
 *
 *     knowing nothing                       3.296    4.75 bits a letter
 *     this table                            2.454    3.54
 *
 * which is makemore's number, on the same file. In bits it is how
 * many yes-or-no questions a letter still costs: 4.75 without the
 * table, 3.54 with it (Shannon, 1948).
 *
 * A pair the text never has gets probability 0, and one such pair in
 * another text makes the loss infinite. [smoothing] adds that much to
 * every count first (makemore adds 1): nothing is impossible any
 * more, and the loss on the names barely moves, 2.4546.
 *
 * {1 The same table, learned}
 *
 * Nothing was learned so far, only counted. Now forget the counts and
 * make the table a layer of a network: a score per pair, all zero, a
 * row's softmax the probabilities after that token ([Grad.softmax]),
 * the loss the same mean surprise, and walk downhill:
 *
 *     step        loss      furthest from the counted table
 *        1        2.882     0.677
 *       10        2.514     0.183
 *       50        2.457     0.016
 *      200        2.4540    0.000
 *
 * It arrives at the table that counting gave, to three decimals
 * (Unit_bigram): **a count and a learned weight are the same thing**,
 * reached from two sides. That is all the reassurance gradient
 * descent ever gives, and the reason to do it the long way once:
 * counting stops here -- three letters of context are 19,683 rows,
 * most of them never seen -- and the learned version does not; put
 * layers between the token and the scores and it is [Ngram_mlp], then
 * a GPT, trained by this same loop.
 *
 * References: Claude Shannon, "A Mathematical Theory of
 * Communication", 1948 (the tables of pairs, and text drawn from
 * them); Andrey Markov, 1913, who counted the pairs of vowels and
 * consonants in Eugene Onegin by hand; Andrej Karpathy, makemore,
 * 2022, lecture 1, which this follows. *)

(*****************************************************************************)
(* {1 Counting} *)
(*****************************************************************************)

(* [counts t words]: a row per token before, a column per token after,
 * each word between two boundaries *)
val counts : Tokenizer.t -> string list -> Matrix.t

(* each row divided by its sum, [smoothing] (0 by default) added to
 * every count first. A row of zeros becomes every token alike. *)
val probabilities : ?smoothing:float -> Matrix.t -> Matrix.t

(* [loss p counts]: the mean of -log p over the pairs counted, in
 * nats. The counts need not be those [p] was made from: a text held
 * out is the honest measure. *)
val loss : Matrix.t -> Matrix.t -> float

(* nats to bits *)
val bits : float -> float

(* a word drawn from the table, a token at a time; cut at [longest]
 * tokens (40) if the boundary never comes *)
val sample : ?longest:int -> Lehmer.state -> Tokenizer.t -> Matrix.t -> string

(*****************************************************************************)
(* {1 Learning} *)
(*****************************************************************************)

(* the scores, and the optimizer's memory *)
type learned = {
  scores : Matrix.t; (* a row per token before, a score per token after *)
  adam : Adam.t;
}

(* [start size]: all scores zero, every token as likely as another.
 * [rate] is Adam's, 0.5 by default. *)
val start : ?rate:float -> int -> learned

(* one step downhill on the whole text, which is its counts. A
 * millisecond: the graph is the table's size, not the text's. *)
val step : Matrix.t -> learned -> learned

(* each row's softmax: the table to compare with [probabilities] *)
val learned_probabilities : learned -> Matrix.t
