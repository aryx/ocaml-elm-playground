(* What comes next, knowing the three letters before: a network
 * instead of a table (notes_ai_learning.md section 12).
 *
 * [Bigram]'s table sees one letter back. Seeing three would take a
 * table of 27^3 = 19,683 rows, most of them for letters that never
 * come together in any name: nothing to count, and nothing learned
 * about "mma" from having seen "nna". The table has no idea that two
 * letters are alike.
 *
 * Bengio's answer (2003) is to give each token a *place*: a few
 * numbers, its coordinates, learned like any weight. The network
 * reads the places of the last three tokens, not the tokens:
 *
 *     . e m          the context: three tokens
 *     | | |
 *     [embedding]    each token's row: 2 numbers           27 x 2
 *     | | |
 *     x              the three rows end to end: 6 numbers
 *     |
 *     tanh (W x + b)                   100 neurons         100 x 6, 100
 *     |
 *     W' h + b'      a score per token                     27 x 100, 27
 *     |
 *     softmax        what comes next: m, probably
 *
 * 3,481 numbers in all, against the table's 19,683 rows of 27. Tokens
 * that behave alike are pulled to the same place by the training, and
 * whatever the network learns about one then holds for its
 * neighbours. With two coordinates the places can be drawn, and the
 * drawing is the lesson: the vowels end up together, nobody having
 * said what a vowel is.
 *
 * The rest is [Bigram]'s learned half unchanged: the loss is the mean
 * surprise, the gradient comes from [Grad], the step from [Adam]. One
 * thing is new. A step cannot look at all 182,000 examples, so each
 * looks at a few drawn at random, a *batch* of 32: a noisy idea of
 * the slope, a thousand times cheaper, and many small noisy steps
 * beat a few exact ones.
 *
 * Measured on the names, 80% to learn from and 10% held out, batches
 * of 32 (the probe is Unit_ngram_mlp's):
 *
 *     knowing nothing                     3.296
 *     Bigram, one letter back             2.454
 *     this, after  2,000 steps            2.49      22 s
 *     this, after 10,000 steps            2.40     110 s
 *     this, after 20,000 steps            2.35     221 s
 *     makemore's, same sizes              about 2.3, after 200,000 steps
 *
 * on names it never learned from (the loss on those it did is within
 * 0.01: 3,481 numbers cannot memorise 182,000 examples). It passes
 * the table at about 4,000 steps and is still going down at 20,000.
 *
 * It is slow, and that is [Grad]'s price: a node per number. A step
 * of 32 examples is 45 ms, 12 with a minor heap large enough for a
 * step's graph (notes_opti_ocaml.md section 20, and the times above),
 * and it was 175 ms before [Grad.dot] made a neuron's sum one node.
 * [Tensor] is where that ends.
 *
 * References: Yoshua Bengio, Rejean Ducharme, Pascal Vincent,
 * Christian Jauvin, "A Neural Probabilistic Language Model", 2003;
 * Andrej Karpathy, makemore, 2022, lectures 2 and 3, which this
 * follows, sizes included. *)

type t = {
  context : int; (* how many tokens back it reads *)
  embedding : Matrix.t; (* a row per token: its place *)
  hidden_w : Matrix.t;
  hidden_b : Matrix.t;
  out_w : Matrix.t;
  out_b : Matrix.t;
  adam : Adam.t;
}

(* [make ~seed vocabulary]: reading [context] tokens (3), each at a
 * place of [dim] numbers (2), through [hidden] neurons (100).
 * [rate] is Adam's (0.01). Its first answer is "every token alike". *)
val make : seed:int -> ?context:int -> ?dim:int -> ?hidden:int -> ?rate:float -> int -> t

(* how many numbers it learns *)
val parameters : t -> int

(*****************************************************************************)
(* {1 Examples} *)
(*****************************************************************************)

(* the tokens before, and the one that came next *)
type example = int array * int

(* every position of every word: "emma" with a context of 3 is
 *
 *     . . .  -> e        . . e  -> m        . e m  -> m
 *     e m m  -> a        m m a  -> .
 *)
val examples : Tokenizer.t -> context:int -> string list -> example array

(* so many examples drawn at random *)
val batch : Lehmer.state -> example array -> int -> example array

(*****************************************************************************)
(* {1 Using and training} *)
(*****************************************************************************)

(* what comes after these tokens: a probability per token *)
val probabilities : t -> int array -> float array

(* the mean of -log p over the examples, in nats *)
val loss : t -> example array -> float

(* one step downhill on a batch. [rate] replaces [make]'s for this
 * step: lowered towards the end, the loss settles. *)
val step : ?rate:float -> t -> example array -> t

(* a word written a token at a time ([Sampling]) *)
val sample : ?longest:int -> ?temperature:float -> Lehmer.state -> Tokenizer.t -> t -> string

(*****************************************************************************)
(* {1 As a file} *)
(*****************************************************************************)

(* what it learned, to be written by a trainer ([Weights]); [notes]
 * say how it was made. The optimizer's memory is not kept: a network
 * read back is for using, or for training afresh. *)
val to_weights : ?notes:(string * string) list -> t -> Weights.t

(* refused if a matrix is missing or their sizes do not fit *)
val of_weights : Weights.t -> (t, string) result

(* a neuron's weighted sum as one node of the graph (the default), or
 * as its products and additions: the same numbers, to time the two *)
val fused : bool ref
