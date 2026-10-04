(* What comes next, knowing everything before: a GPT, on scalars
 * (notes_ai_learning.md section 14).
 *
 * [Ngram_mlp] reads three tokens, each at a fixed place in its input:
 * to read four it needs a wider first layer, and what it learned about
 * a letter in the second place it must learn again for the third. A
 * transformer reads as many as there are, with one set of weights,
 * because of one new operation: **attention**, by which the token
 * being read picks, among all those before it, the ones that matter
 * to it now.
 *
 * Each token is read in turn, and becomes [width] numbers (16) that
 * are changed step by step into the scores of what comes next:
 *
 *     token, position
 *       |
 *     wte[token] + wpe[position]     what it is, and where it stands
 *       |
 *       +-- attention --+            look back: mix in what earlier
 *       |<--------------+            tokens have to say
 *       |
 *       +---- MLP ------+            think: a layer four times as
 *       |<--------------+            wide, and back
 *       |
 *     head                           a score per token: what is next
 *
 * The two side branches *add* to the line rather than replace it
 * (residuals: He et al., 2015), so the gradient has a straight road
 * from the loss to the embeddings, and each branch learns only a
 * correction. Before each, the numbers are brought back to a standard
 * length (RMSNorm), so that no branch's output can run away.
 *
 * {1 Attention}
 *
 * Every token, when read, leaves two things behind for those that
 * come after: a *key*, saying what it is about, and a *value*, what
 * it has to say. The token being read makes a *query*, what it is
 * looking for. Its query against each key so far (a sum of products:
 * large when they point the same way) gives a score per earlier
 * token; a softmax makes the scores shares; and what it gets is the
 * values mixed in those shares.
 *
 *     reading the second m of ". e m m":
 *
 *         query (m)  .  key (.)   0.3  \            0.15
 *         query (m)  .  key (e)   1.9   | softmax   0.72   -> 0.15 value(.) + 0.72 value(e)
 *         query (m)  .  key (m)  -0.2   |           0.09      + 0.09 value(m) + 0.04 value(m)
 *         query (m)  .  key (m)  -0.9  /            0.04
 *
 * Nothing says what to look for: the query, key and value are each a
 * matrix times the token's numbers, and the three matrices are
 * learned. It is a lookup in a table whose rows are the text so far,
 * made soft so that it has a slope. Several *heads* (4) do this side
 * by side, each on its own quarter of the numbers, so one can look
 * for the vowel before while another counts how long the name is.
 *
 * A token only ever sees those before it, which is what lets one pass
 * over "emma" be five examples at once -- every position predicts its
 * next -- and what lets the trained model write: there is nothing
 * after the last token to wait for.
 *
 * Attention alone cannot tell "ma" from "am": a mix has no order. So
 * the position is added to the token at the start ([wpe]), a learned
 * place per position, and the furthest position there is ([block],
 * 16) is the longest text it can read.
 *
 * {1 The numbers}
 *
 * The sizes are microgpt's, on the same names: 27 tokens, 16 numbers
 * wide, 4 heads, 1 layer, 16 positions, 4,192 numbers learned, Adam
 * with its rate falling to zero over the run. One name a step:
 *
 *     knowing nothing                                 3.296
 *     Bigram, one letter back                         2.454
 *     Ngram_mlp, 3 back, after 20,000 batches of 32   2.35      221 s
 *     this, after  1,000 names                        2.36        1 s
 *     this, after  5,000 names                        2.27        5 s
 *     this, after 30,000 names                        2.21       23 s
 *
 * on names never learned from: past the network that reads three
 * letters in a fortieth of its time and a hundredth of its examples.
 * (The times are on whole arrays, the default; a node per number, 27 s
 * for the 5,000.) And
 * with each idea taken out, the same 5,000 steps ([config]'s two
 * switches; the table is scripts/train/measure_gpt's):
 *
 *     all of it                                       2.269
 *     one head instead of four                        2.285
 *     without positions (it sees them, not where)     2.285
 *     without attention (no token sees another)       2.307
 *     without either                                  2.475
 *
 * Without either it knows the token before and nothing else: it is
 * [Bigram] again, squeezed through 16 numbers, and it lands beside
 * the table's 2.454. Each idea alone already recovers most of the
 * rest, since on names this short knowing *where* you are in the name
 * says nearly as much as seeing the letters before. They separate
 * slowly: at 1,000 steps the first four are within 0.01 of each
 * other. An honest lesson of small models: the architecture's ideas
 * pay off with training and with longer texts, not at once.
 *
 * It is written twice. On [Grad], a node per number, as microgpt is:
 * the version to read first, a token at a time, the whole model in
 * [read], every piece a function of ten lines. And on [Tensor], the
 * whole text at once, a row per token: the same arithmetic in forty
 * nodes instead of thirty thousand, ten times faster here and fifty
 * as the model grows ([on_arrays], Tensor.mli's table). The two give
 * the same loss and the same slopes to ten decimals (Unit_gpt).
 *
 * What is left out, as microgpt leaves it out: biases, dropout,
 * batches (a step is one name), a tokenizer beyond characters.
 *
 * References: Ashish Vaswani et al., "Attention Is All You Need",
 * 2017; Alec Radford et al., "Language Models are Unsupervised
 * Multitask Learners", 2019 (GPT-2, whose shape this is); Dzmitry
 * Bahdanau, Kyunghyun Cho, Yoshua Bengio, "Neural Machine Translation
 * by Jointly Learning to Align and Translate", 2014 (attention);
 * Kaiming He et al., "Deep Residual Learning for Image Recognition",
 * 2015; Biao Zhang, Rico Sennrich, "Root Mean Square Layer
 * Normalization", 2019; Andrej Karpathy, microgpt, 2026, which this
 * follows function by function, and nanoGPT, 2022. *)

(*****************************************************************************)
(* {1 Making one} *)
(*****************************************************************************)

type config = {
  vocabulary : int;
  width : int; (* the numbers a token is, all the way through *)
  heads : int; (* must divide [width] *)
  layers : int; (* attention and MLP, so many times *)
  block : int; (* the longest text it reads *)
  positions : bool; (* false: it is not told where a token stands *)
  attention : bool; (* false: no token looks at another *)
}

(* microgpt's: 16 wide, 4 heads, 1 layer, 16 positions, everything on *)
val config :
  ?width:int -> ?heads:int -> ?layers:int -> ?block:int -> ?positions:bool -> ?attention:bool -> int -> config

type t = {
  config : config;
  (* every matrix by its name: "wte" and "wpe" (a token's and a
   * position's numbers), "head", and per layer "0.q", "0.k", "0.v",
   * "0.o" (attention), "0.fc1", "0.fc2" (the MLP) *)
  matrices : (string * Matrix.t) list;
  adam : Adam.t;
}

(* small random numbers from [seed]. [rate] is Adam's (0.01). *)
val make : seed:int -> ?rate:float -> config -> t

(* how many numbers it learns *)
val parameters : t -> int

(*****************************************************************************)
(* {1 Using and training} *)
(*****************************************************************************)
(* A text is its tokens between two boundaries ([Tokenizer.bounded]). *)

(* one step downhill on one text, and the loss it had on it. [rate]
 * replaces [make]'s for this step. *)
val step : ?rate:float -> t -> int list -> t * float

(* the loss on one text and its slope with respect to every number of
 * the model, the matrices' in their order, one after the other: what
 * [step] hands to Adam *)
val gradient : t -> int list -> float array * float

(* true (the default): [step], [gradient] and [loss] run on [Tensor],
 * the whole text at once, a node per matrix operation; false: on
 * [Grad], a token and a number at a time. The same losses and slopes
 * either way; the times are in Tensor.mli. *)
val on_arrays : bool ref

(* the mean over the texts of each one's mean surprise, in nats *)
val loss : t -> int list list -> float

(* [next m tokens]: what comes after these tokens, a probability per
 * token; and where each head looked when reading the last one, layer
 * by layer and head by head, a share per token read, the oldest
 * first *)
val next : t -> int list -> float array * float array list

(* a word written a token at a time ([Sampling]); microgpt writes at
 * temperature 0.5 *)
val sample : ?temperature:float -> Lehmer.state -> Tokenizer.t -> t -> string

(*****************************************************************************)
(* {1 As a file} *)
(*****************************************************************************)

val to_weights : ?notes:(string * string) list -> t -> Weights.t
val of_weights : Weights.t -> (t, string) result
