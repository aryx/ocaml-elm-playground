(* The training loop, and the four things about it that no formula
 * warns you about (notes_ai_learning.md section 6).
 *
 * Backprop.mli has the step; this has the loop around it, and the
 * loop is where networks are actually won and lost:
 *
 *     for each epoch
 *       shuffle the examples
 *       for each batch of them
 *         gradient, averaged over the batch
 *         step downhill
 *       score on examples it was not trained on
 *
 * {1 Batches}
 *
 * Averaging the gradient over 32 examples instead of stepping on each
 * one makes the step less noisy and the arithmetic better shaped (one
 * matrix against many columns, not many matrix-vector products). Not
 * over *all* of them, though: the noise of a small batch is useful,
 * it shakes the search out of narrow valleys, and a full-batch step
 * costs a whole pass over the data to move once.
 *
 * {1 Shuffling}
 *
 * Examples usually arrive sorted -- all the zeroes, then all the ones
 * -- and a network shown a hundred zeroes in a row learns to answer
 * zero. [epoch] shuffles, deterministically from a seed, so that a
 * run repeats.
 *
 * {1 Held-out data, and the curve that tells the truth}
 *
 * Keep a fifth of the examples out of training and score on those.
 * The training loss falls for ever; the held-out loss falls and then
 * *rises*, and where it turns is where the network stopped learning
 * the rule and started memorising the examples. That is overfitting,
 * and the two curves side by side are the single most useful picture
 * in the subject ([history] below, drawn by examples/AiNeuralNet.ml
 * with the "h" key).
 *
 *     loss
 *      |  \
 *      |   \        held out: rises again from here on
 *      |    \______/
 *      |     \______   training: falls for ever
 *      +------------------ epochs
 *
 * {1 The learning rate}
 *
 * Still the one knob that matters most: too small and nothing
 * happens, too large and the loss climbs to infinity in a dozen
 * steps. [decay] lowers it as training goes on, which beats any fixed
 * value -- big steps while far away, small ones to settle.
 *
 * References: Yann LeCun, Leon Bottou, Genevieve Orr, Klaus-Robert
 * Muller, "Efficient BackProp", 1998, which is still the best account
 * of the practicalities; Leon Bottou, "Stochastic Gradient Descent
 * Tricks", 2012. *)

(* what training did, an entry per epoch: for drawing the two curves *)
type history = { epoch : int; training : float; held_out : float }

(* [split ~part examples]: the examples in two, [part] of them (0.2,
 * say) held out. Deterministic, and it takes every k-th example
 * rather than a random subset, so that a sorted set is split evenly
 * between the two. *)
val split : ?part:float -> Backprop.example list -> Backprop.example list * Backprop.example list

(* [epoch ~seed ~rate ~batch net examples]: one pass over the
 * examples, shuffled, a step per batch *)
val epoch : ?seed:int -> ?rate:float -> ?batch:int -> Net.t -> Backprop.example list -> Net.t

(* [run ~epochs ~rate ~decay ~batch ~held net examples]: the whole
 * loop, and what happened. [decay] multiplies the rate after every
 * epoch (1.0: no decay). [held] is scored but never trained on. *)
val run :
  ?epochs:int ->
  ?rate:float ->
  ?decay:float ->
  ?batch:int ->
  ?seed:int ->
  ?held:Backprop.example list ->
  Net.t ->
  Backprop.example list ->
  Net.t * history list

(* [accuracy net examples ~answer]: the share it gets right, where
 * [answer] turns the network's outputs into the game's own answer (the
 * biggest output, usually: see [best]) *)
val accuracy : Net.t -> Backprop.example list -> answer:(float array -> int) -> float

(* the index of the largest output, which is how a network says "this
 * one" when its outputs are one per class *)
val best : float array -> int

(* [one_hot n i]: the target for class [i] out of [n] -- 1 in one
 * place and 0 everywhere else *)
val one_hot : int -> int -> float array
