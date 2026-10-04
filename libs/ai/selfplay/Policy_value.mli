(* A network with two heads: which moves are worth looking at, and who
 * is winning (notes_ai_learning.md section 16).
 *
 * Mcts.mli names the two guesses a search makes: a *policy*, what it
 * thinks of each move before trying any, and a *value*, what a
 * position is worth without playing it out. This is one network
 * giving both:
 *
 *     the position, as numbers
 *       |
 *     layer, relu
 *     layer, relu            the body: what both heads look at
 *       |         \
 *     policy       value
 *     a score      one number, tanh: 1 I win, -1 I lose,
 *     per move     for whoever is to play
 *
 * One body and not two networks (AlphaGo had two, in 2016; AlphaGo
 * Zero joined them, in 2017): what makes a move good and what makes a
 * position good are mostly the same knowledge, and each head's lesson
 * improves what the other reads.
 *
 * A [lesson] is a position with what both heads should have said
 * about it, and it comes from a game the search played against itself
 * (Selfplay.mli): the policy's target is the share of visits the
 * search gave each move -- the search having looked further than the
 * network alone could -- and the value's is how the game ended. The
 * loss is the two added: the policy's surprise at those shares
 * ([Tensor.cross_entropy_to]), and the square of the value's error.
 *
 * It is written on [Tensor], a row per position, so a batch of
 * lessons is one pass.
 *
 * References: David Silver et al., "Mastering the game of Go without
 * human knowledge", 2017 (the two heads on one body, and this loss),
 * and "A general reinforcement learning algorithm that masters chess,
 * shogi, and Go through self-play", 2018. *)

type t = {
  inputs : int; (* the numbers a position is *)
  moves : int; (* the scores the policy gives *)
  (* "body1.w", "body1.b", "body2.w", "body2.b", "policy.w",
   * "policy.b", "value.w", "value.b" *)
  matrices : (string * Matrix.t) list;
  adam : Adam.t;
}

(* two layers of [hidden] neurons (64) under the two heads. [rate] is
 * Adam's (0.003). *)
val make : seed:int -> ?hidden:int -> ?rate:float -> inputs:int -> moves:int -> unit -> t

val parameters : t -> int

(* [opinion n input]: a share per move, summing to 1 over all of them,
 * legal or not (the caller knows the rules: [Selfplay.guides]); and
 * the value, between -1 and 1, for whoever is to play *)
val opinion : t -> float array -> float array * float

(*****************************************************************************)
(* {1 Learning} *)
(*****************************************************************************)

type lesson = {
  input : float array; (* the position, as the network reads it *)
  policy : float array; (* the share of the search's visits each move got *)
  value : float; (* how it ended for whoever was to play: 1, 0 or -1 *)
}

(* the policy's loss and the value's, added, averaged over the lessons *)
val loss : t -> lesson array -> float

(* one step downhill on a batch of lessons, and the loss it had *)
val step : ?rate:float -> t -> lesson array -> t * float

(*****************************************************************************)
(* {1 As a file} *)
(*****************************************************************************)

val to_weights : ?notes:(string * string) list -> t -> Weights.t
val of_weights : Weights.t -> (t, string) result
