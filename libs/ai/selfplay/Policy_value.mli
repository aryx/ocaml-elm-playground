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
 * (Alphazero.mli): the policy's target is the share of visits the
 * search gave each move -- the search having looked further than the
 * network alone could -- and the value's is how the game ended. The
 * loss is the two added: the policy's surprise at those shares
 * ([Tensor.cross_entropy_to]), and the square of the value's error.
 *
 * It is written on [Tensor], a row per position, so a batch of
 * lessons is one pass.
 *
 * {1 Two shapes of body}
 *
 * [Flat] is the body above: the position as so many numbers, each
 * with its own weights. It has to learn that three in a row on the
 * left is three in a row on the right, separately, from games where
 * each happened.
 *
 * [Board] reads the position as a board: its body is *convolutions*
 * (Tensor.mli), a small layer looking at a square and its eight
 * neighbours, the same weights at every square, so what it learns of
 * a shape it knows everywhere.
 *
 *     the position: a row per square, a column per plane
 *       |
 *     conv, relu                 [layers] of them, [channels] wide;
 *     conv, relu, + what it      from the second on each adds to
 *     was given                  what it was given (a residual)
 *       |            \
 *     2 numbers a     1 number a square
 *     square, relu    relu
 *       |              |
 *     the board laid end to end
 *       |              |
 *     a score         64 neurons, relu
 *     per move        one number, tanh
 *
 * A matrix is then one board, not a batch: a batch of lessons is as
 * many graphs, their losses added.
 *
 * References: David Silver et al., "Mastering the game of Go without
 * human knowledge", 2017 (the two heads on one body, and this loss),
 * and "A general reinforcement learning algorithm that masters chess,
 * shogi, and Go through self-play", 2018. *)

(* a board's sizes: the input is [planes] times [height] times [width]
 * numbers, a plane after the other, each plane row after row *)
type board = {
  planes : int; (* the kinds of thing a square can hold, in the input *)
  height : int;
  width : int;
  channels : int; (* what each layer makes of a square *)
  layers : int; (* convolutions, one after the other *)
}

type shape =
  | Flat (* the position as so many unrelated numbers *)
  | Board of board (* the position as a board: convolutions *)

type t = {
  inputs : int; (* the numbers a position is *)
  moves : int; (* the scores the policy gives *)
  shape : shape;
  (* Flat: "body1.w", "body1.b", "body2.w", "body2.b", "policy.w",
   * "policy.b", "value.w", "value.b". Board: "conv0.w", "conv0.b", ...,
   * "policy.conv.w", ".b", "policy.w", ".b", "value.conv.w", ".b",
   * "value.hidden.w", ".b", "value.w", ".b" *)
  matrices : (string * Matrix.t) list;
  adam : Adam.t;
}

(* [Flat], two layers of [hidden] neurons (64) under the two heads;
 * or, given a [board], [Board]. [rate] is Adam's (0.003). *)
val make : seed:int -> ?hidden:int -> ?rate:float -> ?board:board -> inputs:int -> moves:int -> unit -> t

val parameters : t -> int

(* [opinion n input]: a share per move, summing to 1 over all of them,
 * legal or not (the caller knows the rules: [Alphazero.guides]); and
 * the value, between -1 and 1, for whoever is to play *)
val opinion : t -> float array -> float array * float

(* the same, through the graph that [step] builds: for the test that
 * the two agree *)
val opinion_by_graph : t -> float array -> float array * float

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

(* the two halves of [step], for a trainer that works out the slopes
 * of a large batch in several processes at once and averages them:
 * the loss on these lessons with its slope for every number of the
 * network, the matrices' in their order; and a step along slopes *)
val gradient : t -> lesson array -> float array * float
val apply : ?rate:float -> t -> float array -> t

(*****************************************************************************)
(* {1 As a file} *)
(*****************************************************************************)

val to_weights : ?notes:(string * string) list -> t -> Weights.t
val of_weights : Weights.t -> (t, string) result
