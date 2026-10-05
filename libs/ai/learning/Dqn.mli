(* Learning to act from rewards, with a network in the table's place:
 * DQN (notes_ai_learning.md section 18).
 *
 * Qlearn.mli keeps one number per state and action -- how good is
 * this move here -- and moves it, each step lived, towards the reward
 * plus the best of the numbers of the state that followed. That is a
 * table, and a table needs to have been in a state to know anything
 * of it. A screen of 84 by 84 pixels is in a different state at every
 * frame of every game ever played.
 *
 * A *deep Q-network* is the same rule with a network for the table:
 * given a state it answers a value per action, for states it never
 * saw as for the others, and the rule moves the network instead of a
 * cell --
 *
 *     Q(s, a)   towards   r + discount * max over a' of Q(s', a')
 *
 * by a step downhill on the square of the difference. Written like
 * that it does not work: it diverges, the values run off to
 * infinity. The paper is the two things that make it work.
 *
 * **Replay.** The steps lived one after the other are all alike (the
 * ball is where it was a moment ago), and a network taught on a
 * thousand alike in a row forgets everything else. So every step
 * lived is kept, in a [memory] of the last so many, and the network
 * is taught on steps *drawn at random* from it: yesterday's with
 * today's.
 *
 * **A target that holds still.** The rule's right-hand side is the
 * network's own answer. Each step moves the network, so the target
 * moves with the thing chasing it. So the right-hand side is asked of
 * a *copy* of the network, [target], frozen for a few thousand steps
 * at a time and then brought up to date.
 *
 * {1 Two shapes}
 *
 * [Numbers]: the state is so many numbers, through two layers. For a
 * world whose state is small and known, and for checking the rule
 * itself: on Qlearn's cliff (Unit_dqn) the network finds the table's
 * own way, thirteen steps along the edge.
 *
 * [Screen]: the state is the last few frames of a screen, a column
 * each, and the network is the paper's -- two convolutions that
 * *step* ([Tensor.windows]), each shrinking the picture, then a layer
 * that reads what is left:
 *
 *     84 x 84 pixels, the last 4 frames
 *       |  windows of 8 every 4, 16 channels, relu      20 x 20
 *       |  windows of 4 every 2, 32 channels, relu       9 x 9
 *       |  256 neurons, relu
 *     a value per action
 *
 * Several frames and not one, because one frame does not say which
 * way the ball is going.
 *
 * It is given the screen and the score, and nothing else: not where
 * the ball is, not what a paddle is for.
 *
 * References: Volodymyr Mnih et al., "Playing Atari with Deep
 * Reinforcement Learning", 2013 (this network, replay), and
 * "Human-level control through deep reinforcement learning", Nature,
 * 2015 (the frozen target); Long-Ji Lin, "Self-improving reactive
 * agents based on reinforcement learning, planning and teaching",
 * 1992 (experience replay); Christopher Watkins, 1989 (the rule,
 * Qlearn.mli). *)

(*****************************************************************************)
(* {1 The network} *)
(*****************************************************************************)

(* a screen's sizes: the input is [frames] times [height] times
 * [width] numbers, a frame after the other, each row after row *)
type screen = {
  width : int;
  height : int;
  frames : int; (* the last ones, stacked *)
  first : int * int * int; (* the first convolution: window, stride, channels *)
  second : int * int * int;
  hidden : int;
}

type shape =
  | Numbers of int (* two layers of so many neurons *)
  | Screen of screen

type t = {
  inputs : int;
  actions : int;
  shape : shape;
  (* Numbers: "body1", "body2", "out"; Screen: "conv1", "conv2",
   * "hidden", "out"; each a ".w" and a ".b" *)
  matrices : (string * Matrix.t) list;
  adam : Adam.t;
}

(* [Numbers 64] unless said. [rate] is Adam's (0.001). *)
val make : seed:int -> ?rate:float -> ?shape:shape -> inputs:int -> actions:int -> unit -> t

val parameters : t -> int

(* what each action is worth in a state, by a plain pass *)
val values : t -> float array -> float array

(* the same through the graph that learning builds: for the test that
 * the two agree *)
val values_by_graph : t -> float array -> float array

(* the action worth most *)
val best : t -> float array -> int

(*****************************************************************************)
(* {1 Learning} *)
(*****************************************************************************)

(* a step lived: where it was, what it did, what that paid, and where
 * it led; None if it ended there *)
type lived = {
  state : float array;
  action : int;
  reward : float;
  next : float array option;
}

(* [loss ~target n lived]: over these steps, the mean square of how
 * far the value [n] gives to the action taken is from the reward plus
 * [discount] (0.99) times the best value [target] gives in the state
 * that followed *)
val loss : ?discount:float -> target:t -> t -> lived array -> float

(* one step downhill on it, and the loss it had *)
val step : ?discount:float -> ?rate:float -> target:t -> t -> lived array -> t * float

(* the two halves of [step], for a trainer that takes its steps in
 * several processes *)
val gradient : ?discount:float -> target:t -> t -> lived array -> float array * float
val apply : ?rate:float -> t -> float array -> t

(*****************************************************************************)
(* {1 What it has lived} *)
(*****************************************************************************)

(* the last so many steps lived: a ring, the oldest written over *)
type memory

val memory : int -> memory
val remember : memory -> lived -> unit
val remembered : memory -> int

(* so many of them drawn at random, with repeats *)
val recall : Lehmer.state -> memory -> int -> lived array

(*****************************************************************************)
(* {1 As a file} *)
(*****************************************************************************)

val to_weights : ?notes:(string * string) list -> t -> Weights.t
val of_weights : Weights.t -> (t, string) result
