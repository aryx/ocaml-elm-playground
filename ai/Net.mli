(* A stack of neurons: layers, activations, and the forward pass
 * (notes_ai_learning.md section 2).
 *
 * One neuron draws a line (Neuron.mli) and cannot do XOR. Stack them
 * and the lines combine: a hidden layer bends the space the next layer
 * draws its line in, and two layers are enough for any shape you like
 * (Cybenko, 1989 -- "enough" in the sense that a wide enough layer
 * exists, which says nothing about finding it, and finding it is
 * Backprop.mli).
 *
 * A layer of m neurons over n inputs is an m x n matrix of weights, m
 * biases, and a function to squash with, so the whole forward pass is
 * one line per layer:
 *
 *     a = f(W a' + b)
 *
 *     inputs      hidden        output
 *       o -----> o
 *       o -----> o -----> o     2 -> 3 -> 1:  W1 is 3x2, W2 is 1x3,
 *       o -----> o               10 weights and 4 biases
 *
 * {1 The squash, and why it cannot be left out}
 *
 * Without [f] the stack collapses: W2 (W1 x) is (W2 W1) x, one layer
 * again, and a hundred of them are still a line. The choice of [f] is
 * three decades of history:
 *
 *     Sigmoid   1 / (1 + e^-z)     0..1, smooth; what 1986 used, and
 *                                  what makes gradients vanish (its
 *                                  slope is at most 1/4)
 *     Tanh      -1..1              the same shape, centred on zero,
 *                                  which trains better; TD-Gammon's
 *     Relu      max(0, z)          a kink, slope 1 or 0; trains deep
 *                                  nets where the other two stall
 *     Linear    z                  no squash: for an output that is a
 *                                  number rather than a choice
 *
 * {1 Where the weights start}
 *
 * Not at zero: every neuron of a layer would compute the same thing,
 * get the same gradient, and stay identical for ever -- the network
 * would have one neuron per layer, with extra steps. Small and
 * different, then, and *how* small matters: too big and the sigmoids
 * saturate (slope nearly zero, nothing learns), too small and the
 * signal dies out through the layers. [make] uses Glorot's rule
 * (2010): uniform in +/- sqrt(6 / (inputs + outputs)) for a layer,
 * which keeps the variance of what comes out about the variance of
 * what went in.
 *
 * References: David Rumelhart, Geoffrey Hinton, Ronald Williams,
 * "Learning representations by back-propagating errors", 1986; George
 * Cybenko, "Approximation by superpositions of a sigmoidal function",
 * 1989; Xavier Glorot, Yoshua Bengio, "Understanding the difficulty of
 * training deep feedforward neural networks", 2010; Vinod Nair,
 * Geoffrey Hinton, "Rectified Linear Units Improve Restricted
 * Boltzmann Machines", 2010. *)

type activation = Sigmoid | Tanh | Relu | Linear

(* [squash f z] and its slope. [slope] is given both the input [z] and
 * the output [a] because the sigmoid's and the tanh's derivatives are
 * cheapest from the output they already computed -- a (1 - a), and
 * 1 - a^2 -- which is the small piece of bookkeeping that makes a
 * backward pass cheap (Backprop.mli). *)
val squash : activation -> float -> float
val slope : activation -> z:float -> a:float -> float

type layer = {
  w : Matrix.t; (* outputs x inputs *)
  b : Matrix.t; (* outputs x 1 *)
  f : activation;
}

type t = layer list

(* [make ~seed ?hidden ?last sizes]: the sizes from the input to the
 * output, e.g. [2; 8; 8; 1] -- two inputs, two hidden layers of eight,
 * one output. [hidden] is the activation of every layer but the last
 * (Tanh), [last] the output layer's (Sigmoid: an answer between 0 and
 * 1). *)
val make : seed:int -> ?hidden:activation -> ?last:activation -> int list -> t

(* [forward net x]: what it answers *)
val forward : t -> float array -> float array

(* [layers net] and [weights net]: its shape, and how many numbers it
 * has to learn -- the count that says what a machine of this size can
 * be asked to do *)
val sizes : t -> int list
val weights : t -> int

(* the forward pass with its workings kept: the input, then the [z] and
 * [a] of each layer, which is exactly what the backward pass needs
 * (Backprop.mli) *)
type pass = {
  input : Matrix.t;
  steps : (Matrix.t * Matrix.t) list; (* per layer, in order: z, then a = f(z) *)
}

val forward_pass : t -> float array -> pass
val output : pass -> float array
