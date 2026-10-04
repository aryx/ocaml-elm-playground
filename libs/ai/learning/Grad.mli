(* The same derivatives, written once: reverse-mode automatic
 * differentiation (notes_ai_learning.md section 5).
 *
 * Backprop.mli writes the backward pass by hand, layer by layer. That
 * is the way to understand it and a bad way to live: add a layer
 * type, write its derivative; add a loss, write its derivative;
 * forget a minus sign, spend an evening.
 *
 * Here each *value* carries the little graph that made it, and each
 * operation knows only its own local derivative. Ask the last value
 * for the gradient, walk the graph backwards once, and everything
 * that went into it has its slope:
 *
 *     let x = value 3. and w = value 2. in
 *     let y = (x *: w) +: value 1. in     x   w
 *     backward y;                          \ /
 *     slope w  (* = 3. *)                   *   1
 *                                            \ /
 *                                             +
 *                                             y
 *
 * The walk is the chain rule and nothing else: a node knows what its
 * output's slope means for each of its inputs ([*] sends the slope
 * through multiplied by the *other* input, [+] sends it through
 * unchanged), and the nodes are visited so that a node is done only
 * after everything it feeds into is (a topological order), because
 * its slope is the sum of what all of them send back.
 *
 * Two things this is not. It is not symbolic differentiation: nothing
 * builds a formula for the derivative, it computes a number at the
 * point you are standing on. And it is not numerical differentiation
 * ([Backprop.numeric]): nothing is nudged, nothing is approximate --
 * the answers agree with the hand-written pass to the last digit, and
 * [Unit_grad] checks exactly that.
 *
 * The cost is one node per operation, allocated, against a
 * hand-written pass that allocates a matrix per layer. Measured on
 * the same gradient (a 2-8-8-1 network, one example, Unit_grad):
 *
 *     Backprop, by hand       6.4 us
 *     Grad, on scalars       28.6 us        about 4.5x
 *
 * That is the honest price of the convenience at this size -- a node
 * and a closure allocated per operation -- and it is why a real
 * library (PyTorch, JAX) runs reverse mode over whole *arrays* rather
 * than scalars: one node per matrix multiply instead of one per
 * multiplication, while the shape of the idea stays exactly this.
 *
 * References: Seppo Linnainmaa, "The representation of the cumulative
 * rounding error of an algorithm as a Taylor expansion of the local
 * rounding errors", 1970 (reverse mode, fifteen years before
 * backpropagation named it); Andreas Griewank, Andrea Walther,
 * *Evaluating Derivatives*, 2008; Andrej Karpathy, micrograd, 2020,
 * the clearest small implementation and the model for this one. *)

(*****************************************************************************)
(* {1 Values} *)
(*****************************************************************************)

type t

(* a number the graph knows about, and its value *)
val value : float -> t
val of_ : t -> float

(* the slope of whatever [backward] was called on, with respect to
 * this value: zero until then *)
val slope : t -> float

(* [set v x]: change a value in place, which is what learning does to
 * a weight between two graphs. Only for a value made by [value]: the
 * nodes computed from it keep what they computed. *)
val set : t -> float -> unit

(*****************************************************************************)
(* {1 Arithmetic} *)
(*****************************************************************************)

(* the arithmetic. Named with a colon so that ordinary floats keep the
 * plain operators: [a +: b], [a *: b]. *)
val ( +: ) : t -> t -> t
val ( -: ) : t -> t -> t
val ( *: ) : t -> t -> t
val ( /: ) : t -> t -> t
val neg : t -> t

(* what a network needs: the squashes, and a square for the loss *)
val exp_ : t -> t
val log_ : t -> t
val tanh_ : t -> t
val sigmoid : t -> t
val relu : t -> t
val square : t -> t
val pow : t -> float -> t (* to a constant power *)
val sum : t list -> t

(* [dot a b]: sum_i a_i b_i, what a neuron computes of its inputs, as
 * *one* node instead of the 2n that [*:] and [+:] build. Nothing says
 * an operation has to be small: it has to know its value and where
 * its slope goes. A node per matrix product is the same step taken
 * again, and is how a real library works (see the cost above).
 * Measured on Ngram_mlp's step of 32 examples (Unit_ngram_mlp):
 * 175 ms with a node per product, 45 ms with this. *)
val dot : t array -> t array -> t

(*****************************************************************************)
(* {1 Choosing among several} *)
(*****************************************************************************)
(* A network that answers "which one" (which digit, which letter comes
 * next) gives a score per choice, and two more operations turn the
 * scores into a loss. Both are written out of the arithmetic above,
 * so their derivatives are nobody's work.
 *
 * [softmax] makes the scores probabilities -- positive, summing to 1,
 * the largest score the largest share:
 *
 *                 exp s_i             scores    1      2      3
 *     p_i  =  ---------------         p         0.09   0.24   0.67
 *              sum_j exp s_j
 *
 * [cross_entropy scores answer] is how surprised that leaves us by
 * the right answer, [-log p_answer]: 0 when it was given all the
 * probability, growing without bound as it is given none. With the
 * scores above and the answer the last, -log 0.67 = 0.41; had the
 * answer been the first, -log 0.09 = 2.41. Divided by log 2 it is in
 * bits, Shannon's measure (1948).
 *
 * The slope of that loss with respect to each score comes out as
 * simple as a derivative gets (Unit_grad checks it from the graph):
 *
 *     d loss / d s_i  =  p_i - 1    for the answer
 *                        p_i        for the others
 *
 * the probability given, minus the probability deserved. *)

val softmax : t list -> t list
val cross_entropy : t list -> int -> t

(*****************************************************************************)
(* {1 Going backwards} *)
(*****************************************************************************)

(* [backward v]: walk the graph back from [v], filling in every
 * [slope] that led to it. [v]'s own slope is 1 -- it is what
 * everything is being differentiated with respect to. *)
val backward : t -> unit

(* [zero v]: forget the slopes (not the graph), for a second backward
 * pass over the same values *)
val zero : t -> unit

(* how many nodes went into a value: the graph's size, which is what
 * the cost above is about *)
val nodes : t -> int
