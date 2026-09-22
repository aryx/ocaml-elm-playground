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
 *     Backprop, by hand       9.6 us
 *     Grad, on scalars      197.4 us        about 20x
 *
 * That is the honest price of the convenience at this size, and it is
 * why a real library (PyTorch, JAX) runs reverse mode over whole
 * *arrays* rather than scalars: one node per matrix multiply instead
 * of one per multiplication, while the shape of the idea stays
 * exactly this. Some of the 20x is this implementation being the
 * readable one -- [backward] finds its order with a list walk, which
 * is quadratic in the nodes -- and the rest is the allocation the
 * approach itself costs.
 *
 * References: Seppo Linnainmaa, "The representation of the cumulative
 * rounding error of an algorithm as a Taylor expansion of the local
 * rounding errors", 1970 (reverse mode, fifteen years before
 * backpropagation named it); Andreas Griewank, Andrea Walther,
 * *Evaluating Derivatives*, 2008; Andrej Karpathy, micrograd, 2020,
 * the clearest small implementation and the model for this one. *)

type t

(* a number the graph knows about, and its value *)
val value : float -> t
val of_ : t -> float

(* the slope of whatever [backward] was called on, with respect to
 * this value: zero until then *)
val slope : t -> float

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
val sum : t list -> t

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
