(* One neuron, and the first thing anyone taught one to do
 * (notes_ai_learning.md section 1).
 *
 * Weight the inputs, add a bias, and squash:
 *
 *     x1 --w1--\
 *     x2 --w2---> sum ---> f ---> a       a = f(w1 x1 + w2 x2 + b)
 *     x3 --w3--/    +b
 *
 * With a step for [f] -- 1 above zero, 0 below -- that is Rosenblatt's
 * **perceptron** (1958), and its learning rule fits on a line: show it
 * an example, and if the answer is wrong, push the weights towards the
 * right answer by the input.
 *
 *     w <- w + rate * (target - answer) * x
 *     b <- b + rate * (target - answer)
 *
 * Why that works, in one picture. The neuron answers 1 on one side of
 * the line [w . x + b = 0] and 0 on the other, so learning is walking
 * that line into place; [w] is the normal to it, and adding a
 * misclassified point to [w] turns the line towards putting that point
 * on the right side.
 *
 *     x2                          x2
 *      |   o   o    / w            |  o   o
 *      |      /----/               |    \----\
 *      |  x  /   o                 |  x  \   o      the line, after
 *      | x  /                      | x    \         the point it got
 *      +---------- x1              +---------- x1   wrong pushed it
 *
 * Rosenblatt proved it *always* finishes, in a bounded number of
 * mistakes, if such a line exists (the perceptron convergence
 * theorem). Everything hangs on that "if".
 *
 * {1 The failure that is the point}
 *
 * XOR:
 *
 *     x2
 *      1 |  1       0           no straight line puts the two 1s on
 *        |                      one side and the two 0s on the other
 *      0 |  0       1
 *        +-------------- x1
 *           0       1
 *
 * So the rule never settles: it keeps swapping which pair it gets
 * wrong, for ever. Measured (Unit_neuron, and worth knowing exactly):
 *
 *     AND, OR          4 of 4, and it stops
 *     the best line on XOR   3 of 4   (a line can separate one corner)
 *     the rule on XOR        2 of 4   at every seed and every number
 *                                     of epochs tried
 *
 * Read the last two lines twice. Not only is XOR out of reach of any
 * line; the rule falls into a cycle that is *worse than the best line
 * it could have drawn*, because it moves on every mistake and the
 * mistakes never stop, so it never rests anywhere good. A learning
 * rule that cannot converge does not politely stop at the best
 * approximation -- it wanders.
 *
 * Minsky and Papert's *Perceptrons* (1969) made the representation
 * point rigorously, the money left the field for a decade, and the way
 * out -- stack the neurons, and find something that can train a stack
 * -- is Net.mli and Backprop.mli.
 *
 * Keeping the failure runnable is deliberate: it is the shortest
 * demonstration that *what a model can represent* and *what it can be
 * trained to represent* are two separate questions.
 *
 * References: Warren McCulloch, Walter Pitts, "A Logical Calculus of
 * the Ideas Immanent in Nervous Activity", 1943; Frank Rosenblatt,
 * "The Perceptron: A Probabilistic Model for Information Storage and
 * Organization in the Brain", 1958; Marvin Minsky, Seymour Papert,
 * *Perceptrons*, 1969. *)

type t = {
  weights : float array;
  bias : float;
}

(* an example: what it is shown, and what it should answer (0 or 1) *)
type example = float array * float

(* [make ~inputs ~seed]: small random weights (see Matrix.random for
 * why not zero -- here it only breaks the tie on the first example) *)
val make : inputs:int -> seed:int -> t

(* [sum n x]: the weighted sum before the squash, [w . x + b] -- the
 * signed distance to the line, scaled by the weights' length *)
val sum : t -> float array -> float

(* [answer n x]: 1. if [sum] is above zero, 0. otherwise *)
val answer : t -> float array -> float

(* [learn ~rate n example]: the rule above, once. A right answer
 * changes nothing at all, which is the whole of it: the perceptron
 * learns only from its mistakes. *)
val learn : ?rate:float -> t -> example -> t

(* [epoch ~rate n examples]: [learn] over every example, in order *)
val epoch : ?rate:float -> t -> example list -> t

(* [train ~epochs ~rate n examples]: that, so many times, stopping
 * early the moment an epoch makes no mistake -- there is nothing left
 * to learn from, and on a problem that has a line this always happens
 * (Rosenblatt's theorem) *)
val train : ?epochs:int -> ?rate:float -> t -> example list -> t

(* [mistakes n examples]: how many it gets wrong *)
val mistakes : t -> example list -> int

(* [learns examples]: the share of [examples] a trained perceptron
 * gets right, between 0 and 1 -- 1 for AND, 0.5 for XOR, and no
 * amount of training moves the second *)
val learns : ?epochs:int -> ?seed:int -> example list -> float

(* {1 The four two-input problems worth trying} *)

val and_ : example list
val or_ : example list
val xor : example list
