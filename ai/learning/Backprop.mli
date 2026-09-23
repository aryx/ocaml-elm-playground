(* Training a stack: the loss, the chain rule, and one step downhill
 * (notes_ai_learning.md sections 3 and 4).
 *
 * The loss says how wrong the network is over the examples -- here
 * half the squared error, 1/2 (a - y)^2 summed over the outputs and
 * averaged over the examples. It is a function of the weights, so
 * every weight has a slope [dL/dw], and the gradient is the direction
 * in which the loss rises fastest. Step the other way:
 *
 *     w <- w - rate * dL/dw
 *
 * The gradient could be had by nudging each weight and running the
 * network again (finite differences: [numeric] below), which costs one
 * forward pass per weight and is hopeless for seventeen thousand of
 * them. Backpropagation gets all of them in *one* backward pass, by
 * pushing the error back through the graph the forward pass came
 * through, multiplying by local derivatives on the way. It is the
 * chain rule, bookkept.
 *
 * {1 The worked example, by hand}
 *
 * One neuron, one input, a sigmoid, rate 1 (this is [Unit_backprop]'s
 * first test, to the digit):
 *
 *     x = 1,  w = 0.5,  b = 0,  target y = 1
 *
 *     forward
 *       z = w x + b            = 0.5
 *       a = sigma(z)           = 0.62246
 *       L = 1/2 (a - y)^2      = 0.07127
 *
 *     backward
 *       dL/da = a - y          = -0.37754
 *       da/dz = a (1 - a)      =  0.23500     (the sigmoid's slope)
 *       dL/dz = dL/da * da/dz  = -0.08872
 *       dL/dw = dL/dz * x      = -0.08872     (dz/dw is just x)
 *       dL/db = dL/dz          = -0.08872
 *
 *     step (rate 1, and note that b moves too -- it has a gradient,
 *           so leaving it alone would be a different algorithm)
 *       w <- 0.5 + 0.08872     =  0.58872
 *       b <- 0.0 + 0.08872     =  0.08872
 *       z = 0.58872 + 0.08872  =  0.67745
 *       a = sigma(z)           =  0.66317     (closer to 1)
 *       L = 0.05673                           (lower than 0.07127)
 *
 *     (with the weight alone stepped, as the arithmetic is often
 *     written out, a = 0.64307 and L = 0.06370: the same story, one
 *     nudge smaller.)
 *
 * Two structural facts are already in those eight lines, and they are
 * the whole subject. Every step needs only what the forward pass
 * computed at that node ([a], [x]) and the number coming back
 * ([dL/dz]) -- so a network is trained by walking backwards through it
 * once, keeping the forward pass's workings (Net.pass). And [dL/dz] is
 * multiplied by a factor at every layer on the way back.
 *
 * {1 The vanishing gradient, measured}
 *
 * Those factors are the activation's slope, and a sigmoid's is at most
 * 1/4. Measured here ([Unit_backprop], a 4-8-8-8-8-8-1 network, the
 * last layer's gradient against the first's):
 *
 *     sigmoid    2159 : 1    the first layer barely moves
 *     tanh        0.7 : 1    no vanishing at all
 *     relu        1.4 : 1
 *
 * The first line is the famous one and the second is the surprise
 * worth keeping. A tanh is a sigmoid stretched to -1..1, with a slope
 * of 1 at the origin instead of 1/4, and [Net.make]'s Glorot
 * initialisation is designed to keep the signal's size steady through
 * exactly such a layer -- so at this depth nothing vanishes, and the
 * first layer's gradient is if anything the larger. The sigmoid loses
 * on two counts at once: its slope never exceeds 1/4, and its outputs
 * sit around 0.5 rather than 0, so every layer adds a bias the next
 * one has to undo. Depth alone does not kill a gradient; depth with
 * the wrong squash and the wrong starting weights does, which is why
 * 1986's networks were shallow and why two changes -- [Relu], and
 * initialisations that account for it -- were enough to make deep ones
 * trainable.
 *
 * {1 The one test that matters}
 *
 * [numeric] is the gradient by finite differences,
 * (L(w + e) - L(w - e)) / 2e, which is slow, obviously right, and the
 * only way to know that a hand-written backward pass has no sign
 * error. [Unit_backprop] checks the two against each other on random
 * networks of every activation, to six digits. No network should be
 * trusted without that test.
 *
 * References: David Rumelhart, Geoffrey Hinton, Ronald Williams,
 * "Learning representations by back-propagating errors", Nature 1986;
 * Seppo Linnainmaa, 1970, for the same idea a decade and a half
 * earlier (Grad.mli); Yoshua Bengio, Patrice Simard, Paolo Frasconi,
 * "Learning long-term dependencies with gradient descent is
 * difficult", 1994, for why it stops at depth. *)

(* what a training example is: what goes in, and what should come out *)
type example = float array * float array

(* the gradient of the loss, one pair per layer, shaped like the
 * network itself *)
type grads = (Matrix.t * Matrix.t) list

(* [loss net examples]: half the squared error, averaged *)
val loss : Net.t -> example list -> float

(* [gradient net example]: the backward pass, one example *)
val gradient : Net.t -> example -> grads

(* [over net examples]: the gradient averaged over a batch, which is
 * what a step should be taken on (Train.mli, when it exists: the
 * averaging is the whole of "batch") *)
val over : Net.t -> example list -> grads

(* [step ~rate net grads]: one step downhill *)
val step : rate:float -> Net.t -> grads -> Net.t

(* [learn ~rate net examples]: [over] then [step], the loop's body *)
val learn : ?rate:float -> Net.t -> example list -> Net.t

(* [numeric ?epsilon net examples]: the same gradient by finite
 * differences -- slow, obviously right, and the test of the other *)
val numeric : ?epsilon:float -> Net.t -> example list -> grads

(* the size of a gradient, layer by layer: sqrt of the sum of the
 * squares, which is what the vanishing gradient is measured with *)
val magnitudes : grads -> float list
