# Learning, from scratch: neural networks in `ai/`

The other half of [`notes_ai.md`](notes_ai.md). There, every rule was
written by a person: the ghost's target tile, the weight of a corner,
the three flocking forces. Here nobody writes the rule -- a few
thousand numbers are nudged until the behaviour comes out. It is the
same subject seen from the other end, and it is one long application
of the chain rule.

It is also the specification of the learning modules of `ai/` (see
[`plan_ai_teaching.md`](../plans/plan_ai_teaching.md)), written before
them. The constraint that shapes everything below: **pure OCaml, no
BLAS, no GPU, and it has to run in the browser too**. That rules out
the modern scale and rules *in* the thing worth teaching -- every
number in this note is one a laptop computes in seconds, and every
formula is one a reader can check with a pen.

## 0. Where the code is, and a reading order

| module (`ai/`) | what | section |
|---|---|---|
| `Matrix` (done) | dense float matrices, the naive loops | §2 |
| `Neuron` (done) | the perceptron, its rule, and what it cannot do | §1 |
| `Net` (done) | layers, activations, the forward pass | §2 |
| `Backprop` (done) | the loss, gradient descent, the chain rule | §3, §4 |
| `Grad` (done) | reverse-mode autodiff: the same, written once | §5 |
| `Train` (done) | batches, learning rate, train/test, the loop | §6 |
| `Qlearn` (done) | rewards, temporal difference, Q-learning | §8 |

Sections 1 to 7 are supervised learning (here are the answers, find the
rule); 8 and 9 are learning to *play*, where nobody knows the answers
and the program has to find out by playing itself.

## 1. One neuron

Take some inputs, weight them, add a bias, and squash:

```
   x1 --w1--\
   x2 --w2---> sum ---> f ---> a          a = f(w1 x1 + w2 x2 + b)
   x3 --w3--/    +b
```

With a step function for `f`, that is Rosenblatt's **perceptron**
(1958) -- and it has a learning rule so simple it fits on one line: for
each example, if the output is wrong, push the weights toward the
right answer by the input.

```
   w <- w + rate * (target - output) * x
```

Show it a few hundred labelled points and the line separating them
walks into place. `examples/AiPerceptron.ml` is exactly this: click to
drop red and blue points, watch the line move.

Then show it XOR:

```
     x2
      1 |  blue     red              no straight line separates
        |                            the blues from the reds
      0 |  red      blue
        +-------------- x1
           0        1
```

and it never settles. A perceptron computes a line (a hyperplane); XOR
is not a line.

Measured, now that it is written (`Unit_neuron`, and the numbers are
worth more than the story): AND and OR, four of four, and it *stops* --
no example moves it again. XOR, two of four, at every seed and every
number of epochs tried. And the best line that exists gets **three** of
the four, by cutting off one corner. So the rule does not merely fail
to converge: it ends up worse than the best line it could have drawn,
because it moves on every mistake and the mistakes never stop. A
learning rule that cannot converge does not politely stop at the best
approximation; it wanders. `examples/AiPerceptron.ml` shows both: the
line walking into place and stopping dead on AND, and swinging for ever
on XOR. Minsky and Papert's *Perceptrons* (1969) made that point
precisely, and the field's funding went with it for over a decade --
the first "AI winter". The fix was known in principle (stack the
neurons) and useless in practice (nobody could train a stack), and that
is the gap §4 closes.

Keeping this failure in the module is deliberate: it is the shortest
demonstration that *what a model can represent* and *what it can be
trained to represent* are two separate questions, and both can kill
you.

## 2. Layers, and why they're matrices

Stack neurons into layers: every neuron of a layer sees every output of
the one before. A layer of `m` neurons over `n` inputs is an `m x n`
matrix of weights, a vector of `m` biases, and an activation:

```
   a = f(W a' + b)

   inputs      hidden          output
     o -----> o
     o -----> o -----> o       2 -> 3 -> 1:  W1 is 3x2, W2 is 1x3
     o -----> o                10 weights and 4 biases
```

The whole forward pass of a network is that line, once per layer.
`Matrix` is three nested loops over a flat float array; that is the
naive version, and it stays in the module beside a faster one exactly
as `graphics/Opti` keeps the simple rasterizer beside the fast one
(`Matrix.fast` switches, and both are tested to agree).

What the faster one changes is not the arithmetic but the reading:
walking down a column of the right-hand matrix jumps a whole row at
every step, so it copies that matrix transposed first and then walks
both along rows, four products at a time. Measured on this machine,
for one square product: 0.9 ms against 0.6 at n = 64, 7.5 against 4.0
at 128, 65 against 30 at 256 -- between 1.6x and 2.2x, growing with n
as a column stops fitting in cache. Twice, not ten times, and that is
the honest shape of the thing: the next factor of five is blocking the
work so that a *piece* of each matrix stays in cache across many
products, which is what BLAS does and what this deliberately does not.

`f` has to be non-linear, or the stack collapses: `W2 (W1 x)` is just
`(W2 W1) x`, one layer again. Three choices, and the history is in
them:

```
   sigmoid   1 / (1 + e^-z)      smooth, 0..1; what 1986 used; saturates
   tanh      -1..1               the same, centred; TD-Gammon's
   relu      max(0, z)           a kink; trains far faster in deep nets
```

## 3. The loss, and walking downhill

Training needs a single number saying how wrong the network is over the
examples -- the **loss**. For fitting numbers, the mean squared error
`½(a - y)²`; for choosing among classes, cross-entropy with a softmax.

The loss is a function of the weights. Change one weight a little, the
loss changes a little: that ratio is `dL/dw`. Compute it for every
weight and you have the **gradient**, the direction of steepest
increase; step the other way:

```
   w <- w - rate * dL/dw
```

That is all of gradient descent, and the two failure modes are already
visible in it. Too small a `rate` and nothing moves; too large and the
step overshoots the valley and the loss explodes to NaN in a dozen
iterations. Everybody's first network diverges, and knowing that in
advance saves an evening.

## 4. Backpropagation, with numbers

The gradient could be computed by nudging each weight and re-running
the network (**finite differences**) -- one forward pass per weight,
hopeless for 17,000 weights. Backpropagation gets all of them in *one*
backward pass, by pushing the error back through the same graph the
forward pass came through, multiplying by local derivatives. It is the
chain rule, bookkept.

One neuron, one input, done by hand -- this is `Backprop`'s worked
example, and the `.mli`'s job is to make it checkable:

```
   x = 1,  w = 0.5,  b = 0,  target y = 1,  sigmoid, rate 1

   forward
     z = w x + b               = 0.5
     a = sigma(z)              = 0.62246
     L = 1/2 (a - y)^2         = 0.07127

   backward
     dL/da = a - y             = -0.37754
     da/dz = a (1 - a)         =  0.23500      (the sigmoid's derivative)
     dL/dz = dL/da * da/dz     = -0.08872
     dL/dw = dL/dz * x         = -0.08872      (dz/dw is just x)
     dL/db = dL/dz             = -0.08872

   step (rate 1; the bias moves too -- it has a gradient of its own,
         and leaving it out would be a different algorithm)
     w <- 0.5 + 0.08872        =  0.58872
     b <- 0.0 + 0.08872        =  0.08872
     z  = 0.58872 + 0.08872    =  0.67745
     a  = sigma(z)             =  0.66317      (closer to 1)
     L  = 0.05673                              (lower than 0.07127)
```

(Step the weight alone, as this arithmetic is often written out, and
a = 0.64307 with L = 0.06370: the same story, one nudge smaller.
`Unit_backprop` checks both numbers, which is how the discrepancy
turned up.)

Two structural facts are already in those eight lines, and they are the
whole of the subject. Each step needs only the values from the forward
pass at that node (`a`, `x`) and the derivative coming back
(`dL/dz`) -- so a network is trained by walking backwards through it
once, keeping what the forward pass computed. And `dL/dz` gets
multiplied by a factor at every layer, the activation's slope, which
for a sigmoid never exceeds 0.25. That is the **vanishing gradient**,
and it is why deep networks were untrainable for twenty years after
1986.

Measured, on a 4-8-8-8-8-8-1 network with the same starting weights
(`Unit_backprop`), the last layer's gradient against the first's:
sigmoid **2159 : 1**, tanh **0.7 : 1**, relu **1.4 : 1**. The first
number is the famous one; the second is the one worth keeping. A tanh
is a sigmoid stretched to -1..1, with a slope of 1 at the origin, and
Glorot initialisation is built to keep the signal steady through such
a layer -- so at this depth nothing vanishes at all, and the first
layer's gradient is if anything the larger. The sigmoid loses twice
over: its slope is at most 1/4, and its outputs sit around 0.5 rather
than 0, so each layer adds an offset the next must undo. Depth alone
does not kill a gradient; depth with the wrong squash and the wrong
starting weights does -- which is why `relu` and the initialisations
that go with it changed what was trainable.

`ai/tests/` checks backprop against finite differences on random
networks: the analytic gradient and `(L(w+e) - L(w-e)) / 2e` must agree
to six digits. It is the one test that catches every sign error, and no
network should be trusted without it.

## 5. Autodiff: the same derivatives, written once

`Backprop` writes the backward pass by hand, layer by layer, which is
the way to *understand* it and a bad way to live: add a layer type and
you write its derivative, forever.

`Grad` does it once. Every value carries its own little graph -- what
made it, from what -- and each operation knows only its own local
derivative. Ask the loss for its gradient, walk the graph backwards
once, and every weight has its `dL/dw`, whatever the network's shape:

```
   a = x * w  +  b     becomes a graph:     x   w
                                             \ /
                                              *   b
                                               \ /
                                                +
                                                |
                                                a     ...  L
```

This is **reverse-mode automatic differentiation** (Linnainmaa, 1970),
and it is what PyTorch is, underneath the CUDA. Written for scalars, in
OCaml, it is about eighty lines -- Karpathy's micrograd made that point
memorably. Keeping both modules is the pattern used everywhere in this
repository: the version that teaches the mechanism, and the version
that is actually pleasant, side by side -- and here they agree not to
six digits but to the *last* digit, over all 105 weights of a 2-8-8-1
network, because they are the same arithmetic in a different order
(`Unit_grad`).

What the convenience costs, measured on that gradient: 9.6 us by hand,
197 us through the graph, about 20x. That is why a real library runs
reverse mode over whole *arrays* -- one node per matrix multiply
instead of one per multiplication -- while the idea stays exactly this
one. And the reason to have it at all, in one line from the test: the
derivative of (x^2 + 3x)/(x - 1) at x = 2 is -3, I wrote -4 in the
test, and the graph was right.

## 6. Training, in practice

The parts that no formula warns you about:

- **Initialization**: all-zero weights make every neuron in a layer
  identical forever (they get identical gradients); random small ones,
  scaled by the layer's size (Xavier/Glorot, 2010; He, 2015), train.
- **Batches**: average the gradient over 32 examples instead of
  stepping on each one. Less noise, better use of the matrix
  multiply -- and the noise of small batches is itself useful, which is
  why nobody uses the full dataset.
- **Epochs, and overfitting**: keep some examples out of the training
  set. The training loss falls forever; the *held-out* loss falls and
  then rises, and where it turns is where the network stopped learning
  the rule and started memorising the examples. Drawing both curves is
  the single most useful picture in machine learning.
- **The learning rate**, still the one knob that matters most: too
  small, nothing; too large, NaN; and decaying it over time beats any
  fixed value.

## 7. Three examples, each watchable

The reason all of this belongs in a *playground*: training is a loop
with a picture, and 60 frames a second is plenty to watch a network
learn.

- `AiNeuralNet.ml` (**written**) -- points in two spirals, a 2-8-8-1
  network (105 weights), the decision boundary recoloured every few
  frames and the loss curve underneath. The keys are the lesson: "0"
  takes the hidden layers away and the boundary is a straight line
  that stays one -- 3 weights, the loss stuck at 0.075 against the
  full network's 0.023 -- which is model capacity, seen rather than
  defined; "s"/"t"/"e" swap the squash; "-" and "+" the learning rate;
  "h" holds out a quarter of the points and draws the second curve.
  (The ancestor is TensorFlow Playground, playground.tensorflow.org,
  which this project shares a name with by coincidence.)
- `AiPerceptron.ml` -- §1: the line, and XOR defeating it.
- `AiDigits.ml` (**written**) -- draw a digit with the mouse, get ten
  output bars. A 256-64-10 network, 17,098 weights, trained a few dozen
  examples a frame while you watch: about 80% on held-out digits after
  six thousand of them. **And no dataset is downloaded**: the training
  digits are drawn by our own Hershey font (`graphics/font`), each one
  shaken -- scaled, slanted, shifted, thickened, speckled -- and inked
  by distance to the pen strokes, so the edges are soft rather than
  jagged. Self-contained, and a pleasing loop: the renderer teaching
  the network. The instructive failure is built in: it does fine on
  digits that look like its font and worse on yours, which is what "the
  training distribution" means, concretely.

## 8. Learning to play

Supervised learning needs the answers. A game has none -- only a result
at the end, long after the move that caused it. That is the
**credit assignment** problem, and it has an old and beautiful answer.

**Rewards and values.** Instead of the right move, learn the *value* of
a position: how well it is likely to end. Then play by looking one move
ahead and taking the best value.

**Temporal difference** (Sutton, 1988). Don't wait for the end. If the
position after your move looks better than the one before, that
difference is itself the signal -- adjust the earlier estimate toward
the later one:

```
   V(s) <- V(s) + rate * ( r + gamma * V(s') - V(s) )
                          \_________________________/
                           the temporal difference:
                           what you now think, minus what you thought
```

`gamma` (the discount) is how much a reward later is worth than one
now; it is also what keeps the numbers finite in a game with no end.

**Q-learning** (Watkins, 1989) is the same on state-action pairs:
learn `Q(s, a)`, the value of doing `a` in `s`, and you no longer need
a model of the world at all -- you never have to know what a move does,
only what happened after it. On a small grid world that is a table, and
`examples/AiQlearn.ml` draws it: four numbers per cell, an arrow for
the best, a cliff to fall off, and the policy appearing over a few
hundred episodes -- watch the values seep backwards from the goal at
about one cell per episode, which is exactly how far one use of the
rule can carry them.

The exploration knob has a trapdoor in it, and the example ("e") and
the tests both open it. An action never tried is worth 0. Where every
step *costs* something -- this cliff, one point a step -- nought is
better than anything already tried, so a purely greedy learner tries
everything once anyway and still finds the shortest way with
exploration switched off. That is optimism in the initial values
(Sutton and Barto 2.6), turned on by accident by the *sign* of the
rewards. Pay only at the goal instead ("p") and the optimism goes: the
same learner does the same thing five hundred episodes running, ends
with **four** state-action pairs in its whole table, and never reaches
the goal at all.

Off-policy is the other thing to watch. The rule uses the best it
*could* do next, not what it will actually do, so it learns the
cliff-edge path -- the shortest one -- while behaving carelessly
enough to fall in eighty times ("g" walks what it has learned,
without dice). Learning about the policy actually followed is SARSA,
one symbol's difference, and it keeps a safer distance.

The history is the argument for taking this seriously at small scale.
Arthur Samuel's checkers player (1959) learned by playing itself on an
IBM 704 and beat its author -- the first program to learn a game, and
the source of the phrase "machine learning". Gerald Tesauro's
**TD-Gammon** (1992) was a network with **80 hidden units**, trained by
TD on self-play games, and it reached within a hair of the world's best
backgammon players -- and taught them opening moves the human canon had
wrong. Eighty hidden units is a network this project can train in
minutes. The size was never the point; matching the method to the game
was.

## 9. The two halves joined: a network inside the search

[`notes_ai.md`](notes_ai.md) §10 left Monte Carlo tree search at its
ceiling: random playouts, a weak amateur on 9x9 Go. Two places in that
loop are begging for a better guess, and a network fits each:

```
   select    ... + a prior from the network        which moves are worth trying
   expand
   simulate  replace the random playout entirely   how good is this position
   backup
```

That is AlphaGo's shape (Silver et al., 2016; AlphaGo Zero, 2017): a
**policy** head suggesting moves, a **value** head scoring positions,
MCTS using both, and the network trained on the search's own results --
the search makes the network better, the network makes the search
better. Written out, the loop is perhaps two hundred lines, and every
piece of it is in this directory.

**Both hooks are now in `Mcts`** (`?prior` and `?evaluate`), which is
the mechanical half of that paragraph. The selection rule becomes PUCT
when a policy is given -- an unvisited move is no longer infinitely
attractive, it is as attractive as the policy says -- and the value,
when given, replaces the playout entirely. Measured on tic-tac-toe
with a *perfect* value function standing in for a trained one
(`Unit_mcts`, so that the hook is measured and not the network): at
twelve playouts it finds the winning move in 12 of 12 won positions
against 10 of 12 for random playouts, a pointed policy takes 93% of
the visits where a flat one takes 73%, and over twenty games at forty
playouts each the searcher with both wins 11 and loses 0 to the
2006-style version of itself.

Two things fell out of writing it, and both are worth more than the
numbers. A policy must be a *distribution*: priors that do not sum to
1 make PUCT's exploring term swamp the win rate, and the search then
spreads its visits evenly over good moves and bad -- which is how the
first version of that twenty-game match came out 8-4 instead of 11-0.
And "the most visited move" decides nothing at small budgets, where
every child has been visited once: ties now go to the better win rate,
without which a search with a *perfect* evaluation was picking losing
moves at twelve playouts.

The last surprise is about the game rather than the code. From an
empty board, random playouts choose the centre, which everyone knows
is right. The perfect evaluation does not: with best play every
opening move draws, so all nine are worth exactly the same and it
takes any of them. "The centre is best" is not a fact about
tic-tac-toe, it is a fact about opponents who make mistakes -- which
is what playouts measure and a perfect value has no opinion about.

What is *not* here is the compute, nor `AiGo` wired to a network:
AlphaGo Zero was thousands of TPUs for days. On a laptop, in OCaml, on 9x9, with a few thousand weights
and a few thousand self-play games, the realistic outcome is a player
that beats its own random-playout version and loses to a decent human
-- and that is the result to report. The thing being taught is that the
mechanism is small and the scale is not, which is a more useful thing
to know about modern AI than any benchmark.

## 10. What this deliberately isn't, and exercises

No GPU, no convolutions at real scale, no transformers, no pretrained
weights, nothing that needs a download. Those are engineering at a size
this repository cannot teach honestly, and their absence costs nothing
here: every idea above -- the neuron, the chain rule, the gradient, the
value function, the search guided by a guess -- is exactly the same at
seventeen thousand weights as at seventeen billion. The rest is
hardware.

**PyTorch.** `Grad` is PyTorch's `autograd` with the tensors taken out:
PyTorch's graph nodes hold whole arrays, so one node of its graph is a
matrix multiply that ours spells out as thousands of scalar nodes, and
the arithmetic under it is BLAS on a CPU or CUDA kernels on a GPU,
where `Matrix` is three OCaml loops. That is the whole difference in
speed, several orders of magnitude, and none in what gets computed:
the finite-difference test of §4 passes the same way for both.
**micrograd** is the nearest relative, scalar autodiff in about a
hundred lines of Python; `Grad` is the same idea in OCaml, and
`Backprop` beside it is what micrograd leaves out on purpose -- the
derivatives written by hand once. The libraries, the games that learned
and the teaching lineage are in
[`notes_ai_related_work.md`](../related-work/notes_ai_related_work.md).

What real systems have that this design leaves out, each a good
exercise once its module exists, in rough order of difficulty:

- momentum, then Adam (Kingma and Ba, 2015), in `Train`'s step instead
  of §3's plain `w <- w - rate * dL/dw`, and the spiral's loss curve
  with each;
- early stopping, `Train` watching §6's held-out curve and keeping the
  weights from where it turned;
- weight decay and dropout (Srivastava et al., 2014), and the gap
  between the two curves of §6 shrinking;
- forward-mode autodiff (dual numbers) beside `Grad`'s reverse mode,
  and why reverse wins when there are many weights and one loss;
- TD(λ)'s eligibility traces in `Qlearn` (§8), which TD-Gammon used
  instead of the one-step update;
- a network instead of `Qlearn`'s table, with DQN's experience replay
  and target network (Mnih et al., 2015), which are what keep it from
  diverging;
- one small convolutional layer for `AiDigits.ml` (LeCun et al., 1998),
  its weights shared across the image, against the 256-64-10 network;
- §9's loop on tic-tac-toe or connect four before 9x9 Go, where a
  result comes in minutes and perfect play is known to check it.

## Glossary

- **Perceptron**: one neuron with a step, and a one-line learning rule;
  **linearly separable**: what it can (and XOR cannot) do.
- **Activation** (sigmoid, tanh, relu): the non-linearity, without
  which a stack of layers is one layer.
- **Loss**: how wrong, as one number; **MSE**, **cross-entropy**.
- **Gradient**: the derivative of the loss by every weight;
  **gradient descent**, **learning rate**.
- **Backpropagation**: all the derivatives in one backward pass;
  **chain rule**; **finite differences**, its slow check.
- **Vanishing gradient**: the backward signal multiplied away layer by
  layer -- why relu replaced sigmoid.
- **Autodiff** (reverse mode): backpropagation for arbitrary graphs,
  written once.
- **Batch**, **epoch**, **overfitting**, **held-out set**.
- **Initialization**: why not zeros.
- **Reward**, **value function** V(s), **Q(s, a)**, **discount**
  (gamma), **policy**.
- **Credit assignment**: which of the fifty moves lost the game.
- **Temporal difference**, **Q-learning**, **epsilon-greedy**,
  **self-play**.
- **Policy head**, **value head**: a network's two answers inside a
  search.

## References

- Frank Rosenblatt, "The Perceptron: A Probabilistic Model for
  Information Storage and Organization in the Brain", Psychological
  Review 65(6), 1958.
- Arthur L. Samuel, "Some Studies in Machine Learning Using the Game of
  Checkers", IBM Journal of Research and Development 3(3), 1959.
- Marvin Minsky, Seymour Papert, "Perceptrons", MIT Press, 1969.
- Seppo Linnainmaa, master's thesis, University of Helsinki, 1970
  (reverse-mode differentiation).
- David E. Rumelhart, Geoffrey E. Hinton, Ronald J. Williams, "Learning
  representations by back-propagating errors", Nature 323, 1986.
- Richard S. Sutton, "Learning to Predict by the Methods of Temporal
  Differences", Machine Learning 3, 1988.
- Christopher J. C. H. Watkins, "Learning from Delayed Rewards", PhD
  thesis, University of Cambridge, 1989.
- Gerald Tesauro, "Practical Issues in Temporal Difference Learning",
  Machine Learning 8, 1992.
- Gerald Tesauro, "Temporal Difference Learning and TD-Gammon",
  Communications of the ACM 38(3), 1995.
- Yann LeCun, Léon Bottou, Yoshua Bengio, Patrick Haffner,
  "Gradient-Based Learning Applied to Document Recognition",
  Proceedings of the IEEE 86(11), 1998.
- Richard S. Sutton, Andrew G. Barto, "Reinforcement Learning: An
  Introduction", MIT Press, 1998.
- Xavier Glorot, Yoshua Bengio, "Understanding the difficulty of
  training deep feedforward neural networks", AISTATS 2010.
- Vinod Nair, Geoffrey E. Hinton, "Rectified Linear Units Improve
  Restricted Boltzmann Machines", ICML 2010.
- Nitish Srivastava, Geoffrey Hinton, Alex Krizhevsky, Ilya Sutskever,
  Ruslan Salakhutdinov, "Dropout: A Simple Way to Prevent Neural
  Networks from Overfitting", Journal of Machine Learning Research 15,
  2014.
- Diederik P. Kingma, Jimmy Ba, "Adam: A Method for Stochastic
  Optimization", ICLR 2015.
- Kaiming He, Xiangyu Zhang, Shaoqing Ren, Jian Sun, "Delving Deep into
  Rectifiers", ICCV 2015.
- Volodymyr Mnih et al., "Human-level control through deep
  reinforcement learning", Nature 518, 2015 (DQN).
- David Silver et al., "Mastering the game of Go with deep neural
  networks and tree search", Nature 529, 2016 (AlphaGo).
- Daniel Smilkov, Shan Carter, "A Neural Network Playground",
  playground.tensorflow.org, 2016.
- David Silver et al., "Mastering the game of Go without human
  knowledge", Nature 550, 2017 (AlphaGo Zero).
- Andrej Karpathy, "micrograd", github.com/karpathy/micrograd, 2020.
