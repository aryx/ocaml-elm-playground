# The dark arts of machine learning: what went wrong, and what each taught

The tutorial ([`notes_ai_learning.md`](notes_ai_learning.md)) tells
the ideas in the order that makes them clear: a table of letter pairs,
a network, attention, arrays, self-play. Written that way it reads as
if each thing worked the first time. None did.

This is the other half, the part no paper has: what went wrong while
`libs/ai/` got its language models and its self-play
(`plans/plan_ai_zero_to_hero.md`), how each failure showed itself, and
what it taught. They are called the dark arts because they are passed
on by apprenticeship rather than written down, and because of what
makes machine learning unlike other programming:

**a learner that is wrong still gives numbers.** A sort that is wrong
gives an unsorted list. A network trained by a broken loop gives a
loss that goes down, names that look like names, a player that beats a
random one. Nothing crashes. The bug is a number a little worse than
it should be, and nobody knows what it should be. So the craft is
mostly this: knowing what a number ought to be before looking at it,
and not believing it when it is not.

Every entry below is something that happened here, with the numbers
it happened with. It is to be added to as the later phases fail in
their own ways.

## The entries

**A cost blamed on the idea was ours** (`Grad`). Scalar autodiff
measured 20 times slower than the hand-written backward pass, and the
`.mli` explained why that was the price of the approach. It was 4.5:
the walk that orders the nodes looked each one up in a list of those
seen, quadratic, and a network of 105 weights had hidden it. *Before
explaining a number, check it is not a bug.*

**The time was the collector's, not the code's** (`Ngram_mlp`). A
training step took 45 ms and nothing in the profile of the arithmetic
said why. One run with a larger minor heap took 12: a step's graph was
being promoted before it died. *Try the runtime's parameters before
rewriting anything.*

**A test asserted what I expected instead of what was true** (`Gpt`).
"With attention it does better" failed to hold at 300 steps: with and
without, 2.451 and 2.451. At 5,000 steps the difference is there
(2.269 against 2.307), and without positions *or* attention it falls
back to the table of pairs. The architecture's ideas pay with
training, and the first table I wrote would have shown them paying at
once. *Measure the claim before writing the sentence, and say at what
budget it is true.*

**Faster by ten, expected by fifty** (`Tensor`). Whole arrays were to
be the end of the cost; they bought 5x to 12x. What was left was not
the graph any more: a layer written as a product with a transposed
copy made six passes over the weights where one was needed.
`mul_t` took it to 10x to 47x. And the plan's estimate of what a night
can train had assumed a speed nobody had measured; it was off by five
until this was fixed. *An estimate is a measurement not yet made.*

**The same seed stopped giving the same file** (`Weights`). The
trainer promises its weights byte for byte from a seed. Each time the
arithmetic was reordered -- arrays for scalars, `mul_t` for `mul` --
the sums came out a last digit apart, and thirty thousand steps later
the file was another file (2.2188, 2.2180, 2.2145). Reproducible means
*with this code*: the weights have to be made again whenever the
arithmetic under them changes, and the file's header is the record of
what made it.

**Self-play that learned nothing** (`Mcts`, section 16). After 240
games it still lost to a random player now and then. The network and
the lessons were right; the search was not searching. With a policy it
still tried every move of a position once before preferring any, so
fifty playouts saw two moves ahead, and "the search looks further than
the network" -- the premise of the whole loop -- was false. *When a
loop of two parts does not improve, test each part alone*: the search
with a perfect value function had been tested, the search with a weak
one at a small budget had not.

**A graph built to be thrown away** (`Policy_value`). The search asked
the network's opinion through the autodiff graph: a matrix of slopes
allocated and cleared for every weight, a hundred times a move, never
used. A plain forward pass, and the policy asked once per node of the
tree instead of at every visit: a search of a hundred playouts from 19
ms to 8, with the same answers. *What is right for learning is wrong
for using.*

**Twenty games that were two** (`Arena`, the Connect 4 trainer). The
scores against alpha-beta came out 5-0-5, 10-0-0, 0-0-10 and never
anything between. Both players were without dice, so the ten games of
a colour were one game ten times, and the trainer was reporting two
results with the confidence of twenty. Two random opening moves a
game, and the same players score 3-0-17. *A measure made of repeats
measures once*; and scores that only take round values are saying so.

**Not enough games, and the machine idle** (the Connect 4 trainer).
Seventy iterations, 2,900 games, and it was still level with the
search it started from. AlphaZero.jl's tutorial uses that many games
in a few minutes. The games of an iteration do not depend on one
another: 48 processes, a fork and a pipe each, and an iteration is 480
games in the time of 40; and a board mirrored left to right is a
second lesson for free. *In self-play, data is the budget, and it is
the easiest thing to multiply.*

**A training that died without a word.** The first long run stopped at
iteration 16 with nothing in its log. It was never explained. It cost
nothing, because the trainer writes its weights after every iteration
and can go on from the file. *A run of more than a few minutes
checkpoints, from the first version.*

**A long run that learned in its first third** (the Connect 4
trainer). Measured every fifth iteration, the score against alpha-beta
at depth 7 went from 0-0-20 to 10-0-10 in 45 iterations, and then
stayed there for a hundred more: 38 minutes spent where 12 would have
done. Nothing was wrong; the network had learned what a network of
that shape can learn from games of that quality. More of the same
does not move a plateau. What does is changing what limits it: here,
more search when *playing* (1,600 playouts instead of 100: 18-0-22
becomes 26-1-13), and, not yet tried, a network that sees the board
as a board. *Measure as it goes, and stop when the curve does; then
ask what the limit is, not how much longer.*

## The pattern

Each mistake was invisible at the size where the code was written and
tested, and appeared one size up: 105 weights to 4,000, one game to
twenty, 4,000 numbers to 800,000. The worked example in an `.mli`
proves the idea; only the next size proves the code.

And each was found by the same means, a number compared with what it
should have been:

- **a known result to hit.** The bigram's 2.454 is makemore's, and the
  counts are makemore's too; had they differed, the tokenizer or the
  loss was wrong, and nothing else would have said so. This is why
  the datasets are Karpathy's own (`data/README.md`).
- **two ways to the same number.** The table counted and the table
  learned; the slopes by the graph and by a nudge; the model on
  scalars and on arrays; the network through the graph and by a plain
  pass. Each pair is a test that needs no expected value.
- **a truth to play against.** Tic-tac-toe is solved, so "never loses
  to the perfect player" is a fact that can be checked; Connect 4 has
  a player of known strength at each depth.
- **the simplest thing as a floor.** Knowing nothing is log 27; a
  model worse than the table of pairs is broken, not weak; a player
  that loses to a random one has a bug.

## When a learner does not learn: what to try, in this order

1. **Is the number to beat known?** Knowing nothing, the simplest
   model, a published figure. Without one, stop and find one.
2. **Does one example learn?** One lesson, repeated: the loss should
   go to nothing (`Unit_selfplay`'s lesson, `Unit_ngram_mlp`'s batch).
   If it does not, it is the gradient or the step, not the data.
3. **Do the slopes agree with a nudge?** (`Unit_tensor`, `Unit_gpt`.)
4. **Is each part right alone?** The search with a perfect value; the
   network on lessons known to be right.
5. **Is the measure measuring?** Scores that only take round values,
   curves too smooth, a loss on the data it trained on.
6. **Then, and only then: more.** More data first (it is the cheapest
   to multiply), then more steps, then a larger model.

## References

- Andrej Karpathy, "A Recipe for Training Neural Networks", 2019: the
  same advice from far more experience -- among much else, to overfit
  one batch before anything.
- Léon Bottou, "Stochastic Gradient Descent Tricks", 2012; Yann LeCun,
  Léon Bottou, Genevieve Orr, Klaus-Robert Müller, "Efficient
  BackProp", 1998: the older lore.
- D. Sculley et al., "Hidden Technical Debt in Machine Learning
  Systems", 2015: what the same failures cost at scale.
