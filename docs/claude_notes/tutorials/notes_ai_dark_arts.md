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

**The experiment that ran the old program, twice** (the Connect 4
trainer). The trainer is copied out of the build before a long run,
so that a rebuild cannot change it under a run in progress. Once the
copy failed (the old copy was read-only) and once the edit before it
had not applied (a script that changed five files stopped at a syntax
error and changed none); both times the command went on, the old
program ran for minutes, and its numbers were read as the new
program's. What gave it away was a number *exactly* the same as
before -- 172 s for the first iteration, to the second -- where a
change had been made to make it faster. *A run should say what it is:*
the trainer now prints its network's size, its processes and what
each phase of an iteration cost, and a figure that has not moved at
all after a change is a reason to check the change, not the idea.

**The bottleneck guessed, and guessed wrong** (the convolutional
network). An iteration went from 9 s to 145 s. The arithmetic said
the training steps had to be the cost -- 600 steps of 64 positions,
each three passes through the convolutions, in one process -- so the
steps were given 16 processes. The iteration took 160 s. Timed, at
last: the games were 58 s of it, not the 10 assumed, and the steps
with all their processes were still 86. Both halves were slow because
the network was simply four times too large for the hour available,
and no cleverness in the loop changes that. *Time the phases before
optimising one of them*; it is one line of code, and it was written
after the optimisation it should have come before.

**A process per step, and the fix that made it three times worse**
(the convolutional network's trainer). To share a large batch among
sixteen processes, the trainer forked sixteen at every step. Three
hundred steps took 86 s, 0.3 s each for a tenth of that in arithmetic.
The guess was the collector: a child starts with its parent's whole
memory, shared until one of them writes to it, and a major collection
in the child writes to all of it. So the minor heap was made large,
256 MB, to keep the collector quiet. The steps then took 188 s, and
286 the next iteration. A forked child *writes in* its minor heap,
which is its parent's until written to, so every child of every step
was now copying up to 256 MB of it. The cure for the collector was
the disease, moved.

The answer was not to fork at every step at all. Sixteen learners,
forked once an iteration, each take the network as it is and a
hundred steps of their own, apart; then the sixteen networks are
averaged, number by number. The same lessons seen, 10 s.

*Fork is cheap to call and dear to use*: what it costs is every page
the child then touches, and a garbage-collected program touches pages
you did not ask it to. Do the work in few, long-lived processes; and
when a fix makes things worse, the model of the problem was wrong --
the fix was right for the model.

**Two changes at once, and a worse result** (the convolutional
network on Connect 4). The run that was to beat the flat network
scored 1-1-18 against alpha-beta at depth 7 where the flat one had
10-0-10, and its loss had gone from 2.89 to 2.68 in forty-six minutes.
Two things were new in it, the network's shape and the way of taking
steps (learners apart, then averaged), so either could be broken, and
the result alone could not say which.

Each was tried alone, on ten thousand lessons that do not change (the
games of the trained flat network). The convolutional network, with
plain steps: held-out loss 2.95 to 1.93, as good as the flat one's
1.86 with a third of the numbers. The averaging, on the flat network:
sixteen learners averaged reach 1.85, one learner 1.90. Neither was
broken.

What was wrong was a sum. Thirty-two learners averaged are not
thirty-two times the steps: averaging takes the noise out of a
hundred steps, it does not make them three thousand. The run had done
6,000 steps where the flat network had done 27,000 by the time it was
level with depth 7, with a network that learns less per step. And a
loss of 2.7 was no sign of anything: while the network is weak the
search's visits are nearly even over the seven columns, and "even
over seven" is a loss of log 7 = 1.95 for the policy plus what the
value cannot know yet. The loss has a floor set by the lessons, and
the lessons get better only as the network does.

*Change one thing at a time, or keep a fixed set of lessons on which
each part can be tried alone.* And know what the loss can reach
before reading it: here the number to watch was never the loss, it
was the games.

**Better against one, worse against the other** (the two Connect 4
networks). Given its fair budget -- two layers, three hundred steps a
learner, a hundred iterations, 85 minutes -- the convolutional network
of 6,087 numbers was measured against the flat one of 28,424, over 40
games each, at 100, 400 and 1,600 playouts a move:

```
against alpha-beta at depth 7      flat  18-0-22   20-2-18   26-1-13
                                   conv  20-0-20   25-0-15   32-0-8
conv against flat, directly              13-1-26   14-0-26   18-0-22
```

Against the game's own computer the convolutional network is the
better at every budget. Against the flat network itself it loses.
There is no contradiction, only a wrong question: "which is the
stronger" supposes strength is one number, and it is not. Each
network has its own blind spots, alpha-beta at depth 7 has others,
and a player is measured by whom it is measured against. *One
opponent is one opinion.* It is why a rating needs a pool of players,
and why the trainer measures against several that learn nothing; and
it left the decision of which network ships to something other than a
score (the flat one stays: it is the one already trained, described
and drawn).

What the comparison did settle: reading the board as a board is worth
a factor of four in numbers, the same play from 6,087 as from 28,424.
What it did not: the plateau. Neither shape passed alpha-beta at depth
7 by much at the budget they trained with. The limit is somewhere
else -- the hundred playouts of the games they learn from are the
next suspect.

**A run of four hours ended by one process of forty-eight** (the Go
trainer). At its ninety-seventh iteration of a hundred the trainer
stopped on `End_of_file`: one of the processes playing that
iteration's games had sent nothing back. The weights of the
ninety-sixth were on disk, as they are after every iteration, so the
run was not lost; but nothing said which process, or why.

The saved network being exactly the one that iteration started from,
and the games' seeds depending only on the iteration, the iteration
could be played again: it went through. So no game of it raises; the
process was killed from outside, by what was never found (the second
such death in this work, the first having taken a whole run at its
sixteenth iteration).

Two changes, both late. A process now sends back its result *or why
it has none*, so that an exception in a game would be read and not
guessed at. And a process lost is some games fewer, said on the
output, not the end of the run: an iteration with 188 games is as
good as one with 192. *The more processes and the more hours, the
more certain that one of them dies; a long run is built for that from
the start, not after the first time.* (And being able to play an
iteration again exactly, from a file and a number, is what turned "it
crashed" into "it was killed": seeds are a debugging tool before they
are a scientific one.)

**The same method, a far better result** (Go against Connect 4). On
Connect 4 the trained network came level with the game's own computer
and no further; on Go it beat the game's own computer four games in
five with a tenth of the playouts, and two in three with no search at
all. Nothing in the method had improved. The opponents differ:
Connect 4's was told what a position is worth and searches exactly,
Go's was told nothing and plays games out at random. A result is the
method *and what it was measured against*, and "it beats the program
we had" says as much about the program we had. The honest comparison
across the two games is not the scores but this: what was the
opponent told?

**Found, then lost** (`Dqn` on the cliff). The table of `Qlearn`
finds the shortest way along the cliff and keeps it. The network
found it at its 51st episode, and at its 300th was walking into a
wall. Nothing had broken. A table's answer for one cell is that
cell's alone; a network's answers share their weights, so teaching it
about one cell moves what it says of its neighbours, and a policy
that reads "the best of four numbers" flips when two of them cross.
Replay and the frozen target make this survivable, not absent, and
the published curves of DQN are as jagged as this for the same
reason. *In reinforcement learning the last network is not the best
one: keep the best seen, by a measure taken as it goes* -- which the
trainers of this repository already did for another reason. (The test
stops at the first time the way is right, and says so.)

And a small one from the same hour: the first `Dqn.step` took 2.7 ms
for a network of five thousand numbers. The target network was being
asked what the next state is worth once per *action* of each step
lived instead of once per step, four times the work, hidden in a
function called inside a loop that built a matrix. Found by the
arithmetic not adding up: thirty-two small passes cannot take a
millisecond.

**Eighty minutes of nothing, and what looking would have shown in
one** (DQN on TinyBreakout from the screen). The learner had passed
its check on six numbers of the game: 4.7 points a game at random,
498 after three minutes. Given the screen instead, 86 iterations and
13,000 steps later it scored 3 to 5 points, a random player's. Its
loss was small and falling, which said nothing: with a brick hit once
in fifty steps, a network that answers "nothing happens" is nearly
right.

The first thing done then should have been the first thing done at
all: *look at what it is given*. Three screens written out as
pictures showed it in a minute. At 42 by 42 the ball was two thirds
of a pixel, a dim dot; and the game's own effects (it shakes the
screen and flashes it when a brick breaks: "juice") moved or tinted
everything else between one frame and the next. The learner was being
asked to find a grey speck by comparing frames that differed
everywhere.

Then a check made for the question. Is it the picture, or is it
learning from a reward that comes late? Pay the same network, on the
same screens, a point for each move *towards the ball*, nothing to
wait for: no longer learning the game, only reading the screen. With
the effects off and 64 by 64, it went from choosing right a third of
the time to three quarters in fifteen minutes, and scored 30 points a
game as a side effect. The picture was readable; what was left was
the hard part, and time.

*Before a long run, look at one input with your own eyes, and give
the learner an easier question about the same input.* If it cannot
answer the easy one, the long run would have told you nothing; if it
can, you know which half is hard.

**The same mistake, a second time, with the lesson written down**
(DQN from the screen, again). With a readable screen the second run
did 14,000 steps of learning in a hundred minutes and scored a random
player's 3 to 7 points. The check on six numbers had needed 40,000
steps before its score left the ground. Nothing was wrong but the
count: sixteen learners each took 150 steps apart and were averaged,
and sixteen learners averaged are 150 steps, not 2,400 -- the entry
above about Connect 4 says exactly this, and the scheme had been
carried over from the board games without asking whether it still
fitted. There, a step was cheap next to a game, and noise was the
enemy. Here every step counts and there are too few.

What the cores can give a learner that needs *steps*: the slopes of
one batch worked out by several processes, a slice each, and one step
taken with their mean. The processes have to stay, a fork a step
being what it is (above): each is forked once an iteration, and then
asked, down a pipe, "the slopes for these numbers?" eight hundred
times. Fourteen steps a second where there were three.

(And the first version of that hung at its first iteration. A helper
was to stop when its pipe closed; but every helper forked after it
had inherited that pipe, open, so closing it in the parent closed
nothing. They are now *told* to stop. A pipe is closed when its last
holder closes it, and after a fork there are more holders than one
thinks.)

*A scheme that worked is a scheme that worked there.* Before reusing
one, ask what was scarce where it was made, and what is scarce here.

**A reward that was mostly not about the player** (DQN on
TinyBreakout from the screen, the run that learned). With fourteen
steps a second the third run passed 50,000 steps, where the check on
six numbers had long since taken off, and still scored a random
player's 3 to 5 points. So it was not only the count.

What does a beginner's score in this game say about its paddle?
Almost nothing. The serve sends the ball up, it breaks a brick, it
comes down, and it is missed: a point, earned by nobody. A player
moving at random gets nearly all its points that way. The part of the
score that depends on where the paddle is, the second brick, is rare,
and comes a second and a half after the move that deserved it. The
paper's learner dug that out of ten million frames. Ours had a ball
of one pixel and an afternoon.

One line changed: a ball lost costs a point, said at the step it
happens. The same network on the same screens then left the ground at
its fortieth iteration and reached 23 points a game at its ninetieth
(a random player: 4.7), the best of its games 42.

It is a departure from the paper, where a lost life only ends the
episode, and it is written wherever the result is: the learner is no
longer told the score alone. But the general thing is worth more than
the scruple. *Ask of a reward what share of it the learner's own
choices explain.* If the answer is "little", no architecture and no
count of steps is the fix; the signal is. The check that paid for
moving towards the ball had said as much two runs earlier, by
learning in minutes what the score could not teach in hours.

And the curve, once it rose, did what the cliff's did: 23 points at
iteration 90, 10 to 20 for the fifty after. The best network was
kept as it went, which is the only reason there is one to show.

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

0. **Have you looked at what it is given?** One input, as a picture or
   printed, before anything else.
1. **Is the number to beat known?** Knowing nothing, the simplest
   model, a published figure. Without one, stop and find one.
2. **Does one example learn?** One lesson, repeated: the loss should
   go to nothing (`Unit_selfplay`'s lesson, `Unit_ngram_mlp`'s batch).
   If it does not, it is the gradient or the step, not the data.
3. **Do the slopes agree with a nudge?** (`Unit_tensor`, `Unit_gpt`.)
4. **Is each part right alone?** The search with a perfect value; the
   network on lessons known to be right.
5. **Is the measure measuring?** Scores that only take round values,
   curves too smooth, a loss on the data it trained on, a loss whose
   floor nobody worked out.
6. **Did one thing change, or two?** If two, try each on lessons that
   do not change.
7. **Then, and only then: more.** More data first (it is the cheapest
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
