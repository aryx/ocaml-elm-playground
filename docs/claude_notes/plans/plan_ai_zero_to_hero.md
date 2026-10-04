# Plan: from micrograd to a GPT and to AlphaZero, for teaching

## Context

`libs/ai/learning/` stops where Karpathy's course starts being fun. It
has one neuron, layers, backpropagation by hand, `Grad` (micrograd:
scalar reverse-mode autodiff, 91 lines, cited as its model), `Train`
and `Qlearn`. `libs/ai/search/` has `Mcts` with the two hooks a network
goes in, `?prior` (PUCT) and `?evaluate`, measured on tic-tac-toe with
a perfect value function standing in for a trained one. Nothing joins
them: no network is ever trained by the search, and `AiGo` and
`AiConnect4` use neither hook
(`remaining/plan_ai_remaining.md`, section 1, says why: a machine was
missing, and a place to keep what it learned).

Two lines of work come out of the same missing piece, an offline
trainer that writes a weights file a program embeds:

1. **Karpathy's sequence**, derived, not copied: micrograd (done),
   makemore (a bigram table, then Bengio's MLP), microgpt (a GPT in one
   file on scalar autograd), minbpe (the tokenizer), up to a small chat
   model in nanochat's shape -- the app `TinyChatGPT`, on a toy dataset.
2. **AlphaZero on Connect 4**, after Jonathan Laurent's AlphaZero.jl
   and its Connect Four tutorial: self-play, a network with a policy
   head and a value head trained on the search's own results, an arena
   where the new network plays the old one.

**The ideas to teach**, one per step, each a number you can watch:

- a count and a learned weight are the same thing (bigram);
- cross-entropy is a guessing game you can play by hand (Shannon, 1951);
- attention is a soft lookup into what came before;
- arrays instead of scalars is the same idea, several times faster
  (how many, Q4 measures);
- a tokenizer is compression (BPE);
- a chat model is a text model whose text is a dialogue;
- predicting and compressing are the same thing (a model plus an
  arithmetic coder, against our own `Deflate`);
- the search makes the network better, the network makes the search
  better (AlphaZero).

**The tiny rule**, as everywhere: each module small enough to read,
its `.mli` with the diagram, the worked example and the papers; the
older version kept beside the newer one behind a switch; what is left
out listed.

## Where it is going (the author's goals, 2026-10-04)

Three programs are the destinations; every phase is a step to one:

1. **`TinyChatGPT`**, simple: Q7, with Q10 behind it.
2. **`AiGo` with a trained AlphaZero**, 9 by 9: Q11.
3. **`AiChess` with a trained AlphaZero**: Q12.
4. **A network that learns to play our own arcade games**, from the
   score alone: DeepMind's Atari paper on `TinyBreakout` and `Pong`,
   Q13 (added 2026-10-04).

And in both games **the engine is a choice**, a flag and a key: the
engine the game has today (`AiChess`' alpha-beta with its evaluation,
`AiGo`'s random playouts), the search with the trained network, and
the network alone with no search at all (its policy's first choice:
what it "feels", which is the instructive one to lose to or beat).
`ai=classic|network|policy`, the default the one that measures
stronger.

What can honestly be promised, said before any of it is written: the
original AlphaZero's chess took thousands of TPUs for hours; a laptop's
night in pure OCaml is something like a millionth of that. So the
measure is never "superhuman", it is the same one each time, and it
is printed: the trained engine's score against the engine the game
already has, and against its own earlier self. On 9 by 9 Go, beating
the 1000-playout MCTS is a realistic goal, since the network replaces
exactly what is weak there. On chess, beating the existing alpha-beta
at its depth is not likely, and the phase is worth doing anyway for
what it shows: a program that was told only the rules, playing
recognisable chess. Both need Q4's `Tensor`, a convolutional body,
and trainers that run for hours and can be stopped and resumed.

## References, and what each gives

To be read again when each phase starts; the figures below are from
memory and are to be checked against the sources then.

- **micrograd** (Karpathy, 2020): `Grad` already.
- **makemore** (Karpathy, 2022) and its lectures: the bigram by counts
  and by gradient descent; the MLP of Bengio et al., "A Neural
  Probabilistic Language Model", 2003; initialisation and
  normalisation; backpropagation by hand (our `Backprop`).
- **microgpt** (Karpathy, 2026): a GPT trained and sampled in one
  dependency-free file, on names: characters as tokens with one
  begin/end token, scalar autograd, token and position embeddings,
  one layer of multi-head attention and an MLP with residuals, RMSNorm,
  no biases, Adam. A few thousand parameters.
- **minbpe** (Karpathy, 2024): byte pair encoding, train, encode,
  decode (Sennrich et al., 2016; Gage, 1994, the compression it was).
- **nanoGPT / nanochat** (Karpathy, 2022, 2025): the same model on
  arrays, and a dialogue as text with special tokens for whose turn it
  is.
- **Vaswani et al., "Attention Is All You Need", 2017**; Radford et
  al., GPT-2, 2019; Kingma and Ba, Adam, 2014.
- **Shannon, "Prediction and Entropy of Printed English", 1951**: the
  guessing game, bits per letter.
- **Eldan and Li, "TinyStories", 2023**: that a very small model
  speaks coherently if its world is small. The argument for a toy
  dataset rather than a small piece of a big one.
- **Weizenbaum, ELIZA, 1966**: the chat program with no learning, to
  put beside the one with nothing else.
- **The Little Learner** (Daniel Friedman, Anurag Mendhekar, 2023),
  and the author's own OCaml port of its chapters
  (`~/github/little-learner-ocaml/`): two ideas to take. Gradient
  descent as *one* loop whose parameters travel with an accompaniment
  (inflate, deflate, update), so that naked descent, velocity, RMS and
  Adam are four small records and Adam is visibly the two before it
  put together -- for an example racing the four down Rosenbrock's
  valley, and perhaps `Adam`'s own shape. And *extended* operators: a
  function written for one rank, lifted to any (`ext1`, `ext2`) -- to
  weigh for `Tensor` (Q4) against plain two-dimensional matrices.
- **AlphaZero.jl** (Jonathan Laurent) and its Connect Four tutorial:
  the loop as four named parts (self-play, memory, learning, arena),
  the benchmarks against plain MCTS and against a minimax player, the
  parameters one tunes. Silver et al., 2017 (AlphaGo Zero) and 2018
  (AlphaZero); Rosin, 2011 (PUCT), already in `Mcts.mli`.

## What exists, and what each phase leans on

| piece | where | state |
|---|---|---|
| scalar autodiff | `Grad` | done; `backward` orders the nodes with a list walk, quadratic (its `.mli` says so) |
| matrices | `Matrix` | done, with a fast multiply |
| a layered net, MSE only | `Net`, `Backprop`, `Train` | done; no softmax, no cross-entropy, one head |
| MCTS with `?prior`, `?evaluate` | `Mcts` | done; the two are called separately |
| a game as a record | `Minimax.game` | done |
| Connect 4's rules and evaluation | `AiConnect4.ml` | in the game's own file |
| ABC tunes read and played | `audio/formats` | done; one tune in the repository |
| seeds | `Lehmer` | done: the same numbers natively and on the web |

## Decisions

All decided 2026-10-04, as proposed.

- **D1. Where the new modules go.** `libs/ai/README.md`'s rule is that
  no folder uses another, and that what needs two "belongs to a new
  folder above both". So:
  - `libs/ai/learning/` gains what any network needs: `Tensor`
    (autodiff over `Matrix`), `Adam`, `Weights` (the file format).
  - `libs/ai/language/` (`ai_language`, on `ai_learning`), the question
    "what comes next?": `Bigram`, `Ngram_mlp`, `Tokenizer` (characters,
    then BPE), `Gpt`, `Sample`.
  - `libs/ai/selfplay/` (`ai_selfplay`, on `ai_search` and
    `ai_learning`), the question "can it teach itself?": `Policy_value`,
    `Selfplay`, `Arena`.
- **D2. Scalars first, arrays second, both kept.** `Gpt` is written on
  `Grad` first, as microgpt is, because that is the version one can
  read in a sitting; then again on `Tensor`, the same function names,
  the two checked equal by a test and timed against each other. The
  scalar one stays (flag `autodiff=scalar`), as `Backprop` stayed
  beside `Grad`.
- **D3. Weights are files, trained offline, committed, embedded.**
  `scripts/train/` holds OCaml programs (dune executables, in no opam
  package) that train from a seed and write a weights file in
  `data/weights/<name>/` (decided 2026-10-04: the source directories
  stay clean of data), a small library each, embedded by dune at build
  time; `data/weights/README.md` lists each with the trainer that made
  it and the command to make it again.
  Format (`Weights`): a text header naming each matrix and its shape,
  then 32-bit floats, little endian; read by pure OCaml, so the web
  has it too. The header also records the seed, the trainer's
  parameters and the measured result, so that a weights file says how
  it was made.
- **D4. Everything small enough also trains live.** A program whose
  training takes seconds or minutes trains in its window, a few steps
  a frame (as `AiDigits` and `Mcts.think` do); only what takes longer
  loads weights. Each loading program keeps the untrained or
  hand-written version as its default behind a flag (`ai=network`),
  as `ai=engine` works elsewhere, until the measured result says the
  network is better.
- **D5. Datasets: a ladder, the small rungs made here.** What can be
  learned is set by what can be trained, and the arithmetic is short
  (an estimate, to be replaced by Q4's measurement): training costs
  about six floating-point operations per parameter per token, and
  pure OCaml on one core does perhaps a billion a second. So a model
  of a million parameters reads about 150 tokens a second, some ten
  million a night. That is the scale: **a million parameters, ten
  megabytes of text, one night**. The rungs, in order of need:
  1. *names*: makemore's own `names.txt`, 32,033 first names, taken
     as it is (decided 2026-10-04): Karpathy's datasets wherever he
     has one, so that a loss here can be put beside his. It is
     `data/names/` (library `data_names`): datasets live in a
     top-level `data/` (`data/README.md`), `libs/` stays code. The
     next of his, for Q4 and Q5: tiny Shakespeare, nanoGPT's.
  2. *tunes*: public-domain folk tunes in ABC (the Nottingham Music
     Database or O'Neill's are the usual ones), a few hundred, to be
     chosen and their licence checked.
  3. *a toy dialogue*: generated by a script from `CATALOG.md` and a
     small grammar -- questions about the repository's games and apps
     ("what is TinyDoom after?", "name a racing game") with their
     answers, greetings, a few hundred words of vocabulary. A world
     small enough that a model of a few hundred thousand parameters can
     be right about it, which is TinyStories' lesson. The generator is
     the dataset: committed as code, its output not.
  4. *the playground itself*: its sources and its notes, about ten
     megabytes, which is exactly a night's worth by the estimate
     above, and already embedded in tinybox (`Tinybox_sources`). A
     model that writes plausible OCaml in this repository's style and
     plausible `.mli` prose; and the text the chat model's retrieval
     reads (Q10).
  5. *Wikipedia*: `enwik8`, the first hundred megabytes of the English
     one (the Hutter Prize's file, the standard benchmark for exactly
     this), or a slice of the Simple English one. The one rung that is
     downloaded, by a script, by the trainer only: never committed,
     never needed to build or to run.
- **D6. Connect 4's rules move to a kit**, so that the trainer and the
  game share them: `gamekits/boards/` (`kit_boards`), `Connect4` (the
  position, `Minimax.game`, the encoding of a board as a network's
  input), later `Tictactoe`. A move of code out of `AiConnect4.ml`,
  hence a decision.
- **D7. `TinyChatGPT` is an app, in `apps/education/`**, beside
  `TinyStellarium` and `TinyInteractivePhysics`: a program made for a
  learner of a subject. (Or a new category `apps/ai/` if more follow.)
- **D8. No change to `Mcts`' interface.** Root noise is a wrapper round
  `?prior` (noise mixed in when the state is the root), temperature a
  draw among `result.tried`'s visit counts, and the one forward pass
  for both heads a one-entry cache in the closure that makes `prior`
  and `evaluate`. All three live in `Selfplay`.

## The programs

Examples are in `examples/`, games in `games/puzzle/`, each with its
golden frame, its web page and, for games and apps, its `CATALOG.md`
row.

| program | what you watch or do | trains |
|---|---|---|
| `examples/AiGrad.ml` | an expression drawn as its graph; values going forward, slopes coming back, a node at a time; drag an input and see every slope move | nothing |
| `examples/AiNames.ml` | the 27 by 27 table of letter pairs as a heat map, names sampled from it; "g": the same table learned by gradient descent, converging to the counts; "m": Bengio's MLP, its two-dimensional letter embedding drawn, vowels drifting together | live |
| `games/puzzle/AiShannon.ml` | Shannon's guessing game: guess the next letter of a hidden sentence, against the bigram, the MLP, the GPT; the score is bits per letter, yours and theirs | live or loaded |
| `examples/AiGpt.ml` | microgpt training: the loss, names sampled as it learns, the attention matrix over the name being read, temperature on a slider | live |
| `examples/AiTunes.ml` | a GPT writing ABC tunes, played by our synthesizer as they are written | loaded |
| `apps/education/TinyChatGPT.ml` | a chat window; under it, on a key, the tokens, the next-token probabilities as bars, the attention over the conversation; `ai=eliza` answers with Weizenbaum's rules instead; from Q10, answers with the passages it looked up in the playground's notes or a Wikipedia slice | loaded |
| `examples/AiCompress.ml` | the same text through `Deflate`, the bigram, the MLP and the GPT with an arithmetic coder: four bars, bits per character, and the text coming back exact | loaded |
| `examples/AiSelfPlay.ml` | AlphaZero on tic-tac-toe, in the window: the policy on the empty board going from flat to the centre and corners, the win rate against a random player and against perfect minimax as two curves | live |
| `games/puzzle/AiConnect4.ml` | `ai=network`: the same game against the trained network; "v" shows, per column, the policy before the search, the visits after, and alpha-beta's opinion beside them | loaded |

## Phases

Each phase ends with its tests green, its notes written
(`notes_ai_learning.md` gets a section per phase) and its lines
counted. The estimates are of OCaml code, `.mli` comments not counted.

### Q0. Groundwork (about 250 lines)

- `Grad.backward` in linear time (a visited mark in the node instead
  of a list of nodes seen); the old `order` kept in a comment beside
  it with the two timings. `Unit_grad` unchanged and green.
- `Grad`: `pow`, and a softmax with cross-entropy written from the
  primitives, with its worked example.
- `Adam` (on arrays of parameters, usable from `Grad` and `Tensor`),
  tested on a bowl and on Rosenbrock's valley against plain descent.
- `Weights`: write, read, the round trip tested, a bad file refused.
- `scripts/train/` with its dune file and a README line.

**Done (2026-10-04)**, but for `scripts/train/`, which waits for its
first trainer (Q5, or Q8 if that comes first): an empty directory
teaches nothing. `Grad.backward` on 16,000 nodes went from 314 ms to
3.4 ms, and scalar autodiff against the hand-written pass from 20x to
4.5x, most of that price having been the walk's and not the idea's.
`Grad` also has `set`, to change a weight between two graphs. About
200 lines of code, 210 of tests.

### Q1. makemore: `Bigram`, `Ngram_mlp`, `AiNames`, `AiGrad` (about 600 lines)

- `Tokenizer.chars`: a text's alphabet, encode, decode.
- `Bigram`: the counts, the probabilities, sampling, the loss in bits
  per letter; then the same table as one layer trained with `Grad`.
  The test is the lesson: after training, the learned table is the
  counts' table to two decimals.
- `Ngram_mlp`: an embedding, a hidden layer, softmax, on three letters
  of context.
- The two examples. Dataset decision D5.1 needed here.

**Done (2026-10-04)**, `AiGrad` included (240 lines, three golden
frames: micrograd's neuron with Karpathy's numbers, his slopes).
`libs/ai/language/` (`Tokenizer`, `Sampling`, `Bigram`, `Ngram_mlp`:
340 lines), `data/names/`, `examples/AiNames.ml` (270 lines, three
golden frames), 245 lines of tests. On makemore's names:

- the counted table's loss is makemore's, 2.454, and its counts too
  (4,410 names start with an a);
- the learned table reaches the counted one to three decimals in 200
  steps of a millisecond, the graph being the table's size and not
  the text's;
- the MLP (makemore's sizes, 3,481 numbers) reaches 2.35 on held-out
  names after 20,000 batches of 32, in 221 s; makemore reports about
  2.3 after 200,000. Not run that long: at 11 ms a step it is 37
  minutes, which is `Tensor`'s argument (Q4).

Two things learned for Q2: `Grad.dot` (a neuron's sum as one node)
took a step from 175 ms to 45, and a minor heap with room for a
step's graph from 45 to 12. A GPT on scalars needs both.

### Q2. microgpt: `Gpt` on scalars, `AiGpt` (about 500 lines)

- `Gpt` in the order microgpt has it, one function a piece:
  embeddings, RMSNorm, attention (one head, then several), the MLP,
  the residuals, the loss, sampling with a temperature.
- Each piece switchable so its effect is a number: no position
  embedding, no attention (it falls back to a bigram, and the loss
  says so), one head against four.
- Measure first: nodes per training step and steps per second,
  natively and in a browser. That number decides how many steps a
  frame `AiGpt` does, and whether the web page trains live or loads.
- Tests: the gradient against `Backprop.numeric`'s method on a tiny
  model; a fixed seed's loss after a hundred steps; a model with
  attention beating the bigram on held-out names.

**Done (2026-10-04)**: `Gpt` (220 lines), microgpt's
sizes and 4,192 numbers, on `Grad` with `Grad.dot`; its gradient
agrees with a nudge to 6e-10. About 5 ms a step, so it trains live.
Held-out loss 2.36 after 1,000 names, 2.27 after 5,000 (27 s), past
`Ngram_mlp`'s 2.35 after 221 s. The switches measured
(`scripts/train/measure_gpt`): without attention 2.307, without
positions 2.285, one head 2.285, without either 2.475 (the bigram
again); they separate only with training. `examples/AiGpt.ml` (210
lines, two golden frames) trains it two names a frame and draws its
attention over a name. The GPT is `AiShannon`'s third opponent and its default:
`scripts/train/train_names_gpt`, 30,000 names in half a minute on
arrays (143 s on scalars), held-out loss 2.21 (`data/weights/names_gpt/`), against `Ngram_mlp`'s
2.328 after eleven.

### Q3. `AiShannon`, the game (about 350 lines)

The player and a model guess the same letters; the game keeps both
scores in bits. It needs only Q1 to exist (bigram and MLP as
opponents) and gains the GPT from Q2. Its sentences: a decision, a
toy text of our own.

**Done (2026-10-04), before Q2**, on names rather than sentences (the
models are trained on names; sentences are the game's first exercise,
and want a second dataset): `games/puzzle/AiShannon.ml`, 280 lines.
The score is guesses a letter, Shannon's own measure, the model's
guesses being its probabilities in order; its loss in bits beside.
With it, the first trainer and the first weights file (D3):
`scripts/train/train_names`, `data/weights/names_mlp/`,
`Ngram_mlp.to_weights` and `of_weights`, and `Corpus`, the split the
trainer, the game and the tests share so that a hidden name is one no
model saw. Not done: the test replaying a weights file's header
(Verification), which wants a place where a test can read a game's
file.

### Q4. `Tensor`: the same on arrays (about 450 lines)

- Reverse mode over `Matrix`: add, multiply, transpose, the squashes,
  softmax with cross-entropy, row selection (the embedding lookup),
  the causal mask. A node per matrix operation.
- `Gpt` again over it; a test that the two give the same loss and the
  same gradients on the same seed; the timing in `Tensor.mli`, as
  `Grad.mli` has its own.
- This is the phase everything bigger waits for.

**Done (2026-10-04)**: `Tensor` (250 lines, about twenty operations,
each checked against a nudge) and `Gpt` a second time on it (40
lines), the whole text at once; the same loss and the same 4,192
slopes as on `Grad` to ten decimals, `Gpt.on_arrays` choosing. Plain
matrices, no third dimension: the Little Learner's extended operators
are left as the exercise.

What it bought at first: 5x on microgpt's sizes, 12x at 800,000
numbers, 190 ms for one name's gradient -- a fifth of the speed D5
assumed. Hence:

- **Q4b. The product made direct. Done (2026-10-04).** A layer is
  `X W^T`, and through `mul` and `transpose` it was six passes over
  `W`. `Matrix.mul_t` and `Tensor.mul_t` (rows against rows, the
  slopes sent back a row at a time; `Tensor.direct` off is the long
  way): 0.41 ms on microgpt's sizes (10x over `Grad`), 44 ms at
  800,000 numbers (47x), about 800 million multiplications a second.
  D5's estimate stands again, near enough: a million parameters,
  eight megabytes, one night. In OCaml: no BLAS, no C stub, here or
  anywhere in `libs/` (the README's "OCaml all the way down").
  Left to try when a model needs it: a blocked product, 32-bit
  `Bigarray`s.

**The order from here (decided 2026-10-04): the self-play line
first** -- Q4b, then Q8, Q9, Q11, Q12 -- and the language line (Q5 to
Q7, Q10) after.

### Q5. A GPT that writes tunes: `train_tunes`, `AiTunes` (about 300 lines)

The first offline trainer and the first weights file. A character
model on ABC, a few layers. The honest measure: the share of sampled
tunes that our ABC reader accepts, and its bits per character against
the bigram's on tunes held out.

### Q6. `Tokenizer.bpe` (about 200 lines)

minbpe: the most frequent pair merged, again and again; encode and
decode; the worked example Wikipedia's (`aaabdaaabac`). Shown in
`TinyChatGPT`'s token view: the same sentence as characters and as
pieces.

### Q7. `TinyChatGPT` (about 700 lines with its trainer)

- The dialogue generator (D5.3) and its special tokens for the turns.
- `train_chat`, hours of a laptop at most; the weights' size decided
  by the measurement in Q4 (a model that answers a token in a frame or
  two in a browser).
- The app: the chat, the keys showing what is inside, ELIZA beside it.
- What it is not, written in its header: it knows its toy world and
  nothing else, and says nonsense politely outside it. Showing that is
  part of the lesson (a question outside the dataset is one of the
  golden frames).
- Exercises: quantising the weights to 8 bits; a key-value cache for
  generation; a larger context.

### Q8. AlphaZero on tic-tac-toe: `Policy_value`, `Selfplay`, `Arena`, `AiSelfPlay` (about 600 lines)

- `Policy_value`: a small network on `Tensor`, a shared body, a policy
  head (softmax over the moves, the illegal ones masked) and a value
  head (tanh); its loss the two added, as in the 2017 paper.
- `Selfplay`: a game played by MCTS against itself; each position
  kept with the visit counts as the policy's target and the final
  result as the value's; root noise and temperature (D8); the memory
  of the last games.
- `Arena`: two players, so many games, colours alternated, the score.
- The loop, AlphaZero.jl's four parts, about two hundred lines as
  `notes_ai_learning.md` section 9 promised.
- Tic-tac-toe is the check, because perfect play is known: the test
  trains for a fixed small budget and then never loses to `Minimax` at
  full depth over every opening, and the example shows it happening.

**Done (2026-10-04)**: `libs/ai/selfplay/` (`Policy_value` 130
lines, on `Tensor`, which gained biases and a cross-entropy against
shares; `Selfplay` 140; `Arena` 40; `Tictactoe` 50), 150 lines of
tests, `examples/AiSelfPlay.ml` (210 lines, two golden frames). With
the search it never loses to the perfect player after six iterations,
five seconds; alone, its policy goes from 14 wins in 40 against a
random player to 34, and to drawing the perfect one.

D8 did not hold as written: `Mcts`' interface is unchanged, but its
selection with a policy had to be made the real PUCT (a move never
tried competes by its prior instead of every move being tried once
first), or fifty playouts looked two moves ahead and nothing was
learned. `Unit_mcts`' numbers moved with it (7 wins of 20 for 6, a
pointed policy taking all the visits for 93%).

Not as planned: the loop itself is in the test and the example (thirty
lines each), not a function of the library; the trainer of Q9 is where
it becomes one, with its memory and its arena. And the example is
slow, fifteen frames a second: a game and ten steps a frame.

### Q9. AlphaZero on Connect 4: `kit_boards`, `train_connect4`, `AiConnect4 ai=network` (about 400 lines)

- D6's move, then the trainer, each iteration printing AlphaZero.jl's
  two benchmarks: against plain MCTS with the same number of
  playouts, and against `AiConnect4`'s own alpha-beta at its depth.
- The result reported as it comes out, in the game's header and the
  weights' header. If a laptop's night does not beat the alpha-beta
  player, that is the result, and `ai=network` stays a flag.
- Known truths to test against: the first player wins by the middle
  column (Allis, 1988), so the trained policy on the empty board
  should peak there.
- Exercises: a convolutional body; self-play games in parallel
  processes, one seed each; then `AiGo` at 9 by 9, which
  `plan_ai_remaining.md` already describes.

### Q10. Wikipedia, compressed: prediction is compression (size to be decided after Q7)

A "compressed Wikipedia" means two different things, and the phase is
both, kept apart because only one of them is honest about facts.

- **The model as a compressor, exactly.** A model that gives the next
  character's probabilities, plus an arithmetic coder, is a lossless
  compressor: the better the prediction, the fewer the bits (Shannon
  again; it is the Hutter Prize's whole premise). `Arithmetic` goes in
  `libs/compression/` (about 150 lines, with its worked example), and
  `Lm_compress` in `ai_language` joins it to any model of Q1 to Q4.
  The table to print, on the same text: our own `Deflate`, the bigram,
  the MLP, the GPT, in bits per character. Nothing is lost, so every
  fact comes back; and the loss curve of every earlier phase turns out
  to have been a file size all along.
- **The model as a memory, approximately.** A GPT trained on the text
  and asked questions. At the scale D5 computes it writes
  Wikipedia-shaped sentences and gets facts wrong: recalling facts
  reliably takes models thousands of times larger. To be shown, not
  hidden.
- **What makes it useful: looking things up.** The articles kept
  compressed (by the first point, or by `Deflate`), an index over them
  (`Bm25`, about 150 lines: Robertson and Sparck Jones), and
  `TinyChatGPT` answering with the passages it found, the model's own
  words marked as such. First over the playground's own notes and
  `.mli`s, where it would be of real use ("how does the Leslie
  work?"), then over a Wikipedia slice whose size is whatever a
  weights-and-text file of a few megabytes holds.
- Before any of it, a measurement: bits per character on `enwik8`'s
  first ten megabytes after one night, against `Deflate`'s. That
  number says how far to go.

**Mostly done (2026-10-04), the training still running**:
`gamekits/boards/Connect4` (D6: the rules, the evaluation and the
search's hints moved as they were, `AiConnect4.ml` including it),
`Selfplay.iterate` and `learn` (the loop as a function),
`scripts/train/train_connect4`, `data/weights/connect4/`, and
`AiConnect4` with `ai=classic|network|policy`, "a" changing engine in
play, the network's policy and the search's visits shown under the
board.

What it took, each in `notes_ai_dark_arts.md`: the network's opinion
by a plain pass and the policy asked once a node (a search of 100
playouts from 19 ms to 8); the games of an iteration in 48 processes
and each lesson mirrored (480 games an iteration where there were 40);
and a measure from two random opening moves, the first one having
counted two games as twenty.

At iteration 45, twelve minutes, over 20 games each: 15-0-5 against
the search without a network, 15-0-5 against alpha-beta at depth 3,
11-0-9 at depth 5, and 10-0-10 at depth 7, the game's own computer;
from 5-0-15, 3-0-17, 0-0-20 and 0-0-20 knowing nothing. The final
figures, and whether `ai=network` becomes the default, when the run
ends.

### Q11. AlphaZero on 9 by 9 Go: `Conv`, `train_go`, `AiGo ai=network` (size after Q9)

- A convolutional layer on `Tensor` (`Conv`: 3 by 3, the same weights
  at every point of the board, which is what a board is), a few of
  them with residuals, the two heads on top. The board as planes:
  black, white, whose turn, the ko point.
- Go's rules out of `AiGo.ml` into `kit_boards` (`Go9`), D6 again.
- `train_go`: Q9's loop unchanged but for the game and the network,
  resumable (the weights, the memory of games and the iteration
  number written at each iteration), self-play in parallel processes.
  A night at least; the weights' header says how long.
- `AiGo`: `ai=classic|network|policy`, and "v" drawing the policy
  over the board before the search and the visits after.
- The measure: games against `ai=classic` at 1000 playouts, and
  against the previous iteration.

### Q12. AlphaZero on chess: `train_chess`, `AiChess ai=network` (size after Q11)

- The moves as the network sees them (from-square and to-square
  planes; the 2018 paper's 73 planes simplified, and said so), the
  position as piece planes.
- The same loop. What is new is honesty about scale: the result
  printed is the score against `AiChess`' own alpha-beta at depths 1,
  2, 3, and the depth it first loses to is the grade.
- A cheaper start to try first, and to compare: the value head
  trained on positions scored by the alpha-beta engine (supervised,
  as the first AlphaGo was on human games), then self-play from
  there.

### Q13. DQN on our own arcade games: `Dqn`, `TinyBreakout ai=network` (size after Q9)

DeepMind's first famous result (Mnih et al., "Playing Atari with Deep
Reinforcement Learning", 2013; "Human-level control through deep
reinforcement learning", Nature, 2015): one network, given the screen
and the score and nothing else, learning Breakout, Pong and Space
Invaders -- and finding by itself that digging a tunnel up the side of
the wall sends the ball behind it. The fit here is unusually good:
the games exist, each is a pure `update : computer -> model -> model`
with the keys in `computer`, so an agent plays by holding keys, a
game can be stepped thousands of times a second with no window, and
replayed exactly from a seed.

What it is, in `Qlearn`'s terms (`notes_ai_learning.md` section 8):
the table of "how good is this move here" replaced by a network that
answers for positions it has never seen. Two additions keep it from
diverging, and are the paper:

- **experience replay**: every step lived is kept, and the network is
  taught on steps drawn at random from that memory, not on the last
  ones -- which are all alike and would undo what it knew;
- **a target network**: the "what the next position is worth" in the
  lesson comes from a copy of the network frozen for a while, or the
  network chases its own moving answer.

Two versions, the first cheap and soon, the second the paper's:

- **From the game's own numbers** (after Q9, needs only `Tensor`):
  the paddle's place, the ball's place and speed, perhaps the wall as
  a row of counts. A small network, minutes to train, and trainable in
  the window: `examples/AiBreakout.ml`, the paddle flailing, then
  following the ball, then aiming. `libs/ai/learning/Dqn` (the memory,
  the two networks, the step) beside `Qlearn`, with a worked example
  small enough to check (`Qlearn`'s cliff, the table and the network
  agreeing).
- **From the pixels** (after Q11's `Conv`): the software rasterizer's
  frame, greyed and shrunk to 84 by 84, the last four stacked so that
  speed can be seen, through the paper's three convolutions. A
  trainer of hours, a weights file, `TinyBreakout ai=network` and a
  key to hand the paddle over; the measure the paper's own, the score
  against a random player's and a person's. Whether the tunnel is
  found is the result to report, either way.

Open: how the agent reaches a game. A game is a program, not a
library; its `update` has to be reachable by the trainer. Either the
game's rules move to a kit (as D6 does for Connect 4), or the trainer
is a mode of the game itself. To decide when the first version is
written; `Pong` is the smaller place to start.

## What went wrong on the way

Kept as a document of its own, `tutorials/notes_ai_dark_arts.md`, at
the author's request (2026-10-04): each failure, how it showed, and
what it taught -- the dark arts of machine learning, the refinements
no paper has. To be added to as the later phases fail in their own ways;
a phase's status here says what happened, the notes say what was
learned.

## Order, and what depends on what

```
Q0 -- Q1 -- Q2 -- Q4 -- Q5 -- Q6 -- Q7 -- Q10    the language line
        \     \    \
         Q3 ---'    Q8 -- Q9 -- Q11 -- Q12       the self-play line
                           \     \
                            Q13a  Q13b           learning from the score
```

Q0 to Q3 need no offline training and no decision but D1, D2 and
D5.1: the place to start. Q4 is the hinge. After it the two lines are
independent; Q8 and Q9 can come before Q5 to Q7.

## Verification

- Every new module with its `.mli`, its worked example a test in
  `libs/ai/tests/`.
- Every trainer deterministic from its seed; a short run of each in
  the tests (a few steps, the loss falling), never the full one.
- A loaded weights file checked by a test that replays the measure in
  its header on a small sample.
- Golden frames for each program, with `-fixed-time` and a seed.
- `make test-lite` per phase; `tinybox list` (nothing slow at top
  level: weights decoded lazily).

## Out of scope

GPUs and BLAS; a dataset needed to build or to run (Wikipedia is the
trainer's alone, D5.5); pretrained weights from elsewhere; a model
that knows Wikipedia's facts (Q10 says why, and looks them up
instead); convolutions beyond an exercise; diffusion and
images; reinforcement learning past `Qlearn` and self-play (policy
gradients: `plan_ai_remaining.md` section 4; DQN is Q13).
