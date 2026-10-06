# data/weights/: what the networks learned

A network that takes longer to train than a program's window should is
trained once, by a program of `scripts/train/`, and what it learned is
kept here: a weights file (`libs/ai/learning/Weights.mli`: a few lines
of text saying how it was made, then the numbers as 32-bit floats), in
a folder of its own with the few lines of dune that embed it as a
string in a small library. A program that plays with the network names
that library and never trains.

| folder (library, module) | the network | made by | how long | result | used by |
|---|---|---|---|---|---|
| `breakout/` (`data_weights_breakout`, `Weights_breakout.bytes`) | [`Dqn`](../../libs/ai/learning/Dqn.mli), the Atari paper's at a smaller screen: the last 4 screens of 64 by 64 greys, windows of 8 every 4 (16 channels), of 4 every 2 (16), 128 neurons, a value for each of left, nothing, right; 82,467 numbers | [`scripts/train/train_breakout.ml`](../../scripts/train/train_breakout.ml) `pixels`, from the screen and the score, a point taken for a ball lost | 415 iterations of 12,000 steps lived and 800 of learning, in three runs each going on from the best of the one before, 48 processes, about ten and a half hours | a game of three balls, over 40 games: 69.1 points on the dry game it learned on (from 34 to 115), 65.0 on the game with its effects, which it never saw. A random player: 4.7; a wall is 448 | [`TinyBreakout`](../../games/arcade/TinyBreakout.ml), the `a` key (native only) |
| `go9/` (`data_weights_go9`, `Weights_go9.bytes`) | [`Policy_value`](../../libs/ai/selfplay/Policy_value.mli), a board: 3 planes of 9 by 9, two convolutions of 16 channels, a policy over the 81 points and the pass, and a value; 21,498 numbers | [`scripts/train/train_go.ml`](../../scripts/train/train_go.ml), self-play ([`Alphazero`](../../libs/ai/selfplay/Alphazero.mli)) | 96 iterations of 192 games against itself, 18,400 games, 48 processes, 4 hours 15 minutes (a run of 100, stopped at the 97th by a process that died) | against AiGo's own computer, the search with 1,000 random playouts a move, over 40 games from random openings (won-drawn-lost): 24-0-16 with no search at all, 33-0-7 with 100 playouts, 29-0-11 with 400, 35-0-5 with 1,600. Knowing nothing: 0-0-20 | [`AiGo`](../../games/puzzle/AiGo.ml), `ai=network` |
| `connect4/` (`data_weights_connect4`, `Weights_connect4.bytes`) | [`Policy_value`](../../libs/ai/selfplay/Policy_value.mli): 84 numbers in, two layers of 128, a policy over the 7 columns and a value; 28,424 numbers | [`scripts/train/train_connect4.ml`](../../scripts/train/train_connect4.ml), self-play ([`Alphazero`](../../libs/ai/selfplay/Alphazero.mli)) | 150 iterations of 480 games against itself, 72,000 games, 48 processes, 38 minutes | against alpha-beta at depth 7, the game's own computer, over 40 games from random openings (won-drawn-lost): 18-0-22 with 100 playouts a move, 20-2-18 with 400, 26-1-13 with 1,600; 26-3-11 against depth 5 with 400. Knowing nothing: 0-0-20 | [`AiConnect4`](../../games/puzzle/AiConnect4.ml), `ai=network` |
| `names_gpt/` (`data_weights_names_gpt`, `Weights_names_gpt.bytes`) | [`Gpt`](../../libs/ai/language/Gpt.mli), microgpt's sizes: 16 wide, 4 heads, 1 layer, 4,192 numbers | [`scripts/train/train_names_gpt.ml`](../../scripts/train/train_names_gpt.ml) | 30,000 names, one a step, half a minute | loss 2.21 on the names held out | [`AiShannon`](../../games/puzzle/AiShannon.ml) |
| `names_mlp/` (`data_weights_names_mlp`, `Weights_names_mlp.bytes`) | [`Ngram_mlp`](../../libs/ai/language/Ngram_mlp.mli), makemore's MLP: 3 letters back, 3,481 numbers | [`scripts/train/train_names.ml`](../../scripts/train/train_names.ml) | 60,000 batches of 32, eleven minutes | loss 2.328 on the names held out (the table of letter pairs: 2.454) | [`AiShannon`](../../games/puzzle/AiShannon.ml) |

## Making one again

Each trainer's header says its command; from the repository's root:

```bash
dune exec scripts/train/train_names.exe -- data/weights/names_mlp/names_mlp.weights
dune exec scripts/train/train_names_gpt.exe -- data/weights/names_gpt/names_gpt.weights
dune exec scripts/train/train_connect4.exe -- data/weights/connect4/connect4.weights 150
dune exec scripts/train/train_go.exe -- data/weights/go9/go9.weights 100
dune exec scripts/train/train_breakout.exe -- pixels 400 data/weights/breakout/breakout.weights
```

The same seeds give the same file, byte for byte, on every OCaml (the
random numbers are `Lehmer`'s, not the standard library's) -- with the
same code: a change in the order of the arithmetic under a model moves
its last digits, and its weights are then to be made again
(`notes_ai_dark_arts.md`). `train_connect4` is the same from its seed
too, if run in one go with the same number of processes (`WORKERS`). The file's
own header is its record -- the model, the data, the seed, the steps,
the loss reached:

```bash
head -8 data/weights/names_mlp/names_mlp.weights
```

and it is rewritten by the trainer, so this table is what to update by
hand after a new training, with the folder's `dune` comment and its
`.mli`.

## A new one

A folder `<name>/` with `<name>.weights`, a `dune` file like
`names_mlp/dune` (the library `data_weights_<name>`, the module
`Weights_<name>` with `bytes`), its `.mli` saying what it is, a row
above, and a trainer in `scripts/train/` whose header gives the
command.
