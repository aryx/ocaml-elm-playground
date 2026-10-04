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
| `names_gpt/` (`data_weights_names_gpt`, `Weights_names_gpt.bytes`) | [`Gpt`](../../libs/ai/language/Gpt.mli), microgpt's sizes: 16 wide, 4 heads, 1 layer, 4,192 numbers | [`scripts/train/train_names_gpt.ml`](../../scripts/train/train_names_gpt.ml) | 30,000 names, one a step, half a minute | loss 2.21 on the names held out | [`AiShannon`](../../games/puzzle/AiShannon.ml) |
| `names_mlp/` (`data_weights_names_mlp`, `Weights_names_mlp.bytes`) | [`Ngram_mlp`](../../libs/ai/language/Ngram_mlp.mli), makemore's MLP: 3 letters back, 3,481 numbers | [`scripts/train/train_names.ml`](../../scripts/train/train_names.ml) | 60,000 batches of 32, eleven minutes | loss 2.328 on the names held out (the table of letter pairs: 2.454) | [`AiShannon`](../../games/puzzle/AiShannon.ml) |

## Making one again

Each trainer's header says its command; from the repository's root:

```bash
dune exec scripts/train/train_names.exe -- data/weights/names_mlp/names_mlp.weights
dune exec scripts/train/train_names_gpt.exe -- data/weights/names_gpt/names_gpt.weights
```

The same seeds give the same file, byte for byte, on every OCaml (the
random numbers are `Lehmer`'s, not the standard library's). The file's
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
