# data/: files that are not code

What a library needs that nobody here wrote: the datasets that
`libs/ai/`'s models learn from
(`docs/claude_notes/plans/plan_ai_zero_to_hero.md`, decision D5), a
standard's table, what a network learned (`weights/`). `libs/`, `games/`
and `apps/` stay code.

Each is a folder: the file as it was taken, a `dune` file whose
header says where it comes from and under what licence, and the few
lines that embed it as a string in a small library of its own, so that
a program carries it only if it names that library.

| folder (library) | what | from | used by |
|---|---|---|---|
| `brotli_words/` (`compression_brotli_words`: `Brotli_words.bytes`) | Brotli's static dictionary, RFC 7932's Appendix A, 122,784 bytes | github.com/google/brotli (MIT) | `Brotli.decompress ~dictionary`, for who asks |
| `weights/go9/` (`data_weights_go9`: `Weights_go9.bytes`) | what a network learned of Go on 9 by 9 by playing itself: 21,498 numbers, 87 KB | `scripts/train/train_go`, self-play in 48 processes | `AiGo` (`ai=network`) |
| `weights/connect4/` (`data_weights_connect4`: `Weights_connect4.bytes`) | what a network learned of Connect 4 by playing itself: 28,424 numbers, 114 KB | `scripts/train/train_connect4`, self-play in 48 processes | `AiConnect4` (`ai=network`) |
| `weights/names_gpt/` (`data_weights_names_gpt`: `Weights_names_gpt.bytes`) | what `Gpt` learned of the names: 4,192 numbers, 17 KB, loss 2.21 held out | `scripts/train/train_names_gpt`, half a minute | `AiShannon` |
| `weights/names_mlp/` (`data_weights_names_mlp`: `Weights_names_mlp.bytes`) | what `Ngram_mlp` learned of the names: 3,481 numbers, 14 KB, loss 2.328 held out | `scripts/train/train_names`, eleven minutes | `AiShannon` |
| `names/` (`data_names`: `Makemore_names.text`) | 32,033 first names, one a line, 228 KB | Karpathy's makemore (MIT); the US Social Security Administration's names, public domain | `Bigram`, `Ngram_mlp`, their tests and examples |

Rules:

- **Small, and taken as it is**: a file a known result was measured
  on, unchanged, so that a loss here can be put beside the published
  one.
- **Nothing here is needed to build anything but what names it.**
- **What is large is not here**: a trainer downloads it
  (`scripts/train/`), and only the weights it made are kept, in
  `weights/` (its `README.md` says which trainer made each, and how to
  make it again).
