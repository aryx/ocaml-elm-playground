# data/: files that are not code

What a library needs that nobody here wrote: the datasets that
`libs/ai/`'s models learn from
(`docs/claude_notes/plans/plan_ai_zero_to_hero.md`, decision D5), a
standard's table. `libs/` stays code.

Each is a folder: the file as it was taken, a `dune` file whose
header says where it comes from and under what licence, and the few
lines that embed it as a string in a small library of its own, so that
a program carries it only if it names that library.

| folder (library) | what | from | used by |
|---|---|---|---|
| `brotli_words/` (`compression_brotli_words`: `Brotli_words.bytes`) | Brotli's static dictionary, RFC 7932's Appendix A, 122,784 bytes | github.com/google/brotli (MIT) | `Brotli.decompress ~dictionary`, for who asks |
| `names/` (`data_names`: `Makemore_names.text`) | 32,033 first names, one a line, 228 KB | Karpathy's makemore (MIT); the US Social Security Administration's names, public domain | `Bigram`, `Ngram_mlp`, their tests and examples |

Rules:

- **Small, and taken as it is**: a file a known result was measured
  on, unchanged, so that a loss here can be put beside the published
  one.
- **Nothing here is needed to build anything but what names it.**
- **What is large is not here**: a trainer downloads it
  (`scripts/train/`), and only the weights it made are kept, beside
  the program that uses them.
