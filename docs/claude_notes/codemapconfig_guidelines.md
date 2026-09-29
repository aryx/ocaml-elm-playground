# Writing a `.codemapconfig`

For an LLM (or a person) writing the configs the code map reads
(`plan_codemap_v2.md`; the format: `launcher/codemap/Code_guide.mli`).
The map draws what the config says: a directory's name and card, a
file's card, the capitals seen from afar, the lines drawn larger up
close, the tours. So a config is judgement, written once: what a
newcomer should see first, and in what words.

The worked example to imitate: `games/shmup/.codemapconfig`
(TinyInvaders) and `gamekits/shmup/.codemapconfig` (its kit).

## Before writing

Read, in this order: the directory's README if any (and `CATALOG.md`'s
section for a genre), each file's header comment, then the code itself
-- the model, the update, the view for a game; the `.mli` for a
library. The header comments here are good: the config's job is not to
copy them but to pick from them what a card of one or two lines needs,
and to point at the places they talk about.

## One config per directory

- A directory's own config: its `summary`, its `files`, its `tours`.
- Its parent's `dirs:` for a small directory not worth a config of its
  own (one line each).
- The root's `title`: the project in one sentence, for the map's title.

## Summaries

- Say what it is **for**, not what it contains: "The shoot 'em up kit:
  shots that move, curves that enemies fly" -- not "Shots.ml and
  Path.ml".
- One sentence, two at most; about 60 to 100 characters. It is read on
  a card, beside the mouse.
- For a game: the original (name, maker, year) and what makes it that
  game, in the fewest words.
- No "This file...", no "This directory contains...".

## Capitals

What to know first, seen from the whole map: the entry points, the
place where the idea is. Three at most per file, and most files have
none. A capital must say something the file's name does not: for a
program whose entry hides among many files (`~/ix`'s), its `main`; for
a game on the Playground, whose file is its entry, the one function
that is the game's heart (TinyInvaders' `march`), not `update` and
`view` -- every game has them, so as capitals they would crowd the map
and say nothing; they are `important`, and `links`.

## Important

The lines a reader must see to understand the file, drawn larger near
the ground: the model's type, the function holding the trick, the
comment explaining the non-obvious. Not the most used (the map knows
that already), the most telling. `weight` 3 for the two or three that
matter most, 1 for the rest. Five to ten per file.

## Anchors

Prefer `def:` and `type:`; they survive edits. `comment:"..."` needs
words that are on one line of a comment (the checker says if not);
pick a distinctive phrase. `section:` takes a section's title as the
lexer sees it. Never `line:` unless nothing else points there.

## Links, related, tours, views

- `links`: the calls that explain the file's flow (`update` to
  `update_rules`); not every call.
- `related`: files no analysis would link -- the game's page, its golden
  frame, its level file, its editor, its tests.
- `tours`: 5 to 10 stops, in the order to read, each `say` one
  sentence on what to notice there. A stop names its file.
- `views`: a game and its kits; a library file and its users.

## Digests

Each described file gets its `digest`, which `tinybox codemap -check`
prints (the first 12 hex digits of its MD5). When the checker says a
file changed since it was described, reread it and update what it
says, then the digest.

## After writing

`tinybox codemap -check <dir>` must say no mistake. Then look at the
map (`tinybox codemap <dir>`): the cards should read well, the capitals
should not crowd.

Mark what an LLM wrote: `generated: { by: '<model>', on: '<date>' }`.
A person's additions go in the same file, or in an object added to it
(`(import 'x.libsonnet') + { ... }`).
