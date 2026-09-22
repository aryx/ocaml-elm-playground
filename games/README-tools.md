Tools for making games
======================

Most games here draw with shapes typed in their code and play notes
typed in their code too, and that is deliberate: the picture is in the
source. But a game with more levels, sprites or sounds than fit in its
file needs *tools* to make them, as the real ones did: Lode Runner
(Doug Smith, Broderbund, 1983) shipped its level editor on the disk,
Deluxe Paint (Dan Silva, Electronic Arts, 1985) drew the graphics of
many an Amiga game, STOS (1988) and AMOS (1990) came with a sprite editor, a map
editor and a music editor next to their BASIC. This page says where
such a tool goes in this repository, and how a game gets what it made.
`TinySokobanEd` (`games/puzzle/`) is the worked example.

Where a tool goes
-----------------

The question is who reads what the tool makes.

| Who reads it | Where the tool goes | Examples |
| --- | --- | --- |
| one genre's games only (a Sokoban level, a Doom map, a race track) | `games/<genre>/`, beside its game, as `Tiny<Game>Ed`; what the editor and the game share (rules, look, format) in the genre's kit, `gamekits/<genre>/` | `TinySokobanEd` (and the kit's `Sokoban`); a `TinyDoomEd` over `Sectors`, a track editor over `Track3d` |
| any game, through a playground layer (`Sprite`, `Tilemap`, `audio/`'s sound effects) | `apps/gamedev/`, as `Tiny<Original>` | a sprite editor, a tile-map editor (Tiled), a sound-effect maker (sfxr), PICO-8's editors |
| any program at all (a picture, a tune) | the medium's category, `apps/graphics/` or `apps/music/` | a paint program (Deluxe Paint), a tracker (ProTracker) |

A level editor goes with its genre and not in `apps/`, for three
reasons: what it makes means nothing to another genre, it must play
the level to test it (so it needs the game's rules, which is what a kit
is for: the rules of two programs), and the games that shipped with
their editor put it in the same place (Lode Runner, Pinball
Construction Set, the construction kits of `plan_games.md`'s "kits").
Some games are their own editor (`TinyBabaIsYou`'s rules are words on
the board, `TinyIncredibleMachine` is built before it runs): those need
nothing more.

How a game gets what a tool made
--------------------------------

1. **The format belongs to whoever reads it**: the kit for a genre's
   levels (`Sokoban.of_xsb`), the playground layer for the rest (the
   strings `Sprite.pixels` and `Tilemap.of_strings` take). The tool
   writes that format and nothing of its own, so that its work can
   also be written by hand, and read in a diff. Where the game's world
   already has a text format, use it (Sokoban's `.xsb`); otherwise
   strings, one character per pixel or cell, as `Sprite` and `Tilemap`
   do.
2. **The work is a file in the game's directory**, next to the game
   (`games/puzzle/TinySokoban.xsb`), committed with it.
3. **dune embeds the file in the game at build time**, as a module
   holding it as a string (`Sokoban_levels.xsb`), so that the game
   still needs no file at run time, natively or in the browser, and
   its `web/` and `software/` copies get it with `copy_files ../*.ml`.
   Text is embedded with `cat` (`games/puzzle/dune`, and
   `graphics/font/dune` for the Hershey font); anything else (a PNG, a
   WAV) as base64 with `scripts/build/file_to_base64_ml.ml`
   (`games/fps/dune`'s `minecraft.png`, `games/adventure/dune`'s
   `tomb.png`).
4. **The tool starts from the game's own file** (the same embedded
   module) and **exports it back** with `Playground_platform.export`:
   natively a file in the current directory, in the browser a
   download. Copied over the game's file, it is the game's at the next
   build. The export needs `Cap.open_out`, which the tool's `main` gets
   from `Cap.main`: a game's type says it writes no file, a tool's
   that it does.
5. **A test checks the round trip**: the file the tool would export,
   nothing changed, is the one the game was built with, byte for byte
   (`tests/games/Unit_games.ml`). Otherwise the first export of an
   untouched level makes a diff that no one asked for.

Checklist for a new tool
------------------------

- its header says what it edits, the keys, and where its file goes, as
  a game's header does (what it uses, and not; its exercises);
- its row in `CATALOG.md`, with its genre for a `Tiny<Game>Ed`, and its
  golden frames (`tests/2d/Golden_frames.ml`: at rest, and a few
  scripted ones, since a script can type characters and click);
- keyboard first: the golden frames' scripts hold keys and type
  characters, and so can a player without a mouse;
- the checks a text editor cannot do (`TinySokobanEd`: one player, as
  many boxes as goals, the solver's answer), and a way to test the work
  without leaving the tool.
