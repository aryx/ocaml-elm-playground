# The code map: a manual

The code map is tinybox's code visualizer: a codebase drawn as a map, to
be read the way one reads a map of a country, from the whole down to a
street. It runs on any directory (`tinybox codemap <dir>`), on a program
from tinybox's menu (`s`, or a click on the code under a game's
preview), and on this repository itself.

This manual describes the map's current style, v2 (`m` switches to the
older ones: classic, atlas, streets). It says what the map shows, how to
move and search in it, how to see the dependencies between its parts,
and how to describe a codebase for it: the `.codemapconfig` files.

## Contents

1. Principles
2. Starting it
3. The levels: earth, region, ground, street
4. Moving
5. Reading: cards, peeks, capitals
6. Searching
7. The X-ray: skeletons and plates
8. Layers
9. Dependencies: ties, the tied view, the matrix
10. Tours and views
11. Every key
12. Describing a codebase: `.codemapconfig`
13. Writing the configs with an LLM: codellm
14. Colours and conventions
15. Limits

---

## 1. Principles

**A codebase is a territory.** Its folders are regions, its files are
towns, its definitions are streets. Area is size: a folder takes the
room its lines take (a treemap). One reads it the way one reads a map:
first the continents, then a country, then a town, then a street, each
level showing what matters at that distance and hiding the rest.

**What matters is judged, not only measured.** Program analysis says
what is there: sizes, definitions, who uses whom. It cannot say what a
reader should look at first, or what a folder is *for*. That judgement
is written down, once per directory, in a `.codemapconfig` file, by an
LLM reading the code (helped by the map's own analysis, codellm) and
corrected by the humans who know it. The map draws what those files
say: a directory's description, its *capitals* (the definitions to see
first), its *skeleton* (the parts the rest hangs on), its layers, its
tours.

**Centrality, not size.** A game is large and nobody uses it; the
Playground is small and every program is written with it. Games and apps
are like a kernel's device drivers: numerous, but not the core. The map
ranks what it shows by how many files depend on it (fan-in), and draws
the core's names largest.

**Move by units, see transiently.** The camera moves a whole folder or
file at a time, animated, never to an arbitrary zoom where nothing is
framed. What one wants to read without leaving (a definition's body, a
match's context, a dependency explained) comes as a card or a peek over
the map, gone when the mouse moves on.

**One key per feature, cycling its modes.** `a` cycles the street's
modes, `x` the skeletons, `l` the layers, `d` the tied view's modes.

**The same colours mean the same thing everywhere.** Green is the user,
red the used: a road from a use to a definition goes from green to red;
a capital many files use is red; in the matrix, a row's dependencies
turn red and its users green.

## 2. Starting it

```
tinybox codemap <dir>            the map of a directory (tinybox codemap ~/ix)
tinybox codemap . focus=<path>   opened on a folder or file
tinybox                          the menu: s (or a click on the code under
                                 a program's preview) opens the program's map
tinybox codemap -check <dir>     the configs checked (section 12)
tinybox codemap -facts <root> <dir>  a directory's brief, for writing its config
```

A program's map (from the menu) shows the program's own code, its file
and the kits it uses; `w` widens it to all it uses, then to the whole
repository, then back.

The map reads every source file under the directory (OCaml's `.ml`,
`.mli`, `.mll`, `.mly`, and C's `.c`, `.h`), skipping what a
`.codemapignore` excludes (gitignore's syntax), and every
`.codemapconfig` and `*.libsonnet` it finds.

## 3. The levels

The map changes what it draws with the distance, like a map app.

**The earth** is the whole directory. Folders are coloured regions,
their names written as large as their block allows (the top ones
largest, their subfolders smaller). Files show as columns of tiny lines,
the code's shape. *Capitals* are dots with a name: the definitions the
configs call central, as large as the files depending on them are many
(a red capital: a definition many files use). Hovering a folder's or
file's name shows its card (its description) and its ties (section 9).

**A region** is a folder flown into. Its files show their cards and
their section titles (clickable: a peek at the section). A folder that
would leave much of the screen empty is laid out again alone, filling
the screen, with an animation; going up from its top returns.

**The ground** is a file. Its lines are drawn as code, each as tall as
it matters: definitions and capitals large, comments and blank lines
thin, the rest in between. The notes the config gives beside important
lines are drawn next to them.

**The street** (`a` at a file) is the file with its neighbours: on the
left the files it uses, on the right the files using it, each laid out
as a small ground with the lines that tie them to the file enlarged.
Roads go from each use to its definition. `a` again cycles: uses,
users, both, off (the first press picks what fits the file).

The title at the top says where one is: at the earth, the project's
sentence; in a folder or a file, its description.

## 4. Moving

| | |
|---|---|
| click, wheel forward, `+` | in: the unit under the mouse, a folder or a file at a time |
| right click, wheel back, `-`, Backspace | out: the unit around |
| arrows | beside: the next unit left, right, up, down |
| `0`, Home | the whole map |
| `b` | back, after a jump to a definition |

The breadcrumb at the top left (`ix > builder > Mkfile.ml`) says where
one is; its parts are clickable.

## 5. Reading

**Cards.** Hovering a folder's or file's name shows its card: its path
and what its config says of it ("not described yet" when nothing does).
A capital's card adds how central it is ("its module named by 408 files:
the core") and how often it is used.

**Peeks.** A click on a name in the code opens a *peek*: the definition
of that name, readable, over the map, with the comment just above it. A
click on a top-level comment (a file's header) peeks at the whole
comment. The wheel scrolls a long peek; a click on a name inside a peek
opens a peek of *that* definition on top (a few deep); a click outside,
or Escape, closes the top one. Hovering a name defined elsewhere shows
the first lines of its definition beside the mouse.

**Glow.** Hovering a name in the code lights its definition and all its
uses, on the ground and in the street's panels; a use in a line too thin
to read is magnified while the mouse is there.

**Enter** opens the file view: the file read whole, as in an editor.

## 6. Searching

`/` opens the search box. What is typed searches as one types, every
match lit where it is on the map at the level one is at: a folder or
file framed, a definition a dot (a bar on its line at the ground).

| query | finds |
|---|---|
| `step` | directories, files, definitions, views, tours whose name contains it (the name itself first, then its start, a word's start, inside) |
| `shmup/step` | the same, under a path |
| `arm/` | directories only |
| `arm//` | every directory named exactly `arm`: Enter shows them all together |
| `file:` `dir:` `def:` `type:` | only files, directories, functions and values, types |
| `view:` `tour:` | the configs' views and tours (alone: all of them, to explore) |
| `bone:` | the skeletons' bones, by role or name (`bone:the state`) |
| `text:Cap.fork` (or `"Cap.fork`) | the lines whose text contains it (case ignored unless it has a capital) |
| `ref:Cap.fork` (or `@Cap.fork`) | the lines whose *code* refers to it: not a comment's or a string's words |

In the box: up and down choose a match (it pulses on the map, a thread
from its row to it), Tab completes, Enter goes there (a definition's
file, its definition peeked), **shift+Enter** shows all the matches'
files together, **ctrl+Enter** keeps the query as a layer (section 8).
A `/` typed first restricts the search to the files shown. Escape
closes the box.

Hovering a match (a dot, a bar) shows the matched line and the code
around it; a click goes there.

## 7. The X-ray

`x` turns the X-ray on: the map in the shade, and the unit's
**skeleton** drawn over it: its bones (definitions, files or folders,
each with its role) and the joints between them, roads from a bone to
the next. `x` again shows the next skeleton of the unit; past the last,
the X-ray turns off.

The skeleton is always the unit's own: at a file, the file's; at a
folder, the folder's. It comes from the configs; where none is written,
the map derives one from the code (a file's capitals and most used
definitions and who calls whom; a folder's most tied parts), marked
"(derived)". Hovering a bone shows its card (a definition's first lines,
a unit's description); a click peeks at it.

The legend at the top right lists the **plates**, other systems of the
body read into the code; hovering a row explains it, a click (or its
key) turns it on or off:

| key | plate | marks |
|---|---|---|
| 1 | skeleton | the architecture: the parts and how they connect |
| 2 | blood | what flows along the joints, pulses from user to used |
| 3 | muscles | where the work is: definitions dense with loops |
| 4 | nerves | the inputs: keyboard, mouse, events |
| 5 | lungs | the I/O: files, network, console, processes |
| 6 | skin | what a module exports: the `.mli`'s definitions barred, the private ones shaded |

## 8. Layers

A layer lights, everywhere at once and at any level, the lines matching
its rules, each rule in its colour, with a legend at the bottom left.
The configs define layers (this repository's root: Capabilities, each
`Cap.*` in its colour); a search can be kept as a layer (ctrl+Enter in
the search box). `l` cycles: the layers kept, each config's, none.

Matches glow slowly, to tell them from capitals. Hovering one shows its
line and the code around it; a click goes there.

## 9. Dependencies

**Ties on hover.** Hovering a folder's or file's name draws its ties:
roads from its users (green end) to it and from it to what it uses (red
end), as wide as the uses (the square root of their share), labelled
with their count. Each other end is shown at the level where it parts
from the hovered unit: from the earth, a region; next door, the
neighbouring folder or file.

**The tied view** (shift+click on a name): the unit and the units it is
tied to, laid out together (~/ix's `version_control` with `lib_core`,
`lib_security`, `lib_compression`). There, the hover's roads go file to
file. `d` cycles: its users and what it uses, its users only, what it
uses only, it alone.

**The matrix** (`g`, or ctrl+click on a name) is codegraph's dependency
structure matrix (DSM). Its rows are units, and the columns the same
units: the cell at row *i*, column *j* is how many uses row *i* makes of
column *j*.

- `g` over a name: that unit and the units tied to it.
- `g` over nothing: where one is, its parts against each other. At the
  earth, the project's top folders; in `games/`, its genres; at a file,
  its definitions; at the street, the file open with its neighbours.

The rows are **layered** (codegraph's partition): what uses nothing
among them first, what nothing uses last. A clean architecture is then a
lower-triangular matrix: every use below the diagonal, blue. A use above
the diagonal goes against the layers, a cycle, magenta.

```
            1  2  3  4
  1 libs    .
  2 lang    342 .
  3 play    1377 1 .
  4 games   9348 9 22060 .       games uses playground 22,060 times
```

The column names are written slanted above the matrix. Rows and names
take the map's colours: a definition its kind's (a function yellow, a
type green), a folder or file its region's.

| in the matrix | |
|---|---|
| hover a row or a column's name | its card (a definition's code, a unit's description, its counts); what it uses red, its users green |
| hover a cell | the uses behind it, at the definitions' grain (`TinyWumpus.app -> Tty_wumpus.program 1`) |
| click a row | expand it into its parts (a folder into folders and files, a file into its definitions), or collapse it |
| click a cell | zoom into it: a matrix of only the row's parts and the column's parts that the cell's uses touch |
| click a definition, shift+click any row | back to the map, there |
| Backspace | undo the last expansion or zoom |
| Escape | back to the map |

Three clicks take "games uses appkits: 1" down to the one definition
that does it.

## 10. Tours and views

A **tour** (from a config, found by the search: `tour:`) walks through a
program's code stop by stop: each stop flown to, its definition peeked
at, its words in a banner. `n` goes to the next stop, `p` to the one
before, Escape ends it.

A **view** (from a config, `view:` in the search) is a set of files and
folders shown together: a program with its kit, an instrument's app with
its voice and panel, a mini program with its tiny twin.

## 11. Every key

`h` shows them all on the map. The mouse:

| | |
|---|---|
| click | in; on a name in the code, a peek; on a match, there; on a bone, a peek; on a street panel's name, that file |
| shift+click on a name | the tied view |
| ctrl+click on a name | the matrix |
| right click | out |
| wheel | in and out; in a peek, scroll |
| hover | cards, ties, glow, previews |

The keys:

| key | |
|---|---|
| `h` | every key |
| `/` | search |
| `a` | the street (cycles) |
| `x` | the X-ray (cycles skeletons); `1`-`6` its plates |
| `l` | the layers (cycles) |
| `g` | the matrix |
| `d` | in the tied view: its modes |
| `n`, `p` | a tour's next and previous stop |
| `w` | a program's map: its code, with what it uses, the whole repository |
| `b` | back after a jump |
| Enter | the file view |
| `m` | another style of map |
| `0`, Home | the whole map |
| arrows | beside |
| Backspace, `-` | out |
| Escape | close what is open (a peek, the search, the help), else back |

## 12. Describing a codebase: `.codemapconfig`

A `.codemapconfig` sits in a directory and speaks of it: the directory,
its files, its immediate subdirectories without a config of their own.
It is written in **jsonnet** (JSON with comments, variables, functions,
imports), so that shared shapes are written once and colours can be
named.

```jsonnet
// games/shmup/: what the code map says of it
local skeletons = import '../../skeletons.libsonnet';
local enemy_color = '#e05050';
{
  generated: { by: 'claude-opus-5-5', on: '2026-09-29' },
  summary: "Shoot 'em ups: a ship, and waves of enemies to shoot.",
  files: {
    'TinyInvaders.ml': {
      summary: 'Space Invaders (Taito, 1978): 55 aliens marching one per frame.',
      digest: '4b2f0c1a9e33',
      capitals: [{ at: 'def:march', say: 'one alien per frame' }],
      important: [{ at: 'def:drop_bomb', say: 'a table of columns', weight: 2 }],
      related: ['web/TinyInvaders.html'],
    },
  },
  dirs: { tests: { summary: 'Golden frames of the games.' } },
  skeletons: [skeletons.game('TinyInvaders.ml', 'def:march', 'the march')],
  tours: [{ name: 'How Space Invaders runs', stops: [{ at: 'TinyInvaders.ml:def:update', say: 'A frame.' }] }],
  views: [{ name: 'Space Invaders and its kit', files: ['TinyInvaders.ml', '../../gamekits/shmup/'] }],
  layers: [{ name: 'Enemies', rules: [{ ref: 'Alien.spawn', color: enemy_color, say: 'an enemy made' }] }],
}
```

The fields (a misspelt one is a mistake, not ignored):

| field | what |
|---|---|
| `title` | the project's sentence (the root's config only) |
| `summary` | what the directory is *for*, one sentence or two: its card |
| `generated` | who wrote it and when |
| `colors` | a region's colour: `{ 'games/': '#3050c0' }` |
| `dirs` | a subdirectory without its own config: `{ name: { summary } }` |
| `files` | a file's `summary`, `digest`, `capitals`, `important`, `links`, `related` |
| `skeletons` | `[{ name, bones: [{ at, role }], joints: [{ from, to, say }] }]` |
| `tours` | `[{ name, stops: [{ at, say }] }]` |
| `views` | `[{ name, files: [path] }]`, or `{ name, of: path, with: 'users' }` |
| `layers` | `[{ name, rules: [{ text or ref, color, say }] }]` |

**Anchors** point at a place by what it is, not its line, so that
editing the code does not break them:

| anchor | |
|---|---|
| `def:march` | a function's or value's definition |
| `type:model` | a type's |
| `module:Make` | a module's |
| `section:Model` | a section's title, `(* Model *)` between rules of stars |
| `comment:"one alien per frame"` | the first comment containing those words |
| `line:42` | a line (discouraged: it moves) |

In a skeleton, a tour or a link, a path comes first:
`'../../gamekits/shmup/Shots.ml:def:advance'`; a bone that is a whole
file or folder is its path alone (`'gamekits/'`).

**The `digest`** is the first 12 hex digits of the file's MD5, as the
brief gives it; when the file changes, `-check` warns that its
description may be stale.

**Shared shapes** go in a `skeletons.libsonnet` at the root, as
functions of a file: this repository's `mvu` (Model-View-Update),
`game` (MVU and the game's heart, reached from `update`), `drawn` (the
heart reached from `view`), `via`, `untyped`, `scene3d`, `still`, `way`;
~/ix's `cli` (Main, the command line, the core).

**`tinybox codemap -check <dir>`** reads every config and says:

- *mistakes*: a field unknown, an anchor found nowhere, a file described
  that is not there;
- *warnings*: a file changed since it was described (its new digest);
- *missing*: a program with no skeleton, a hub (a file many others name)
  with no capital, a module (150 lines or more) or a folder with no
  skeleton, a source no config describes.

A pass over a codebase is finished when `-check` says 0 of each. Run it
from the repository's root: a skeleton reaching into another directory
only resolves from there.

## 13. Writing the configs with an LLM: codellm

The configs are meant to be written by an LLM session (Claude Code),
directory by directory, and read and corrected by people. Two things
help it:

- **The brief**, `tinybox codemap -facts <root> <dir>`: what analysis
  knows of a directory. For each file, its lines and digest, its header
  (or "in its .mli"), its sections, its definitions with their uses and
  the definitions calling them ("called by"), its fan-in (how many files
  name it, "A HUB" when many do), and for a program the template names
  it has.
- **The guidelines**, `docs/claude_notes/codemapconfig_guidelines.md`:
  how to write a summary, choose capitals (centrality, not size), shape
  a skeleton, anchor it, and the lessons of the passes so far (this
  repository's, ~/ix's).

The loop: brief, read the code, write the config, `-check`, fix, until
nothing is missing. Parallel agents can each take an area; the check
keeps them honest.

## 14. Colours and conventions

| colour | meaning |
|---|---|
| a region's colour | its top folder's (configurable: `colors`) |
| green, red | user, used: a road's two ends, a street's margin bars, the matrix's hover |
| red capital | a definition many files use; yellow, the others |
| ivory | the skeleton's bones and joints |
| yellow glow | the search's matches, the chosen one pulsing |
| a layer rule's colour | its matches, glowing |
| blue, magenta | in the matrix: a use down the layers, a use against them (a cycle) |
| a definition's colour | its kind's, as the code highlighter colours it |

## 15. Limits

- Names are resolved by the map's own analysis, not the compiler's:
  OCaml's opens and qualified names, C's includes; a module alias
  (`module L = Lexer`), a call through a record of functions, or an
  `external` to C is not followed.
- Assembly files are not sources: a boot skeleton names them in roles.
- The first use of the dependencies on a large repository takes a few
  seconds: every file's uses are resolved, then kept.
- Only OCaml and C are read as code; other languages show as text.
