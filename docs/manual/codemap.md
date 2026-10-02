# The code map: a manual

The code map is tinybox's code visualizer: a codebase drawn as a map, to
be read the way one reads a map of a country, from the whole down to a
street. It runs on any directory (`tinybox codemap <dir>`), on a program
from tinybox's menu (`s`, or a click on the code under a game's
preview), and on this repository itself.

This manual describes the map's current style, v2 (`y` switches to the
older one, classic: every file's code painted from afar, the wheel
zooming freely). It says what the map shows, how to
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
8. Marks and layers
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
first), its *skeleton* (the parts the rest hangs on), its marks, its
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
modes, `x` the skeletons, `m` the marks, `d` the tied view's modes.

**The same colours mean the same thing everywhere.** Green is the user,
red the used: a road from a use to a definition goes from green to red;
a capital many files use is red (its file's module named by 30 files, or
the definition itself used by 10), and one whose calls reach many files
(30), an entry point, a main loop, green; in the matrix, a row's dependencies
turn red and its users green.

## 2. Starting it

```
tinybox codemap <dir>            the map of a directory (tinybox codemap ~/ix)
tinybox codemap . focus=<path>   opened on a folder or file
tinybox codemap . code=<Program> opened on a program's own code (w widens);
                                 all: all of it at once, not its main file
tinybox codemap . def=<name>     opened on a definition, peeked at
                                 (focus=<file> line=<n>: the one there)
tinybox codemap . focus=<file> street   the file at the street (a); street=<n> a mode
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

**On a web page.** A project's site can carry its map, and its
documents can then link to a part of the code in the map rather than
to GitHub's file view:

```
make codemap-web DIR=~/github/ix PAGE=~/github/ix/docs
```

makes three files. Only the page goes to the project; the two big ones
go to the assets repository (`~/github/assets`, `ASSETS=` to change it),
as the games' pages do, so as not to fill the project with generated
megabytes:

| file | what | where |
|---|---|---|
| `codemap.html` | the page, naming the two others | `PAGE` (ix's `docs/`) |
| `codemap.bc.js` | the map, the same program for every project (`launcher/codemap/web`, 0.75 MB) | `assets/js/codemap/` |
| `<name>.txt` | the directory's code and configs as one file (`make_codemap_data`, ix's 3.7 MB, compressed on the way) | `assets/codemap/` |

Run it again when the code or its configs change, and push both
repositories, the assets first. `NAME=` names the bundle (the
directory's name by default), so that xix and principia get their own
beside ix's.

The page's URL says where the map opens, with the flags `tinybox
codemap` takes:

```
codemap.html?focus=version_control                a folder
codemap.html?focus=version_control/Commands.ml    a file, flown to
codemap.html?focus=kernel/proc.c&line=120         the definition at that line, peeked at
codemap.html?def=diff                             a definition by name (under focus= if given)
codemap.html?code=TinyMario                       a program's own code, as tinybox's menu shows it (w widens)
codemap.html?code=TinyChrome&all                  the same, all of it at once, not flown into its main file
codemap.html?code=TinyTurboPascal&street          the same, its file at the street: what it stands on beside it
codemap.html?focus=windows/rio/wind.c&street=3    a file at the street, in a mode (1 both sides, 2 its users, 3 its uses)
codemap.html?data=<url>                           another bundle
```

The map's version is at the bottom right ("code map 0.07"), raised at
each publish of a change: a browser may keep the program it has for ten
minutes, so a page not showing the latest one wants a reload that skips
the cache (Cmd+Shift+R).

The page's address follows the map as one moves (the unit looked at,
the definition peeked at), so the address bar is always a link to
where one is: copy it to point someone at a part of the code.

The page must be served, not opened as a file, since it fetches its
data. The page names its data by an absolute URL (`https://aryx.github.io/assets/...`), so before the assets are
pushed, try it with a local server and `?data=` pointing at the local
bundle; `aryx.github.io/assets` and a project's `aryx.github.io/ix`
being one site, the browser lets the page fetch it.

## 3. The levels

The map changes what it draws with the distance, like a map app.

**The earth** is the whole directory. Folders are coloured regions,
their names written as large as their block allows (the top ones
largest, their subfolders smaller). Files show as columns of tiny lines,
the code's shape. *Capitals* are dots with a name: the definitions the
configs call central, as large as the files depending on them are many
(a red capital: a definition many files use; a green one: a definition
whose calls reach many files, an entry point). Hovering a folder's or
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

A click on a panel's name makes that file the street's: it moves to the
middle, its own neighbours round it. Clicking the files on the left one
after the other walks down the dependencies, what uses what, a step at
a time; on the right, up them. A link can open a file at the street
(`&street`, section 2): a small program's main file shown with what it
stands on.

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
the core"), how often it is used, and how many files its calls reach.
The capitals that matter are larger (used by 10 files or more, or
reaching 30) and win over a file's card where they meet. And each unit
shows its **entry point** whether a config names it or not: of the
definitions under it, the one whose calls reach the most files, green,
its file said ("threadmain (rio.c)"; an OCaml program's unnamed
`let () = ...`, "main (Tinybox.ml)"). A definition's reach follows its
calls to other files and within its own (a helper calling on).

**Peeks.** A click on a name in the code opens a *peek*: the definition
of that name, readable, over the map, with the comment just above it
(a literate program's markers, `/*s: function [[f]] */` and `/*e: ...
*/` or OCaml's `(*s: ... *)`, left out: boilerplate). A
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
files together, **ctrl+Enter** keeps the query as a mark (section 8).
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

The joints give the order to read it in: each bone is numbered and
coloured by its depth, the longest chain of joints leading to it from a
bone nothing leads to -- 1, green, where to start (the main), then on
through yellow to red, the end; a joint goes from its start's colour to
its end's, and the bones of a cycle share a number. A skeleton without
joints stays ivory. Its name and the plates' legend move to the corner
where they hide the fewest bones.

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
| 2 | muscles | where the work is: definitions dense with loops |
| 3 | nerves | the inputs: keyboard, mouse, events (the configs' `anatomy: nerves:` rules) |
| 4 | lungs | the I/O: files, network, console, processes (`anatomy: lungs:`) |
| 5 | skin | what a module exports: the `.mli`'s definitions barred, the private ones shaded |

## 8. Marks and layers

A mark lights, everywhere at once and at any level, the lines matching
its rules, each rule in its colour, with a legend at the bottom left.
The configs define marks (this repository's root: Capabilities, each
`Cap.*` in its colour); a search can be kept as a mark (ctrl+Enter in
the search box). `m` cycles: the marks kept, each config's, then the
nerves and the lungs (the X-ray's plates 3 and 4 as marks, a colour
per anatomy rule, so that every line reading the keyboard, or writing
to the disk, shows at once, with its count), none.

Matches glow slowly, to tell them from capitals. Hovering one shows its
line and the code around it; a click goes there.

**Layers** colour the whole map by a measure, at two levels (as
`~/codemap`'s): from afar a file in one colour (the macro level); nearer,
when a line is big enough to see, each definition's lines in its own
(the micro level), so that zooming in shows which definition makes a
file red. Under a layer the regions lose their colours, the layer's the
only ones. `l` cycles through the layers, then none, shift+`l` back; a
key at the bottom right names the colours.

**Used vs using**, the first, orients: what is the bottom of the
project, the middle, the top. The parts of the unit looked at (at the
earth the top folders, in a region its subfolders and files) are each
coloured by the uses coming into them from the other parts over those
and the uses going out to them: red, the bottom, what is used (at
principia's earth, lib_core and include); yellow, the middle (the
libraries on them); green, the top, what uses (the programs). Flying in
recomputes it for the new unit's parts. Nearer, each definition by the
files using it over those and the files its calls reach.

**The call depth**, the second: each definition by its place in the call
graph (its calls resolved as its uses are), its depth (the longest chain
of callers above it) over its depth and its height (the longest chain of
callees below it), ranked among all; a file, its definitions' mean. The
same graph gives a definition's *reach*, the files its calls reach,
transitively: a capital reaching 30 or more is green.

**Roles**, the third (after `~/codemap`'s architecture layer): each file
by what it is there for -- tests, examples, generated code, third party,
per CPU, per OS, parsing, network, graphics and UI, audio, storage,
security, utilities, entry points, interfaces; the key lists those found
under the unit looked at, how many files each. A path is read as words
(`TinyVisiCalc.ml` is tiny, visi, calc), each matched whole, not as
substrings; and the map's own knowledge counts: a file nothing uses but
that uses others is an entry point, a `.ml` beside its `.mll` or `.mly`
generated, assembly per CPU. A file gets the first role, in that order,
it has evidence for.

**Tested**, the fourth: the tests' files blue, and each other file green
if the tests reach it (through the files they use, and those use,
transitively), red if not -- where tests are missing (interfaces left
out, checked through their implementations). **Described**, the fifth:
each file by what its config says of it -- a summary and capitals or
important lines, green; a summary alone, yellow; nothing, red -- where
the configs are still to write.

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
| `x` | the X-ray (cycles skeletons); `1`-`5` its plates |
| `m` | the marks (cycles) |
| `l` | the layers (cycles; shift+`l` back): the map coloured by a measure |
| `g` | the matrix |
| `d` | in the tied view: its modes |
| `n`, `p` | a tour's next and previous stop |
| `w` | a program's map: its code, with what it uses, the whole repository |
| `b` | back after a jump |
| Enter | the file view |
| `y` | the other style of map (classic) |
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
  marks: [{ name: 'Enemies', rules: [{ ref: 'Alien.spawn', color: enemy_color, say: 'an enemy made' }] }],
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
| `marks` | `[{ name, rules: [{ text or ref, color, say }] }]` (the old name, `layers`, still read) |
| `anatomy` | `{ nerves: [{ text or ref, say }], lungs: [...] }`: the X-ray's nerves and lungs |

**Anchors** point at a place by what it is, not its line, so that
editing the code does not break them:

| anchor | |
|---|---|
| `def:march` | a function's or value's definition |
| `type:model` | a type's |
| `module:Make` | a module's |
| `section:Model` | a section's title, `(* Model *)` between rules of stars |
| `comment:"one alien per frame"` | the first comment containing those words |
| `code:"verify_area"` | the first line of code (not a comment) containing those words: an uncommented line of old C; whitespace exact, no tab, no inner quotes |
| `line:42` | a line (discouraged: it moves) |

A literate program's chunk markers make the surest anchors:
`comment:"function [[mountio]]"`, `comment:"struct [[Node]]"`. Such an
anchor lands on the code under the marker (the `struct Node` line), and
the map names it by the chunk's name alone: `Node`, not `struct
[[Node]]`.

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
| red capital | a definition many files use (its module named by 30, or it used by 10); yellow, the others |
| green capital | a definition whose calls reach 30 files or more: an entry point, a main loop |
| the layer's green to red | the top (what uses) to the bottom (what is used), the regions grey under it |
| ivory | the skeleton's bones and joints |
| yellow glow | the search's matches, the chosen one pulsing |
| a mark rule's colour | its matches, glowing |
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
- The call graph (the reach, the call stack) follows the calls the names
  resolve to: a call through a table of functions (tinybox's launcher
  starting a program by its registered main) is not seen, so a
  dispatcher does not reach what it dispatches to. Nor a system call: a
  kernel, the bottom of any running system, is not seen used by the
  programs calling it.
