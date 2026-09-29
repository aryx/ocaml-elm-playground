# Plan: the code map, v2 -- what the LLM knows, drawn

(the name v2 is temporary: `launcher/codemap/Map_v2.ml`, the default of
`tinybox codemap <dir>` since 2026-09-29, a blank slate for now)

## Context

The author (2026-09-29), after the street map and the atlas
(`plan_codemap_google_maps.md`): "We got it wrong, trying to use program
analysis and dependency analysis and parsers alone to know what is
important in a codebase. You know! The LLM knows!"

So the map's judgement -- what matters, what to name, what to say about
it -- is written once, ahead of time, by an LLM that has read the whole
codebase (its README, its code, its dependencies, and what our analyses
compute), into `.codemapconfig` files that the map reads at run time. The
analyses stay, as the LLM's evidence and as the map's geometry (edges,
uses); they stop being the judge.

The first reader: a learner in tinybox, looking at a game's or an app's
code and the kits it stands on (TinyInvaders and `gamekits/shmup/`). Then
the author himself, on tinybox's `launcher/` and on the code map itself
(meta), and on `~/ix`.

## The principles

1. **The LLM is the judge, the analyses its evidence.** A tool
   (`codellm`, below) gathers the facts (the tree, the READMEs, each
   file's header, the definitions ranked by uses, who uses whom) and an
   LLM writes, from them and from reading the code, what a reader should
   see first.
2. **A `.codemapconfig` per directory**, next to the code it describes,
   readable and writable by a human too. It says what the directory is,
   what its files and subdirectories are, what is important in them, how
   to colour them, which tours go through them, which layers to show.
3. **Anchors, not line numbers.** Everything the config points at is
   found by what it is (`def:view`, `type:model`, `comment:"the trick of
   this game"`, later a semgrep pattern), so editing the code does not
   break it.
4. **Earth level: names and sizes, no code.** From afar the code's pixels
   are clutter. A file is its columns (a strip per 80 columns of text,
   codemap's column hint: how long it is, at a glance); a directory is its
   name, big, the programmer's own choice -- clickable, and hovering gives
   its card. Plus *capitals*, the few places the LLM says to know first
   (for `~/ix`, every program's entry point).
5. **The unit of movement is a directory, a file, or a group of them.**
   The camera flies (animated, for orientation) from one unit to another;
   no stopping at a zoom halfway, showing half a region.
6. **Near the ground, the focus decides.** A file's code drawn with its
   lines at different heights (a function's or type's header taller, a
   statement lower, an important call or comment bigger, as the config
   says). And when a file is the focus, the other files are drawn *for
   it*: its associations first -- an edge from where it uses a
   definition to that definition, coloured by direction, the definitions
   it uses in another file bigger than the rest of that file, the uses
   joined to them by edges bundled along the directory tree (Holten's,
   already in `Map_atlas`: bundles save room and need no arrowheads);
   and the same inside it (`update` to `update_game`, `view` to
   `view_game`).
7. **Humans write rules too.** The LLM writes the first configs; the
   human extends them -- new facts about what matters, new layers, new
   colour schemes, whatever helps to see the code. Layers and searches in patterns (semgrep's
   spirit, adapted to a map): "colour every `Cap.$X` by `$X`, count them
   per file, show the counts from afar with a legend".

## What changes from the previous plans

Two decisions of `plan_codemap_google_maps.md` are reversed; saying so
rather than overriding them quietly:

- **Importance**: there, a definition's population (its uses, `Code_rank`)
  decided a label's size and minzoom. Here the config decides; the uses
  become one of the LLM's facts, and the fallback when a directory has
  no config.
- **Zoom**: there (the author's answer of 2026-09-28), "the zooming
  smooth, the display changing continuously as you zoom". Here the
  flights are still smooth, but they go from unit to unit; the wheel
  steps one level of the tree, not a factor of 1.1. Free zoom stays in
  the other views (`m`), which remain as alternatives.

What is kept: the treemap layout (`Treemap`, ordered), the camera and
van Wijk's flights (`Code_map`), the names found and their uses
(`Code_names`, `Code_rank`: the edges of principle 6), the labels'
placement without overlap (`Code_map_base.place`), the file view
(`Code_view`), the parts' colours (the current `.codemapconfig`'s
`colors`, which stays valid), the edge bundles (`Map_atlas`).
`Map_classic`, `Map_streets`, `Map_atlas` stay, as alternative views
behind `m`: v2 the default, they the free zooming.

## The `.codemapconfig`, version 2

jsonnet (the author's choice, 2026-09-29: more elegant than JSON, and
semgrep's language for rules): today's files, being JSON, stay valid
jsonnet. What jsonnet adds that a config wants: fields unquoted,
comments, multi-line strings (`|||`, for patterns), `local`s and
functions (a colour scheme named once), `import` (the layers a repository
shares, in a `.libsonnet`), and `+` to extend an object (a human's
additions on top of what the LLM wrote, in the same file or another).
One per directory, checked into the repository it describes, each
describing its directory, its files and its immediate subdirectories;
the root's also the project. The cards' words come from the config
alone, not from the code at run time (the author, 2026-09-29): what a
header comment says that matters, codellm or Claude itself puts into
the config's summaries, judged and shortened once, instead of the map
guessing a first sentence each time. A directory without a config is
drawn with its names and sizes only (its card: the counts), uses as
importance.

A sketch, `games/shmup/.codemapconfig`:

```jsonnet
// written by an LLM from `codellm` facts, then extended by hand
{
  summary: "Shoot 'em ups: you fly or stand, they come in waves, you shoot.",
  generated: { by: 'claude-opus-5-5', on: '2026-09-29' },
  files: {
    'TinyInvaders.ml': {
      summary: 'Space Invaders (1978): a marching formation, one shot at a time, bunkers that crumble.',
      digest: '9f3a...',
      capitals: [
        { at: 'def:update', say: 'one frame of the game' },
        { at: 'def:view', say: 'the frame drawn' },
      ],
      important: [
        { at: 'type:model', say: 'everything the game remembers', weight: 3 },
        { at: 'def:march', say: 'one alien moved per frame: why the last one runs', weight: 3 },
        { at: 'comment:"one alien per frame"', weight: 2 },
        { at: 'def:column_table', say: 'who drops a bomb, no randomness', weight: 1 },
      ],
      related: ['TinyInvaders.html', '../../tests/2d/golden/TinyInvaders.png'],
      links: [
        { from: 'def:update', to: 'def:update_rules' },
        { from: 'def:view', to: 'def:view_game' },
      ],
    },
    'TinyGalaga.ml': { summary: '...' },
  },
  views: [
    { name: 'Space Invaders and its kits', files: ['TinyInvaders.ml', '../../gamekits/shmup/'] },
    { name: 'Who shoots with Shots', of: '../../gamekits/shmup/Shots.ml', with: 'users' },
  ],
  tours: [
    {
      name: 'How Space Invaders runs',
      stops: [
        { at: 'TinyInvaders.ml:type:model', say: 'The state first: the scenes, the score, the effects.' },
        { at: 'TinyInvaders.ml:def:march', say: 'The accident Nishikado kept.' },
        { at: '../../gamekits/shmup/Shots.ml:def:advance', say: "The kit's shots, shared with Galaga." },
      ],
    },
  ],
}
```

and the root's, `.codemapconfig`:

```jsonnet
local layers = import '.codemap/layers.libsonnet';  // shared, hand-written
{
  title: 'Pictures, animations and games in OCaml, native and in the browser, from the same code',
  colors: { games: '#e08030', libs: '#5080c0' },
  dirs: {
    games: { summary: 'The games, a directory per genre (CATALOG.md).' },
    libs: { summary: 'The from-scratch libraries: pixels, samples, bytes; no Playground.' },
  },
  layers: [layers.capabilities],
}
```

with `.codemap/layers.libsonnet`:

```jsonnet
{
  capabilities: {
    name: 'capabilities',
    rules: [{ pattern: 'Cap.$X', 'color-by': '$X' }],
  },
}
```

The fields:

| field | where | what |
|---|---|---|
| `title` | root | the project in a sentence, in the map's title |
| `summary` | any | the directory in a sentence (its hover card) |
| `dirs.<name>.summary` | any | an immediate subdirectory's card, when it has no config of its own (a leaf of little code) |
| `files.<name>.summary` | any | a file's card |
| `capitals` | a file | shown from the earth level: a dot and a name |
| `important` | a file | drawn bigger near the ground; `weight` 1 to 3 |
| `links` | a file | edges inside it the LLM or the human finds worth showing, bundled with the uses at street level |
| `tours` | any | named walks, each stop an anchor and a sentence; paths relative to the config |
| `related` | a file | files to see with it, which no analysis would link (a game and its level editor, its level file, its tests, its web page; a module and the plan that explains it): shown beside it at the street level, and in its views of dependencies and users, marked as linked by hand |
| `views` | any | named sets of files and directories shown together, their own layout ("a game and its kits", "the renderer and its users"); paths relative to the config, or a unit and a relation (`{ of: 'Shots.ml', with: 'users' }`) |
| `layers` | any | pattern rules, their colours and legend, for the directory and below |
| `colors` | any | the parts' colours (today's field), paths relative to the config |
| `digest` | a file | its contents' digest when described: the map and the checker say "stale" when it changed |
| `generated` | any | who wrote it, when (a human's edits keep it or drop it) |

**Anchors**, resolved against the file's definitions (`Code_file.defs`)
and comments, first match:
`def:<name>`, `type:<name>`, `module:<name>`, `comment:"<words>"` (a
comment containing them), `line:<n>` (discouraged: breaks), and later
`pattern:"<semgrep pattern>"`. `file.ml:<anchor>` from another
directory's config (a tour going through a kit).

**The checker**, `tinybox codemap -check <dir>`: every config parses,
every anchor resolves, every described file exists, digests are fresh
(a warning). Run on the repository's configs in `make test`, so a renamed
`view` breaks the test, not the map.

### jsonnet, the language

`languages/jsonnet/` (the author's place for it, 2026-09-29): a
jsonnet evaluator, pure, its value a `Json.t` (the manifested object).
The official spec is the reference, not ojsonnet (the author, 2026-09-29:
"I actually had bugs in ojsonnet and mismatch with the original jsonnet
official spec"), which gave only the idea of objects as layers; where
ours departs from the spec is listed in `Jsonnet.mli`, kept while it is
enough for the configs. `import` given a host function (a file's text by
its path), so the evaluator stays pure and tinybox's embedded sources
serve it too.

Beside `json/`, with a lexer of its own (done, 2026-09-29): jsonnet's
# comments, text blocks and verbatim strings, its operators as runs of
symbols, and no regular expressions to make a / ambiguous, did not fit
`Js_lexer` without harming JavaScript. A row in `languages/README.md`'s
table.

## codellm: the LLM's evidence, and its guidelines

No API call (the author, 2026-09-29): an interactive Claude Code
session writes the configs, following the guidelines; `codellm`, a
program of ours, assists it when the session needs facts it cannot
cheaply get by reading.

- `tinybox codemap -facts <dir>` (or its own exe, `codellm`): for each
  directory, a Markdown brief of what the analyses know -- its tree and
  sizes, its README or the files' header comments, each file's
  definitions ranked by uses (`Code_rank`), who uses whom across
  directories (`Code_rank.links`, `Code_deps`), the entry points (a
  `main`, `Program.main`, `Cap.main`), the existing config and which of
  its digests are stale.
- `docs/claude_notes/codemapconfig_guidelines.md`: how to write a good
  config -- a summary is what the directory is *for*, not a list of its
  files; three capitals at most per file, fewer per directory; `important`
  is what a reader must see to understand the file, not what is used
  most; tours of 5 to 10 stops, in the order a reader should read; prefer
  `def:` anchors; mark nothing that the name already says. With a worked
  example (TinyInvaders') to imitate.
- The loop: facts, then the LLM reads the code and writes or updates the
  configs, then the checker; stale digests say which directories to
  redo.

codegraph's part: the "facts" are the start of it -- `Code_names`,
`Code_rank`, `Code_deps` computed over a directory and dumped. Richer
analyses (types, calls resolved through modules, the playground's
`~/github/codegraph` graph for `~/ix`) add to the brief later, without
changing the config.

## The levels, sketched

### Earth: the whole directory (`~/ix`, or a program's code in tinybox)

```
 ix -- "A Plan 9-like OS and its tools, from scratch in OCaml"   507 files
+--------------------------------------------------------------------------+
| KERNEL                       | MACHINE            | TINY                   |
|  ||||| |||| ||| || ||||| ||  |  ||||||| |||| ||  |  ||| ||||||| || |||   |
|   devices   processes  ...   |   Arm32  Plan9     |  TinyML  TinyShell     |
|  ||||| ||| ||||   ||| ||||   |  ||||| || |||     |  |||| ||| |||| ||      |
|   * main (Main.ml)           |                    |  * main (TinyShell.ml) |
+------------------------------+--------------------+------------------------+
| SHELL        | ASSEMBLER | BUILDER    | VERSION_CONTROL                    |
|  * main      |  * main   |  * main    |  ||||| ||| ||||| |||   * main      |
+--------------+-----------+------------+------------------------------------+
 hover "kernel" -> [ kernel/: the Plan 9 kernel: processes, devices, 9P  ]
                   [ 38 files, 12,400 lines   capitals: main, syscall    ]
```

`|` a strip of 80 columns: a file of 400 lines in a narrow column of the
treemap is several strips, its length readable at a glance, no text.
Directory names in capitals and large; subdirectories' smaller under
them (diagonal when their block is too narrow for the name horizontally);
capitals as `*` with their name. Nothing else.

### Region: one directory flown into (click, or wheel one step)

The same picture one level down: its subdirectories' names large, its
files named, each file's capitals, the column strips still. Still no
code. The wheel out goes back to the parent, in one flight.

### Ground: a file (TinyInvaders.ml)

```
  TinyInvaders.ml -- Space Invaders (1978): a marching formation, ...
  ==================================================================
  (* A toy version of Space Invaders ... *)              (header, small)
  THE MODEL                                              (section, large)
  type MODEL = { scenes; hi_score; fx; wave_shown }      (weight 3)
  THE MARCH
  let MARCH (g : game) : game =                          (weight 3)
      . . . . . . . . . . . . .                          (statements, 1 px)
  SHOTS
  let drop_bomb  let erode  let hit_bunker  let move_shot (headers, medium)
  UPDATE
  let UPDATE computer model =  ----------------->  update_rules   (arrow)
  VIEW
  let VIEW computer model =    ----------------->  view_game      (arrow)
```

The code is there, but its lines' heights say what matters: sections and
`important` definitions large, other headers medium, bodies thin (a
line a pixel or two, SeeSoft's), important comments large. Readable
where it matters without flying closer; closer still, the ordinary file
view (`Code_view`).

### Street: the file and its associations (TinyInvaders + gamekits/shmup)

```
 +------------------------------------+      +-------------------------+
 | TinyInvaders.ml (the focus, large) |      | gamekits/shmup/Shots.ml |
 |                                    |      |   (small, dim)          |
 |  let drop_bomb g =                 |      |                         |
 |     ... Shots.straight ... ---.    |      |                         |
 |  let move_shot g =             \    |      |                         |
 |     ... Shots.advance ... -------=====---->|  let STRAIGHT (large)   |
 |                                    |   \  |  let ADVANCE  (large)   |
 |                                    |      |  (the rest: thin)       |
 +------------------------------------+      +-------------------------+
            ^ uses (green)                    used by (red) v
```

The focus file large and central; the files it uses (its kit, here) and
those using it around it, small, but the definitions actually used drawn
large; the uses joined to their definitions by edges bundled along the
directory tree (`Map_atlas`'s Holten bundles: one thick road out of the
file, splitting near the kit), coloured by direction (green what it
uses, red who uses it, as codemap's); the same inside the focus, with the
config's `links`. What the focus does not touch, dimmed.

## Skeletons (the author, 2026-09-29)

"One notion we might want to add is 'skeleton' like in biology, where it
helps to see the underlying structure; for space invader for instance
the model, view, update are pretty fundamental"; and "great idea that
skeletons span files", "what would be the skeleton at the earth level!"

Done the same day: `skeletons` in a config (bones: an anchor with its
file and a role; joints between bones), checked; a template in the
root's `skeletons.libsonnet` (`mvu(file)`), extended by `+:`;
TinyInvaders' (MVU, its spine to `march`, across to `Shots.advance`)
and the repository's ("How a program runs", `Program.main` to the frame
loop, across `libs/`, `playground/` and its platforms). The X-ray, `x`,
at every level (`Map_v2.skeleton_shapes`); from afar, a file's close
bones packed into one dot. To come: a skeleton for every game (the
template's default, checked), the apps' and the languages' (a
compiler's passes), the earth level marking each file by its skeleton,
the tour following a skeleton's joints.

The skeleton at every level (the author, 2026-09-29: "Ideally we want
the same skeleton at the earth level, and region level and maybe in the
'views' level"; "let's focus on making skeleton good at the ground,
region, earth level and we can tackle the other plates later"): a bone
may be a whole file or directory; a skeleton's level is its config's
directory; the X-ray shows the unit's skeletons one at a time (`x` the
next), the deeper ones as named dots, a banner at the map's foot naming
the one shown. Written: the root's layers, `playground/`'s one API and
its platforms, `launcher/codemap/`'s own. To come: a view's skeleton,
the bones in the view's files (when views are built); a skeleton for
every important directory, by the codellm loop.

## Anatomy: beyond the skeleton (later)

The author (2026-09-29): "go even deeper with the biology terminology
and at some point in addition to xray show also blood flows, muscles,
etc.", "air flow". A body's systems, each a way of looking at the same
code, each a mode like the X-ray, each from the configs where judgement
is needed and from the analyses where it is not:

| system | in the code | seen as | from |
|---|---|---|---|
| skeleton | the structure the rest hangs on | bones and joints (done) | the configs |
| blood | the data carried round: the model through update and view, a token stream through a compiler's passes | a flow along the joints, colored by type, pulsing each "frame" | the types of the bones' parameters and results (the analyses), named in the config |
| muscles | the code that does the work: the heavy loops, the rasterizer, the physics step | the definitions shaded by their weight (size, loops, calls per frame) | the analyses (size, nesting), hot ones named in the config |
| nerves | the signals: input events, messages, subscriptions and commands | from the keyboard and mouse in, to update, out as commands | Sub/Cmd uses, `computer.keyboard`/`mouse` (the analyses) |
| lungs, air | the exchange with the outside: files, network, the screen, the clock | the Cap.* uses, where the program breathes (the Caps layer) | a layer's pattern rules (`Cap.$X`) |
| skin | what is shown to others: the interfaces | the .mli, the public names | the files themselves |
| organs | the parts with one job: a kit, a subsystem | regions of the map (the directories) | the configs' summaries |

The same mode switch, several systems: `x` the skeleton, and others to
choose (keys or a menu), several shown at once, each in its colour, as
an anatomy atlas's plates.

Done, 2026-09-29 (the author: "love those biology elements! Let's do
it!"): `Code_anatomy` (the facts: nerves' and lungs' lines by their
words, outside comments and strings; muscles, a definition's loop
density -- not its size, which the map's area already shows, and which
made every big file look strong; skin, the definitions a .mli exposes);
in the X-ray, 1 to 6 the plates, a legend; blood as pulses running
along the skeleton's joints; from afar a file tinted by its muscles
(the strongest sixth of the files known), a dot for its nerves and one
for its lungs, as big as they are many, its skin a faint frame; at the
ground and the street the lines tinted, the skin a mark in the margin.
The facts found a few files a frame from afar, so that the whole
repository's X-ray opens at once. To come: organs (the directories'
summaries as the plates' captions), a config's `layers` adding its own
words (a kernel's syscalls as lungs), blood along the street's roads.

## What works, the author's word (2026-09-29)

"The earth view, with the color scheme ... and folder names, and
subfolders, and files when pixel space allows it is very good. The
skeleton LLM-generated view is also great, at the earth level and
street level. The street level with 'a' is also great, and the glow,
peek, use/def is also great and varying height pixel is also very
useful." The region level is the weak one ("not sure what we can do at
that level"): its files' code drawn there was unreadable and removed;
each file's card and table of contents (its sections, a click peeking
at one) are the current attempt -- and the author, later that day: "I
start to like the region with this file + summary + clickable red
sections and capital. Love the peek and hover generalized."

The peek grew: a section peeked at, a long peek scrolled by the wheel, a
name hovered in a peek glowing there and on the map, a name defined
elsewhere previewed on hover (its first lines) and a peek of a peek on a
click, four deep; a program's map resolving its names against every
source (a click on game in TinyInvaders peeks at Playground's).

## Layers and views, next (the author, 2026-09-29)

"We also want at some point to work on those layers and views: layer so
one can in the codemapconfig give for instance a pattern to find
Cap.fork and color it in a special way and then run that through the
codebase and glow/highlight where the match is at the earth level and
region and street." The plan's step 9 (patterns, regexps first) and 10
(views), the Cap.fork layer their first test.

## Views: several units at once (later)

The author (2026-09-29): "at some point we might want to zoom in
multiple dirs at the same time or a 'view' of the codebase with
selected files, like one game and its immediate deps, or one lib file
and its immediate users". So a unit is not only a node of the tree, and
`focus : int` will become a set. Two cases:

- **Neighbours in the layout** (two sibling directories, a directory and
  the one beside it): the same map, the camera framing their union,
  the rest shaded -- today's step 2 with a set instead of one index.
- **Scattered** (a game in `games/shmup/` and its kits in `gamekits/`
  and `playground/layers/`; a file of `libs/` and its users all over):
  their union is the whole map, so framing it says nothing. Instead a
  *view*: a new treemap of just those files, each still in its
  directories (their paths kept, the directories between them folded),
  the focus the largest. tinybox's code map of a program already is one
  (`Codemap`'s own code, `Code_deps.closure`), and the street level's
  focus and its associations is one too.

And by hand (the author, 2026-09-29): a file's `related` in the config,
files to see together that no analysis links -- `TinySokoban.ml` with
`TinySokobanEd.ml` and `TinySokoban.xsb`, a module with its tests, a
game with its page; its views include them, drawn with a link of their
own (not a use's colour). Written in the config by hand at first;
later perhaps from the map itself (a key to relate the file looked at
to the one under the mouse, the config's jsonnet appended to, which the
checker then validates).

Entering a view from a unit (keys to decide: its dependencies, its
users, both, its related files), leaving it back to the unit it came from (the breadcrumb
gets a step "> uses of Shots.ml"); the transition animated, each file
moving from its place in the whole map to its place in the view, so the
eye follows (the orientation that the smooth flights give). A view is a
list of paths, so the `.codemapconfig` can name some too (`views:`,
beside `tours:`), written by the LLM ("the renderer and its users") or
by hand; the same list is what a tour's stops pass through.



A layer is rules, each a pattern and a colour (or `color-by` a
metavariable), with a legend. Near the ground, the matched lines lit
(codemap's `micro_level`); from afar, each file's share of each colour as
bands across its block (codemap's `macro_level`) -- the `Cap.*` of `~/ix`
seen from the earth level: which programs open files, fork, use the
network (`~/ix` today: 54 files, `Cap.open_in` 32 times, `Cap.fork` 12).

Patterns, in three steps, each useful alone:
1. regular expressions over lines (enough for `Cap\.([a-z_]+)`);
2. token patterns, semgrep-lite over our highlighters' tokens: `$X`
   a name, `...` any tokens balanced, `Cap.$X`, `Shots.$F (...)` -- for
   OCaml and C, in `libs/code/pattern/`;
3. a generic AST and its matcher (`libs/code/ast/`), OCaml and C first,
   for patterns that need structure (`let $F ... = ... Cap.fork ...`).

The same patterns serve search (`/` with a pattern) and anchors
(`pattern:`). A VCS layer (age, activity, codemap's) later.

## Steps

0. `Map_v2` as a style, the default of `tinybox codemap <dir>`; the
   glass off by default. (Done, 2026-09-29.)
1. **Earth and region levels, without config**: column strips instead of
   code, directory and subdirectory names large (diagonal when narrow),
   clickable (fly into it); hover cards with the counts only (files,
   lines, subdirectories) -- their words come with the configs, step 4.
   Golden frame of a `~/ix`-like fixture.
2. **Navigation by units**: click a name or block to fly into it, wheel
   one level up or down the tree, arrows to the sibling directories;
   van Wijk's flights kept. Tested on the camera's targets (pure). (Done, 2026-09-29: `Code_units`; the
   units being tall and narrow on a wide screen, the one framed shares it
   with its neighbours, so outside it the map is shaded, and the unit and
   its ancestors named on a breadcrumb. The camera still eases, van
   Wijk's flight to come. Step 1 done too, the same day.)
3. **jsonnet** (`languages/jsonnet/`): lexer, parser, desugaring,
   evaluator, the standard library's first functions, `import` through a
   host; tests from the jsonnet spec's examples and ojsonnet's. (Done,
   2026-09-29: `Jsonnet_lexer`, `Jsonnet_ast`, `Jsonnet_parse`,
   `Jsonnet`; 11 tests, the plan's root config among them.)
3b. **The config, version 2**: `Code_config` reads the new fields,
   per directory, merged from the root down; anchors resolved; the
   checker (`-check`), run in `make test`; tinybox embeds the configs
   with the sources (`Tinybox_sources`). (Done, 2026-09-29, but the
   embedding and `make test`'s check, which come with step 4's configs:
   `Code_guide` reads every directory's config, strictly, its colours
   among the rest (`Code_config` keeping `.codemapignore`); anchors
   `def:`, `type:`, `module:`, `section:`, `comment:"..."`, `line:`;
   `tinybox codemap -check <dir>`; the map's title and cards from it.)

4. **The pilot's configs, written by hand with the LLM**: the root,
   `games/`, `games/shmup/`, `gamekits/`, `gamekits/shmup/`; the map's
   title, cards and capitals from them. (Done, 2026-09-29: the root's,
   `games/`, `games/shmup/` (TinyInvaders: capitals, important lines,
   links, related files, a tour, a view), `gamekits/`, `gamekits/shmup/`,
   written after `codemapconfig_guidelines.md`; a game's capital its
   heart, not `update` and `view`, which every game has. Embedded by
   tinybox beside the sources (`Code_deps.repository_configs`,
   `Codemap.guide_of`); checked by the code map's tests, a mistake
   failing them, a stale digest only the checker's warning. The
   capitals drawn by `Map_v2`, a dot and a name, their card saying why.)

5. **Ground level**: lines' heights by category and weight; tested with
   a golden frame of TinyInvaders. (Done, 2026-09-29: `Code_ground`,
   the weights, the columns, the painter; `Map_v2` at the ground when the
   unit is a file and the camera there, the important lines marked in
   the margin, the config's words as notes after them when the column
   has room; Enter and the status line through `style.pick`. To come: a
   note for a line with no room left, names clicked at the ground.)

6. **Street level**: the focus and its associations; edges from
   `Code_names`' resolutions and the config's `links`, bundled
   (`Map_atlas`'s bundles, moved to a module both use). (Done, 2026-09-29, the uses: `Code_street`;
   at the ground, `a`: the focus on the left, a panel per file it uses
   on the right (six at most, the program's own code first and thrice
   its share, then the most used), each laid out by `Code_ground` with
   the definitions used tall and the rest squeezed; roads
   (`Map_atlas.road`) from each use to its definition, bundled per file;
   operators' uses left out, only sure resolutions kept. `focus=<path>`
   opens the map on a unit. Then two-sided (the author: "on the right of
   the focused file the callers of this file, and on the left the
   callees ... so we have full context for a file", "something we can
   also cycle through; just users, just uses, both"): `a` cycles what it
   uses (left), what uses it (right, from `Code_rank`'s links), both,
   off; six panels a side, the others named at its foot, the eight most
   tied lines of a panel tall; the roads from the name used to the name
   defined (not from the file's edge, which the author found useless),
   faint, the hovered line's lit; the marks green at a user's line, red
   at a definition used. A click peeks at a definition's body. To come:
   grouping a crowded side by directory; a config saying which
   neighbours matter. The hover (the author, 2026-09-29: "as we
   hover we probably want to highlight what is hovered on"): the line
   under the mouse framed; its roads lit, the others dimmed, their ends
   framed (a use green, a definition red); a name's binding pulsing and
   its uses lit, as on the map up close, in the focus and the panels.
   The config's notes (its `say`s, which the author likes) at the
   street too, beside the focus's lines and the panels' tall ones. To come: the users (red, the other way),
   the config's `links` inside the focus, its `related` files as
   panels of their own, an .ml and its .mli as one panel.)

7. **codellm** (started, 2026-09-29: `Code_facts`, `tinybox codemap
   -facts <root> <dir>`; `skeletons.game`; `games/arcade/` written, its
   18 games' cards, capitals, important lines, ten game skeletons and
   the region's own, what its games share; then `games/platform/`, its
   region skeleton the platformer kit and the kits it borrows, the
   off-map ends drawn as one port each on the edge): the facts brief and the guidelines; configs for
   `launcher/` and `launcher/codemap/` (the map explaining itself), then
   `~/ix`; a tour of each.
8. **Tours from the config** replacing today's (headers, sections,
   tricks: those become the fallback).
9. **Layers**: regexps, then token patterns (`libs/code/pattern`), the
   Caps layer of `~/ix`; the generic AST after.
10. **Views**: a set of units framed together; then views of scattered
    files, their own layout, entered from a unit (dependencies, users),
    named in the config; the animated transition.
11. v2 the default of tinybox's own code map (`s`) too, the other styles
    kept as alternative views (`m`). (Done, 2026-09-29: the author, "in
    the tinybox menu, I wonder if we would want to show the v2 view for
    the file instead of the current view? but still also with the
    gamekits it depends on as before": a program's map opens on its
    file at the ground, its kits at the street (a) and the wheel away;
    the panel's preview v2 from afar.)

## Decisions (the author, 2026-09-29)

1. The config in jsonnet, as semgrep's rules; the evaluator in
   `languages/jsonnet/`.
2. The configs checked into each repository, written from what codellm's
   session judged useful, extended by hand: new facts, layers, colour
   schemes.
3. v2 the default; the other views (`m`) for zooming freely.
4. An interactive Claude Code session writes the configs, following the
   guidelines, helped by a `codellm` program if needed; no API calls.
5. Edges as bundles (`Map_atlas`'s), not arrows.
6. The other views kept as alternatives.
