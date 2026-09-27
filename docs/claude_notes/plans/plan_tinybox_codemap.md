# Plan: tinybox as a code visualizer -- TinyCodemap

## Context

The author (2026-09-27): "I really like this idea of making tinybox
also a code visualizer!" Every program in tinybox is a toy made to be
read; the menu shows how it plays (`plan_launcher.md`), and this plan
is about showing how it is *written*, after codemap -- the author's own
visualizer (pfff, 2010), itself after SeeSoft (Eick, Steffen and Sumner,
Bell Labs, 1992: every line of a program as a coloured row, a whole
system on one screen).

The author's directions, in order:
- a `languages/ocaml/` (decided: `ocaml/`) that **parses, no eval or
  compile**;
- "maybe a bit more than ocaml-light, but ideally the Playground at some
  point can be compiled by ocaml-light (which has restrictions)":
  ocaml-light (`~/github/ocaml-light`) is OCaml 1.07 with objects and
  functors taken out, the compiler of `xix` and Principia Softwarica;
- "a graphics/treemap/ or something, even powerful zooming, maybe a
  faster font engine though";
- also, as a first cheap version: a screenshot of the real codemap per
  program (`plan_launcher.md`, step 15);
- and "more generally a code visualizer of the games and apps and libs
  too": the whole repository, not only one program's files.

## What the repository's OCaml is

2084 `.ml`/`.mli` files, 278k lines (outside `_build/` and `docs/`).
ocaml-light's grammar is OCaml 1.07's (`~/github/ocaml-light/parsing/
parser.mly`, its reference). What this repository uses beyond it, all
later than 1.07:

| feature | since | in this repository |
|---|---|---|
| labelled and optional arguments (`~width`, `?(scale = 1.)`) | 3.00 | everywhere: the API's style |
| polymorphic variants | 3.00 | a few hundred uses |
| `lazy`, `assert` | 3.00 | `lazy` in tables made on first use (tinybox) |
| objects, as types only: `< Cap.open_in; .. >`, `caps#...` | 3.00 | the capabilities, and nothing else (the repository's rule, for ocaml-light's sake) |
| `let open`, `M.(e)` | 3.12 | several hundred |
| `{|quoted strings|}` | 4.02 | embedded data, levels written as text |
| attributes `[@...]`, extensions `[%...]` | 4.02 | a handful |
| `let*` binding operators | 4.08 | a few libraries |
| locally abstract types `(type a)`, GADTs | 3.12/4.00 | rare |
| functors | 1.07 has them; ocaml-light does not | `Map.Make`, rare |

So "a bit more than ocaml-light": the parser takes all of the above,
and a second pass, the **light checker**, says what in a file is not
ocaml-light -- the list of what stands between the Playground and
ocaml-light, per file, per library, and shown as a layer of the map.

## What codemap does, to borrow

The author: "look at the code of ~/codemap and its semgrep-pfff-libs and
semgrep-pfff-langs as inspiration". Surveyed 2026-09-27 (paths under
`~/github/codemap`):

- **The treemap**, `libs/treemap/treemap.ml` (and its literate
  `Treemap.tex.nw`): `Classic` (slice-and-dice), `Squarified`,
  `SquarifiedNoSort`, `Ordered` (pivot by size or middle), chosen by
  `layoutf_of_algo`. Nesting drawn by painting a directory black and
  laying its children out in it shrunk by a border that thins with depth
  (0.003 at depth 2, 0.001 at 3): the black showing through is the
  border. The space is 1.71 by 1. Labels (`src/gui/draw_labels.ml`):
  size and alpha by depth, rotated along the diagonal of a tall
  rectangle, their extents memoized. The tree built by
  `treemap_of_tree ~size_of_leaf ~color_of_leaf`, single-child
  directories removed (`remove_singleton_subdirs`).
- **The categories**, `semgrep-pfff-langs/highlighting/highlight_code/
  Highlight_code.mli`: some 80 (keywords, literals, entities with their
  kind and def/use, locals and parameters, comment sections...), def and
  use with their arity (how used a thing is); the colours in
  `info_of_category` (comment gray, keyword orange, string
  MediumSeaGreen). Ours: a subset, the same colours.
- **The OCaml highlighter**, `semgrep-pfff-langs/languages/ocaml/
  highlight/highlight_ml.ml`: the tree first (defs and uses resolved),
  then the tokens, with lookahead heuristics (`let x =` a definition,
  `type t`, `module M`) so a file that fails to parse is still well
  coloured; the tree's tag wins (`tag_if_not_tagged`).
- **The two levels** (`src/gui/draw_macrolevel.ml`,
  `draw_microlevel.ml`): a file's rectangle, filled with its colour
  (or split among a layer's colours); inside it, the code, in as many
  columns as make the font biggest (`optimal_nb_columns`, 41 characters
  a column), drawn only above a real font size of 2.5 pixels, and only
  when fewer than 2500 rectangles are on screen; a dark background from
  5 pixels. **Semantic zoom**: definitions drawn bigger (a module or a
  type 5 times, a function 3.5, a section comment 5, more if much used:
  `Style.size_font_multiplier_of_categ`), damped as one zooms in, so a
  far view shows the names of what is defined. The line under the mouse
  magnified.
- **Architecture colours**, `src/archi_code/`: a file's role (Main,
  Test, Core, Utils, Ui, Parsing, Network, ThirdParty, AutoGenerated...)
  guessed from its name and directory by an ocamllex lexer. Ours would be
  simpler, from the repository's layout (games, apps, kits, the
  playground, `libs/`'s folders, tests, generated modules).
- **Layers**, `semgrep-pfff-langs/indexing/layer_code/layer_code.mli`: a
  title, kinds with colours, and per file its lines' kinds (micro) and
  its percentages (macro); git's blame as a layer (age, authors).
  The light checker is a layer of that shape.
- **Navigation**: no continuous zoom -- a right click re-roots the map
  on a directory or file, Back, Up, Top; a search by name (completion)
  and by grep, matches in purple; painting in idle time slices.
  **Ours will zoom continuously** (the author: "even powerful
  zooming"), which codemap does not.
- **Fonts**: Cairo's toy text, no glyph cache; speed only from the
  thresholds and lazy painting. Hence our glyph atlas (section 3).
- **Screenshots**: codemap has no flag to write a picture without a
  window (`src/main/Main.ml`): the "cheap first version" (a codemap
  screenshot per program) would need one added to codemap first.

## Design

### 1. `languages/ocaml/`: the parser (library `ocaml`... or `lang_ocaml`)

Parse only, as its README entry will say (the folder's rule is "parsed
and run": this one is a visualizer's).

**ocamllex and ocamlyacc** (decided, the author: "like we did in
~/ix/languages/ml/", whose dune says `(ocamllex Lexer)` and `(ocamlyacc
Parser)`), not a hand parser as `javascript/`'s:
- the grammar to match exists, and is a yacc grammar: ocaml-light's
  `parsing/parser.mly` (1081 lines) and `lexer.mll` (372), OCaml 1.07's,
  its precedences declared once -- the start, extended with what this
  repository uses beyond 1.07 (the table above); OCaml's own parser was
  ocamlyacc up to 4.07;
- OCaml's quirks come with it (`;` and sequences, `match` without an
  `end`, how far `fun` reaches, the dangling `else`): 278k lines must
  parse as the compiler parses them, which a hand parser gets right
  only after many rounds;
- ocaml-light has ocamllex and ocamlyacc (not Menhir): the parser itself
  can one day be compiled by ocaml-light, the goal of the Playground;
- the folder then shows both ways: `javascript/`'s `.mli` says why not
  yacc for JavaScript, `ocaml/`'s why yacc for OCaml. yacc's weakness,
  its errors, matters little here: a file that does not parse is still
  coloured by its tokens (codemap's fallback).
pfff's OCaml grammar (`codemap/semgrep-pfff-langs/languages/ocaml/menhir/
parser_ml.mly`, Menhir, modern OCaml, keeping every token for
highlighting) is the reference for the constructs added.

- `Lexer_ml.mll`: the tokens, comments nested and kept (the header
  comment is what a reader reads first), `{|...|}` and `{id|...|id}`,
  chars vs type variables (`'a'` vs `'a`), every position (line,
  column).
- `Ast_ml`: a small tree, enough to know **what is defined where and what
  is used where**: structure items (`let`, `let rec ... and`, `type`,
  `exception`, `module`, `open`, `external`, `include`), signatures
  (`val`, `type`), expressions and patterns with their spans. Not a full
  typed tree: no types checked.
- `Parser_ml.mly`, and `Parse_ml` around it: the parser; on a syntax error, the error and the position,
  and the file still usable by the lexer alone (codemap's fallback: a
  file that does not parse is still coloured by its tokens).
- `Highlight_ml`: every token's category, codemap's (pfff's
  `Highlight_code`): keyword, comment, string, number, constructor,
  module, a definition (function, value, type, field, constructor, module:
  in bold), a use of a global (another module's: `Push.move`), a local,
  a label, and, of our own, **a capability** (`Cap.*`, `caps`): where
  the authority is, visible at a glance.
- `Light_ml`: the light checker, the table above as rules over the tree.
- Tests: the worked examples of each `.mli`; and **every `.ml`/`.mli` of
  the repository parsed** (a test listing the files that fail, the goal
  zero), and the light checker's counts per library printed, the
  road to ocaml-light measured.

### 2. `libs/graphics/treemap/`: the treemap (library `graphics_treemap`)

Pure, rectangles from a tree of sizes, no Playground (the rule of
`libs/`: it knows rectangles, never shapes). Two algorithms kept side by
side, the teaching style:
- slice-and-dice (Shneiderman, 1992): each level cut the other way;
  simple, and long thin rectangles;
- squarified (Bruls, Huizing and van Wijk, 2000): rows of rectangles as
  square as they can be; codemap's;
with worked examples in the `.mli` and tests. And the nesting: a
directory's border and title, its files inside.

### 3. A faster font: `libs/graphics/font/`'s bitmap font

A treemap of code is thousands of lines on screen. The playground's
`words` is one shape per string, drawn by Cairo (or Hershey's strokes by
the software backend): fine for a menu, too slow for a map. The usual
answer, and what the code view needs:
- a **glyph atlas**: a fixed-width bitmap font (the VGA's 8 by 16, code
  page 437, public domain; TinyTurboPascal's box characters are its
  cousins), each glyph a small alpha mask;
- a file's text **blitted into one `Rgba_image`** with it (`Blit`, the
  software rasterizer's), shown as one `bitmap` shape, cached per
  rectangle and zoom: a whole file is one image, not a thousand shapes;
- and, below a size where glyphs cannot be read, codemap's **semantic
  zoom**: each line drawn as a thin bar, its colour its category and its
  length the line's (SeeSoft's rows), the text appearing as one zooms in.

### 4. The view: `appkits/codemap` and `apps/devtools/TinyCodemap.ml`

- **The whole repository** as one treemap, its size the lines: `games/`
  by genre, `apps/` by category, `gamekits/`, `appkits/`, `playground/`
  (the API, the layers, the ways, the backends), `libs/` (the
  from-scratch libraries: graphics, audio, crypto, languages...), each
  directory a nested box -- the repository's architecture seen at once,
  as codemap shows pfff's. Opened on the whole, or on a folder.
- **One program**: its own files and what it uses (its dune stanza's
  libraries: kits, appkits, layers, `libs/`), the rest of the map dimmed
  -- what tinybox's `s` opens on the chosen program.
- **Zooming**, powerful: the wheel zooms at the mouse, a click zooms
  into a box (animated, `Camera2d`'s way), Escape out; at the bottom, the
  file readable, highlighted, scrolled.
- Layers, codemap's: by category (the default), by age (git, later), and
  **light**: files coloured by how far they are from ocaml-light.
- A search: a name, and its definition and uses lit up across the map.
- TinyCodemap is an app of its own (decided) in `apps/devtools/` (after codemap and SeeSoft,
  in `CATALOG.md`), which tinybox's `s` starts on the chosen program
  (`tinybox TinyCodemap program=TinyMario`), and which alone opens on
  the whole repository (`tinybox TinyCodemap`, or `dir=libs/crypto`).

### 5. The sources, embedded

At build time, like the thumbnails (`plan_launcher.md`): every `.ml`
and `.mli` of the repository (games, apps, kits, the playground,
`libs/`), about 10 MB of text -- acceptable in tinybox's binary, and
what the whole-repository map needs anyway. What a program uses comes
from the dune files (s-expressions: `(names ...)`, `(modules ...)`,
`(libraries ...)`, `(name ...)`, read by a small reader at build time):
a library's directory, a program's libraries, their closure.

## Steps

1. `Lexer_ml.mll` and the highlighter over tokens alone; a code view in
   the menu (`s`: the program's own file, coloured, scrolled) -- already
   useful, and the font's first user.
2. The bitmap font and the blitted text (the faster font engine),
   measured against `words`.
3. `Ast_ml`, `Parser_ml.mly` (from ocaml-light's); the test parsing every file of the repository;
   definitions and uses in the highlighter.
4. `Light_ml`, the checker, and its counts per library.
5. `libs/graphics/treemap/`, both algorithms, tested.
6. TinyCodemap: the whole repository's map and a program's, the
   semantic zoom, the layers, the search; the sources and the dune
   files' dependencies embedded; tinybox's `s` starting it.
7. The cheap version, if wanted earlier: the real codemap's screenshot
   per program. codemap cannot write one without a window today: a
   batch PNG output (Cairo's `write_to_png` of its main map) added to
   codemap first, in its own repository.
8. Optional, later (the author, 2026-09-27: "for later, let's keep with
   what we have for now"): codemap's own fonts. Codemap draws its code
   in **serif**, proportional (`Style.font_text`), each line token after
   token with Cairo's `show_text`, so the text flows (hence its 41
   characters a column, not 80); its definitions *inline* bigger, 5 for
   a module or a type, 3.5 a function, 3 a global, more if much used
   (`Style.size_font_multiplier_of_categ`), damped as one zooms in; its
   directories' and files' labels serif **bold**, size and alpha by
   depth (`draw_labels.ml`: 0.1 of the map at depth 1, 0.05, 0.03...);
   its overlays serif. The Playground cannot say that today: `words`
   has one family (`words_font_family`, sans-serif) and nothing places
   coloured runs one after the other (no measuring). So, a shared change
   (plan it, have it reviewed, then):
   - the Playground: flowing text,
     `run : ?font:font -> ?bold:bool -> ?size:number -> color -> string -> run`
     and `runs : run list -> shape` (left-aligned at the origin, each run
     after the last; `type font = Sans | Serif | Mono`), `words`
     unchanged. Cairo: `select_font_face` per run, `show_text` advances;
     the web: SVG `<text>` with `<tspan>`s; the software rasterizer and
     OpenGL's HUD (`Hud_render`): Hershey's widths advance, serif its
     Times-like faces if there;
   - the map and the view: a line one `runs` in serif, the definitions'
     multipliers inline, the labels serif bold by codemap's depth table,
     the overlays serif, 41 characters a column.
   To decide then: the file view in serif too (codemap's), or kept on a
   fixed grid, the indentation exact. An alternative to step 2's bitmap
   font, or on top of it (the map's far text in serif, the view's in the
   bitmap font).

## Progress

2026-09-27: steps 1, 5 and a first 6, laid out differently from the
plan at the author's request (a `launcher/codemap/` imitating codemap,
the treemap in it, a program's several files, zooming smoothly):
- `languages/ocaml/` (library `lang_ocaml`): `Token_ml`,
  `Lexer_ml.mll`, `Highlight_ml` (tokens only); and, language
  independent (the author: C may come), `libs/program_analysis/highlight/`'s
  `Highlight_code`. Every `.ml`/`.mli` of the repository lexed: 2.1M
  tokens, no error, 1.5 s with the highlighting.
- `launcher/codemap/` (library `tinybox_codemap`): `Treemap` (squarified
  and slice-and-dice, pure), `Code_file`, `Code_map` (the treemap under
  an eased camera: the wheel at the mouse, drag, click to fly in, the
  code turning into text up close, definitions placed greedily without
  overlap from afar), `Code_view` (a file, SeeSoft's overview beside
  it), `Codemap` (which files: the program's and the modules it names,
  transitively, the platforms left out). tinybox's `s`; `w` the whole
  repository. The code is drawn with `words`, a shape per character:
  the font (step 2) is still to come.

Then, the same day: step 2, the font, the VGA's 8 by 16 (the author's
choice, "we can always offer the option for the xterm one later"):
`libs/graphics/font/Vga_font`, code page 437 from the Linux console's
u_vga16 fonts; the map paints the glyphs in its one image (a pixel samples
its character's glyph once a line is 6 pixels high), the file view is a
1:1 page. And the map's scopes (the author: "by default a simpler view
with just the program and really the related necessary code"): its own
code by default (its folder's and the kits' modules), then, `w`, all it
uses, then the whole repository; each folder with files of its own has
its path on a tab, for finding it later in the repository.

And tinybox itself went wide (the author: "start the tinybox in a wide
setting ... proportional to modern screen"): `run_app ~screen` (a shared
change, reviewed first), the menu at 1778 by 1000, the grid of 5 columns
on the left, on the right the live preview, the catalogue's text beside
it, and under both its code: the program's own code's map,
`Codemap.preview`, a click (or `s`) opening the full map, which fills
the wide screen (`Code_map`'s area now a parameter).

## Open questions for the author

- The font: decided, the VGA's 8 by 16; xterm's misc-fixed maybe
  later, as an option.
- The modules' names: pfff's style (`Lexer_ml`, `Parser_ml`,
  `Highlight_ml`, as codemap's) was chosen here; the library's name
  (`ocaml` clashes with nothing in dune, but reads oddly).
