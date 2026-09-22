# Plan: one tree for 2D and 3D

## Context

The top level splits the repository by dimension: `playground/` and
`playground3d/`, `examples/` and `examples3d/`, `games/`, `games2.5d/`
and `games3d/`. The goal is to remove that split from the top level:
2D vs 3D should only show up *inside* a directory, the way
`graphics/{2d,3d}` and `tests/{2d,3d}` already do it.

Three moves, then one merge of the docs, decided on 2026-09-22:

1. `playground3d/` into `playground/`, merging the **directories** but
   not the **libraries**: the eleven libraries and their opam packages
   stay as they are.
2. `examples3d/` into `examples/`.
3. `games/`, `games2.5d/`, `games3d/` into `games/<genre>/`, one
   directory per section of `CATALOG.md`, so that twins sit side by side
   (TinyDoom and TinyDoom3d, TinyDDR / TinyGuitarHero / TinyRockBand).

4. `README-3d.md` into `README.md`.

`apps/` is not touched.

## Principles

- **Directories merge, libraries don't.** Merging `elm_playground` and
  `elm_playground_3d` into one virtual library per backend would lose:
  choosing the 2D and 3D backends separately (the OpenGL games use
  `elm_playground_software` for their HUD today), a 2D install without
  OpenGL/tgls, and having two 3D web backends (SVG and WebGL). Out of
  scope "for now".
- **`git mv` only, contents unchanged**, so that `git log --follow` and
  `git blame` still work. The only edits in the moved files are path
  strings (e.g. `"examples3d/checker.png"`).
- **Each step is its own commit on a clean tree**, with nothing else in
  it: another session often shares this tree, and a move that size
  conflicts with anything in flight. Build and `make test` pass after
  each step. The golden frames are **not** re-approved: a move must not
  change a pixel.
- **No website regeneration.** The Makefile's paths get updated, but
  `docs/` is not rebuilt as part of this plan (the published URLs, e.g.
  `docs/games3d/webgl/`, change the next time it is).

## Can several stanzas share a directory?

Yes, and that is what makes the directory merge possible without a
library merge: each stanza says which modules it owns with
`(modules ...)`. The constraint is that two modules with the **same
name** can't be in the same directory. Checked for every merged
directory:

| directory | 2D modules | 3D modules | clash? |
|---|---|---|---|
| `playground/` | `Playground`, `Physics`, `Logo`, `Camera2d`, ... | `Playground3d`, `Physics3d`, `Logo3d`, `Camera3d`, ... | no |
| `native_common/` | `Native_loop_2d`, `Input_script`, `Store` | `Native_loop_3d` | no |
| `native/` | `Playground_platform`, `Shape_render_native`, `Image_native` | `Playground3d_platform` (OpenGL) | no |
| `software/` | `Playground_platform`, `Shape_render_software`, `Help_overlay` | `Playground3d_platform`, `Shape3d_render_software` | no |
| `web/` | `Playground_platform`, `Web_store` | `Playground3d_platform` (WebGL) | no |
| (SVG) | | `Playground3d_platform` (SVG) | **yes, with WebGL** |

An implementation's module must be named after the virtual module, so
the SVG and WebGL backends can't share a directory. Decision: `web/`
gets WebGL (the 3D backend every game's web page uses), and the SVG
one gets its own `svg/`.

The same holds for the programs: every 3D example and game already
ends in `3d` or has a name no 2D program has (checked below, step 3).

## Step 1: `playground3d/` into `playground/`

```
playground/               elm_playground + elm_playground_3d
playground/native_common/ elm_playground_native_common + elm_playground_3d_native_common
playground/native/        elm_playground_native (Cairo) + elm_playground_3d_opengl
playground/software/      elm_playground_software + elm_playground_3d_software
playground/web/           elm_playground_web (vdom) + elm_playground_3d_webgl
playground/svg/           elm_playground_3d_web (3D compiled to 2D shapes, drawn as SVG)
playground/tests/         unchanged (already tests both)
```

- `playground/dune`: the 3D stanza lists its nine modules
  (`Playground3d Playground3d_platform Camera3d Character3d Gpu_scene
  Logo3d Physics3d Portal3d Ragdoll3d`), the 2D one takes
  `(modules :standard \ <those>)`. A new 3D module forgotten in the
  list ends up in the 2D library and fails loudly (it needs
  `Playground3d`), so the list can't silently rot.
- The backend directories: explicit `(modules ...)` on both stanzas
  (they are short). The WebGL stanza keeps its own
  `(preprocess (pps js_of_ocaml-ppx))`.
- The library `elm_playground_3d_web` keeps its name, although it now
  lives in `svg/`. Renaming it `elm_playground_3d_svg` changes an
  opam package: later, separately, if at all.
- Update: every dune file that says "see playground3d/...", the
  `playground3d/` paths in the `.ml`/`.mli` comments, `CLAUDE.md`,
  `README-3d.md`, the Makefile's comments.

## Step 2: `examples3d/` into `examples/`

```
examples/          2D native (Cairo) stanza + 3D native (OpenGL) stanza
examples/software/ 2D + 3D software stanzas
examples/web/      2D (vdom) + 3D (WebGL, today's examples3d/webgl/) stanzas, all the .html pages
examples/svg/      3D SVG (today's examples3d/web/), its .html pages
examples/gui4/     unchanged
```

- The 21 3D examples all end in `3d`: no clash with the 2D ones.
- `examples/software/dune` uses `(copy_files ../*.ml)`: it now copies
  the 3D sources too, which is fine since the directory also has the 3D
  stanza, but both stanzas need `(modules ...)`. `examples/web/dune`
  lists its files one by one already.
- The five 3D examples that are not in the OpenGL list today
  (`Cube3d`, `InteractiveCube3d`, `PaintersAlgorithmFail3d`,
  `FloatingCity3d`, `Corridor3d`, see `examples3d/dune`'s trailing
  comment) stay out of it: fixing that is not this plan's job. With
  explicit `(modules ...)` on the top-level stanzas, a source that no
  stanza lists is simply not built there.
- **The texture path**: `TexturedCube3d.ml` loads
  `"examples3d/checker.png"`, a path relative to the repository root,
  which the WebGL page resolves relative to itself, hence today's
  `examples3d/webgl/examples3d/dune` copying the image one level down.
  Becomes `"examples/checker.png"` and `examples/web/examples/dune`.
  Check that the software backend (run from the root) and the WebGL page
  both still find it.
- Update: `tests/3d/dune` and `tests/3d/Golden_frames.ml`
  (`examples3d/software/X` becomes `examples/software/X`), the Makefile
  (`js`, `website`), `CLAUDE.md`, `README-3d.md` (every `dune exec
  examples3d/...` line).

## Step 3: `games/` by genre

### The directories

One per section of `CATALOG.md`, 97 games in 13 directories:

| directory | catalogue section | 2D | 2.5D | 3D |
|---|---|---|---|---|
| `games/shmup/` | Shoot 'em up | 9 | 1 | 1 |
| `games/fighting/` | Beat 'em up and fighting | 2 | | 2 |
| `games/platform/` | Platform | 9 | | 1 |
| `games/arcade/` | Maze and arcade classics | 10 | | 3 |
| `games/puzzle/` | Puzzle and board games | 10 | | 3 |
| `games/adventure/` | Action-adventure and horror | 1 | | 2 |
| `games/rpg/` | Role-playing and dungeons | 2 | 2 | 1 |
| `games/fps/` | First-person | | 3 | 7 |
| `games/flight/` | Flight and space | 1 | 3 | 3 |
| `games/racing/` | Racing | 1 | 2 | 2 |
| `games/sports/` | Sports and tables | 4 | 1 | 1 |
| `games/strategy/` | Strategy and simulation | 7 | | |
| `games/rhythm/` | Rhythm | 1 | 1 | 1 |

`games/template.ml` stays at `games/`. `games2.5d/README.md` (the
tricks compared) becomes `games/README-2.5d.md`; the `grep "the trick of
this game" games2.5d/*.ml` it and `CLAUDE.md` mention becomes a
`grep -r` over `games/`. The textures go with their game:
`minecraft.png` to `games/fps/`, `tomb.png` to `games/adventure/`.

No name clash between the three directories today (checked: the 3D
twins end in `3d` or are named after another original, e.g.
`TinyMarioKart64`, `TinyPortal` vs `TinyPortal2D`).

### The dune files

Backends stay **central**, as today, rather than a `software/`, `web/`
per genre (which would be 13 x 3 more dune files):

```
games/<genre>/dune   native: a 2D stanza (2D + 2.5D, Cairo) and a 3D stanza (OpenGL),
                     each with (modules ...), each only as needed
games/software/dune  (copy_files ../<genre>/X.ml) for all; a 2D stanza + a 3D stanza
games/web/dune       (copy_files ../<genre>/X.ml) for all; a 2D (vdom) stanza + a 3D (WebGL)
                     stanza; all the .html pages
```

The kits a genre needs move from the three current `(libraries ...)`
lists to the genre's stanzas, keeping their comments (e.g.
`kit_rhythm ; TinyDDR's judging and clock, shared with TinyRockBand`).
`TinyMinecraft`'s own stanza (it uses no kit) and the two base64
texture rules move to `games/fps/dune` and `games/adventure/dune`.

### What else changes

- **`CATALOG.md`**: each link becomes `games/<genre>/<Name>.ml`. The
  **Dir** column already says 2D / 2.5D / 3D (not the directory), so it
  stays. The introduction's conventions become: run online
  `games/web/<Name>.html` for every game (the `games3d/webgl/` special
  case disappears), run natively `dune exec games/<genre>/<Name>.exe`,
  screenshot `tests/2d/` for 2D and 2.5D, `tests/3d/` for 3D.
- **`tests/catalog/Unit_catalog.ml`**: `dirs` lists the 13 genre
  directories, with `games/web` as the web page directory for all; the
  golden directory is chosen per program (from the stanza it is in, or
  from the catalogue's Dir column) instead of per directory.
- **`tests/2d/`, `tests/3d/`**: the `%{project_root}/games*/software/X.exe`
  deps and the paths in `Golden_frames.ml` become
  `games/software/X.exe`. The split by renderer stays.
- **`tests/games/dune`**: its `%{project_root}/games/TinySokoban.ml`,
  `games2.5d/TinyDoom.ml`, `games3d/minecraft.png` deps.
- The Makefile (`js`: `dune build games/web`, once instead of three
  times), `CLAUDE.md` (the whole gamekits paragraph names games by their
  directory), the gamekits' dune comments and `.mli`s
  (`games3d/TinyStarFox.ml` and the like), `README.md`, `README-3d.md`.

### Order

1. **Rhythm first**, as the trial: TinyDDR, TinyGuitarHero and TinyRockBand
   are one per dimension and all share `kit_rhythm`, so this one genre
   exercises both native stanzas, the software and web backends,
   `tests/2d`, `tests/3d` and the catalogue test. Meanwhile the flat
   directories keep working (the catalogue test lists both kinds).
2. The other twelve genres, one commit each, or all together once rhythm
   has shown the pattern.
3. Remove `games2.5d/`, `games3d/` once they are empty; update
   `CLAUDE.md`, `README*.md`.

## Step 4: `README-3d.md` into `README.md`

Last, once the paths have stopped moving: steps 1-3 update both READMEs'
paths in passing, this step only merges them. `README.md` (215 lines)
is the 2D playground's introduction, `README-3d.md` (225 lines) the 3D
one's; with one tree, a reader should find one entry point.

The 2D walkthrough stays first (it is the Elm playground's, and the
simplest way in); the 3D parts fold into it rather than being appended
as a second README:

| `README.md` today | + from `README-3d.md` |
|---|---|
| intro, Documentation | the credits (elm-3d-playground, ...) |
| Features | one sentence: the same API style in 3D |
| Install | the `elm_playground_3d*` packages |
| Simple native / web application | |
| Parameters (flags) | the WebGL pages' URL parameters |
| | **new: Backends**, one table for 2D and 3D, laid out like `playground/` after step 1 (native: Cairo / OpenGL, software, web: vdom / WebGL, svg) |
| | **new: 3D**: "Try it", "A minimal example", "Choosing how an app is drawn", "Current limitations" |
| | **new: Debug keys** (`-debug-keys`, `-fixed-time`, `-script` ...), which apply to both software backends, not just the 3D one |
| Next steps | `examples/`, `games/<genre>/`, `CATALOG.md` |

About 400 lines; the long lists (every debug key, every limitation) can
be shortened to pointers into the `.mli`s if it reads too long.

Update what links to `README-3d.md`: `README.md` itself,
`examples3d/dune`'s comment (by then in `examples/dune`), `CLAUDE.md`,
and the open notes (`notes_3d.md`, `notes_playground3d_related_work.md`,
`plan_3d_remaining.md`, `plan_raytracing_teaching.md`).

## Path references

About 115 `.ml`/`.mli` files and 30 notes name `playground3d/`,
`examples3d/`, `games3d/` or `games2.5d/`, mostly in comments.
Updated with the step that moves the file they name: the dune files,
source comments, `CLAUDE.md`, `README*.md`, `CATALOG.md`,
`docs/claude_notes/tutorials/` and the plans that are still open.
Not updated: `docs/claude_notes/plans/done/` (history) and `docs/`'s
generated site.

## Open questions

- The genre directory names above (`shmup`, `fps`, ... or longer ones).
- `examples/`: flat as above, or later grouped by topic (the prefixes
  are already there: `Physics*`, `Gui*`, `Ai*`, `Audio*`, `Logo*`)?
  Not needed for the 2D/3D goal.
- Renaming `elm_playground_3d_web` to `elm_playground_3d_svg`.
- The five 3D examples missing from the OpenGL stanza: a separate fix,
  easier to see once all 3D examples sit in `examples/dune`.
