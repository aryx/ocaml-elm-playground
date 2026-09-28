# Plan: factorizing the Tiny games and apps, v1

## Context

The question: can the ~50k lines of `games/`, `games2.5d/`, `games3d/`,
`apps/` and `examples*/` shrink further, **without losing a feature and
without trading clarity for brevity**? And are the Playground's
abstractions (2D, 3D, AI, audio, physics, GUI) the right ones, as close
to Evan's style as they could be?

A survey of the whole codebase (2026-09-22, five read-only passes: 2D
games, 3D and 2.5D games, apps and GUI, the library APIs, xix's
`lib_core/commons` and ppx) answered: **the games are already well
factored**. Scene2d (80 games), Tilemap (24), Sprite (22), Camera2d
(17), Audio (32) and the gamekits carry most of what is shared, and the
usual suspects -- `take`/`drop`/`nth`/`shuffle`/`zip`, `clamp`/`lerp`,
enum printers -- turned out *not* to be duplicated: where a name like
`take` or `pick` repeats, the code behind it is unrelated.

What is left is about **1,500-2,000 lines, 3-4%**. The better reason to
do it is consistency, and a few bugs the duplication causes (phase 1).

| where | size | lines | the biggest duplicate |
|---|---|---|---|
| `games/` | 57 files | 23.7k | `text` helper, pure random, explosions |
| `games3d/` | 27 | 12.8k | rules copied from the 2.5D twin |
| `apps/` | 19 | 6.5k | key/click edges, page drawing, menu bar |
| `games2.5d/` | 13 | 5.8k | (the twins' other half) |
| `examples/`, `examples3d/` | 61 | 6.8k | `space_was_down` |

## Principles

The README's eight, and for this plan in particular:

- **Clarity first.** A helper goes in only if the call reads at least as
  well as the code it replaces. A title-screen helper that saves a line
  per game but hides what the screen shows is not a win (see phase 7).
- **The trick stays in the game.** `games2.5d/README.md`: each 2.5D
  game writes out its original's projection trick on purpose. Only
  scaffolding around the trick is shared, never the trick.
- **Deliberate duplicates stay**: the two code paths behind
  `physics=engine` (TinyMario64, TinyMarbleMadness, StarCollector3d,
  TinyMinecraft), the games' own `frames` counters (keep `update_game`
  pure over `game`, and `Scene2d.go` resets its own).
- **Small diffs**: each phase is one library addition, then a mechanical
  sweep, with the game headers' "uses" lists updated.
- **Pixels unchanged**: every phase except the doc fixes should leave
  all golden frames identical; `make test-golden-all` at the end of each.

## Design, and what was decided against

### Not ppx_deriving

About 24 variant-to-string functions exist, but only ~2 are the
constructor's name (`TinyCivilization.ml:237`), ~6 are its name in
upper case, and ~16 are custom text ("Bronze Working", `Bucket ->
"Fill"`). The 13 "all constructors" lists are often in a deliberate
order, and ppx_deriving has no `enum`-as-list anyway. There are no
hand-written equality or compare functions. So about 8 functions for a
build dependency every user of the library pays -- the same reason
`related-work/notes_inspect_related_work.md` gave for not using it in
the debugger. For dumping a model while debugging, xix's `Dumper.dump`
(through `Obj`, no ppx) is the better tool.

### Not a big xix-style `commons`

The stdlib already covers what the games use: `List.init` 280 uses,
`concat_map` 202, `filter_map` 149, `filteri` 38. xix's commons is
smaller than remembered anyway (`Common.mli`, 143 lines: `spf`, `|||`,
`List_.take`/`exclude`/`span`/`enum`, `Assoc.group_by`...). A *small*
`core/List_.ml` is worth it, phase 3.

### Not sharing the 2.5D projections

The pinhole division is in 7 games2.5d files, `to_eye` in 2
(`TinyBattlezone.ml:275`, `TinyDescent.ml:174`), near-plane clipping in
2. They are the point of those games (principle above). Only the thick
2D line helper (7 copies) is scaffolding.

## Phasing

### Phase 1: key and click *edges* in `computer`

The largest win, and it fixes bugs. Edge detection is solved today in
seven places: `Scene2d.pressed`, `gui/Immediate.ml:29,41`,
`gui/Retained.ml:45`, `gui/Mvu.ml:40`, 13 apps with `was : string list;
was_down : bool` in their model (17 local `pressed`, 26 `{ m with was
= now; was_down = ... }`), 16 games with a `prev_mdown`, 3 Physics
examples with `space_was_down`. The apps get it wrong on their early
returns: `TinyExcel.ml:196` sets `was_down` but not `was`, TinyWord sets
`was` (`:306`) and `was_down` (`:382`) in different places. And 18
games wrap `Set_.mem "x" k.keys` in their own `letter`/`key`.

The platform already sees the key-change and button events, and
`mclick` is already a transient cleared on the next tick, so:

```ocaml
type keyboard = { ...; pressed : string Set_.t }  (* down this frame, up the one before *)
type mouse    = { ...; mpress : bool }            (* mclick is the release *)
val key : string -> keyboard -> bool              (* held *)
val key_pressed : string -> keyboard -> bool      (* went down *)
```

Then `Scene2d.pressed` becomes a one-liner over it (kept, 79 uses, with
its "not pressed in the new scene" guarantee of `go` checked), the
three `gui/` loops read it, and the sweep removes the apps' two fields.
Both backends (native loop, web) fill it; a unit test with a scripted
key (`-script`) checks "held 3 frames = pressed once".

~150-200 lines, and the two bugs.

### Phase 2: `text`, `axis`, `radians` in Playground; the doc fixes

- `let text color size s = words color s |> scale size` is in **87
  games** (51 byte-identical). `val text : color -> number -> string ->
  shape` in `Playground.mli`. The 2 white-only variants
  (`TinyCameltry.ml`, `TinyPong.ml`) keep theirs.
- `let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.` in 21
  files: `val axis : bool -> bool -> number`, beside `to_x`/`to_y`.
- `pi /. 180.` written 56 times inline, ~10 local `radians`:
  `Basics.degrees_to_radians` exists but nobody finds it; a short
  `radians`/`degrees` pair.
- Hand-written clamps (~12 places, `TinyDefender.ml:264`,
  `TinyXCOM.ml:194`, `TinyDiablo.ml:301`...) and 4 local `clamp`s
  (`Car.ml:40`, `AiChess.ml:528`, `TinyBoomerangFu.ml:251`) switch to
  `Basics.clamp`.
- **`Playground.mli`'s examples use Elm names**: `moveLeft`,
  `lightBlue`, `computer.mouse.x`, `to_x` documented with `left` where
  the field is `kleft`, the `move_x` example with `wave`'s arguments in
  the wrong order. Fix them.
- **Colors**: only 4 of the 21 Tango light/dark colors are exported,
  though `core/Color.ml` has them all (103 files fall back to `rgb`).
  Export them (and decide camelCase vs snake_case: every function is
  snake_case, the colors are camelCase).

~200 lines.

### Phase 3: `core/List_.ml` and `spf`

Only what is actually repeated:

- `take`, `drop` (`Tetris.ml:64-73`, whose comment asks for a
  `core/List_.ml`; `TinyTeardown.ml:492`; `List.filteri (fun i _ -> i <
  n)` 6 times in apps and examples),
- `update_nth`, `remove_nth` (4 copies each: `TinyFrameMaker.ml:404`,
  `TinyOffice.ml:488,591`, `Gui7Circles.ml:142`),
- `sum`, `count` (`Tetris.ml`, `TinySimCity.ml:152`, `Sheet.ml:134`;
  `List.length (List.filter ...)` 24 times),
- `index_of` on strings, into `gui/Text` (4 copies in apps, 3 of them
  without the end-of-string bound check: `TinyBravo.ml:92`,
  `TinyWord.ml:100`, `TinyFrameMaker.ml:211`),
- `spf` in `Basics` (`Printf.sprintf` 460 times; 3 backends already
  define it).

~60 lines; mostly clarity.

### Phase 4: a pure `Random`, the seed in the model

The LCG `1103515245` is written 11 times (`TinyRogue.ml:67`,
`TinyMissileCommand.ml:97`, `TinyPuzzleBobble.ml:132`,
`TinyZelda.ml:156`, `TinyRobotron.ml:153`, inline in TinyFinalFight,
TinyStreetFighter, TinyGalaga, TinySpacewar...), `roll seed n -> (n,
seed')` 3 times, `Hashtbl.hash` used as dice (TinyXCOM, TinyCivilization),
and 7 games on the global `Random` with `Random.init` code to keep the
golden tests deterministic. Elm's API, in `core/`:

```ocaml
module Random : sig
  type seed
  type 'a gen
  val seed : int -> seed
  val int : int -> int -> int gen
  val float : number -> number -> number gen
  val pick : 'a list -> 'a gen
  val map : ('a -> 'b) -> 'a gen -> 'b gen
  val list : int -> 'a gen -> 'a list gen
  val step : 'a gen -> seed -> 'a * seed
end
```

Keeping each game's exact sequence (the goldens) means the generator
*is* that LCG; games whose goldens depend on another generator
(`Chase.next_random` is Pac-Man's own, on purpose) keep theirs.

~40 lines, and principle 5 stated once instead of in each game.

### Phase 5: `Vec2` and `Vec3` in the Playground API

- `graphics/2d/geometry/Vec2` is already linked into `elm_playground`,
  but only `TinySoldat.ml` uses it; `Float.hypot (a - b) (c - d)` is
  written 54 times, and `near r a b` defined 5 times with 3 different
  argument orders (`TinyGalaga.ml:238`, `TinyRobotron.ml:251`,
  `TinyGauntlet2.ml:405`, `TinyMarioWorld.ml:431`, `TinyRick.ml:256`).
  Add `distance`; document it in `Playground.mli`.
- `Vec3` is in the private `graphics_3d_geometry`, never mentioned by
  `Playground3d`'s docs, so vector math is redone in `TinyElite(3d)`
  (a `{x;y;z}` record), `TinyDescent(3d)`, `TinyQuake.ml:103`,
  `TinyMarbleMadness.ml:249`, `TinyBattlezone.ml:68` -- and in the
  library itself: `Sixdof.ml:20`, `Character3d.ml:31`, `Logo3d.ml:66`.
  Add `neg`, `lerp`, `distance`; document it. games2.5d can depend on
  it: it depends on nothing, so no 3D engine comes with it.
- Ai/Steering's private `vec` becomes `Vec2`.

~80 lines.

### Phase 6: the 2.5D/3D twins share their rules (ask first)

The largest single item, ~500 lines. Each 3D twin copies its 2.5D
game's rules, marked `(* coupling: ... a copy of ... *)`:

| pair | lines in common |
|---|---|
| TinyElite / TinyElite3d | ~193 (vectors, hulls, model, `update`) |
| TinyBattlezone / TinyBattlezone3d | ~115 |
| TinyDescent / TinyDescent3d | ~92 |
| TinyComanche / TinyComanche3d | ~62 |
| TinyWolfenstein / TinyWolfenstein3d | ~43 (the map) |
| TinyDoom / TinyDoom3d | ~40 |

The rules (model, `update`) move to a module depending only on
Playground, like the kits already do: into the existing kits where
there is one (Segments for Descent, Heightmap for Comanche, Sectors for
Doom), new ones otherwise (Elite, Battlezone). Each game keeps its
`view` -- which is then exactly the difference the pair exists to show.
New modules and a new kit directory: **to be agreed on before
starting**, one pair at a time, Elite first.

### Phase 7: the smaller helpers

Each on its own, each only if the calls read better:

| what | where | copies | lines |
|---|---|---|---|
| first-person look: `mouse_look`, `look_dir ~yaw ~pitch`, strafe (`Camera3d.mli:33` lists it as an exercise) | Camera3d | 9 (HL2, Portal, Teardown, Minecraft, Quake, Doom3d...) | ~70 |
| page drawing: glyphs, selection, caret (TinyBravo:207 ~ TinyWord:394) | new `apps/Page_view` | 8 | ~90 |
| menu bar: `menu_box` (its width drifts 90-110), the same fold over `menus`, `File_menu.status` | `Gui.menu_bar` or `File_menu.bar` | 8 | ~80 |
| tool palette (TinyMacDraw's and TinyMacPaint's loops are byte-identical) | `Gui.palette_in` | 4 | ~50 |
| a typed `'a Grid` (inside, neighbours, cell/screen, `dir`, `delta`, `opposite`); Tilemap is `char Grid` | playground/ | 3 whole + 9 neighbour lists; `dir` twice (Grid_move, Lightcycles) | ~40 |
| explosions as `(x, y, age)`: aging and drawing | `gamekits/shmup/Blasts` | 9 | ~25 |
| `hud` over a list (`List.map hud` in 24 games), `place pose shape` (8), a blob-shadow quad (6), `at x y h` map-to-3D (4) | Playground3d, Camera3d | | ~60 |
| `darken`, `mix`, `fog` on colors | Color | 7 | ~35 |
| thick 2D segment | Playground | 7 | ~25 |
| bitmap to shapes with its cache, `dot_at` | new `apps/Bitmap_view` | 3 | ~30 |
| `Rich.key` (typed, Enter, Backspace, arrows; edit vs move for undo); single-line editing through `gui/Text.edit` (4 of 5 copies cut Backspace by a byte, breaking UTF-8) | appkits, gui | 4 + 5 | ~50 |
| positive modulo, `angle_diff` | Basics | 7 | ~8 |

Considered and left for later: a standard title/game-over screen
(`Scene2d.title ~name ~help`, ~70 games, ~100 lines) -- each game's
screen is short and says what it shows; a helper would hide it.

## Not about size, recorded here

Found on the way; each its own decision:

- **Two effects in views and updates**: `Audio.play` is called from
  `update` (Audio.mli admits it), `Gui` keeps hidden per-frame state and
  `Gui.draw ()` ends the frame (12 calls). Evan's answer would be
  `update` returning `sound list` (as elm-audio does), and `gui/Mvu`
  for the apps (no app uses it). A design change, not a saving.
- **No `dt` in `computer`**: `/. 60.` written 32 times; timers count
  frames. Fine at a fixed 60 Hz; a `dt` would be the honest version.
- **Ai's verbs** mix moods (`seek`, `flee` / `escaping`, `wandering`,
  `flocking`), and Steering names two of them `pursue`/`evade`. No game
  uses `Ai` yet (TinyBoomerangFu's header says it would want it).
- **3D API workarounds**: 12 games turn back-face culling off globally
  so `Camera3d.sky` (a plane seen from below) is drawn; no per-shape
  unlit color, so 4 games turn lighting off for the whole scene; no fog;
  no `cylinder`/`cone`.
- `physics/{collision,gravity,mechanics}` are empty directories.
- `dune-project` says `ocaml >= 4.07`, but `playground/` uses
  `List.concat_map`/`filteri`/`find_map` (4.10/4.11).

## Verification

- `make test`, and `make test-golden-all` at the end of every phase:
  no golden frame changes, except where a phase says so.
- Phase 1: a unit test of the edges (held 3 frames = pressed once;
  `Scene2d.go` then no `pressed` in the new scene), and the two app
  bugs reproduced with `-script` before, gone after.
- Phase 4: the goldens of the games switched to `Random` unchanged
  (same generator, same sequence).
- The line counts, before and after each phase, in Status:
  `cat games*/*.ml apps/*.ml examples*/*.ml | wc -l`.

## Status

- 2026-09-22: survey done, plan written. Nothing started. Baseline:
  games 23,708, games2.5d 5,832, games3d 12,833, apps 6,497, examples
  4,337, examples3d 2,504 lines.
