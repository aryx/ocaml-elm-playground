# Plan: the teaching languages, made layers of the playground

## Context

Three of the modules in `playground/` are not layers a game calls, they
are *other ways of writing the whole program*, borrowed from systems
that were built to teach programming:

| Module | After | What a program written on it is |
| --- | --- | --- |
| `Logo.mli` | Logo (Seymour Papert, Wally Feurzeig, Cynthia Solomon, BBN, 1967) | a `command list`: forward, right, repeat |
| `Bigbang.mli` | the world programs of *How to Design Programs* (Felleisen, Findler, Flatt, Krishnamurthi, 2001) | a world value, `to_draw`, `on_tick`, `on_key` |
| `Puzzlescript.mli` | PuzzleScript (Stephen Lavelle, 2013) | things on layers, a map, and rewrite rules |

Each of them builds a `Playground.app` for you, so a program written on
one has no `update` and no `view` of its own. That is the line between
`playground/` and `gamekits/`, and it is worth keeping sharp:

- a **kit** module is something a game *calls* from its own update or
  view (`Push`, `Undo`, `Shots`, `Road`); the game keeps the
  Model-View-Update shape and stays in charge;
- a **way of programming** takes the shape away. You hand it data and
  it runs the program. `examples/PuzzleScriptSokoban.ml` has no
  functions in it at all.

The pull of this is that each one is a different answer to "what is a
program?", which is the question the systems below were built to ask
children, and they can be read side by side, in one library, each in a
few hundred lines.

### Three homes for a language

A teaching language can be written in the repository in three ways.
Which one depends on who writes the program, and in what language:

| Where | Who programs, in what | Example |
| --- | --- | --- |
| `playground/` | the OCaml programmer, in OCaml: the language is an API (a `command list`, records, rules as data) | `Logo`, `Bigbang`, `Puzzlescript` |
| `games/programming/` | the *player*, in a small language the game parses, typed into the game's editor; the levels are problems and you win when the program solves one | `TinyCoreWar` (Redcode) |
| `apps/devtools/` | the user, in a language, with the tools around it: edit, run, the error on its line, a REPL (Turbo Pascal, a notebook); the language is the tool's subject, not a puzzle | none yet (its dune file names this plan) |

One language can live in more than one place: the world and the
interpreter are written once, and the playground layer, the game and
the environment each put a different surface on them. The text
languages already here are `libs/languages/formula`'s `Formula`,
`libs/languages/hypertalk` and TinyCoreWar's Redcode assembler, so a parser
with its errors shown on their lines is a solved problem to copy, not
a new one.

## Done

- **Logo** (`Logo.mli`, `examples/LogoFractals.ml`): the
  turtle, and recursion as drawing. `Logo3d` too.
- **Big bang** (`Bigbang.mli`, `examples/BigBangRocket.ml`,
  `examples/BigBangWorm.ml`): images that know their size and compose,
  events rather than polled state, HtDP's top-left coordinates. Elm's
  architecture before Elm.
- **PuzzleScript** (`Puzzlescript.mli`,
  `examples/PuzzleScriptSokoban.ml`, `examples/PuzzleScriptBoulders.ml`):
  a game as a map and a few rules, where Sokoban is one rule. Its .mli
  carries the lineage this whole plan sits in: Papert, then KidSim /
  Cocoa / Stagecast Creator (David Canfield Smith, Allen Cypher, Kurt
  Schmucker, 1994, where a child *shows* the machine a before and an
  after), Repenning's AgentSheets (1991 on), PuzzleScript, and TileCode
  (Thomas Ball, Stefania Druga et al., 2020).

## Candidates

Roughly in the order they look worth doing.

### 1. Turtles and patches: StarLogo, NetLogo

Mitchel Resnick's StarLogo (MIT Media Lab, early 1990s; his book
*Turtles, Termites and Traffic Jams*, 1994) took Papert's one turtle and
made it thousands, and gave the grid itself a mind: a **patch** is a
cell of the world that has state and runs code too. Uri Wilensky's
NetLogo (1999, Northwestern) is its successor, and the model library it
ships is the argument for the whole idea -- ants, flocking, fire, the
spread of disease, Schelling's segregation -- each a page of code whose
point is that nobody is in charge and the pattern appears anyway.

    ask turtles [ forward 1 right random 30 ]
    ask patches [ set pheromone pheromone * 0.99 ]

- **What it would be here**: `playground/Turtles.mli`, a natural sequel
  to `Logo.mli` -- a turtle is a record (position, heading, colour), the
  world a grid of patches with a float or two, and the whole API is
  `ask_turtles`, `ask_patches`, `sprout`, `die`, `diffuse`, `hatch`,
  plus the neighbourhood queries (`turtles_here`, `patch_ahead`). No
  interpreter: the "language" is OCaml functions over those records, as
  Logo's commands are OCaml values.
- **What it teaches**: emergence and decentralized thinking (Resnick's
  own subject: people reach for a leader to explain a flock, and there
  isn't one); and, in passing, that a simulation loop and a game loop
  are the same loop.
- **The examples it gives, nearly free** (each a page, each a classic
  of the NetLogo model library, and each worth its own `examples/`
  file):
  - `TurtlesFlocking.ml` -- Craig Reynolds' boids (1987): separation,
    alignment, cohesion. Three rules, a flock, and no leader;
  - `TurtlesAnts.ml` -- ants that lay pheromone, follow it, and carry
    food home; the trail evaporates and diffuses (`diffuse` is the
    patches' whole reason to exist), and the shortest path appears
    without anyone computing one;
  - `TurtlesFire.ml` -- fire spreading through a forest, and the
    percolation threshold: at about 59% density it stops crossing, and
    the fun is watching the number matter;
  - `TurtlesSegregation.ml` -- Thomas Schelling (1971): everyone is
    mildly happier with a few neighbours like them, and the map
    separates completely. The most uncomfortable page of code here;
  - `TurtlesTermites.ml` -- Resnick's own, from *Turtles, Termites and
    Traffic Jams*: termites pick up wood chips and drop them next to
    others, and the chips end up in piles;
  - `TurtlesTraffic.ml` -- cars that only brake and accelerate, and a
    jam that travels backwards down the road while every car goes
    forwards.
- **Cost**: a few hundred lines for the layer, and probably the most
  spectacular thing per line in the whole repository.

### 2. Karel the Robot

Richard E. Pattis, Stanford, 1981 (*Karel the Robot: A Gentle
Introduction to the Art of Programming*): a robot on a grid of streets
and avenues with five commands -- move, turn left, pick up a beeper, put
one down, turn off -- three questions it can ask (is there a wall in
front, a beeper here, which way am I facing), and one real idea: you
**teach it new words**, so `turnRight` is three `turnLeft`s and
`harvest` is a field of them. Forty years of first lectures (Stanford's
CS106A still opens with it, in Java now).

- **What it would be here**: two surfaces on one world.
  - `games/programming/TinyKarel.ml`, the main one, because it is what
    Karel was: a beginner types a program in Pattis's language
    (`BEGINNING-OF-PROGRAM`, `DEFINE-NEW-INSTRUCTION turnright AS`,
    `ITERATE 3 TIMES`, `WHILE front-is-clear DO`) into the game's
    editor, presses Run, and watches the robot. The exercises below are
    its levels, a level won when the beepers end up where the level
    says. Laid out like TinyCoreWar: the editor, the parse errors on
    their lines, Run/Step/speed.
  - `Karel.mli`, the smallest of these layers by far -- a
    world of walls and beepers read from strings (a `Tilemap`), a
    program as a `command list` the way Logo's is, and the same
    `picture`/`animation` pair so you can watch it walk.

  The world, the commands and the stepping are the layer's; the game
  adds the parser and the levels on top of it, the way `Puzzlescript`
  takes a level as data.
- **What it teaches**: procedures and decomposition, before variables,
  before arithmetic. It is the anti-Logo: no coordinates, no turtle
  geometry, only "what can I see from here".
- **The levels** (as `examples/` of the layer too), which are Pattis's
  own exercises and still the first week of a lot of courses:
  - `KarelHarvest.ml` -- a field of beepers picked row by row, where
    the whole lesson is that you write `harvestRow` once and the
    program is then five lines;
  - `KarelStairs.ml` -- climbing a staircase, the canonical "teach it a
    new word": `climbStair` is move, turn, move, turn;
  - `KarelMaze.ml` -- out of a maze by the right-hand rule, which is
    three questions and a loop, and the first program that works on a
    world it has never seen;
  - `KarelNewspaper.ml` -- fetch the newspaper from the porch and come
    back, Stanford's opening exercise.
- **Cost**: small for the layer, a good afternoon and a good bookend
  to Logo; the game adds a parser and an editor, both done once
  already in TinyCoreWar.
- **Its descendants**, the same idea as commercial games, for
  TinyKarel's header or later levels: Lightbot (Danny Yaroslavski,
  2008: the program as tiles, procedures as a second row of slots),
  and Human Resource Machine (Tomorrow Corporation, 2015: assembly
  language disguised as an office job).

### 3. Actors on a grid: Greenfoot

Michael Kölling, Kent, 2006: a Java environment where you write an
`Actor` subclass with an `act()` method, drop instances into a world,
and press go. It is close to what this playground already is, which is
the interesting part -- the exercise would be to find what it has that
we do not (the world as an object you populate by hand, actors that ask
the world about their neighbours) rather than to port it.

- **Probably not a module**: more likely a note, or one example, unless
  something specific falls out of reading it.

### 4. Rules in sentences: Inform 7, Ceptre

Graham Nelson's Inform 7 (2006) writes interactive fiction as English
rules ("Instead of taking the lamp, say ..."), and Chris Martens' Ceptre
(2015) writes game rules as linear logic, where a rule *consumes* its
inputs -- which is exactly what `Puzzlescript`'s "a thing named on the
left and not on the right is taken away" is, with a theory behind it.

- **What it would be here**: not a module. A paragraph in
  `Puzzlescript.mli`'s related work, once the linear-logic reading of
  its rules is written down properly, and perhaps a rule form that
  makes the consuming explicit.

### 5. The ones to read but not port

- **Boxer** (Andrea diSessa, Hal Abelson, 1980s): Logo's successor,
  where a program is boxes you open. Needs its editor to mean anything.
- **Etoys** (Alan Kay, Squeak, 1997) and **Scratch** (Resnick, 2007):
  tiles and blocks, which is a user interface, not an API.
- **Alice** (Randy Pausch, CMU): 3D storytelling; `playground3d` could
  host something like it, but the idea is the authoring tool.
- **VGDL** (Tom Schaul, 2013): PuzzleScript's compression done so that
  one AI can play hundreds of games; interesting next to
  `ai/` more than next to `playground/`.

## Where this meets the GUI plan

[`plan_gui_teaching.md`](done/plan_gui_teaching.md) gave the
repository its first two text languages: TinyVisiCalc's formulas
(`libs/languages/formula`'s `Formula`, the smallest useful language here) and
TinyHyperCard's HyperTalk (`libs/languages/hypertalk`, cards and scripts, the
closest thing to a "way of programming" that a document can be). With
TinyCoreWar's Redcode they are what a new parser should look like.
`apps/devtools/`'s integrated environment (Turbo Pascal: edit, compile,
run, the error on its line) wants one of these small languages as its
subject -- Karel's would do, and would make TinyKarel's editor and the
environment's the same code.

## Ordering

1. **Turtles** (StarLogo/NetLogo), for the examples above and because
   it is Logo grown up, and Logo is already here. Flocking and the ants
   first: they are the two that make people want to write one.
2. **Karel**, small, and it completes the pair: one turtle that draws,
   one robot that cannot see coordinates at all. The layer first, then
   TinyKarel in `games/programming/` on it. Harvest and the staircase
   first, since both exist to show the same thing -- that you can
   teach it a word.
3. The rest as reading, folded into the .mli related-work sections
   where they belong.

Each new one needs, as the three done ones have: an `.mli` that says
where it comes from and what it does *differently* (not a manual), one
or two `examples/`, worked examples in `playground/tests/`, and a
golden frame. A game in `games/programming/` needs, as any game, its
`CATALOG.md` row, its golden frame and its web page.
