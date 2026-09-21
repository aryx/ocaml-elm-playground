# docs/claude_notes/

Notes written while building this project with Claude Code: tutorials
explaining how each piece works from scratch, comparisons with other
work, developer tools, and the plans. (The code is the other half: each
module's `.mli` has its diagram, worked example and references, and the
tutorials point to them.)

## The principles

The teaching libraries -- `graphics/`, `physics/`, `audio/`, `ai/`,
and the planned `network/` -- and the documents here all follow the
same rules. Each plan restates the ones that matter to it, which is
fine and deliberate; this is the full list.

1. **Independent of the Playground.** The library speaks its own
   vocabulary -- pixels, bodies, samples, nodes, messages -- and knows
   nothing of `computer` or `shape`; `playground/X.ml` is the adapter.
   That is what keeps it readable alone, and usable by every backend.
2. **One idea per module, one feature per function**, so the simple
   path stays short and readable on its own (`Line.draw` is
   `Line.clip` then `Line.bresenham`, and neither knows the other).
3. **The simple version stays, beside the better one, switchable**
   (`Opti` and the "o" key; the naive oscillator beside the
   band-limited one; minimax beside alpha-beta). The simple one
   explains the idea; the switch shows, as a number on screen, what
   the better one buys.
4. **Every `.mli` explains its idea**: an ASCII diagram, a worked
   example with concrete numbers, and the paper or book it came from,
   with its year -- and **the tests check that worked example**, so the
   code and its explanation cannot drift apart silently.
5. **Deterministic, therefore testable**: a fixed time step, no wall
   clock inside a library, no global `Random` -- randomness is an
   explicit seed. The golden frames, the golden WAVs and every
   reproducible bug depend on it.
6. **Test against the field's own laws** where it has them: momentum
   and energy for physics, a spectrum's peaks for audio, node counts
   and gradients against finite differences for AI. Worked examples
   catch typos; laws catch misunderstandings.
7. **Comments describe the code as it is** -- not what it used to be,
   not what it replaced; the long explanations live in the `.mli`s and
   in these notes.
8. **Honest about scale.** Each `.mli` says what its idea reaches *and
   what it does not*: no reverb, a chess engine of a few hundred elo, a
   weak amateur at 9x9 Go. The teaching is in the mechanism, and
   overselling it would make the whole exercise dishonest.

And the four kinds of document: a **plan** (`plans/`) says what gets
built and in what order, and its Status section is a log -- the
numbers, the dates, and the wrong turns kept; a **tutorial**
(`tutorials/`) explains how a piece works from scratch, for someone
reading the engine, and is often written *ahead* of the code as its
specification; a **related-work note** (`related-work/`) says where
this sits among the real systems, and where its ceiling is; a **dev
note** (`dev/`) says how to run, debug or measure something.

The long form -- each document's skeleton, what the code owes the
documents (`.mli`s, game headers, debug keys, small diffs), and the
prose habits -- is in
[`guide-principles.md`](guide-principles.md); how a *file* is laid out
-- the `(****)` banners, the names the sections have, where the long
comment goes, and the traps met on the way -- is in
[`guide-code-style.md`](guide-code-style.md).

## tutorials/: how it works, from scratch

A reading order, from the simplest:

1. `notes_2d.md`: pictures as pixels, the 2D software rasterizer
   (`graphics/2d/`); `notes_font.md`, text with Hershey's vector fonts;
   `notes_opti.md`, making it fast without losing the simple version.
2. `notes_3d.md`: 3D, from a camera to pixels (`graphics/3d/`);
   `notes_3d_shading.md`, light and color; `notes_3d_opti.md`, speed.
3. `notes_opengl.md` and `notes_opengl_shaders.md`: the same done by a
   GPU, and what changes; `notes_raytracing.md`, the other renderer --
   a pixel asking what the eye sees, and the shadows, mirrors and
   glass that follow (software backend, for stills).
4. `notes_2d_physics.md`: motion, from Newton's laws to stacks of boxes
   (`physics/2d/`); `notes_3d_physics.md`, the same a dimension up
   (`physics/3d/`): quaternions, inertia tensors, capsules, and why a
   player is not a rigid body.
5. `notes_audio.md`: sound, from samples to a synthesizer (`audio/`);
   `notes_audio_midi.md`, music as data: MIDI, and the trackers.
6. `notes_ai.md`: deciding -- a way through a maze, a flock, a
   ghost's mind, an opponent's search (`ai/`);
   `notes_ai_learning.md`, the same when nobody writes the rules:
   neural networks, from one neuron to self-play.
7. `notes_inspect.md`: seeing all of the above while it runs -- an
   engine drawing its own thinking, and a run you can rewind
   (`playground/Inspect`).
8. `notes_gui.md`: interfaces -- widgets, layout, text editing, and
   the four answers to where an interface's state lives, one of which
   this playground already is (`gui/`, `appkits/`, `apps/`).
9. `notes_networking.md`: other players -- packets and latency in
   frames, lockstep, rollback, client-server, and the determinism
   every one of them rests on (`network/`).

## related-work/: where this project stands

- `notes_similar_projects.md`: the projects like this one as a whole,
  and what's unique here.
- The lineage of each layer, against its own field:
  `notes_playground_related_work.md` (2D),
  `notes_playground3d_related_work.md` (3D),
  `notes_raytracing_related_work.md` (the other renderer),
  `notes_physics_related_work.md` and
  `notes_physics3d_related_work.md`, `notes_audio_related_work.md`,
  `notes_ai_related_work.md`, `notes_inspect_related_work.md` (the
  debuggers and live environments), `notes_gui_related_work.md` (the
  toolkits, the architectures, and the compound documents),
  `notes_networking_related_work.md` (the netcodes, and the
  distributed-systems algorithms the games rediscovered); and
  `notes_vs_doom_quake.md`, the software renderers of the id Software
  classics.

## dev/: developing and debugging

- `notes_debugging_techniques.md`: how things were debugged here
  (offscreen frames, scripted keys, golden tests, ...).
- `notes_headless.md`: running the web programs without a browser
  (headless Chrome, or `scripts/web/web_headless.js`).

The tools themselves are in `scripts/` (see `scripts/README.md`).

## plans/: what's next, and what was done

- The open plans: `plan_physics3d_teaching.md`,
  `plan_raytracing_teaching.md`, `plan_inspect_teaching.md`,
  `plan_gui_teaching.md`, `plan_ai_teaching.md`,
  `plan_audio_teaching.md`, `plan_games.md`, `plan_games3d.md`,
  `plan_networking_teaching.md`,
  `plan_teaching_languages.md`, `plan_teaching_other.md`,
  `plan_playground_other.md`, and what's left of finished ones
  (`plan_*_remaining.md`: 2D, 3D, WebGL, Minecraft, physics).
- `plans/done/`: the finished plans, kept for their history and
  numbers (the 2D and 3D software rasterizers, OpenGL, WebGL, the
  playground3d API, the HUD, TinyMinecraft, physics).

(The notes' links to each other and the code's comments name the files
from before this layout, `docs/claude_notes/notes_2d.md` and so on:
the file names didn't change, only their folders.)
