# docs/claude_notes/

Notes written while building this project with Claude Code: tutorials
explaining how each piece works from scratch, comparisons with other
work, developer tools, and the plans. (The code is the other half: each
module's `.mli` has its diagram, worked example and references, and the
tutorials point to them.)

## tutorials/: how it works, from scratch

A reading order, from the simplest:

1. `notes_2d.md`: pictures as pixels, the 2D software rasterizer
   (`graphics/2d/`); `notes_font.md`, text with Hershey's vector fonts;
   `notes_opti.md`, making it fast without losing the simple version.
2. `notes_3d.md`: 3D, from a camera to pixels (`graphics/3d/`);
   `notes_3d_shading.md`, light and color; `notes_3d_opti.md`, speed.
3. `notes_opengl.md` and `notes_opengl_shaders.md`: the same done by a
   GPU, and what changes.
4. `notes_2d_physics.md`: motion, from Newton's laws to stacks of boxes
   (`physics/`).
5. `notes_audio.md`: sound, from samples to a synthesizer (`audio/`);
   `notes_audio_midi.md`, music as data: MIDI, and the trackers.

## related-work/: where this project stands

- `notes_similar_projects.md`: the projects like this one as a whole,
  and what's unique here.
- The lineage of each layer, against its own field:
  `notes_playground_related_work.md` (2D),
  `notes_playground3d_related_work.md` (3D),
  `notes_physics_related_work.md`, `notes_audio_related_work.md`; and
  `notes_vs_doom_quake.md`, the software renderers of the id Software
  classics.

## dev/: developing and debugging

- `notes_debugging_techniques.md`: how things were debugged here
  (offscreen frames, scripted keys, golden tests, ...).
- `notes_headless.md`: running the web programs without a browser
  (headless Chrome, or `scripts/web/web_headless.js`).

The tools themselves are in `scripts/` (see `scripts/README.md`).

## plans/: what's next, and what was done

- The open plans: `plan_audio_teaching.md`, `plan_games.md`,
  `plan_games3d.md`, `plan_networking_teaching.md`,
  `plan_teaching_languages.md`, `plan_teaching_other.md`,
  `plan_playground_other.md`, and what's left of finished ones
  (`plan_*_remaining.md`: 2D, 3D, WebGL, Minecraft, physics).
- `plans/done/`: the finished plans, kept for their history and
  numbers (the 2D and 3D software rasterizers, OpenGL, WebGL, the
  playground3d API, the HUD, TinyMinecraft, physics).

(The notes' links to each other and the code's comments name the files
from before this layout, `docs/claude_notes/notes_2d.md` and so on:
the file names didn't change, only their folders.)
