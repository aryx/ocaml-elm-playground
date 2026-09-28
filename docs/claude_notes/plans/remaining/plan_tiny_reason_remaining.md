# Plan: what's left for TinyReason and the music parts

The plan is done: see [`done/plan_tiny_reason.md`](../done/plan_tiny_reason.md).
Every Tiny instrument's panel is a part (`music_parts`: `Part_hammond`,
`Part_juno`, `Part_tr808`, `Part_minimoog`, `Part_rhodes`, `Part_dx7`,
`Part_cs80`, `Part_tb303`), hosted full size by its stand-alone app and
scaled by TinyReface; TinyReason is Reason's rack of `Rack_module`s over
`Studio_reason`'s graph, its back's cables `Rack_cable` ropes. What's
left, roughly from most to least worth doing. Anything that changes
pixels ends with new golden frames, approved after looking at them.

## 1. The rack saved and loaded

A rack as text: its devices' kinds top to bottom, each front's `save`
(every part has one: its voice's `to_string`, the mixer's and the
Matrix's knobs), the cables as port pairs, the tempo. Loaded through a
registry of kinds, Component's `placeholder` for a kind this build
lacks (it keeps its text, and saves it back). Over `File_menu`, as the
office's documents. The golden check: a rack saved, loaded, saved again,
the same text.

## 2. More modules, and the second level of "a module in a few lines"

- `Rack_module.of_voice`: a voice with no panel of its own as a module,
  its front `Part_voice` (the grid of its knobs, now used by nobody)
  and its CV inputs by knob name -- the plan's level 2, not written.
- The other Tiny voices in the catalogue (Minimoog, DX7, CS-80,
  TB-303, Rhodes): a line each, over their parts -- within the 5,000
  lines: all eight voices are ~4,985 lines by themselves, so a pick, or
  a second rack program.
- TinyReBirth plugged in whole, Reason's ReBirth Input Machine:
  `Studio_rebirth`'s hub as a device, a jack per machine, its transport
  the rack's.

## 2b. The sequencer

The piano roll under the rack is one loop of two bars (`Song.mli`).
Reason's has more: a song of any length and its loop inside it, the
arrange view (a track's patterns as blocks along the song), a velocity
lane under the notes, quantize, notes moved and resized by dragging,
several selected, the 808's track shown as drum rows instead of keys,
recording from the letters as they are played, and the notes on the
exact sample (now up to a chunk early).

## 3. The Spider, and one output into several inputs

Reason 2.0's audio and CV splitters: a device with one input and four
outputs (and the merger, four into one). Today an output takes one
cable (`Studio_reason.connect` pulls the old one out).

## 4. Moving a device in the rack

Dragged by its ears to another place, the others making room, its
cables following and swinging (the ropes' ends move with the jacks
already: only the patch's order and a drag to add). Folding a device to
one unit (Reason's fold arrow).

## 5. The Matrix and the timing

- Sample-exact CV events: the chunk split where a step starts, instead
  of up to 63 samples late (`Rack_matrix.mli`, `Unit_reason`'s chunk 87).
- The Matrix's front: its octave switch (two octaves from C2 now), its
  curve drawn and clicked (a ramp now), 32 steps, pattern banks.
- CV trim knobs on the back, the CV added to the knob's position rather
  than replacing it.

## 6. The cables

- Stiffer ropes: at rest a cable hangs at 107 pixels where the
  parabola says 97 (`Rack_cable.mli`): more relaxations, or the length
  corrected for the stretch.
- The exact catenary (`y = a cosh (x / a)`, `a` by Newton's method for
  a given length) beside the parabola as the first shape, in
  `libs/physics/2d/` (plan's step 14).
- Reason's show/hide/auto cables; the cables of the device under the
  mouse lit.
- L and R mono jacks, a mono cable into L feeding both.

## 7. The look

- The flip a turn in 3D (the Playground has no x-only scale: a
  polygon per device, its corners projected).
- The front's selected device: its keyboard shown (the letters play it
  now, no keys drawn).
- A golden WAV of the default rack playing two bars (plan's test 7, not
  written; `make approve-golden-music`), and the rack listened to.

## 8. The parts' loose ends

- `Piano` and `Meters` in TinyOp1, TinyOpxy and TinyReBirth, which keep
  their own copies (plan's step 15); the Minimoog's and TB-303's
  zero-crossing scopes as a `Meters` option.
- TinyReface's CS face has no golden scene.
- TinyTR808: START clicked and space in the same frame toggle twice
  (the button in the part, space in the host).
