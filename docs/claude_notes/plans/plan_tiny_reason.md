# Plan: TinyReason, a rack of modules, and the component architecture under it

Status: step A done (2026-09-28): Panel, Piano, Meters, Part_hammond,
Part_voice in music_parts; TinyHammond over them (golden frames
identical), TinyReface's YC face TinyHammond's panel scaled. Step B.4
done: Part_juno, Part_tr808, TinyJuno and TinyTR808 over them (golden
frames identical). Next: B.5 or C.

## The goal: a module added in one file

What matters is the architecture: a *module* (a synthesizer, a drum
machine, an effect, a sequencer) is one value of one type, and adding
one to the rack is writing it and naming it in a catalogue -- the host
(TinyReason, TinyReface, a stand-alone app) knows nothing about any
module in particular. TinyOffice's parts (below) are where the idea
comes from, not a constraint.

```ocaml
(* Rack_module.mli: what a host needs from a module, nothing more *)
type t = {
  kind : string;               (* "hammond", its name in a catalogue and a saved rack *)
  units : int;                 (* its height in rack units *)
  front : Component.part;      (* its panel: draw into a box, take the mouse, save *)
  device : Rack_device.t;      (* its back and its sound: jacks, stages, notes, transport *)
}
type catalogue = (string * (unit -> t)) list   (* the Create menu *)

(* the two shortcuts that make most modules a few lines *)
val of_voice : kind:string -> 'p Part_voice.voice -> Instrument.t -> ?cv:... -> unit -> t
val of_effect : kind:string -> Effect.t -> t
```

Three levels of effort, from cheapest:

1. **An effect of libs/audio/effects**: one line, `Rack_module.of_effect
   ~kind:"ddl-1" (Delay.fx ())` in the catalogue -- its knobs drawn as
   a generic panel from `Effect.knobs`, Audio In and Out on its back.
2. **A voice with no panel of its own**: ~10 lines of data --
   `of_voice` over its `Patch_text.knob`s (a generic grid of knobs, the
   preset menu), which CV jacks drive which knobs.
3. **A module with its own panel** (the Hammond's drawbars, the 808's
   steps): one file `Part_x.ml` giving `make : Voice_x.t ->
   Component.part` (what a stand-alone app and TinyReface host) and
   `rack : unit -> Rack_module.t` (the part plus its `Rack_device`).

Adding it to TinyReason is then one catalogue line, `("Hammond",
Part_hammond.rack)`: auto-routing, the back's jacks, the cables, save
and load all come from the record. A new kind of host (TinyReface, a
future modular synth) gets every module for free.

### The real ones: plug-ins inside a program, ReWire between programs

The originals had the same two layers, each with its standard:

- **A module inside a host**: Steinberg's VST (Cubase, 1996), plug-in
  effects and instruments; and Reason's own, Rack Extensions (Reason
  6.5, 2012): third-party devices with a front and a back panel, their
  jacks and controls declared as data, their sound as code behind
  them. That is `Rack_module.t` (a Rack Extension's jacks as data is
  `Rack_device.jacks`).
- **A program inside another: ReWire** (Propellerhead with Steinberg,
  1998; ReBirth into Cubase VST first). A whole application, the
  *device*, streams its audio channels to another, the *mixer* (the
  host); they share one transport (tempo, song position, play/stop),
  and the host sends it notes. ReBirth ran in Cubase that way, then in
  Reason as the ReBirth Input Machine (Reason 2.x, to check), and
  Reason itself in Cubase or Logic.

We don't emulate either: they are where our two layers come from, not
specifications. A module is our own `Rack_module.t`, nothing to do with
VST's or Rack Extensions' formats; and ReWire is not built -- inside
one process, a whole studio (TinyReBirth's `Studio_rebirth`, already a
hub `Instrument.t` with a transport) could simply be wrapped as one more
module if ever wanted: the ReBirth Input Machine, left as an exercise.

The one limit is the budget, not the architecture: a module is easy to
write, but a program counts the code of every module its catalogue
names (the voices are ~500-900 lines each), so TinyReason's catalogue
is a pick (Part 2's budget), and a module left out of it is an
exercise of one line.

## Context

The wish: "a TinyReason with the back rack and physical drag of cables
super nice like in the original", its devices modules one can add (the
Hammond and the other voices), "components like in TinyOffice (OLE
style)", and TinyReface over the same components, so that the
stand-alone Hammond and the one in TinyReface share most code.

Propellerhead's Reason (2000) followed ReBirth (1997, TinyReBirth): the
fixed studio became a rack of devices (Hardware Interface, Mixer 14:2,
SubTractor, Redrum, NN-19, Dr. Rex, the Matrix, the effects RV-7,
DDL-1, D-11, ECF-42, CF-101, PH-90, COMP-01, PEQ-2). Tab turns the rack
round; on the back, cables dragged from jack to jack sag and wobble,
coloured by kind; a device added is routed automatically.

What exists:

- **The voices share an interface**, `Voice.S` (patch, knobs by name
  through `Patch_text.knob`, presets, `Instrument.t`, recent): why
  TinyReface is thin.
- **The panels are not shared.** Copies counted in apps/music/:
  `letters` 11 programs; the on-screen keyboard (`key_at`,
  `keyboard_view`, `whites_before`) 9; `scope_view` 10;
  `spectrum_view` 8; `segment` 11; the Control.t-to-widget `control` 7.
  TinyReface's faces are a third panel per voice (grids of knobs, not
  TinyHammond's drawbars): it shares only the voice with TinyHammond.
- **The office solved "one editor, several hosts"**:
  `Component.part` (appkits/embed), a record of functions (kind,
  height, natural, `draw box ~active`, `input computer box -> part`,
  menu, command, save), a registry by kind, a placeholder for unknown
  kinds, and scaling (`draw_in`/`input_in ~scaled`, the mouse mapped
  back into the part's natural size). Part_sheet is TinyExcel's engine
  and view behind that record; TinyOffice activates one part in place.
- **Free libraries** (libs/, not counted in the budget): audio/effects
  (`Effect.t` and seven effects), `Sequencer`, physics/2d `Particles`
  (Verlet ropes), gui's `Immediate` (widgets as values).
- **ocaml-light**: no functors, no objects, no first-class modules:
  records of functions. TinyReface's `(module V : Voice.S ...)` goes.

## Part 1: the music component, OLE style

### The parallels

| Office | Music |
|---|---|
| `Component.part` | the same type, unchanged: a synthesizer's front panel is a part |
| Part_sheet: Sheet + Sheet_view behind the record | Part_hammond: Voice_hammond + its panel (drawbars, tabs, Leslie) |
| `natural = Some (300, 144)` | `natural = Some (960, 440)`, the stand-alone panel's size |
| TinyExcel: the sheet full screen, a formula bar, a chart | TinyHammond: Part_hammond full screen, plus its keyboard, spectrum, Leslie cabinet |
| TinyOffice: parts embedded, one active in place | TinyReface: four parts behind a switch, the one shown scaled into the case |
| TinyOpenDoc's column of parts | TinyReason: a rack, a column of parts |
| menu merging | the active device's presets in the host's top bar |
| save/load, registry, placeholder | save = `Voice_x.to_string patch`; the host's catalogue; an unknown device kept as a placeholder |
| (no audio) | `Rack_device.t`, the audio half: jacks, stages, the instrument -- the "server" behind the object |

**The one difference**: a music part drives a sound living outside the
model (the mixer pulls it between frames, Instrument.mli). The part's
state holds the patch as a value; its `input` also pushes a changed
patch to its voice (`Voice_x.set_patch`), as each Tiny synth's update
does today.

**Activation**, OLE's two levels: *in-place active* gets the mouse (a
stand-alone app's one part; TinyReface's face shown; in TinyReason the
front under the pointer, keeping it while the button is held --
OLE 96's inside-out activation). *UI-active* gets the letters and the
merged menu (TinyReason: the device clicked). No part gets input while
`Gui.modal ()`.

**Widgets in a part are values**, not the global Gui: each part keeps
its own `Immediate.t`; `input` runs `Immediate.frame (Gui.input
computer)` then its knobs; `draw` is `Gui.shapes (Immediate.paint ui)`
plus its own shapes. That makes a part scalable, as Part_sheet is.
Wrinkle: `input_in` maps mx/my, not mdx/mdy, so a scaled knob turns at
screen speed -- harmless; an optional 2-line fix in Component.ml.

### Interfaces

```ocaml
(* Part_voice.mli: TinyReface's face made a part, the generic grid *)
type 'p voice = { knobs : 'p Patch_text.knob list; presets : (string * 'p) list;
  patch : unit -> 'p; set_patch : 'p -> unit; to_string : 'p -> string;
  of_string : string -> ('p, string) result }
val make : kind:string -> 'p voice -> controls:(string * string) list -> Component.part

(* Part_hammond.mli, and one per voice *)
val kind : string
val make : Voice_hammond.t -> Component.part
val load : Voice_hammond.t -> string -> Component.part

(* Piano.mli: the on-screen keyboard and the letters *)
type t
val make : keys:int -> left:float -> top:float -> white:float * float ->
  black:float * float -> octave:int -> t
val update : Playground.computer -> t -> Instrument.t -> t
val view : Playground.computer -> t -> lit:Playground.color -> Playground.shape list

(* Meters.mli: segment, scope, spectrum *)

(* Rack_device.mli (music_voices, pure): the audio half *)
type signal = Audio | Cv
type dir = In | Out
type jack = { label : string; dir : dir; signal : signal }
type io = { audio_in : int -> Signal.stereo; cv_in : int -> float option;
            audio_out : int -> Signal.stereo; cv_out : int -> float -> unit }
type stage = { reads : int list; writes : int list; run : io -> unit }
type t = { kind : string; jacks : jack array; stages : stage list;
  note_on : int -> float -> unit; note_off : int -> unit; set : string -> float -> unit;
  run : bool -> unit; tempo : float -> unit; step : unit -> int option;
  recent : unit -> Signal.t }
val of_instrument : kind:string -> Instrument.t -> recent:(unit -> Signal.t) ->
  ?keys:bool -> ?cv:(string * (float * float) * string) list -> ?transport:... -> unit -> t
val of_effect : kind:string -> Effect.t -> t
```

A rack entry pairs the two halves over one voice, made together by the
catalogue: `{ front : Component.part; device : Rack_device.t }`. The
back is drawn from `device.jacks`.

### Where they live, and the moves (for approval)

- **music_voices** (pure, tested headless): voices, studios, Patch_text
  unchanged; Rack_device, Studio_reason, Rack_cable join them; its
  libraries gain `physics_2d graphics_2d_geometry` (and the tests').
- **A new library, music_parts** (apps/music/dune, wrapped false):
  Piano, Meters, Part_voice, the Part_x, and the rack-only Part_mixer,
  Part_matrix, Part_effect; libraries `elm_playground gui appkit_embed
  music_voices`. In the programs' folder, as office's Part_* are (a
  module becomes an appkit when another category needs it).

| | From | To | What |
|---|---|---|---|
| MV1 | each TinyX.ml (Hammond first; then Juno, TR808, Minimoog, Rhodes, DX7, CS80, TB303) | Part_x.ml/.mli | the panel: theme, places, custom drawing, `control` over Immediate |
| MV2 | the 9-11 copies | Piano.ml, Meters.ml | letters and keyboard; scope, spectrum, segment |
| MV3 | TinyReface.ml | Part_voice.ml | the face record, its builder, `control` |
| MV4 | dune files | | the music_parts stanza; software/, web/, launcher/native gain it |

Not moving: no module changes directory or library; Component
unchanged (but the optional fix); nothing in libs/ or playground/.
TinyOp1, TinyOpxy, TinySoundtracker, TinyReBirth keep their panels
(Piano and Meters later, optional).

Rejected: voices into libs/ to escape the budget (they model
particular machines: the programs' code); an `over_budget` entry
(reserved for a language); one module naming every voice (the budget
counts what a program names, transitively).

### What each program becomes (estimates, measured in step A)

| Program | Own file now -> after | Its part |
|---|---|---|
| TinyHammond | 350 -> ~150 | Part_hammond ~170 |
| TinyJuno | 316 -> ~130 | Part_juno ~170 |
| TinyTR808 | 315 -> ~150 | Part_tr808 ~170 |
| TinyMinimoog | 497 -> ~180 | Part_minimoog ~260 |
| TinyRhodes | 307 -> ~150 | Part_rhodes ~130 |
| TinyDX7 | 447 -> ~200 | Part_dx7 ~220 |
| TinyCS80 | 348 -> ~150 | Part_cs80 ~190 |
| TinyTB303 | 369 -> ~170 | Part_tb303 ~180 |
| TinyReface | 273 -> ~170 | the four real parts, scaled |

The nine files: 3,222 -> ~3,150 lines (panel code moved, ~450
duplicated lines gone). Each stand-alone closure grows ~300, far under
5,000; TinyReface's ~2,740 -> ~3,700. TinyReface's faces become the
originals' panels scaled (YC: the B-3's drawbars): its header and three
golden frames change on purpose. The alternative, keeping its small
faces as Part_voice grids, shares only the voice again.

## Part 2: TinyReason

### Budget

Voices (.ml + .mli): Minimoog 784, TR-808 766, Hammond + Tonewheel
465, Rhodes 331, TB-303 462, Juno 507, CS-80 661, DX7 884. All eight
with Voice/Patch_text: ~4,985, the whole budget. So a pick:

- **Juno in the SubTractor's place** (polyphonic subtractive, as the
  SubTractor; the Minimoog is monophonic, and 277 lines dearer);
- **TR-808/909 as the Redrum**;
- **the Hammond**, asked for by name.

| Item | Lines |
|---|---|
| voices + Voice/Patch_text | 1,863 |
| parts (Juno, TR-808, Hammond) | 510 |
| Mixer, Matrix, effect fronts | 310 |
| Piano + Meters | 160 |
| Component | 166 |
| Rack_device | 180 |
| Studio_reason | 520 |
| Rack_cable | 150 |
| TinyReason.ml | ~600 |
| **total** | **~4,460** |

The Rhodes (+461 with its part) just fits; Minimoog for Juno ~+370;
the effects cost nothing (libs/).

### Devices

| Device | Front | Back |
|---|---|---|
| Hardware Interface (fixed) | a meter | Audio In "Out 1-2" |
| Mixer 14:2 | Part_mixer: 14 strips (level, pan, aux, mute), master, LED meters | 14 channel ins, Aux Send, Aux Return, Master Out |
| Juno | Part_juno | Audio Out; Seq Note, Seq Gate, Filter CV, Mod CV |
| Hammond | Part_hammond | Audio Out; Seq Note, Seq Gate |
| Redrum | Part_tr808 | Audio Out; its own sequencer on the transport |
| Matrix | Part_matrix over Sequencer: 16 steps, key grid, gate bars (tie = slide), curve bars | Note, Gate, Curve CV outs |
| DDL-1, RV-7, D-11, COMP-01, CF-101, PH-90, PEQ-2 | Part_effect over `Effect.knobs`, a bypass | Audio In, Audio Out |

Ours, and said so in the header: one stereo cable per jack (Reason: L
and R); 16 Matrix steps (32); one aux send (four); a CV replaces its
knob's value (Reason adds it through a trim knob).

### Data model (Studio_reason, pure)

```ocaml
type id = int
type port = { device : id; jack : int }
type cable = { out : port; into : port }
type patch = { devices : (id * string) list; cables : cable list;
  tempo : float; volume : float; next : id }
val connect : jacks:(id -> Rack_device.jack array) -> stages:(id -> Rack_device.stage list) ->
  patch -> cable -> (patch, string) result
val disconnect : patch -> port -> patch
val order : ... -> patch -> (id * int) list
val add : ... -> patch -> kind:string -> below:id option -> selected:id option -> patch * id
val remove : patch -> id -> patch
type t
val create : registry:(string * (unit -> Rack_device.t)) list -> patch -> t
val set_patch : t -> patch -> unit
val device : t -> id -> Rack_device.t
val run : t -> bool -> unit
val running : t -> bool
val peak : t -> port -> float
val recent : t -> Signal.t
val instrument : t -> Instrument.t
val chunk : int (* 64 *)
```

- The patch is data in the model; the devices live with the sound, as
  TinyReBirth's, made lazily (tinybox).
- `connect` refuses, with a message: In to In, Out to Out, audio into
  CV, "a loop through Mixer 1" (a DFS over the stage nodes). An input
  takes one cable (a drop replaces it); an output one too (fan-out is
  the Spider's, an exercise).
- **Stages**: send/return (Mixer -> Delay -> Mixer) is a loop between
  devices. The Mixer has two stages chained inside it: "sends" (reads
  the 14 channels, writes Aux Send, keeps their sum), "master" (reads
  Aux Return, writes Master Out). Send/return is accepted, a channel
  fed by its own send refused.

### Audio evaluation

- **Fixed 64-sample chunks** (1.45 ms, the control rate), whatever the
  pull: pulls of 735, 100 or 1 give the same samples, by construction.
  Buffers preallocated.
- Each chunk, every stage in topological order, ties by rack order;
  every device runs, connected or not (ReBirth's rule).
- An input reads its source's buffer, no copy; only the Mixer adds.
  Out: the Hardware's input times the volume, `Mix.soft_clip`.
- **CV once per chunk**: the Matrix `Sequencer.advance 64`; Gate the
  last Note_on's velocity or 0; Note n/127; Curve the step's lock.
  Voices turn gate edges into note_on/off, a note change while open
  legato. Modulation: `set name (from + cv (to - from))` when it
  changes; unplugged, the front's value back.
- Matrix events up to 63 samples early (said so; sample-exact an
  exercise); the Redrum sample-exact through its own sequencer.
- One clock, one tempo. The letters play the UI-active device.
- **Auto-routing**: an instrument to the first free Mixer channel (or
  the Hardware); a Matrix to the selected instrument's Note and Gate;
  an effect with the Mixer selected a send, with an instrument
  selected an insert; a Mixer to the Hardware.

### View

- A 920 px rack with ears and screws (TinyReBirth's). Each front
  `Component.draw_in ~scaled:true` (Hammond 960x440 -> 920x422),
  rounded up to 60 px units. Wheel scrolls, a scrollbar; only visible
  fronts drawn and given input.
- Top bar: title, Create menu, the merged presets, "Tab: back". Bottom:
  play/stop (space), tempo, volume, latency.
- The back: darker metal, the name stencilled, jacks from
  `device.jacks` (a black hole in a silver ring, radius 9).
- The flip: Tab, 12 frames counted in frames; each device a box
  920 |cos(pi k/12)| wide, dark stripes, the side switching at k = 6
  (the Playground has no x-only scale).

### Cables: sag and sway (Rack_cable, pure)

- A cable is 14 particles, 13 sticks (`Particles.rope`), both ends
  pinned (the last by hand), set each frame to their jacks or the
  pointer.
- Rest length L = 1.08 d + 60 px (d between the ends), each stick
  L/13, recomputed while dragging.
- Each update: `Particles.step ~drag:0.03 ~accel:(0, -1800)
  ~dt:(1/60)`, then `relax ~iterations:15`. A swing halves in ~23
  frames: a wobble of about a second. An end moved (drag, scroll, a
  plug going in) makes the cable trail and swing by itself; L changing
  at the plug is the bounce.
- Initial shape: the parabola approximating the catenary, sag
  s = sqrt(3 d (L - d) / 8), from L ~ d + 8 s^2 / (3 d). Worked
  example: d = 300, L = 384, s ~ 97 px. Points lerp(a, b, t) -
  (0, 4 s t (1 - t)). 90 silent steps at creation: golden frames start
  settled.
- Sleep: ends still and every |pos - old| < 0.05 px for 30 frames.
- Drawing, a ribbon polygon (~5 shapes a cable): Catmull-Rom x3 (39
  segments); normal n_i = (-t_y, t_x), t_i = normalize(p_i+1 -
  p_i-1); the points p_i + n_i w/2 then p_i - n_i w/2 reversed. A
  shadow (fade 0.3, offset (+6, -8)), the body (w = 8), a highlight
  (w = 2); a 12x26 plug at each end along its tangent.
- Colours: audio reds and oranges, CV yellows and greens, the shade
  seeded by the ports (check Reason's manual first).
- Why Verlet: trails and swings from any end, ~40 lines, over a
  library the budget doesn't count.

### Interactions

- Back: press on a free jack, a new cable; on a plugged one, pick up
  that end; **shift-press on a plug pulls it off**, and the cable
  falls away (~40 frames). Release on a valid jack connects; elsewhere
  the cable falls.
- Dragging: valid jacks (a dry-run `connect`) ringed green, larger
  within 14 px; invalid dimmed, the refusal shown; near the top or
  bottom edge, autoscroll 8 px a frame.
- Anywhere: wheel, Tab, space, Create (below the selected, auto-routed),
  Backspace (removes a device, never the Hardware Interface).
- Front: the part under the pointer in-place active, a click makes it
  UI-active.

## Shared code (Playground, Gui, Audio, platforms)

None needs changing: ribbon polygons stand in for a stroked path; Tab
is already a game key on the web; `-script` does drags and Shift;
Immediate and Look exist; `Audio.instrument` takes the hub. Optional:
the mdx/mdy fix in `Component.input_in` (appkits/embed).

## Tests

- **The refactor proof**: each stand-alone app rewritten over its part
  keeps its golden frames pixel-identical (TinyHammond first); if
  Immediate changes a drawing order, look at the diff and approve it.
  TinyReface's frames change on purpose; the golden WAVs don't.
- **Unit_reason** (headless, sine devices): topological order, the
  same samples from two rack orders; send/return accepted, the
  channel -> send -> delay -> same channel loop, Out-to-Out,
  audio-to-CV and a self loop refused, an occupied input replaced; a
  sine at level 0.5 gives 0.5 sine (1e-9), mute and unplugging
  silence, a bypassed insert equal to none; pulls of 735, 100, 1 the
  same; at 120 BPM the Matrix's gate opening at chunk 86 (sample 5504,
  9 early) -- numbers checked in the test; auto-routing, a removed
  insert bridged; a golden WAV of the default rack.
- **Unit_rack_cable**: the sag within 5% at d = 300, L = 384; the ends
  exactly pinned; the energy halving every 30 frames after a jerk,
  then asleep.
- **Golden frames**: the front (frame 5); `_back` (Tab:2, frame 20);
  `_drag` (a cable from the Juno's out held in mid-air, frame 28);
  `_plugged` (frame 50, still swinging); `_running` (space:3, frame
  40).
- tests/catalog checks the budget and the row; CPU measured natively
  and in JavaScript for the header.

## Files

apps/music: Rack_device, Studio_reason, Rack_cable; Part_mixer,
Part_matrix, Part_effect; TinyReason.ml; tests (Unit_reason,
Unit_rack_cable, Test.ml). The dune files, web/TinyReason.html,
Scenes_2d.ml and the golden PNGs, a notes_synth.md row, music_parts in
launcher/native/dune. The CATALOG row:

`| [TinyReason](apps/music/TinyReason.ml) | app | 2000 | PC | 1 | Reason (Propellerhead, 2000) | A studio rack: mixer, synthesizers, drum machine, pattern sequencer and effects, each the Tiny instrument's own panel; Tab turns it round, cables dragged from jack to jack, drooping and swinging. | The patch is a graph: devices evaluated in topological order, loops refused; each device an embedded part, OLE style, over its voice; cables as Verlet ropes. |`

## Steps (lines added / removed, estimates)

**A. The proof: one component, two hosts**
1. music_parts, Piano, Meters; TinyHammond over them, frames
   identical. (+160 / -70)
2. Part_hammond (a Component.part over Immediate); TinyHammond over
   it, frames identical. (+170 / -130)
3. Part_voice; TinyReface over parts, YC as Part_hammond scaled;
   frames re-approved. (+60 / -100)

**B. The other parts**
4. Part_juno, Part_tr808, their apps rewritten (frames identical),
   TinyReface's faces switched. (+340 / -300)
5. Part_minimoog, rhodes, dx7, cs80, tb303 (can come after C).
   (+970 / -900)

**C. TinyReason**
6. Rack_device, Rack_module (`of_voice`, `of_effect`) and
   Studio_reason's graph, tests. (480, 200 of tests)
7. Evaluation, Mixer, Matrix, Hardware, transport, tests. (260, 120)
8. Rack_cable, tests. (150, 60)
9. Part_mixer, Part_matrix, Part_effect. (310)
10. v0: catalogue, scaled parts, activation, backs, still cables, Tab,
    transport, default rack; dune, web, CATALOG, the front frame. (350)
11. Cable interactions, Create/Backspace, the merged menu, three
    frames. (200)
12. The flip, shadows, meters, `_running`, a budget check. (50)

**D. Optional**
13. The mdx/mdy fix in Component.
14. libs/physics/2d/Catenary (cosh, solved by Newton) as the initial
    shape, beside the parabola.
15. Piano and Meters in TinyOp1, TinyOpxy, TinyReBirth.

## Exercises (the header's)

L/R mono jacks; Spider splitters; CV trim knobs. Feedback through a
one-chunk delay; sample-exact Matrix events. The real SubTractor; the
NN-19 over Sampler; Dr. Rex. Redrum's per-drum gates; 32 steps and
pattern banks; Reason's sequencer and song mode. The rack saved and
loaded through the parts' save, the registry and the placeholder.
Folding devices; a device dragged to another place, its cables
following; show/hide cables. Another pick of voices; Studio_rebirth as
a device (the ReBirth Input Machine).

## Decisions for the author

0. The module architecture above (`Rack_module.t` = a panel, a
   `Component.part`, plus its `Rack_device.t`; a catalogue of makers;
   the three levels of effort). **Approved 2026-09-28.**
1. The moves MV1-MV4 (Part_x out of each TinyX.ml; Piano and Meters;
   Part_voice; the music_parts library). **Approved 2026-09-28.**
2. TinyReface's faces becoming the originals' panels, scaled (its
   look and golden frames change), or its small faces kept.
   **Approved 2026-09-28: the originals' panels.**
3. The rack's pick: Juno, TR-808/909, Hammond (Rhodes optional).
4. The optional mdx/mdy fix in appkits/embed's Component.
