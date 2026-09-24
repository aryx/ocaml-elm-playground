# Plan: virtual analog synthesizers and effects, for teaching (TinyMinimoog first)

## Context

`plan_audio_teaching.md` built `audio/`: samples, band-limited
oscillators, ADSR envelopes, a one-pole and a biquad, two-operator FM,
an echo and Schroeder's reverb, all rendered **ahead of time** into
sounds that are values (`Synth.t`), plus the `Mixer`'s continuous
voices for a theremin or a ship's thrust. That is enough for a game
saying what it sounds like. It is not enough for an **instrument**: a
synthesizer is a voice running live, sample after sample, while
someone plays notes and turns knobs, with some of its knobs turned by
*other parts of the synthesizer* (an LFO, an envelope on the filter).

This plan teaches **virtual analog synthesis**: how the classic
analog synthesizers made their sound, and how a program reproduces it,
the way Arturia's V Collection does (the Mini V, the Jup-8 V, the DX7
V...). Then the **effects** a synthesizer goes through on its way out:
drive, EQ, chorus, delay, reverb, compression. And it builds the first
application of `apps/music/`, the placeholder whose `dune` file already
imagines "a synthesizer (a Minimoog on screen)".

**TinyMinimoog first.** The Minimoog Model D (Moog Music, 1970) is
the synthesizer that made subtractive synthesis a portable instrument,
and everything this plan teaches is in it: three oscillators, a
mixer with noise, the **ladder filter** (the one sound Moog patented),
two envelopes, glide, oscillator 3 turned into an LFO, the modulation
wheel. Its panel is a fixed signal path you read left to right, no
patch cords: the right size for a Tiny program and for a first
lesson. Its successors come in the same order the history went (see
"The collection" below): the TB-303, the DX7, the Juno, and the
originals behind Yamaha's Reface series, the Hammond, the Rhodes and
the CS-80.

Companion, written ahead of the code as its specification, like
`notes_audio.md`: `docs/claude_notes/tutorials/notes_synth.md`, and
Part 7 of `notes_audio_related_work.md` (the synthesizers, their
virtual versions, the books).

## Principles (the same as `audio/`)

- **The building blocks in `audio/`, the instrument in the app.**
  `audio/` gets the generic modules (an LFO, a ladder filter, a
  compressor, ...); *what a Minimoog is* -- which blocks, wired how,
  with which ranges on its knobs -- is `apps/music/`'s. The TB-303
  then reuses the blocks with another wiring.
- **The simple version next to the better one, switchable**, so the
  difference is *heard* and *seen* (the oscilloscope and spectrum of
  `Audio_debug`, drawn on the panel): a linear ladder vs a saturating
  one; straight envelope segments vs the capacitor's exponential ones;
  perfectly tuned oscillators vs drifting ones (the "warmth" of analog
  is partly its imprecision); a drive without and with oversampling
  (its aliases, Nyquist again). One feature per toggleable function.
- **Every `.mli` explains its idea** with a diagram, a worked example
  with numbers, and its reference; `audio/tests/` checks the examples.
- **Deterministic, so testable.** A scripted performance (notes and
  knob moves stamped by frame) renders the same samples every time:
  golden WAVs of a patch playing a riff, golden frames of the panel.
- **Comments describe the code as it is**; the long explanations in
  the `.mli`s and in `notes_synth.md`.

## What the Minimoog Model D is (the target)

Left to right on its panel, as it will be on the screen:

```
 CONTROLLERS      OSCILLATOR BANK      MIXER        MODIFIERS              OUTPUT
 tune             osc 1  range  wave   osc 1 vol    FILTER                 volume
 glide            osc 2  range  wave   osc 2 vol     cutoff  emphasis      A-440
 modulation mix   osc 3  range  wave   osc 3 vol     contour amount
  (osc 3 / noise    (osc 3: "control": noise vol     attack decay sustain
   vs filter EG)     off the keyboard,  (white/pink)  keyboard control 1, 2
 osc modulation      a slow LFO)       ext. input   LOUDNESS CONTOUR
 filter modulation                                   attack decay sustain
 pitch wheel, mod wheel                             decay switch (= a release)

 keyboard (44 keys, monophonic, low-note priority) --> pitch, gate
```

- **Oscillators**: ranges LO, 32', 16', 8', 4', 2' (organ footage:
  octaves); waveforms triangle, "shark tooth" (triangle-saw), sawtooth,
  square, wide and narrow pulses; oscillators 2 and 3 detuned by a
  knob.
- **Filter**: the four-pole transistor ladder, -24 dB/octave;
  "emphasis" is its resonance, self-oscillating at its top;
  "contour amount" is how far the filter envelope opens it; the two
  "keyboard control" switches make the cutoff follow the note (a
  third, two thirds, or all of it).
- **Envelopes**: attack, decay, sustain; no release knob -- the
  "decay" switch makes the release equal to the decay. Their curves are
  a capacitor charging, not straight lines.
- **Modulation**: the mod wheel sends the modulation mix (oscillator 3
  and/or noise; the 2016 reissue adds the filter envelope as a source)
  to the oscillators' pitch (vibrato, or sweeps) and/or the filter's
  cutoff (growl, wah).
- **Monophonic**: one voice, low-note priority, glide (portamento) as
  a time constant.

Checked in phase 4 against the documentation (see its status entry
below, and `Voice_minimoog.mli` for which ranges are the Model D's and
which are ours).

## Groundwork decisions

### A voice that runs live: processors with state

Today a sound is rendered whole (`Synth.render`), and only the
`Mixer`'s continuous voices go on from pull to pull (a phase, a clock,
a `Filter.memory`). An instrument is that second kind everywhere. So
each new block follows the pattern `Filter.memory` started:

```ocaml
type t                          (* its parameters and its memory *)
val create : ... -> t
val set_... : t -> float -> unit (* a knob turned, between blocks *)
val process : t -> Signal.t -> unit   (* a block, in place *)
(* or, for a source: *)
val fill : t -> Signal.t -> unit
```

Blocks of samples (a pull, ~735 samples a frame), processed in
place: no allocation per sample, and a knob's value **ramped over the
block** (as the `Mixer` does for a voice's volume) so turning it
doesn't click ("zipper noise"). Modulation that must be faster than a
block (an envelope on the cutoff, an LFO at audio rate) is a signal
too: a block of values beside the audio block (`~cutoff:Signal.t`).

Decided in phase 0: records of mutable fields, behind each `.mli`'s
`t` (the simple, C-like way, closest to the DSP books), not immutable
states returned per block (the Elm way: a copy of every delay line 60
times a second). The `Mixer` was already the stateful part of `audio/`;
`Instrument.mli` writes the pattern down on its `sine`.

### How the Playground hears an instrument

A new kind of sound in `Mixer` and `Audio`, beside one-shots, loops and
continuous voices: **an instrument**, a named source the mixer pulls a
block from.

```ocaml
(* audio/Mixer.mli *)
val instrument : t -> string -> (int -> Signal.stereo) -> unit

(* Audio.mli, Evan-style *)
type instrument
val instrument : string -> instrument     (* made once, kept by name *)
val note_on : instrument -> string -> unit  (* "C4" *)
val note_off : instrument -> string -> unit
val set : instrument -> string -> number -> unit (* a knob, by name *)
```

The app's `update` sends note events and knob values (stamped with the
frame, like `Audio.play`'s calls: a scripted run plays the same
samples), the instrument renders between frames. A frame's
granularity (16.7 ms) is the timing of a note: fine by ear for a
keyboard (a real MIDI keyboard's jitter is of that order), not for a
sequencer, so the TB-303's sequencer will run in the audio clock (the
music's clock of `Audio.position`), not in `update`.

Built in phase 0 a little differently from the sketch above: the
mixer holds an `Instrument.t`, a record of four functions (`note_on`
with a MIDI key and a velocity, `note_off`, `set`, `fill` a stereo
block in place), not an `int -> Signal.stereo`; and
`Audio.instrument name make` takes the maker, run the first time the
name is asked for (`Audio.instrument "keys" Instrument.sine`). A
`Voice_minimoog` will be one more maker; a generic `Patch` that
`audio/` interprets is left to the modular exercise.

### Knobs

`gui/`'s `Immediate` has a `slider`; a synth panel needs a **knob**
(a rotary control, dragged vertically: the convention of every
software synth since the 1990s, and why), a **rocker switch** and a
**selector** (the oscillators' range and waveform). They go into
`gui/` as widgets (other apps will want them; decided with the user,
2026-09-23), with `Immediate`'s pattern (`knob : t -> Widget.box ->
from:float -> to_:float -> float -> t * float`, as `slider`), a
default look in `Theme`, unit tests in `gui/tests/`, and a Minimoog look
(black knobs, a wooden cheek) in the app. The keyboard: the computer's
letters as AudioPiano plays them (two rows, a piano's white and black
keys), and a keyboard drawn on screen, clickable; the pitch and mod
wheels as vertical drags.

### Patches as data

A patch is the panel's positions: a record, printed and read as a
small text file (`name = value` lines, as the Minimoog's paper patch
charts were knob positions drawn on a sheet). A few presets of our own
(a bass, a lead, a brass, a flute-like sine, a sweep of noise: the
classic categories, our settings), in that text form in the
repository. Loading and saving the user's patches through the File
menu every app shares, `File_menu`: made an appkit for this plan
(`appkits/file_menu`, library `appkit_file_menu`, the user's go-ahead
2026-09-23), the office apps its first users. It saves an app's data
with `Saved` (Marshal behind a magic line); the text form is its
Export.

## The modules

In `audio/` (the building blocks), then in `apps/music/`.

### Sources and modulators

- **`Oscillator`, extended**: the **pulse** of any width (two PolyBLEPs
  at 0 and w; the square is w = 0.5), so **pulse-width modulation**;
  the Minimoog's triangle-saw; **hard sync** (oscillator 2 restarted by
  oscillator 1, the jump corrected by a BLEP at a fractional sample
  position: the teaching case of *where* a discontinuity falls between
  samples); a streaming `fill` with the phase kept. Reference:
  Välimäki and Huovilainen, "Oscillator and Filter Algorithms for
  Virtual Analog Synthesis", Computer Music Journal, 2006.
- **`Drift`**: analog imprecision, a slow random walk of a few cents
  per oscillator (seeded: deterministic), switchable. Why two "perfect"
  detuned saws beat and a real Minimoog breathes.
- **`Lfo`**: a low-frequency oscillator -- sine, triangle, square,
  sawtooth up and down, sample-and-hold (a random step each period,
  from `Noise`'s LFSR) -- its rate in Hz or synced to a tempo, its
  output from -1 to 1 or 0 to 1 ("unipolar", for tremolo). Naive
  waveforms on purpose (at 5 Hz nothing aliases: the `.mli` says why
  PolyBLEP is not needed here and is at 440 Hz). Worked example: a
  vibrato of 6 Hz and 0.3 semitone as a frequency (x 2^(0.3/12 sin)).
  `Effect.Vibrato` becomes a use of it.
- **`Envelope`, extended**: a **streaming** generator driven by a gate
  (on at a key press, off at its release: the envelope as a state
  machine, idle / attack / decay / sustain / release, retriggered or
  not, legato); **exponential** segments, the RC curve of an analog
  envelope (a one-pole towards a target beyond 1 for the attack, so it
  arrives in its time: the "overshoot" trick, with its numbers), next
  to the existing straight lines. Reference: the Minimoog's contour
  generators; Pirkle, *Designing Software Synthesizer Plug-Ins in C++*,
  2014, chapter on EGs.
- **`Voicing`**: keys to voices. Monophonic: a stack of held keys, the
  priority (low note: the Minimoog; last note: most later monosynths;
  high note), legato (no retrigger while a key is held) and **glide**
  (the pitch a one-pole in semitones: a constant *time*, not rate, and
  why that sounds right). Polyphonic, for the Juno later: N voices,
  which one a new note takes, **voice stealing** (the oldest, the
  quietest, the same note first). Also the voice limit the audio plan
  left for its MIDI player.

### Filters

- **`Moog_ladder`**: the Moog ladder, the heart of the plan. Four one-pole
  low-passes in a row (-6 dB/octave each, so -24), the output fed back
  to the input, inverted, by k (the "emphasis"): the resonance, and at
  k = 4 an oscillator at the cutoff. Three versions, switchable:
  1. **naive linear**: the four poles and the feedback with its
     one-sample delay -- which detunes the cutoff and changes the k
     where it oscillates (Stilson and Smith's analysis, with numbers);
  2. **zero-delay feedback** (Zavalishin's topology-preserving
     transform): the feedback solved instead of delayed, the cutoff
     and resonance where the analog ones are;
  3. **nonlinear**: a tanh per stage (the transistors saturating,
     Huovilainen's model): drive it harder and it gets fatter, not just
     louder -- the sound people mean by "Moog".
  The passband's loss as the resonance rises (the bass thins: the
  Minimoog's known behaviour, some clones compensate: an option).
  Tests: -24 dB/octave above the cutoff (the linear one, measured on
  sines), the self-oscillation's frequency within a few cents of the
  cutoff at k = 4 (ZDF) vs the naive one's error, the tanh one's
  harmonics rising with the input level. References: Robert Moog,
  "A Voltage-Controlled Low-Pass High-Pass Filter for Audio Signal
  Processing", AES convention, 1965, and US patent 3,475,623 (1969);
  Stilson and Smith, "Analyzing the Moog VCF with Considerations for
  Digital Implementation", ICMC 1996; Antti Huovilainen, "Non-Linear
  Digital Implementation of the Moog Ladder Filter", DAFx 2004; Vadim
  Zavalishin, *The Art of VA Filter Design*, 2012 (rev. 2018).
- **`Svf`**: the state-variable filter, low / band / high / notch from
  one structure. Chamberlin's (*Musical Applications of
  Microprocessors*, 1980), then its ZDF version (Zavalishin; Andrew
  Simper's "Cytomic" SVF). Why it and not `Filter`'s biquad for a
  synth: the biquad's coefficients recomputed every sample blow up or
  zip when the cutoff moves fast; the SVF's parameters are the cutoff
  and the Q themselves. The TB-303 and the Juno (whose filters are
  different ladders, but whose *fast sweeps* this handles) and a
  multimode filter use it. A test: a cutoff swept by an audio-rate LFO,
  the biquad's output vs the SVF's (bounded).

### The effects

Each a streaming processor (the pattern above), in stereo where it
makes sense, each with a dry/wet mix, and each usable by the games too
(a `Synth` node and an `Audio` function, as `echo` and `reverb` are).

- **Gain and drive** (`Drive`): gain in dB (`Mix.decibels` exists),
  then **waveshaping** -- hard clip, tanh, a cubic soft clip, a
  diode-like asymmetric one (even harmonics) -- and the teaching
  point: a waveshaper *adds harmonics*, some above Nyquist, which fold
  back (§2 of `notes_audio.md` again); **oversampling** (x2 or x4: up,
  shape, low-pass, down, with `Resample` and `Filter`) switchable, the
  aliases seen on the spectrum. Reference: Zölzer (ed.), *DAFX:
  Digital Audio Effects*, 2nd ed. 2011, the chapter on nonlinear
  processing.
- **EQ** (`Filter`, extended): the rest of Bristow-Johnson's cookbook,
  the peaking and the low and high **shelves**; a three-band EQ as a
  chain; `response` already draws its curve.
- **Chorus, flanger** (`Modulated_delay`): one idea, a delay line whose
  length an LFO moves, read between samples (`Resample`'s
  interpolation: linear, then an all-pass). A few milliseconds with
  feedback: a flanger (the comb's notches sweeping); 10-25 ms, no
  feedback, two voices: a chorus (the Juno's, later its own stereo
  one). Reference: Jon Dattorro, "Effect Design, Part 2: Delay-Line
  Modulation and Chorus", JAES 1997.
- **Phaser** (`Phaser`): all-pass stages whose break frequency an LFO
  sweeps, added to the dry signal: notches without a delay line -- the
  difference with the flanger, heard and seen (notches evenly spaced
  for the flanger, not for the phaser).
- **Delay** (`Delay`): `Effect.echo`, streaming: the time in seconds or
  **synced to a tempo** (a dotted eighth), the feedback **filtered**
  (each repeat darker: the tape and the bucket brigade's sound, the
  Roland Space Echo), **ping-pong** (the repeats alternating sides).
- **Reverb** (`Reverb`): Schroeder's, streaming, then **Freeverb**
  (Jezar Wakefield, 2000: eight combs with a low-pass in their
  feedback, four all-passes, two channels decorrelated), the exercise
  `notes_audio.md` §12 already lists; Dattorro's plate (JAES 1997,
  "Effect Design, Part 1") as the better one. Tests: the decay time
  (-60 dB) measured against the setting, as the Schroeder one's was.
- **Dynamics** (`Dynamics`): the **compressor** -- an envelope follower
  (peak or RMS, its attack and release as one-poles), the gain
  computer (threshold, ratio, a soft knee) in dB, the make-up gain --
  and, the same machine with other settings, the **limiter** (ratio
  infinite, look-ahead) and the **noise gate** (below the threshold,
  silence). Worked example, the static curve: threshold -20 dB, ratio
  4:1, an input at -8 dB comes out at -17 dB. The gain reduction drawn
  as a meter (every compressor's needle), and the side-chain (a kick
  ducking a bass: "pumping", heard and explained). Reference:
  Giannoulis, Massberg and Reiss, "Digital Dynamic Range Compressor
  Design -- A Tutorial and Analysis", JAES 2012. `Mix.limit` (the tanh
  at the end of the mixer) stays; the `.mli` compares it with a real
  limiter.
- **The chain** (`Rack`): effects in an order (drive, EQ, chorus,
  delay, reverb, compressor, the usual one, and why the order matters:
  a reverb before a drive is mud), each bypassable; the app's effects
  section is a rack.

Reference for the lot, the closest book to this plan: Will Pirkle,
*Designing Audio Effect Plugins in C++*, 2nd ed., 2019; and Julius O.
Smith III, *Physical Audio Signal Processing* (online, CCRMA) for the
delay lines.

### The instrument (`apps/music/`)

- `Voice_minimoog.ml` (`.mli`): the Model D's signal path over the
  blocks above -- three `Oscillator`s with `Drift`, a mixer with noise,
  the `Moog_ladder`, two `Envelope`s, `Voicing` (mono, low note, glide), the
  modulation mix and wheel -- and its knobs' ranges and curves (the
  cutoff exponential over ~10 octaves, the glide's time, the
  envelopes' 1 ms to 10 s). Its `Patch` record, the presets, the text
  format.
- `TinyMinimoog.ml`: the panel (drawn with shapes: black knobs,
  the silver labels, the wooden cheeks, the 44 keys), the `gui/` knobs
  over the patch, the keyboard, the wheels, a scope and a spectrum
  (`Audio_debug`) in a corner as the teaching magnifier, the effects
  section as a rack after the output (the Model D has none; Arturia's
  Mini V adds one the same way), the presets menu, and the simple /
  better switches with `-debug-keys` (linear or saturating ladder,
  linear or exponential envelopes, drift on or off, naive or
  band-limited oscillators).

## The collection, after TinyMinimoog

In the order the history went, each adding one lesson on top of the
blocks:

| App | Original | Its lesson, and what it adds |
|---|---|---|
| **TinyMinimoog** | Minimoog Model D (Moog, 1970) | subtractive synthesis: everything above |
| **TinyTB303** | TB-303 Bass Line (Roland, 1981) | a sequencer in the audio clock (16 steps, accent, slide), the diode ladder (18 dB/octave-ish, why it squelches), an envelope whose decay the accent shortens: acid, playing itself |
| **TinyDX7** | DX7 (Yamaha, 1983) | FM with six operators, the 32 algorithms as small graphs, operator feedback, the 4-rate/4-level envelopes; reading real `.syx` cartridges (the published 4096-byte format; the user brings their own, as for music=) |
| **TinyJuno** | Juno-106 (Roland, 1984) | polyphony (six voices, `Voicing`'s stealing), a digitally controlled oscillator with a sub-oscillator, PWM, and its stereo chorus |
| **TinyHammond** | Hammond B-3 (1955) and the Leslie 122 | **additive synthesis**: nine drawbars mixing sines, the tonewheels' not-quite-equal temperament, percussion, key click, the scanner vibrato; the Leslie, a rotating speaker, as a Doppler shift plus a tremolo (`Space.doppler`) |
| **TinyRhodes** | Fender Rhodes Mark I (1970), and the Wurlitzer 200A and Clavinet D6 as variants | **modal / physical modelling**: a struck tine as a few decaying sine modes, the pickup's asymmetric nonlinearity (the "bark"), the Suitcase's stereo tremolo; the Clavinet a struck string (`Pluck`) |
| **TinyCS80** | Yamaha CS-80 (1977) | **polyphony with two layers** per voice, the ribbon controller, polyphonic aftertouch, the ring modulator: Vangelis's Blade Runner brass |

The last three, with TinyDX7, are the originals behind Yamaha's
Reface series (next section). Between them they cover the four ways
of making a sound: subtractive (TinyMinimoog, TinyCS80), FM
(TinyDX7), additive (TinyHammond), physical (TinyRhodes).

A **TinyVirtualSynth** hub (the collection in one program, as
TinyOffice gathers the office apps) only if the instruments make it
worth it; the Reface-shaped **TinyReface** (below) is one such hub. A
**pedalboard** app (the effects alone, on a recording or the
microphone, which is out of scope) likewise later.

## The Reface series, and the originals it revives

Yamaha's Reface (2015) is four small keyboards, the same case and 37
mini keys each, and each a modern tribute to a family of classic
instruments. Building the Reface keyboards themselves would copy
tributes; the **originals** are the machines with the history, the
famous sounds and the published papers -- the same choice as
TinyMinimoog, after the Model D, not a modern Moog. The Reface is
then the map, and its reductions (4 operators in place of 6, one
organ in five types) useful as "the simple version" switches.

| Reface | What it revives | Tiny version | What it teaches that's new |
|---|---|---|---|
| **YC** (organs) | Yamaha YC-45D, Hammond, Vox Continental, Farfisa, Ace Tone | **TinyHammond** (B-3 + Leslie, 1955) | **Additive synthesis**: 9 drawbars mixing sines, percussion, key click. The Leslie: a Doppler shift plus a tremolo, `Space.doppler` already there. The Vox and Farfisa as variants: square/divider organs, filtered, a subtractive organ next to the additive one. |
| **CP** (electric pianos) | Rhodes, Wurlitzer, Clavinet, CP-80 (and a toy piano) | **TinyRhodes** (Fender Rhodes Mark I, 1970) | **Physical/modal modelling**: a struck tine as a few decaying modes, the pickup's nonlinearity. The Wurlitzer a reed and an electrostatic pickup (another nonlinearity), the Clavinet a struck string (`Pluck`, Karplus-Strong). The Reface CP's effects row (drive, tremolo/wah, chorus/phaser, delay, reverb) is this plan's phases 6 and 7 in order. |
| **DX** (4-operator FM) | DX7 | **TinyDX7** (in the collection above) | **FM**. The Reface DX's 4 operators (and its feedback on every operator) as the "simple version" switch next to the DX7's 6. Four operators is the Sega Genesis's YM2612 too, and two the AdLib's OPL2 (`Fm.mli`): the games' FM chips, one lesson apart. |
| **CS** (virtual analog) | CS-80 (1977), CS-01, CS-15 | **TinyCS80** | **Polyphony with two layers** per voice, the ribbon, polyphonic aftertouch. The Reface CS's five oscillator types (multi saw, pulse, sync, ring, FM) are mostly `Vco`'s already; the multi saw (a supersaw: several detuned saws) is the one new block. |

Things decided, and to decide:

- **Polyphony is the prerequisite.** Every Reface is polyphonic, and
  `Voicing` is mono; stealing is planned for TinyJuno. The organ is
  the easy start: a voice per key and nothing stolen (on a Hammond
  every tonewheel is always turning; a key only connects it), so
  polyphony comes in two steps -- `Voicing`'s keys held as a set
  (TinyHammond, TinyRhodes: a voice per key, freed when silent), then
  a fixed number of voices and stealing (TinyDX7's 16, TinyCS80's 8,
  TinyJuno's 6).
- **TinyCS80 overlaps TinyJuno**: both polyphonic subtractive. Keep
  one, or keep both with the CS-80's lesson strictly its two layers,
  the ribbon and the aftertouch (the Juno's its DCO and chorus). To
  decide when the first of the two starts.
- **Order**: TinyHammond first (additive, the simplest polyphony, the
  Leslie a striking effect), then TinyRhodes, then TinyDX7 with its
  4-operator switch, then TinyCS80 or TinyJuno.
- **Facts looked up first**, as for the Model D (phase 4): the
  B-3's service manual (the tonewheels' gear ratios, the drawbars'
  foldback, the percussion's times, the scanner's taps), the Leslie
  122's speeds and ramp times, the Rhodes service manual, the CS-80's
  owner's manual, the Reface manuals (the DX's 12 algorithms). What is
  not documented is ours, and said so in the `.mli`.
- **A TinyReface hub** (the four behind one 37-key keyboard, a switch
  for YC, CP, DX, CS), only once the four exist: a thin program over
  their `music_voices` modules, as TinyOffice is over its parts.

## The climax: TinyOp1 and TinyOpxy

Everything above builds towards two instruments that are more than
a synthesizer: Teenage Engineering's OP-1 (2011) and OP-XY (2024),
small, playful, and versatile enough to make whole songs on (the
user's favourites; facts here from memory, to check against their
manuals first, as the Model D's were). Each is a studio in a box, so
each gathers the collection's lessons, which is why they come last:

| App | Original | Its lesson, and what it adds |
|---|---|---|
| **TinyOp1** | OP-1 (Teenage Engineering, 2011) | **the tape as the studio**: a 4-track tape recorder, each track a `Signal.t` recorded from the instrument's output, the tape speed as `Resample` (pitch and time together, as a real tape's), reverse, lift and drop (cut and paste); several **synth engines** behind the same four knobs (a subtractive one, `Fm`'s, `Pluck`'s string, a noise one), as the rack's `Modulation` slot is several effects behind one set; the **four coloured encoders** and a cartoon screen per engine, the whole UI in four knobs; its effects the rack's |
| **TinyOpxy** | OP-XY (Teenage Engineering, 2024) | **the groovebox**: 8 tracks, a **step sequencer in the audio clock** (TinyTB303's, grown), **parameter locks** (a knob's value stored per step, Elektron's idea: a sequence of sounds, not only of notes), a sampler and drum tracks, chords and scales ("brain"), scenes; its engines TinyOp1's |

What they need first: **polyphony** (H1, a voice per key, then a fixed
number with stealing), the **audio-clock sequencer** (TinyTB303), and
for TinyOpxy a **sampler** (samples played at pitches, `Resample`, the
audio plan's `AudioSampler` made an instrument). The order then: H1,
TinyOp1 (its tape needs no sequencer), TinyTB303, TinyOpxy.

## Phasing

0. **The notes and the groundwork**: `notes_synth.md` written ahead
   (the Minimoog's path, each block's idea, the effects), the
   related-work section; the streaming processor pattern decided and
   written in one module's `.mli` as the model; `Mixer.instrument` and
   `Audio.instrument`, `note_on`, `note_off`, `set`, with a trivial
   instrument (one sine) to test them: a scripted run's dumped WAV has
   its notes at the right frames.
1. **Sources**: `Oscillator`'s pulse, PWM, hard sync, streaming
   (`Vco`); `Drift`; `Lfo`. Tests: the pulse's harmonics (a width of
   1/3 has no 3rd, 6th, ...), sync's aliases under -60 dB below 5 kHz,
   an LFO's period, S&H's seeded values.
2. **Envelopes and voicing**: the streaming, exponential `Envelope`;
   `Voicing` mono (priorities, legato, glide). Tests: the attack reaches
   1 in its time, the release from wherever the level was, the three
   priorities on the same chords, glide's 63% at its time constant.
3. **The ladder**: the three versions, the tests above, golden WAVs (a
   sawtooth swept, at three resonances; the self-oscillation), plotted
   before approving. Then `Svf`.
4. **TinyMinimoog, the sound**: `Voice_minimoog`, the patches, the
   presets; played from the keyboard with a plain panel (sliders).
   Golden WAVs of each preset playing the same riff (a script).
5. **TinyMinimoog, the panel**: the `gui/` knob, rocker and selector,
   the Model D's look, the drawn keyboard and wheels, scope and
   spectrum; golden frames (a preset loaded, the help). `CATALOG.md`'s
   music section and row, its web page, `tests/catalog/` passing.
6. **Effects, part 1, the gain and time ones**: `Drive` (with
   oversampling), the EQ's shelves and peak, `Delay`, `Reverb`
   (Freeverb, then Dattorro); the `Rack`; TinyMinimoog's effects
   section. Tests per effect (the drive's aliases with and without
   oversampling, an EQ's measured gains, the delay's repeats and their
   darkening, the reverb's decay time).
7. **Effects, part 2, the modulated ones and dynamics**:
   `Modulated_delay` (chorus, flanger), `Phaser`, `Dynamics`
   (compressor, limiter, gate, side-chain). Tests: the static curve's
   worked example, attack and release times measured, the flanger's
   and the phaser's notches where the `.mli` says. The effects offered
   to the games (`Audio.drive`, `Audio.chorus`, `Audio.compressed`,
   ...) and one game using one (TinyRockBand's guitar through the
   drive, the natural first).
8. **The web**: the instrument in the browser (our samples, as for the
   rest of `audio/`); the latency of a key to a note measured on
   both, since an instrument is where it shows.
9. **Docs**: `notes_synth.md` checked against the code, its numbers
   filled in.
10. *(later)* TinyTB303, TinyDX7, TinyJuno, each with its own short
   phase list here when started; a live MIDI keyboard (the audio
   plan's leftover: an instrument is where it matters) with MIDI's
   control changes mapped to the knobs.
   - **TinyTB303, B1-B3** (started 2026-09-24). The facts first: Robin
     Whittle's "303 unique" (the Devil Fish's author): the main
     envelope's decay set by the Decay knob for normal notes and the
     shortest for accented ones; the *accent sweep*, the accented
     envelope charging a 1 uF capacitor through a diode and 47k,
     discharging through 100k and a 100k pot, so a run of accents
     climbs, each starting where the last left the capacitor; the
     volume envelope a sharp attack and a long fixed exponential decay;
     env mod never quite zero. Tim Stinchcombe's analysis of the filter:
     a 4-pole diode ladder, its poles coupled (unlike the Moog's,
     buffered), the first an octave up -- why it is called 18 dB an
     octave, and isn't quite. The cutoff up to about 2.5 kHz (the Devil
     Fish doubled it to 5). Commonly quoted, to check: the Decay knob's
     200 ms to 2 s, the slide's 60 ms, the gate half a step long.
   - **B1, what audio/ gains**: `Diode_ladder` in instruments/, the
     diode ladder's four coupled poles solved zero-delay (Zavalishin's
     method, as `Moog_ladder`'s), beside the Moog's for comparison: its
     slope and resonance measured against the Moog ladder's on the same
     sweep, why it squelches (the coupling, the resonance's peak moving
     with the cutoff differently). And `Sequencer` in instruments/, the
     step sequencer in the *audio clock*: an instrument's events come
     between blocks (Instrument.mli), a frame's 16.7 ms apart, too
     coarse for a sequencer (a 16th at 130 BPM is 115 ms: an error of
     14%); so the sequencer is inside the voice, stepping at the
     sample, its steps (a note, its octave up or down, accent, slide, a
     rest, a tie) and its tempo. Tests: the diode ladder's slope and
     self-oscillation threshold; a step's first sample exactly at
     60 / (bpm x 4) seconds in, whatever the block size (735, 500,
     1).
   - **B2, the voice**: `Voice_tb303` in `music_voices`: a `Vco`, saw or
     square; the diode ladder; the main envelope, its decay the Decay
     knob or, accented, the shortest, into the cutoff scaled by Env
     Mod; the accent sweep capacitor into the cutoff and the volume;
     the volume envelope; the slide, a one-pole on the pitch with the
     gate held; the knobs (tuning, cutoff, resonance, env mod, decay,
     accent, waveform, tempo); patterns as text; a few of our own (an
     acid line, a bass, a climbing accents study). Tests: accented
     notes' decays, the accent sweep climbing over three accents in a
     row, a slide's pitch at its time constant, the gate's length; a
     golden WAV per pattern.
   - **B3, the panel**: TinyTB303, the silver box: its six knobs and the
     waveform switch, the 16 steps as a grid (pitch, up/down, accent,
     slide, rest) edited with the mouse and the keys, run/stop, tempo,
     the running step lit; the scope and spectrum. Its golden frames
     (a pattern, and playing), web page, catalogue row.
11. *(later)* The Reface originals, in the order above, each with the
   same shape as TinyMinimoog's phases 4 and 5 (the facts looked up,
   the voice in `music_voices` with its presets and golden WAVs, then
   the panel with its golden frames, `CATALOG.md`'s row, its web
   page):
   - **H1, polyphony step one**: `Voicing`'s keys held as a set, a
     voice per key, freed once its envelope is idle; `Instrument`'s
     voices summed. Tests: a chord's three voices, one freed after its
     release, none leaking.
   - **H2, TinyHammond's sound**: `Tonewheel` (91 wheels, their gear
     ratios' frequencies against equal temperament, measured in
     cents), the drawbars' nine footages (16', 5 1/3', 8', 4', 2 2/3',
     2', 1 3/5', 1 1/3', 1') and their foldback at the ends of the
     keyboard; percussion (2nd or 3rd harmonic, single-triggered, fast
     or slow); key click; the scanner vibrato (a delay line tapped,
     V1-V3, C1-C3). Tests: a drawbar setting's spectrum (888000000
     its three sines, their levels), the percussion's decay times, the
     tonewheels' A4 against 440.
   - **H3, the Leslie**: horn and drum turning opposite ways, chorale
     and tremolo, the ramp between speeds (the horn faster than the
     drum): the Doppler and the amplitude from each rotor's angle, the
     crossover splitting the sound between them. Tests: the horn's
     pitch swing at a known speed and radius, the ramp's time
     constant. Offered to the games too (`Audio.rotary`), as the
     other effects.
   - **H4, TinyHammond's panel**: the drawbars pulled with the mouse,
     two manuals (upper and lower, the letters the upper), the Leslie's
     switch, the presets as registrations ("888000000", "808808008").
   - **TinyRhodes, R1-R2** (started 2026-09-24, then the CS-80 and
     the Juno: the classics in order). The facts first, from Florian
     Pfeifle's "Real-time physical model of a Wurlitzer and Rhodes
     electric piano" (DAFx 2017, with high-speed camera measurements):
     the Rhodes' tine a steel cantilever struck by a neoprene-tipped
     hammer, coupled to a brass tone bar (the "tuning fork"), its tip
     moving on an arc in front of an electromagnetic pickup; the voltage
     the flux's rate of change (-dPsi/dt), the flux a bell-shaped field
     around the magnet's tip, so a tine centred on it gives mostly the
     second harmonic and one slightly off centre the Rhodes' tone (the
     "voicing", the tine's alignment); hit harder, more of the field's
     curve is swept: a richer spectrum (the bark). The Wurlitzer's reed
     nearly one mode, its pickup a capacitor whose capacitance varies
     inversely with the reed's distance, the current u0 dC/dt; harder,
     richer too. The Suitcase's "vibrato" a stereo tremolo (the sound
     panned left and right). A clamped-free beam's overtones at 6.27
     and 17.55 times its fundamental (Euler-Bernoulli): the tine's
     "ping", quickly gone. Ours, and said so: the decay times, the
     field's width, the hammer's brightening.
   - **R1, TinyRhodes's sound**: `Modal` (a mode as a two-pole
     resonator struck, or a sine and its decay), a tine of a few
     modes, their ratios and decays per key; the hammer's velocity
     brightening it; the pickup (an asymmetric curve of the tine's
     displacement: the bark), the Suitcase's stereo tremolo. The
     Wurlitzer (a reed, the electrostatic pickup) and the Clavinet
     (`Pluck` struck, the damper on release) as `~model` variants.
     Tests: the modes' frequencies and decay times as set, the
     pickup's 2nd harmonic growing with velocity, golden WAVs of each
     model on the same phrase.
   - **R2, TinyRhodes's panel**: the Stage 73's case, the tremolo's
     rate and depth, the model switch, the effects row once phases 6
     and 7 exist.
   - **TinyDX7, D1-D3** (started 2026-09-24, before TinyRhodes: the
     OP-1's FM engine is then this voice, O2 plugging it in). The facts
     first, from Dexed's engine (Raph Levien's msfa, the DX7
     reverse-engineered, measured against the hardware; its constants
     are the best published and said so where used) and the voice
     format as Dexed reads it:
     - six operators, each a sine whose phase the operators above it
       modulate; 32 **algorithms**, Dexed's table a byte per operator
       (op 6 first: bits for the bus read, the bus written, added to
       the output, feedback in and out); one operator per algorithm fed
       back on itself, its last two outputs averaged, scaled by 2^(fb -
       8) (fb 0 to 7, 0 none);
     - the frequency: a ratio, coarse 0 to 31 (0 meaning 0.5), fine
       times (1 + fine / 100); or fixed, 10^(coarse mod 4) x
       10^(fine / 100) Hz; detune -7 to +7, a few cents, less at the top
       (Dexed's 0.0209 e^(-0.396 octave) / 7);
     - levels in 0.75 dB steps: output level 0-99 mapped onto 0-127
       (Dexed's `scaleoutlevel`: a table below 20, then 28 + level),
       256 of the envelope's units a doubling; keyboard level scaling
       (a break point, a depth and a curve, linear or exponential,
       either sign, on each side, a step every 3 keys); velocity
       sensitivity 0-7 (Dexed's velocity table); rate scaling 0-7, the
       envelope faster up the keyboard (a step every 3 keys from the
       note 21);
     - the **envelope**, 4 rates and 4 levels, 0 to 99, in the log
       domain: a rate to a speed as 2^(qrate / 4) (qrate = rate x 41 /
       64, plus the rate scaling), falling linearly in dB, rising along
       a curve that slows as it nears the top (Dexed's attack `level +=
       (17 - level) x inc`: the DX7's snap);
     - the pitch envelope (4 and 4, 50 the centre), the LFO (triangle,
       saws down and up, square, sine, sample and hold; speed, delay,
       pitch and amplitude depths and sensitivities, key sync),
       transpose;
     - a cartridge: 32 voices of 128 packed bytes after a 6-byte header
       (F0 43 00 09 20 00), a checksum (the bytes' negated sum, 7
       bits), F7; 4104 bytes. Yamaha's ROM voices are Yamaha's: the
       user brings a `.syx` (flag `cart=`, as TinyMario's `music=`),
       ours are our own.
   - **D1, what audio/ gains**: in `instruments/`, `Dx_envelope` (the
     rate/level envelope in the log domain, tested: a rate's time from
     0 to 99 and back, the attack's curve against the decay's line) and
     `Fm_algorithm` (the 32 algorithms as readable lists of who
     modulates whom, which ones are heard, which one is fed back; drawn
     as ASCII in the `.mli` for a few; checked in a test against
     Dexed's byte table decoded), and `Fm`'s operator (a sine read at a
     phase plus the modulation, the feedback's average of two). And
     polyphony step two in `Polyphony`: a fixed number of voices,
     stealing the oldest released first, then the oldest. Tests: a
     two-operator algorithm against `Fm.render` (the same Bessel
     sidebands), feedback 7's sawtooth-like spectrum, 17 notes into 16
     voices stealing the right one.
   - **D2, the voice**: `Voice_dx7` in `music_voices`: the patch as the
     155 unpacked parameters with names, read from and written to the
     128 packed bytes; a cartridge read (header, checksum checked,
     errors as values); the note's frequencies, levels, rate scaling,
     velocity, the pitch envelope and the LFO; 16 voices; the Reface
     DX's reduction as a switch (4 operators, its algorithms); our own
     patches (an electric piano, a brass, a bass, a bell, a marimba),
     through `Patch_text` too. Tests: a patch's bytes round trip, a
     made-up cartridge's checksum, each operator's frequency for the
     ratios and fixed modes, level scaling's curves at known keys; a
     golden WAV per patch.
   - **D3, the panel**: TinyDX7: the membrane buttons and the LCD (two
     lines of 16 characters), the DX7's "one parameter, one data slider"
     editing (the lesson: why every synth after it had knobs again),
     next to it what the DX7 hid: the algorithm drawn as a graph, the six
     envelopes drawn, each operator's level lit as it plays; the 32
     patches of the cartridge; keyboard; scope and spectrum. Golden
     frames (a patch, playing), web page, catalogue row.
   - **TinyCS80, C1-C2, then TinyJuno, J1-J2** (both kept, 2026-09-24:
     the CS-80's lesson its two layers, its touch and its ribbon, the
     Juno's its DCO, one envelope and its chorus). The CS-80's facts,
     from Old Crow's panel tour (cs80.com) and the owner's manual: 8
     voices, each two identical sections; each a VCO (sawtooth, a pulse
     50 to 90% wide with its own PWM, noise), 12 dB/octave high-pass
     and low-pass state-variable filters (their resonance kept from
     self-oscillating by Yamaha's limiting resistors, so the touch can't
     overdrive them), a filter envelope with an Initial Level and an
     Attack Level besides its attack, decay and release, a VCA with ADSR
     mixing the filtered sound with a pure sine; per section, the
     velocity ("initial") and the pressure ("after") each into the
     brilliance and the level -- polyphonic aftertouch, each key its
     own; the sub-oscillator an LFO (sine, saw, ramp, pulse, noise) into
     VCO, VCF, VCA; the ring modulator, an LFO multiplying the sound
     with its own attack and decay; section II detuned about half a
     semitone; a BBD chorus and a tremolo; the ribbon, bending the held
     notes from where it is first touched.
   - **C1, the voice**: `Voice_cs80` in `music_voices`: the two layers
     over `Vco`, `Svf` (high-pass then low-pass), `Envelope` and the
     filter's IL/AL envelope (ours, a small one: IL to AL in the attack,
     to the cutoff in the decay, back to IL in the release), the touch
     (velocity at the key, pressure while held, `pressure t key p`, a
     panel's extra), the ribbon (`bend`), the sub-oscillator (`Lfo`),
     the ring modulator, `Modulated_delay`'s chorus; 8 voices
     (`Polyphony`); our presets (the Blade Runner brass: the filter
     opening with the pressure), a golden WAV each. Tests: the filter
     envelope's levels at its times; the pressure brightening a held
     note and only that one (polyphonic); the ribbon bending the held
     notes; the resonance never oscillating.
   - **C2, TinyCS80**: its panel (the two sections side by side, the
     touch sliders, the ribbon along the keyboard, dragged), the
     pressure from the mouse's height on a held key.
   - **J1, J2, TinyJuno** (Juno-106, 1984). The facts, from Andy
     Harman's measurements of a Juno-60 (github.com/pendragon-andyh/Juno60,
     the 106's voice and chorus the same design) and the manuals: one
     DCO per voice, its waveforms analogue but its timing a digital
     clock (no drift: why it stays in tune), a sawtooth, a pulse with
     PWM (by the LFO or by hand), a square sub-oscillator an octave down,
     noise; a resonant 4-pole low-pass (self-oscillating at the top),
     its cutoff moved by the envelope (either way), the LFO and the key;
     one ADSR for the filter and the amplifier (the amplifier also a
     plain gate), the attack 1 ms to 3.25 s, the decay and release 2 ms
     to 19.8 s, Harman's fitted curves; the LFO a triangle with a
     delay; one high-pass for all six voices, after them, four
     positions (the 106's: a +6 dB shelf below 65 Hz, flat, 6 dB/octave
     from 225 Hz, from 720 Hz); the chorus one triangle LFO moving two
     bucket-brigade lines, the right one's modulation inverted (180
     degrees), each side the dry sound and its delayed copy -- I at
     0.513 Hz and II at 0.863 Hz between 1.66 and 5.35 ms (flangers by
     their delays, Harman notes), I+II at 9.75 Hz between 3.3 and 3.7
     ms, mono (a vibrato, "a Leslie"). 6 voices.
   - **J1, the voice**: `Voice_juno` in `music_voices`, a `Voice.S`:
     the DCO over `Vco` (the sub a pulse at half the frequency), the
     filter `Moog_ladder`'s nonlinear one (the IR3109 is an OTA
     four-pole ladder too), `Envelope` for both, the shared high-pass,
     the chorus (its own: `Modulated_delay` has one sine and a quarter
     turn); our presets; tests: Harman's times from his curves, the sub
     an octave down, the high-pass's four positions measured, the
     chorus's delays at its phases, a golden WAV per preset.
   - **J2, TinyJuno**: the 106's panel, its sliders in its sections
     (LFO, DCO, HPF, VCF, VCA, ENV, the chorus's buttons).
   - **T1, TinyReface**, the hub, if the four make it worth it.
   - **The ReBirth line** (after the Juno): Propellerhead's ReBirth
     RB-338 (1997), the first software studio people made records
     with, was two TB-303s, a TR-808 and (from 1998) a TR-909 on one
     clock. **TinyTR808** (Roland TR-808, 1980): its sounds
     synthesized, not sampled -- the kick a bridged-T resonator struck,
     its pitch falling, the snare two resonators and filtered noise, the
     hi-hats six square waves at odd ratios through band-passes, the
     clap noise in bursts; its 16-step sequencer (TinyTB303's `Sequencer`)
     and accent; facts from its service notes and the published circuit
     analyses first. The facts (2026-09-24), from Kurt Werner,
     Jonathan Abel and Julius Smith's two papers (the bass drum, DAFx
     2014; the cymbal, ICMC 2014): each drum a bridged-T network, a
     band-pass on the edge of ringing, struck by a 1 ms trigger pulse
     (the accent its height) -- the bass drum's at about 49.5 Hz
     (Roland's chart: 56), its frequency and Q raised by more than an
     octave for its first ~6 ms (the punch, too short to hear as pitch),
     a retriggering pulse after it, and the "sigh" (the leakage making
     the pitch follow the level, falling as it dies), its decay a
     feedback buffer's shelf, its tone a passive low-pass; the snare,
     toms, congas, rim shot and clave bridged-Ts too; the hi-hats,
     cymbal and cowbell six Schmitt-trigger square oscillators (205.3,
     304.4, 369.6, 522.7, and 540 and 800 Hz trimmed, 47.98% duty),
     summed, through two band-passes (about 3440 and 7100 Hz: the
     squares' overtones, not their fundamentals), the cymbal three
     bands with their own envelopes into high-passes (one resonant near
     10.5 kHz), the cowbell the two trimmed oscillators alone. Ours,
     and said so: the snare's, toms', clap's frequencies (the commonly
     quoted ones, not yet checked against the service notes), all the
     decay times.
   - **T1, the voices**: `Voice_tr808` in `music_voices`: the drums
     over `Modal` (the kick with its punch and sigh), filtered noise
     (the snare's snappy, the clap's bursts), the six squares over
     `Vco` and `Svf`'s band-passes (hats, cymbal, cowbell; the closed
     hat choking the open), each instrument's level, tone, decay or
     tuning; `Sequencer` as the clock (a step a sixteenth, each
     instrument's track, the accent's); our patterns; golden WAVs.
     Tests: the kick's frequency after its attack and before it, its
     sigh; the metal's spectrum (the band-passes' peaks, no
     fundamentals); a step's hit at its sample whatever the blocks; the
     choke.
   - **T2, TinyTR808**: its panel (the knobs over each instrument, the
     16 step buttons in the 808's red, orange, yellow and white, the
     instrument selector, start/stop, tempo, accent). **The 909** as its variant (1983: its kick and toms
     analogue, its cymbals 6-bit samples -- the first drum machine
     half sampled). The 909's facts (2026-09-24), from its history as
     Wikipedia tells it (a service-notes analysis like Werner's for the
     808 not found yet): designed under Tadao Kikumoto (the TB-303's
     designer too); its kick, snare, toms, rim shot and clap analogue,
     the kick "punchy" where the 808's is "boomy" (tune, attack,
     decay), the snare's tone and snappy; its crash, ride and hi-hats
     recordings of Paiste and Zildjian cymbals made in Roland's office,
     6-bit, no EQ nor compression; its sequencer's shuffle, flam and
     accents. Ours, and said so: the kick as a sine VCO whose pitch drops
     fast from high (the commonly described circuit) with the attack's
     click, all the frequencies and times, and the cymbals' recordings
     -- Roland's are Roland's, so ours are made once from many
     inharmonic struck modes (`Modal`), quantized to 6 bits, a ROM
     played through `Resample` at its tune: what a sample-playback drum
     is, the 64 levels' crunch heard.
   - **N1, the 909 in Voice_tr808**: a machine switch (808, 909), the
     cowbell's slot the 909's ride and the cymbal's its crash; the 909's
     kick, snare and toms; the cymbals and hats from the 6-bit ROM;
     shuffle (the even sixteenths late) and flam (a step struck twice,
     a few ms apart) in the sequencer, for both. Tests: the kick's pitch
     falling; the ROM's 64 levels; a hat's tune playing it faster;
     shuffle and flam at their samples; golden WAVs.
   - **N2, TinyTR808 switching**: the 808 and the 909's panels (the
     909's grey and orange), its shuffle and flam.
     **TinyReBirth**, the hub: two TinyTB303 voices,
     the 808 and the 909 on one clock, ReBirth's mixer and its effects
     (delay, distortion, compressor, the "PCF" filter: the rack's), and
     its pattern mode -- a song made of patterns, as TinyTB303 plays one.
12. *(later, the climax)* TinyOp1 and TinyOpxy (the section above),
   their manuals read first:
   - **O1, the tape**: a `Tape` of 4 tracks in `audio/` (recording an
     instrument's output block by block, playing back at a speed
     through `Resample`, reverse, lift and drop), tested on a recorded
     phrase played back an octave up in half the time.
   - **O2, TinyOp1's engines** in `music_voices` (its FM engine
     `Voice_dx7`, four operators, after D2), polyphonic (after H1),
     each four knobs; **O3, its panel**: the four encoders, the screens,
     the keyboard, the tape's transport.
   - **X1, the sequencer's parameter locks** on TinyTB303's audio-clock
     sequencer; **X2, the sampler instrument**; **X3, TinyOpxy**: 8
     tracks, the step grid, the engines, scenes.

## Status

- **Phase 0, DONE (2026-09-23)**: `notes_synth.md` written ahead (the
  Minimoog's path, the blocks, the effects, their worked examples: the
  ladder's k = 4 and 1 / (1 + k), the drive's 9th harmonic folded to
  0.9 kHz, the chorus's +- 16 cents, the compressor's -17 dB), and
  Part 7 of `notes_audio_related_work.md`. `audio/Instrument` (the
  interface, and `sine`, the pattern's model: mutable state, events
  between blocks, the knob ramped over a block, the gate over 5 ms);
  `Mixer.instrument`, `instruments`, `stop` for instruments too (faded
  over a pull); `Audio.instrument`, `note_on`, `note_off`, `set`. Tests
  (`Unit_instrument`): A4 pressed at frame 10 of a golden run's pulls
  silent until sample 7350 and sounding in that frame's block, under
  the gate's ramp for 221 samples, at tanh 0.5 and 440 Hz, silent 221
  samples after frame 40's release; the volume from 0.2 to 0.8 with no
  step above C1's slope plus 0.00082; legato; asked again, stopped.
  `playground/tests/Unit_audio`: made once by name, made afresh after
  `stop`. Also done before it: `File_menu` an appkit
  (`appkits/file_menu`).
- **Phase 1, DONE (2026-09-23)**: the sources. `Oscillator.pulse` and
  `pulse_band_limited` (any width, two PolyBLEPs, the average 2w - 1
  taken away so PWM doesn't thump); the live oscillator is its own
  module, `Vco`, rather than more of `Oscillator` (the waveforms'
  formulas stay there): a frequency and a width per sample, and hard
  sync, the master recording where in each step it wraps, the slave
  correcting the sample before and the one after at that fraction, its
  own phase-0 jump's correction left out where the restart replaced
  it. `Lfo` (six shapes, naive on purpose, sample and hold,
  `of_tempo`); `Drift` (Ornstein-Uhlenbeck, stepped every 64
  samples of the audio clock); `Noise.lcg` and `uniform` (the LFSR's
  states are shifts of each other, poor random numbers; 32-bit ints in
  a browser handled). Tests (`Unit_vco`): the pulse's harmonics
  against (4 / (pi k)) |sin (pi k w)| within 3% (PolyBLEP dulls the 4th
  by 2.7%), the 3rd missing at w = 1/3, no average; PWM from 0.1 to 0.9
  with every period's average within 0.02; sync's loudest alias below 5
  kHz -29.5 dB naive, -30.7 corrected on the sample, -69.9 at the
  fraction; the LFO's shapes, 5 periods a second across blocks, the
  vibrato's 0.98282 and 1.01748; the generator from 0 (1013904223),
  sample and hold's 9 changes a second, seeded; the drift's spread
  2.97 cents, correlated by 0.35 two seconds apart (e^-1: 0.37), the
  same in blocks of 735 or 500. Golden WAVs, plotted before approving:
  `vco_sync_sweep` (a saw synced to 110 Hz, swept from 1 to 4 times
  it) and `vco_pwm` (a 110 Hz pulse, its width moved at 0.5 Hz). Left
  for phase 4, with the Model D's manual: the triangle-saw ("shark
  tooth") waveform, whose exact shape is to check there.
- **Phase 2, DONE (2026-09-23)**: envelopes and voicing. `Envelope`,
  live: `start`, `gate_on`, `gate_off`, `fill` with a curve, the ADSR
  record kept as the knobs' values, read at each block; a state machine
  (Idle, Attack, Decay, Sustain, Release), the attack from the current
  level; `Linear` (the offline `level`'s values within a sample's step)
  or `Exponential` (one-poles towards targets, the attack aiming at 1.5
  to arrive at 1 in its time, the decay and release reaching a
  thousandth of the distance in theirs). `Voicing`, mono: the
  keys held, priority `Low` (the Minimoog), `High`, `Last`; events
  `Begin`, `Change`, `End` for the voice; legato or `~retrigger`;
  `glide`, a one-pole in semitones, kept when the keys come up. Tests
  (`Unit_envelope`, `Unit_voicing`): the exponential's 0.633 halfway up
  (half a sample short of 0.634), 1 after 441 samples, 0.5158, 0.5005,
  0.0158, 0.0005; released mid-attack and pressed again from there,
  idle when silent; the three priorities on C4, E4, D4; a trill,
  legato and retriggered; the glide's 67.59 and 405.5 Hz, 71.92 and
  520.8 Hz, an octave up and down in the same time. Golden WAVs,
  plotted before approving: `envelope_linear_vs_exponential`, and
  `mono_legato_glide`, a phrase from Voicing, Vco and Envelope put
  together as `Voice_minimoog` will be. Polyphony (voice stealing) left
  for the Juno.
- **Phase 3, DONE (2026-09-23)**: the filters. `Moog_ladder` (not
  `Ladder`: the platformer kit has one, and both libraries are
  unwrapped), three
  models behind one `process` (the cutoff per sample, the resonance k,
  `~compensation` for the bass): `Naive` (the feedback a sample late),
  `Zero_delay` (the loop solved, y = (G^4 x + sigma) / (1 + k G^4)),
  `Nonlinear` (the same with a tanh at the loop's input and each
  pole's, the linear solution predicting the loop's input). The analog
  numbers worked out from 1 / ((1 + s)^4 + k) first (the peak at k =
  3.5 59.3 cents under, +9.25 dB), then the models measured against
  them (`Unit_moog_ladder`): -12.04 dB at the cutoff, -23.3 then -24.0 dB
  an octave; the bass at 1 / (1 + k), all of it compensated; the peak
  at k = 3.5 at -60, -60, -55 cents (zero-delay, 440 Hz, 1 and 5 kHz,
  a 5-cent grid) against -35, -10, +130 (naive); oscillation from k =
  4.000 (zero-delay) against 4.06, 4.26, 4.64 and none up to 8 at 5 kHz
  (naive); the nonlinear one oscillating within a cent, held at 0.08 (k
  = 4.2) and 0.12 (4.5), its 3rd harmonic 49.9, 24.3, 16.8, 10.6 dB
  under as the input goes 0.1, 0.5, 1, 4. A lesson on the way: at k = 4
  the zero-delay filter's 0 Hz gain first measured 0.0845, not 0.2 --
  exactly at its threshold, a step sets off an undamped ring. `Svf`,
  Chamberlin's and the zero-delay one (Simper's form), low, band, high,
  notch: -3.01 dB at the cutoff to 12 kHz; Chamberlin's blown up at 8
  kHz (stable to 7,637 Hz at Q = 0.707); a cutoff swept at audio rate,
  the biquad recomputed each sample too loud (6.66 against 2.40) at 500
  Hz and blown up at 3 kHz, the SVF steady. Golden WAVs, plotted before
  approving: `ladder_sweep` and `ladder_self_oscillation` (C4, E4, G4,
  C5 on the cutoff alone, each measured within 0.3 cents).
- **Phase 4, DONE (2026-09-23)**: TinyMinimoog's sound. The Model D's
  facts looked up first (its manual, the 2016 reissue's, its service
  manual, reviews): oscillator 3's reverse sawtooth in place of the
  shark tooth, the shark tooth a passive mix (about 75% triangle, 25%
  sawtooth), the contours' 1 ms-10 s and 4 ms-35 s, glide 1 ms-10 s,
  the filter contour's 4 octaves, the tracking's thirds, 7 semitones of
  detune, the modulation mix of oscillator 3 and noise; the reissue's
  additions (a separate LFO, the filter contour as a source, a choice
  of note priority) left out. Not documented, so ours, said so in the
  `.mli`: LO 6 octaves under 8', the rectangles 30% and 10% wide, the
  modulation's depths, k = 4.5 at the emphasis's top (4.2 first: the
  whistle preset too quiet, 0.06). `apps/music/Voice_minimoog` (library
  `music_voices`): the patch a record of the panel's positions, `knobs`
  the table of its controls (name, kind, get, put) that makes both the
  text format ("name = value" lines, a switch on/off, a selector by
  its label) and TinyMinimoog's panel; six presets of our own written
  in that text; the voice as an `Instrument.t` (Voicing low note and
  legato, three Vcos with Drift, white noise from Noise.lcg, the two
  contours, Moog_ladder, the knobs ramped over each block). `Voicing`
  got `fill_pitch` (the pitch in semitones, the voice adding the
  wheel, the tune, the ranges before turning it into hertz).
  `TinyMinimoog` plain: a slider, a box or a menu per control,
  generated from `knobs`; the letters as keys, z/x the octave, the
  arrows the pitch wheel, 1-4 the teaching switches; the voice outside
  the model, as the mixer's. Tests (`apps/music/tests/Unit_minimoog`):
  every preset written and read back unchanged, the parser's errors;
  the laws (632.46 Hz at 0.5, 1 ms / 100 ms / 10 s, 35 s, the four
  trackings); low note and legato (C3 held under E3); the whistle
  within 2.5 cents at C3, C4, G4, C5, its cutoff doubled an octave up;
  the mixer's overload (0.123, 0.312, 0.446 at levels 0.1, 0.33, 1);
  a golden WAV per preset playing the same riff (plotted before
  approving; the brass's attacks lengthened after the plot showed no
  swell). A scripted run of the app (-dump-audio) measured: C3 on the
  bass preset's 32' at 32.7 Hz, held under E3, G3 at 49.1 Hz. Its
  golden frame, CATALOG.md's Music section, its web page. On the way:
  `Noise.lcg` in Int32 (the same numbers, and no more js_of_ocaml
  warning on a 32-bit mask); a stray `actual/` of golden WAVs at the
  repository's root removed, left by running the audio tests' binary
  from there.
- **Phase 5, DONE (2026-09-23)**: the panel. `gui/`: `Widget.paint`
  got a `Disc` and a `Segment` (a knob's face and pointer; the
  toolkit had drawn everything with rectangles and text), drawn by
  `Gui.ml`, printed by the gui and gui4 tests; `Theme` a
  `dial` size, `dial_face`, `pointer`; `Look.knob` (eleven ticks,
  270 degrees), `Look.rocker`, `Look.selector` (a rotary switch, its
  labels around it); `Immediate.knob` (dragged up or down, *relatively*:
  the mouse's move since the last frame, 200 pixels for the range, so
  a press doesn't jump it -- `Immediate` now keeps the move since the
  last frame and since the press), `Immediate.rocker` (a click), and
  `Immediate.selector` (a step per 24 pixels, the next on a click);
  the Playground's `Gui.knob`, `rocker`, `selector` and their `_in` and
  `_size`. Tests (`Unit_immediate`): the knob's worked example (0.3,
  0.3, 0.4, 0.5, 0.6), clamped, not turned by a drag begun elsewhere;
  the rocker; the selector two steps up, stopped at the end, back
  down, a click round to the first. No other golden frame moved.
  TinyMinimoog: the Model D's layout, CONTROLLERS, OSCILLATOR BANK,
  MIXER, MODIFIERS, OUTPUT, in black between wooden cheeks, the knobs
  placed by name over `Voice_minimoog.knobs`, the rotary switches with
  short labels; a two-octave keyboard played with the mouse too, the
  pitch and mod wheels; an oscilloscope and a spectrum of the voice's
  last 2048 samples (`Voice_minimoog.recent`, since `Audio_debug` is
  fed only by the software backend). Golden frames: at rest, and
  `playing` (the cutoff knob dragged up 40 pixels by a script, 0.32 to
  0.52, C3 and E3 held). Left: saving patches with the File menu (an
  exercise in the header), the panel's own knob look (cream skirts, as
  the Model D's) and the wood's grain.
- **Phase 6, DONE (2026-09-23)**: the gain and time effects, each a
  streaming processor in `audio/`. `Drive`: four curves (hard, tanh,
  the cubic, an asymmetric one, tanh biased by 0.3, for the even
  harmonics), a 10 Hz DC blocker after them, oversampling x1, x2 or x4
  (zeros stuffed, an 8th-order Butterworth at 18 kHz either side of the
  curve; `Filter.biquad` got `?rate` for it). `Filter`: the cookbook's
  `peaking`, `low_shelf`, `high_shelf`, and `process` (a block in
  place). `Delay`: stereo, the time gliding to a new setting (a one-pole,
  63% in 50 ms: the tape's pitch bend), read between samples, the
  feedback through a one-pole low-pass, ping-pong, `beats`. `Reverb`:
  Schroeder's streaming, Freeverb (Jezar's numbers, his all-pass as he
  wrote it, each comb's feedback set from the time rather than his room
  size), Dattorro's plate (the paper's lengths scaled from 29,761 Hz,
  the tank's first all-passes modulated by 16 samples at 1 Hz, read
  linearly), all three with the time to fall 60 dB as the knob, the
  plate's decay from the loop's 0.725 s. `Rack`: drive, EQ (200 Hz
  shelf, 1 kHz bell, 4 kHz shelf), delay, reverb, each switched on
  separately; the order's reasons in its `.mli`. Tests
  (`Unit_filter`'s EQ, `Unit_drive`, `Unit_delay`, `Unit_reverb`,
  `Unit_rack`): the EQ's table at six frequencies, measured = formula;
  the 5 kHz sine's loudest alias through tanh at +12 dB, -32.6 dB at
  904 Hz (the 9th, folded, as notes_synth.md predicted), -58.6 dB at
  3198 Hz with x2 (the 17th), -81.7 dB with x4; tanh without a 2nd
  harmonic, the asymmetric curve with one; the echoes of a 200 Hz
  burst 1, 0.499, 0.249, of an 8 kHz one 0.842, 0.131, 0.033 -- the
  first echo's 0.842 the linear read's own low-pass (16,537.5 samples,
  half-way between two: cos (pi 8000 / 44100)), a lesson found by the
  test and written into `Delay.mli`; ping-pong left, right, left; T60
  measured (T30 doubled) against 1 and 2 s: Schroeder 1.02 and 2.00,
  Freeverb 1.00 and 2.00, the plate 1.14 and 1.94; damped, Freeverb's
  highs dying in 0.58 s, the plate's in 1.24; a bypassed rack and a
  flat EQ changing nothing. Golden WAVs, their spectrograms looked at
  before approving (ffmpeg's `showspectrumpic`): `drive_sweep_naive_vs_x4`
  (the aliases falling as the note rises, then gone),
  `delay_dotted_eighth`, `reverb_rooms`. TinyMinimoog: the patch has an
  `effects` field (`Rack.settings`, all off at first, so the presets'
  golden WAVs didn't move), 19 controls in `knobs` in their own units,
  so the text format carries them; the rack after the output, the
  scope showing the sound as heard; under the panel, an "effects"
  button swapping the scope and spectrum for the rack (its controls
  only run while shown). Golden frames: the two old ones moved by the
  button alone (3312 pixels each), `effects` new (the drive, delay and
  reverb rockers clicked by a script). Left: the effects' knobs aren't
  ramped over a block (a drive's dB jumped clicks a little); a preset
  using the rack; the effects offered to the games, with phase 7's.
- **Phase 6b, DONE (2026-09-24)**: `Effect.t`, an effect as a rack
  holds it (a name, knobs by name, `set`, `process`: `Instrument.t`'s
  shape), made by each effect beside its typed interface; `Rack` an
  ordered list of them, `reorder`, "effect.knob" names with an "on"
  each, `standard_knobs` known before any rack is made; `Control` (a
  knob, a switch, a selector, and their text), shared by the effects and
  `Voice_minimoog`, whose patch keeps the rack's knobs as named numbers.
  The old `Effect` split: `Pitch_effect`, and the offline echo and
  reverb into `Synth`. TinyMinimoog's key 5, the reverb before the
  drive. Then audio/ in layers (signal, synthesis, the engine,
  instruments, effects), ai/, networking/ and playground/ in folders,
  and the from-scratch libraries under libs/.
- **Phase 7, DONE (2026-09-24)**: the modulated effects and dynamics.
  `Modulated_delay` (chorus and flanger: a delay an LFO moves, read
  between samples, the right side's LFO a quarter turn ahead):
  measured, the chorus's copy of a 1 kHz sine at 1009.42 and 990.58 Hz
  (+16.2, -16.4 cents, notes_synth.md's +-16), the flanger at 1 ms
  cancelling 500 and 1500 Hz (0.0002, 0.002) and doubling 1 kHz, 4.05
  with feedback 0.7. `Phaser`, four first-order all-passes: 2.000 at fc
  and the notches found at 415 and 2395 Hz where the analog formula says
  414 and 2414 (the bilinear's warping), 5.8 times apart where the
  flanger's are evenly spaced. `Dynamics` after Giannoulis, Massberg and
  Reiss: the static curve (-8 in, -17 out; the knee's -20.56), the
  reduction smoothed (63% in 4.99 ms for 5, 37% in 99.96 for 100,
  measured on a square wave so the detector's ripple is out of the
  way), the gate (-50 in, -140 out), the side-chain (a bass ducked 13.7
  dB by kicks). A lesson found by the test: the limiter's 0.5 ms attack
  let a 441 Hz sine through at -5.24 dB against a -6 dB ceiling, a
  one-pole can't follow a peak a third of a millisecond wide; now it
  hears the loudest sample of its look-ahead window and turns down at
  once, and nothing passes (0.5012). `Modulation`, the rack's one slot
  for the three, the same knobs; the rack's order drive, EQ, modulation,
  delay, reverb, dynamics. `Effect.t` got `meters` (the compressor's
  gain reduction) and `Effect.ramp`: the knobs that multiply the sound
  ramped over the next block (the drive's gain, the mixes, the
  feedbacks, the makeup), the EQ's gains in steps of 32 samples; tested
  as no step at a block's edge. For the games: `Synth.Processed`, a
  sound rendered then run through a processor made fresh at each
  rendering (the engine not knowing the effects, above it), and
  `Audio.drive` (its gain in, the same back out: the level kept),
  `chorus`, `flanger`, `phaser`, `compressed`; TinyRockBand's guitar
  notes as power chords through the drive, the difference tone an
  octave under (196 Hz, 36.5 dB under the note, 79 clean). TinyMinimoog:
  the rack's second page (the button: scope, effects, more), the
  compressor's needle, and the preset "space" (the lead chorused,
  echoed, in the plate, compressed; its golden WAV). A second lesson:
  the rack's slot first named "mod" shared "mod.mix" with the Model D's
  own modulation mix -- the patch's initial text no longer read back,
  and the second page's MIX knob turned the panel's; renamed
  "modulation", and a test that the patch's control names are unique.
  Golden frames: `effects` moved by the button's label, `effects2` new.
  Golden WAV `modulation_chorus_flanger_phaser` (a sawtooth through the
  three, its spectrogram looked at before approving: the flanger's comb
  sweeping, the phaser's two notches). Left: an all-pass interpolator
  for the delays (the exercise in `Modulated_delay.mli`), the rendering
  cost of `Audio.drive` on a long loop in a browser (x4 oversampling),
  to measure in phase 8.
- **Phase 8, DONE (2026-09-24)**: the web, and the latency. The web
  platform already played our samples (every sound is `Audio.pull`'s,
  instruments included), so TinyMinimoog runs in a browser as is:
  checked in headless Chrome, the page drawn whole. `Audio.latency`, set
  by each platform from what's queued ahead of the card when a frame's
  sound goes in (natively the SDL queue plus its 1024-sample buffer; in
  a browser the next buffer's start plus Web Audio's `baseLatency` and
  `outputLatency`), averaged over a second, shown by TinyMinimoog above
  its panel (0 in golden runs: no card; the frames moved by that text).
  Measured natively 56 ms (SDL's dummy driver draining in real time).
  In a browser: ~83 ms scheduled plus the browser's own, read on the
  page in Chrome on Linux: 90 ms (headless Chrome's audio clock isn't
  real time: it showed 12 ms, meaningless). The cost, under Node (js_of_ocaml,
  release) and natively: the voice 9.6% and 3.3% of real time, with the
  whole rack 32% and 14%. And `Audio.drive`'s rendering over a 20 s
  loop, 1.2 s natively and 2.1 s in JavaScript at x4 -- TinyRockBand
  frozen at every song's start: now x2 (aliases still -58.6 dB), one
  channel when both are the same, 0.32 and 0.56 s, and the game's
  driven guitar part rendered once, lazily, shared by the three bands.
  Written into notes_synth.md section 1. Then cuts heard in the
  browser: frames later than the 100 ms schedule. Measured in headless
  Chrome (a temporary probe): TinyMinimoog's frames about 60 ms (update
  and sound 16, its 800 shapes built 15, turned into a virtual DOM 18,
  the page patched 12), the first over 200. The web platform's
  schedule is now a jitter buffer: 30 ms deeper at each gap, up to 300,
  12 ms shallower each second without one, down to 100; read in
  Chrome, 160 ms after the start's gaps, then 110 while playing, no
  cuts. Then the frames measured again, without the screenshot mode
  that had slowed headless Chrome's (its 60 ms frames were that): 21 ms,
  9.8 of them turning the 800 shapes into a virtual DOM -- every number
  through `sprintf "%f"`, OCaml's formatting emulated in JavaScript. With
  JavaScript's `String(x)` and `^`, 1.1 ms, the frame 12 ms, 59 a second
  (every web page gains, not only TinyMinimoog); the schedule's floor
  then 50 ms, three frames, the native queue's. Left: the new floor read
  in a real browser.
- **H1, DONE (2026-09-24)**: polyphony, step one. `Polyphony`, its own
  module in instruments/ (`Voicing` stays the monophonic case): a voice
  per key, a voice being a record of functions (released, filled a
  block at a time, silent once its release is over), so the same
  machinery serves any instrument's voices; a key let go releases its
  voice, freed only once silent (not at once: a click; not never: a
  leak); a key pressed again while its voice releases gets a new one
  beside it; the voices' blocks summed. `Polyphony.sine`, a sine and an
  exponential envelope, the worked example and an organ's single
  drawbar. Tests (`Unit_polyphony`): a chord's block the sum of its
  three voices rendered alone; E let go, three voices still after a
  block, two after the second (silent is -100 dB, which an exponential
  release reaches at 5/3 of its time: 25 ms for 15 -- a 10 ms release
  had it freed right at the block's edge, the test moved to say
  something); all let go, none left and silence; a key pressed again,
  two voices, then one. Step two (a fixed number, stealing) with the
  DX7 or the Juno, as planned.
- **H2, DONE (2026-09-24)**: TinyHammond's sound, in `music_voices`.
  The facts looked up first: the 12 gear ratios (HammondWiki's "Gear
  Ratio"), 20 revolutions a second, 91 wheels, 2 to 128 bumps by octave
  and 192 on the last seven, which take F's to B's gears (goodeveca's
  tonewheel table: the top wheel 5924.57 Hz, not an exact octave). Not
  found, so ours and said so: 3 dB a drawbar step, the contacts' 2 ms,
  the percussion's times (fast -60 dB in 0.3 s, slow 1.2 s, soft -6 dB),
  the click's 4 ms, the scanner's 6.9 Hz and depths. `Tonewheel` (the
  91 frequencies, foldback by octaves, cents from equal temperament);
  `Voice_hammond` over `Polyphony`: nine drawbars on the wheels, read
  from one clock shared by every voice (the wheels never stop: keys
  sharing a wheel add in phase), the percussion single-triggered and
  taking the 1' drawbar's circuit, the key click, the scanner vibrato
  and chorus (V1-V3, C1-C3) on the whole organ; registrations as nine
  digits; five presets of ours (jazz, full, gospel, ballad, flute).
  `Patch_text`, the patch as "name = value" lines, taken out of
  `Voice_minimoog` now that two instruments share it (its golden WAVs
  unchanged). Tests (`Unit_hammond`): A4 440 exactly, C4 261.538 Hz
  (-0.58 cents), F#8 5924.57, the foldback; the tempered harmonic, C4's
  2 2/3' G5's wheel at 784.0 Hz against the true 784.62, 1.36 cents
  flat; 888000000's three lines equal within 1%, nothing at 4'; a
  drawbar two steps in, -6 dB; the percussion struck and 60 dB down a
  second later, a second key without it; a chord's voices freed; the
  presets as text. A golden WAV per preset (C, F, G major), their
  spectrograms looked at before approving (the percussion flaring at
  each chord, the chorus's sweep); "full" had clipped at an output gain
  of 0.1, now 0.05, peaking at -4.9 dB. Next: H3 (the Leslie), H4 (the
  panel, TinyHammond itself).
- **H3, DONE (2026-09-24)**: the Leslie. The 122's numbers looked up
  (its manual, Wikipedia, the Electronic Music Wiki): the horn 0.8 and
  6.8 turns a second, the drum 0.7 and 5.6, the crossover at 800 Hz,
  the horn reaching a new speed in 0.5 s, the drum in 1.2; ours, said
  so: the horn's 15 cm radius, the drum's 10, their levels facing and
  away. `Leslie` in effects/, modelled from the geometry, not faked: a
  rotor's sound in a delay line read at its mouth's distance to each
  microphone over the speed of sound -- the Doppler shift made by the
  moving delay itself, the tremolo by the mouth's direction; two
  microphones a quarter turn either side, stereo; the speeds following
  the switch as one-poles. Its `Effect.t` ("leslie": fast, mix; meters
  the rotors' speeds, for a panel to draw them turning); `Voice_hammond`
  into it ("leslie", "leslie.fast"; gospel fast, ballad slow: their
  golden WAVs moved on purpose, the others not); `Audio.rotary` for the
  games. Tests (`Unit_leslie`): the horn at 4.59 after 0.5 s, the drum
  at 3.80 after 1.2 s, 3.01 slowing back after 0.5 s; a 5 kHz sine at
  tremolo swinging between 5093 and 4907 Hz (5000 x (1 +- 0.0187), v / c
  with v = 2 pi 0.15 m x 6.8), its level between 1.017 and 0.20; the two
  microphones differing.
- **H4, DONE (2026-09-24)**: TinyHammond, the panel. The nine drawbars
  pulled down with the mouse (0 to 8, the B-3's colours: brown under the
  note, white the octaves, black the rest), the registration written as
  its digits; the percussion tabs, the vibrato's rotary switch, the
  Leslie's rockers, the click and volume knobs (`Gui`'s, the panel's
  controls placed by name over `Voice_hammond.knobs`); the keyboard
  polyphonic (the letters several at once), space flipping the Leslie's
  speed as an organist's foot; under the panel the spectrum
  (`Voice_hammond.recent`) and the cabinet seen from above, its horn
  and drum drawn turning at `Voice_hammond.rotors`' speeds. Its golden
  frames: at rest (the jazz registration), and `playing` (the 4'
  drawbar pulled to 6 by the mouse, C E G held, the Leslie switched to
  fast half a second before: the horn at 4 turns a second, the drum at
  2); its web page; `CATALOG.md`'s row. Left, in the header's
  exercises: the lower manual and the pedals, drawbars heard while a
  note sounds, the Leslie's brake, saving registrations.
- **B1, DONE (2026-09-24)**: what audio/ gains for the TB-303.
  `Diode_ladder` in instruments/: the diode ladder's four coupled
  equations (the first capacitor half: its pole an octave up), the
  trapezoidal rule with a prewarped cutoff, a 4 x 4 linear system solved
  each sample, a tanh on the input. Measured against the Moog ladder at
  500 Hz: -21.3 dB at the cutoff (the Moog's -12.0: the poles spread,
  the knob their scale, not a -3 dB point), -15.8 dB an octave from 1
  to 2 kHz (the "18 dB"), -20.7, then -25.8; the bass 1 / (1 + k) as the
  Moog's; ringing on its own from k = 22.1 (the Moog's 4); its peak
  above the cutoff, 667 Hz at 80% of that. `Sequencer` in instruments/:
  steps in the audio clock, each event at the first sample at or after
  its exact time, the voice rendering its block in pieces between them;
  a note's gate half its step unless it slides into a note (the gate
  held, the next note glided to). Tests: at 120 BPM, steps at 0, 5513,
  11025, 16538, gates closing at 2757, 8269, 13782, 19294, the same in
  blocks of 735, 500 or 1 (a frame's clock would have put the second
  step at 5880, 8.3 ms late); a slide into a rest is none.
- **B2, DONE (2026-09-24)**: `Voice_tb303` in `music_voices`. The facts:
  Robin Whittle's "303 unique" (the accent's shortest decay, the accent
  sweep's 47k and 1 uF charging, 100k and a 100k pot draining, a run of
  accents climbing; the volume envelope's long fixed decay; env mod
  never none), Tim Stinchcombe's diode ladder, the Devil Fish's page
  (the cutoff to about 2.5 kHz, doubled by the mod); commonly quoted,
  ours to check: the Decay knob's 200 ms to 2 s, the 60 ms slide, the
  half-step gate. A `Vco` (saw or square), the diode ladder, the main
  envelope, the sweep capacitor into the cutoff and the volume, the
  volume envelope, the slide (`Voicing`'s glide, 60 ms when sliding, a
  jump otherwise); the sequencer inside, the keys a monophonic line
  (last note); patterns as text ("C2 C2~ C3* ."), the knobs through
  `Patch_text` and a "pattern" line; our patterns acid, bass, accents.
  Tests (`Unit_tb303`, at 120 BPM): an accented note's envelope -60 dB
  at 200 ms, a normal one's at Decay 0.5 -19 dB; three accents, the
  sweep's peaks 0.283, 0.374, 0.402, climbing; a slide C2 to C3 63% of
  the way at 60 ms; the gate's 3 ms close, 60 dB down 25 ms later; the
  patterns as text. A golden WAV per pattern, the spectrograms looked
  at (each note's resonant peak falling with its envelope: the squelch).
  Two test mistakes of mine on the way, the code right: a one-step
  pattern repeats (its envelope started again at 125 ms), and a 3 ms
  close can't be -60 dB at 10 ms.
- **B3, DONE (2026-09-24)**: TinyTB303. The 303's knobs in its order and
  the waveform, tempo and volume; run and stop (space too); the
  pattern as a grid instead of the 303's blind keypad: 16 columns, a
  piano roll of an octave above C2 (a click sets a step's note, the same
  cell again a rest), then each step's octave, accent and slide
  toggled, the step playing lit; the letters playing along; the scope
  and spectrum. Golden frames: stopped, and `running` (RUN clicked,
  step 5's rest made a G2 by a click, that step lit as it plays); its
  web page; `CATALOG.md`'s row. Left, in its header's exercises:
  pattern chains, the gate's and slide's knobs (the Devil Fish's),
  swing, the effects rack after it, saving patterns.
- **O1, DONE (2026-09-24)**: `Tape`, in `audio/` beside `Mixer`, from
  Teenage Engineering's tape-mode guide (6 minutes, 4 tracks, the speed
  turned even while recording, recording always overdubs, reverse, loop
  in and out, lift and drop). The head moves `speed` samples a sample;
  the tracks read at it through `Resample` (linear) and mixed at their
  levels; the input added at it, spread over the two samples around it
  (no filter: the lesson, a real head's gap a low-pass for free); loop
  points; the ends stop it; lift leaves silence, drop adds at the head.
  Tests (`Unit_tape`, a 344.5 Hz phrase of a second): played at 1, the
  phrase exactly; at 2, bin 64 (an octave up) and silent after sample
  22,049; at -1, the samples reversed, then stopped at the start;
  recorded at 0.5 and played at 1, bin 64 again, over 22,050 samples
  (the last input sample written at 22,049.5, half spilt onto the
  next); overdub doubling the track; lift and drop moving a piece, twice;
  a loop of 100 samples still going round after 1,000. Left for O3: the
  transport on the panel, split and join.
- **D1, DONE (2026-09-24)**: TinyDX7 moved before TinyRhodes (the
  OP-1's FM engine will be its voice). The facts from Dexed's msfa
  (above, in D1-D3's entry). `Dx_envelope`: the rate/level envelope in
  Dexed's steps (256 a doubling), the attack's jump to 1716 and its
  curve, the decay's line; tests: an R1 99 attack 33 samples, a decay
  99 to 0 at R2 50 61,184 samples (-64.8 dB after a second), from 50
  36,608 (a rate a speed), a release from L3 80 at R4 60 17,323, all
  as computed by hand from the constants. `Fm_algorithm`: the 32 as
  (modulator, target) pairs, carriers and the feedback's (from, to),
  checked against Dexed's byte table decoded in the test; algorithms
  4 and 6's loops through two operators kept (Dexed runs them as 6 on
  itself); six operators run a sample at a time; tests: 2 on 1 is
  `Fm.render`'s pair to 1e-9; a lone operator at full, 431 Hz: fb 5 a
  darkened sawtooth (harmonics 2-5 at -7.2, -11.6, -14.8, -17.4 dB,
  the noise 58 dB down), fb 6 buzzing (-7.7), fb 7 noise 5.5 dB above
  the harmonics -- measured, and the DX7's way to noise. `Polyphony`
  step two: `?voices`, the oldest released stolen, else the oldest
  held (cut, a click: its exercise); tested on two voices and on 17
  notes into 16.
- **D2, DONE (2026-09-24)**: `Voice_dx7` in `music_voices`. The patch
  as the DX7's own parameters (145 and the name), read from and written
  to the 128 packed bytes (Dexed's layout; out-of-range clamped); a
  cartridge read (the header, the length, the checksum, errors as
  values) and written (filled to 32 with INIT VOICE); the patch as
  text. Dexed's formulas: the ratio and fixed frequencies, the detune
  (+6.8 cents at A4, +14.7 at A1: nearly even in Hz), the keyboard level
  scaling (-lin 99 three octaves away 95 steps, +exp 15), the velocity
  (Dexed's table: +224 steps at 127, -448 at 64, sensitivity 7), the
  rate scaling, the pitch envelope's tables, the LFO's speeds and its
  two-speed delay, the pitch modulation; the amplitude modulation ours
  (linear in dB, to silence at full: Dexed's own is marked "needs some
  real tuning"). 16 voices (`Polyphony`'s stealing); a block of 64: the
  pitch, the LFO and the envelopes (`Dx_envelope.run`) moved once a
  block, the gains ramped between, as Dexed does. Our six patches (an
  electric piano on algorithm 5, a brass on 22, a bass on 1, bells and
  a marimba on 5, an organ on 32), their brightness measured as the
  spectrum's centroid (the brass's swell 367 to 2568 Hz in 0.1 s, the
  bass's pluck 504 to 141), a golden WAV each, peaks 0.28 to 0.69 on a
  riff of four-note chords. The cost, 16 voices sounding: 0.65 s of CPU
  a second of sound at first, 0.21 after (the modulators found once
  per algorithm, the gains once a block, the feedback's history two
  numbers); in JavaScript 0.73 s (4 voices 0.18): the web page's voice
  count to set in D3. Left: the Reface DX's reduction (4 operators, its
  own 12 algorithms and feedback on every operator) as a header
  exercise, its facts to look up; key sync off (every note starts at
  phase 0); the pitch bend and the modulation wheel (with the MIDI
  keyboard).
- **D3, DONE (2026-09-24)**: TinyDX7. The DX7's front: its two-line LCD
  (the voice, the parameter and its value), < PARAM > and < OP > (an
  operator at a time) through the 145, the data slider, -1 and +1 --
  the lesson, why every synthesizer after it had knobs again. Beside it
  what the DX7 hid: the algorithm drawn as its graph (the carriers in a
  row, each modulator above its first target, the feedback's loop),
  each operator lit by its amplitude on a 60 dB scale, a click on one
  choosing its output level; the six envelopes drawn (each segment its
  time, square-rooted). The keyboard's velocity where the mouse presses
  a key (0.2 at its back to 1 at its front: FM's timbre follows it); the
  spectrum and the scope. `cart=` a .syx, fetched (natively for now),
  its voices after ours, < and > stepping through them (a menu of 38 is
  taller than the screen). Golden frames TinyDX7 and TinyDX7_playing
  (OP > clicked, C E G held: the carriers lit, the tine's partials);
  its web page; `CATALOG.md`'s row. 16 voices on the web too, to check
  in a browser (0.73 s of CPU a second for 16 sounding in node). Left,
  in its header's exercises: the Reface DX's mode, saving voices and
  cartridges, the operators switched on and off, a MIDI keyboard.
- **R1, DONE (2026-09-24)**: `Modal` in instruments/ (a two-pole
  resonator struck: its peak, frequency and t60 as set, damped while
  it rings, two strikes the sum: linear), ready for the 808's drums
  too. `Voice_rhodes` in `music_voices` (the name the start of the
  `Voice_xxx` renaming, next): the Rhodes' tine as three modes (1,
  6.27, 17.55) through the pickup's bell (a Lorentzian, ours), the
  output its rate of change scaled by the period; the Wurlitzer's reed
  through 1 / (1 - x); the Clavinet as `Pluck`; the dampers; the
  Suitcase's stereo tremolo, the others' loudness. Measured, C4: the
  2nd harmonic -24.8 dB at velocity 0.2, +1.2 at 1 (the bark); voicing
  near the centre +10.5, off it -14.3 (Pfeifle's figure 7, heard); the
  Wurlitzer's harmonics -25.8 to -11.5; the tremolo's sides 3.3 to 1.
  My mistake on the way, and the lesson it makes: the upper modes set
  as if the pickup read position peaked at 5.7 -- it reads a rate of
  change, so a mode 6.27 times higher is 6.27 times louder. Golden WAVs
  of the five presets (their brightness falling after the strike, the
  dampers at the release). Not done: the tine's arc (the paper's), its
  two polarisations, the hammer's contact as a force (Hunt and
  Crossley's), the Clavinet's own pickups.
- **The voices renamed (2026-09-24)**: `Voice_minimoog`,
  `Voice_hammond`, `Voice_tb303`, `Voice_dx7`, `Voice_rhodes`, as the
  office suite's `Part_xxx`, each checked against `Voice.S` (a module
  type only: the patch, its knobs and text, the presets; the player's
  patch, `Instrument.t`, recent samples) in the music tests -- what
  TinyOp1's engines will be.
- **R2, DONE (2026-09-24)**: TinyRhodes. The Stage 73's knobs (the
  model, voicing, hammer, decay, the Suitcase's rate and depth,
  volume); what the real one hides, the pickup's curve against the
  tip's position (the Rhodes' bell, the Wurlitzer's 1 / (1 - x)) and on
  it the span the last note's tip swept over the last block
  (`Voice_rhodes.span`: a tip sampled once a frame would jump; its span
  is what teaches -- short on the bell's side soft, across its top
  hard); the Suitcase's two speakers lit by the pan. The keyboard's
  velocity by where the key is pressed; spectrum and scope. Golden
  frames TinyRhodes and TinyRhodes_playing (C E G held, the swing
  across the bell's top); web page; `CATALOG.md`'s row. The model
  names shortened (Rhodes, Wurlitzer, Clavinet) for the selector's
  labels. Left, in its header: the Reface CP's effects row, the tine's
  arc, a sustain pedal, the 88 keys.
- **C1, DONE (2026-09-24)**: `Voice_cs80` in `music_voices`, a
  `Voice.S`. Two sections per key over `Vco` (sawtooth and a pulse 50
  to 90% with its own PWM, noise), `Svf` (a high-pass then a low-pass,
  their Q kept under 4: no self-oscillation), `Envelope` (the VCA's
  ADSR) and the filter envelope's IL and AL (ours: straight lines, IL
  to AL, down to the cutoff set, back to IL); the pure sine after the
  filters; the touch per section (velocity and pressure into
  brilliance and level, ours: 3 octaves each), `pressure t key p` each
  key's own, `bend t semitones` the ribbon; the sub-oscillator (`Lfo`)
  into VCO, VCF and VCA; the ring modulator with its attack and decay;
  section II's detune; `Modulated_delay`'s chorus, a tremolo; 8 voices.
  The voice's blocks 64 samples at a time (the LFO, the pressures, the
  ribbon read between them). Measured: E pressed in C E G, its 5th
  harmonic 31.0 dB up, C's and G's within a dB; the ribbon's B4; no
  sound after the release at the resonance's most. Our five presets
  (brass after Blade Runner, strings, pad, ring bells, lead), golden
  WAVs; the bells lengthened after their loudness was read (-96 dB at
  0.5 s at first). The cost 0.33 s of CPU a second for 8 voices
  natively, 0.84 s in JavaScript, then 0.17 and 0.35 with the envelope
  and the width once a chunk and the unused noise and sine skipped.
  Not done: the initial touch's pitch bend (a slide from a semitone
  below), portamento, the expression pedal's wah.
- **C2, DONE (2026-09-24)**: TinyCS80. The panel as the CS-80's rows:
  each section's sound (feet, saw, pulse, width, PWM, noise, the two
  filters, the sine) then its shapes and touch (IL, AL, the envelopes'
  times, level, velocity and pressure into brilliance and level), the
  shared row (mix, detune, the sub-oscillator, the ring modulator,
  chorus, tremolo, volume). The touch played by the mouse: pressed on a
  key, its velocity where (as TinyRhodes); dragged down while held, its
  pressure (120 pixels for all of it), a bar on the key; a key already
  held on the letters pressed harder, not struck again -- a chord on
  the letters, one note pushed by the mouse, only it swelling. The
  ribbon, a strip over the keys: from where it's first touched, an
  octave across its width, back to 0 when let go. Spectrum, scope.
  Golden frames TinyCS80 and TinyCS80_playing (C E G held, E pressed to
  0.75, three voices); web page; `CATALOG.md`'s row. Left, in its
  header: the letters' pressure, the initial pitch bend, portamento,
  the memories, a MIDI keyboard's polyphonic pressure.
- **J1, DONE (2026-09-24)**: `Voice_juno` in `music_voices`, a
  `Voice.S`. The facts from Andy Harman's Juno-60 measurements (above).
  The DCO over `Vco` (sawtooth, a pulse whose width is set or moved by
  the LFO, the sub a pulse at half the frequency, noise); the filter
  `Moog_ladder`'s nonlinear one, self-oscillating at the top, moved by
  the envelope (either way), the LFO and the key; one `Envelope` for
  both, its times Harman's fitted curves (attack 0.001 to 3.25 s, decay
  and release 0.002 to 17.5 s), the VCA a gate if asked; the LFO a
  triangle faded in over its delay; the high-pass one for the six
  voices, after them (the 106's four positions, one-pole: 1.86 at 30 Hz
  in position 0, 0.13 in 2, 0.04 in 3); the chorus its own (a triangle,
  the right line inverted, 1.66-5.35 ms at 0.513 and 0.863 Hz, 3.3-3.7
  ms at 9.75 Hz mono), each side the dry sound and its copy. Five
  presets (brass, strings, bass, pad, lead), golden WAVs, peaks 0.34 to
  0.66 (the gain first guessed at a quarter of that: the nonlinear
  ladder's loss and the chorus's half). 6 voices: 0.12 s of CPU a
  second natively, 0.30 in JavaScript. `Modulated_delay.mli`'s claim
  that the Juno's chorus had its right side a quarter turn ahead
  corrected (inverted, Harman measured). Not done: the 12 dB low-pass
  before the chorus's lines, the BBD's own grit, the voices assigned in
  turn (ours are fresh per note: nothing to rotate).
- **J2, DONE (2026-09-24)**: TinyJuno. The 106's panel as vertical
  sliders in its sections (LFO, DCO, HPF, VCF, VCA, ENV), drawn and
  dragged by the panel itself (a slider held follows the mouse; the
  high-pass's snapping to its four positions), and its buttons with
  their lights (the range, pulse and sawtooth, the width by the LFO,
  the envelope's polarity, the gate, the chorus off, I, II, I+II). The
  keys at full: the 106 has no velocity. Spectrum, scope. Golden frames
  TinyJuno and TinyJuno_playing (the cutoff slider dragged up, chorus
  II clicked, C E G held); web page; `CATALOG.md`'s row. Left, in its
  header: the 128 patches as banks and the 106's SysEx, the bender, the
  hold button, the 60's arpeggiator.
- **The classics done (2026-09-24)**: TinyMinimoog, TinyHammond,
  TinyTB303, TinyDX7, TinyRhodes, TinyCS80, TinyJuno. Next in the plan:
  the ReBirth line (TinyTR808, the 909, TinyReBirth), then TinyOp1 and
  TinyOpxy, the climax.
- **T1, DONE (2026-09-24)**: `Voice_tr808` in `music_voices`, a
  `Voice.S`. The facts from Werner, Abel and Smith (above). Eleven
  instruments: the kick and toms a sine whose frequency the model
  moves (2.2 times higher its first 6 ms: the punch; then 12% of its
  level above its own: the sigh), the tone a low-pass on the trigger's
  click; the snare two `Modal` tones and high-passed noise (the
  snappy); the rim shot two short tones; the clap band-passed noise in
  three bursts and a tail; the six squares (`Vco`, 47.98%) through the
  two band-passes (`Svf`, 3440 and 7100 Hz) for the hats and cymbal,
  the 540 and 800 Hz ones for the cowbell; the closed hat choking the
  open. The sequencer `Sequencer` itself, each step a `Note_on` with its
  accent, the tracks as "x..." lines; the General MIDI drum keys for
  live playing. Four patterns of ours (electro, house, hip hop, latin),
  golden WAVs. Measured: the sigh 52.7 Hz early, 49.6 late; the closed
  hat 90% above 5 kHz; the choke; the same samples in blocks of 735 or
  100 -- which took three fixes, each found by that test: the squares
  always running (stopped between hits, their phases depended on the
  blocks: and the 808's never stop), each hit's own noise (one shared
  generator drawn a segment at a time), a hit's end decided at its
  sample; the tempo a selector of whole BPMs (127 came back 126.96 as
  a knob's three decimals). Cost: the busiest pattern 6% of a CPU
  natively, 12% in JavaScript. Ours and not yet checked against the
  service notes: the snare's, toms', rim shot's, clap's and cowbell's
  frequencies, the decays; not done: congas, clave, maracas (the 808's
  switch-selected alternates), the bridged-T's own retriggering pulse.
- **T2, DONE (2026-09-24)**: TinyTR808. A column per instrument, its
  knobs (the 808's: the kick's level, tone and decay; the snare's
  level, tone, snappy; a tom's level and tuning; ...) over its name, a
  click on the name striking it and choosing its track (AC the
  accents'); the 16 step buttons in the 808's red, orange, yellow and
  cream, four by four, their lights the track's hits, the step playing
  lit; start/stop and space, the tempo (whole BPMs), the accent, the
  volume. Under the panel what the 808 never showed: the whole pattern,
  a row per instrument and the accents, its cells clicked to toggle, the
  step playing a column lit. The letters strike the drums live. Golden
  frames TinyTR808 and TinyTR808_running (space, a kick added on step
  16, step 6 lit); web page; `CATALOG.md`'s row. Left, in its header:
  patterns chained into a song (A/B, rhythm track), saving them, the
  swing, the alternate instruments.
- **N1, DONE (2026-09-24)**: the 909 in `Voice_tr808`, a machine switch
  (`label` giving the 909's RD and CR in the 808's CB and CY slots).
  The kick and toms a sine whose pitch falls from high (`sweep_drum`:
  the kick from 2 + 5 x its tune times 50 Hz, e-fold in 12 ms) with the
  attack's click; the snare two tones and low-passed noise; the hats,
  crash and ride from 6-bit ROMs, our own recordings (inharmonic
  `Modal` partials and a hiss, normalized, quantized), played through
  `Resample` at their tune. Shuffle (the even steps up to a third of a
  step late) and flam (twice, 10 to 40 ms apart) for both machines: the
  hits now scheduled at their samples, a list carried across blocks.
  Measured: the kick 107.8 Hz on average 5-30 ms, 50.0 later; the ROMs'
  26, 28, 35 levels; an open hat's centroid 9611 to 11541 Hz tuned up;
  the shuffle's 5513 to 7351; the flam's second hit; the 909 shuffled
  and flammed, the same samples whatever the blocks. Two patterns (909
  house, 909 techno), golden WAVs; the 808's four unchanged. The
  samples' levels raised after measuring each instrument alone (10 to
  15 dB under the 808's metal at first: a recording normalized to its
  peak, which a few modes in phase make high), the crash less.

## Verification

- `make test`: each `.mli`'s worked example, the filters' measured
  responses, the effects' measured times and curves, golden WAVs of
  the presets and effects, golden frames of the panel, the catalogue.
- By ear: the presets, the switches' simple vs better versions, on
  native and in a browser -- a person's job; the checks here are on
  dumped WAVs, measured.
- Performance: one Minimoog voice with its effects in a small part of
  a frame on native (a budget to measure in phase 4), so the Juno's six
  voices fit later.

## Out of scope

- A modular synthesizer with patch cords (the Moog modular, VCV Rack):
  an exercise once the blocks exist, its lesson the patch as a graph.
- Circuit-level modelling (the Minimoog's schematics solved as
  equations, SPICE-like): the plan stops at Huovilainen's model.
- Plugin formats (VST, AU, LV2), audio input, a DAW.
- Sampled instruments (the Mellotron, the Fairlight): `AudioSampler`
  and a MOD player are the audio plan's. TinyRhodes and TinyHammond
  are models, not recordings: that is their lesson.
