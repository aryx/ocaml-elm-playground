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
"The collection" below): the TB-303, the DX7, the Juno.

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
  and/or noise, *or* the filter envelope) to the oscillators' pitch
  (vibrato, or sweeps) and/or the filter's cutoff (growl, wah).
- **Monophonic**: one voice, low-note priority, glide (portamento) as
  a time constant.

To check against the Model D's owner's manual and schematics when
writing each module (the ranges, the priority, the curves).

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

(* playground/Audio.mli, Evan-style *)
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
`Minimoog_voice` will be one more maker; a generic `Patch` that
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

- **`Ladder`**: the Moog ladder, the heart of the plan. Four one-pole
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

- `Minimoog_voice.ml` (`.mli`): the Model D's signal path over the
  blocks above -- three `Oscillator`s with `Drift`, a mixer with noise,
  the `Ladder`, two `Envelope`s, `Voicing` (mono, low note, glide), the
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

A **TinyVirtualSynth** hub (the collection in one program, as
TinyOffice gathers the office apps) only if the four make it worth
it; a **pedalboard** app (the effects alone, on a recording or the
microphone, which is out of scope) likewise later.

## Phasing

0. **The notes and the groundwork**: `notes_synth.md` written ahead
   (the Minimoog's path, each block's idea, the effects), the
   related-work section; the streaming processor pattern decided and
   written in one module's `.mli` as the model; `Mixer.instrument` and
   `Audio.instrument`, `note_on`, `note_off`, `set`, with a trivial
   instrument (one sine) to test them: a scripted run's dumped WAV has
   its notes at the right frames.
1. **Sources**: `Oscillator`'s pulse, PWM, triangle-saw, hard sync,
   streaming; `Drift`; `Lfo`. Tests: the pulse's harmonics (a width of
   1/3 has no 3rd, 6th, ...), sync's aliases under -60 dB below 5 kHz,
   an LFO's period, S&H's seeded values.
2. **Envelopes and voicing**: the streaming, exponential `Envelope`;
   `Voicing` mono (priorities, legato, glide). Tests: the attack reaches
   1 in its time, the release from wherever the level was, the three
   priorities on the same chords, glide's 63% at its time constant.
3. **The ladder**: the three versions, the tests above, golden WAVs (a
   sawtooth swept, at three resonances; the self-oscillation), plotted
   before approving. Then `Svf`.
4. **TinyMinimoog, the sound**: `Minimoog_voice`, the patches, the
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
  and a MOD player are the audio plan's.
