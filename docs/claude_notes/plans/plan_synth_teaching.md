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
  and/or noise; the 2016 reissue adds the filter envelope as a source)
  to the oscillators' pitch (vibrato, or sweeps) and/or the filter's
  cutoff (growl, wah).
- **Monophonic**: one voice, low-note priority, glide (portamento) as
  a time constant.

Checked in phase 4 against the documentation (see its status entry
below, and `Minimoog_voice.mli` for which ranges are the Model D's and
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

- `Minimoog_voice.ml` (`.mli`): the Model D's signal path over the
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
- **Phase 1, DONE (2026-09-23)**: the sources. `Oscillator.pulse` and
  `pulse_band_limited` (any width, two PolyBLEPs, the average 2w - 1
  taken away so PWM doesn't thump); the live oscillator is its own
  module, `audio/Vco`, rather than more of `Oscillator` (the waveforms'
  formulas stay there): a frequency and a width per sample, and hard
  sync, the master recording where in each step it wraps, the slave
  correcting the sample before and the one after at that fraction, its
  own phase-0 jump's correction left out where the restart replaced
  it. `audio/Lfo` (six shapes, naive on purpose, sample and hold,
  `of_tempo`); `audio/Drift` (Ornstein-Uhlenbeck, stepped every 64
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
  thousandth of the distance in theirs). `audio/Voicing`, mono: the
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
  together as `Minimoog_voice` will be. Polyphony (voice stealing) left
  for the Juno.
- **Phase 3, DONE (2026-09-23)**: the filters. `audio/Moog_ladder` (not
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
  exactly at its threshold, a step sets off an undamped ring. `audio/Svf`,
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
  whistle preset too quiet, 0.06). `apps/music/Minimoog_voice` (library
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
  `playground/Gui.ml`, printed by the gui and gui4 tests; `Theme` a
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
  placed by name over `Minimoog_voice.knobs`, the rotary switches with
  short labels; a two-octave keyboard played with the mouse too, the
  pitch and mod wheels; an oscilloscope and a spectrum of the voice's
  last 2048 samples (`Minimoog_voice.recent`, since `Audio_debug` is
  fed only by the software backend). Golden frames: at rest, and
  `playing` (the cutoff knob dragged up 40 pixels by a script, 0.32 to
  0.52, C3 and E3 held). Left: saving patches with the File menu (an
  exercise in the header), the panel's own knob look (cream skirts, as
  the Model D's) and the wood's grain.

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
