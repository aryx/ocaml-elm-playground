# Plan: what's left for the synthesizers

The synth plan is done: see
[`done/plan_synth_teaching.md`](done/plan_synth_teaching.md) (the
blocks in `audio/instruments/` and `audio/effects/`, the voices of
`music_voices` behind `Voice.S`, and the Tiny instruments from
TinyMinimoog to TinyOpxy, phases 0-12, O1-O4, X1-X4, T1, C1) and its
tutorial, [`notes_synth.md`](../tutorials/notes_synth.md), whose table
lists every module. What's left, roughly from most to least worth
doing, each item where a header already names it as an exercise. As
before: the facts first (a manual, a paper, a measurement), what isn't
documented ours and said so, a worked example in the `.mli` tested,
golden WAVs and frames approved after listening and looking.

## 1. What every instrument lacks

- **Saving**: patches, patterns, registrations, cartridges, songs --
  the text is there (`Patch_text`, each voice's `to_string`), the File
  menu too (`appkits/file_menu`, TinyOffice's). One step for all the
  instruments at once: a File menu in each panel, the `.patch` text,
  and the formats of the originals where they are documented (the
  DX7's `.syx` written back, `Voice_dx7.to_cartridge`; the Juno-106's
  SysEx; ReBirth's `.rbs`).
- **A MIDI keyboard**: velocity (the DX7's timbre, the Rhodes' bark),
  pitch bend, the CS-80's polyphonic pressure, the Juno's bender -- the
  panels' exercises, all waiting for input. The browser has Web MIDI;
  natively, a `Cap.midi` and ALSA's raw MIDI (`plan_caps.md`'s way).
  `Instrument.t` already takes a velocity; aftertouch and bend need a
  second entry point (`Instrument.set` by name would do).
- **The effects rack after a voice**: the Reface CP's row (drive,
  tremolo or wah, chorus or phaser, delay, reverb) on TinyRhodes and
  TinyReface's CP; the 1988 acid house chain (a delay and a
  distortion) after TinyTB303. The blocks exist (`Rack`); it's the
  panels' wiring.

## 2. The next music programs

The seeds of a new plan (apps/music's dune file lists them), each with
its original:

- **A sequencer** (Cubase, 1989): tracks of MIDI notes on a timeline, a
  piano roll, `Midi`'s reader and `Music.render_score`, and now our
  instruments as its tracks' sounds -- the natural next step after the
  grooveboxes.
- **A sound editor** (SoundEdit, 1986; Audacity): a waveform to cut,
  fade and filter, its spectrum (`Spectrum`), what `Tape` and `Sampler`
  record.
- **A score editor** (Finale, MuseScore): ABC in, staff notation out.
- **A pedalboard**: the effects alone, on a recording (the microphone
  is out of scope below).

## 3. The OP-1 and the OP-XY, the rest

- **The OP-1**: the DNA engine ("CPU id noise synthesis": filter, wave
  number, wave modifier, noise); drum mode (the drum sampler: note,
  in, out, loop off/once/on, reverse; the d-box); the tape's lift and
  drop as buttons (`Tape.lift`/`drop` exist) and its loop; the mixer
  page; the sequencers (pattern, endless, tombola, sketch, finger,
  arpeggio); the effects grid, phone, CWO; the LFOs bend, crank,
  element, MIDI, random.
- **The OP-XY**: the ten other step components (velocity, ramp up and
  down, random, portamento, bend, tonality, jump, skip a lock, skip a
  component); the engines axis, dissolve, epiano, prism; the filter's
  envelope and key tracking, the second envelope; the LFOs (element,
  random, tremolo, value); the effects sends FX I and II and the
  punch-in effects; patterns longer than a bar (up to 64 steps, the
  track's scale); songs, scenes in order; the brain detecting the key;
  the auxiliary tracks. And the multisampler (24 zones).
- **Their oscillators**: voltage's, hardsync's and simple's are naive
  (aliasing at high notes): `Oscillator`'s PolyBLEP, the same lesson
  as the band-limited VCO. The sampler pitched up folds its highs: a
  low-pass first (`Resample.mli`'s exercise). The loop's crossfade is
  linear: an equal-power curve beside it, measured on two noises.

## 4. The classics, the rest

From each panel's header:

- **TinyMinimoog**: the reissue's additions (a separate LFO, note
  priority chosen, the filter contour as a modulation source); a
  second voice (duophonic, as the ARP Odyssey).
- **TinyHammond**: the lower manual and the pedals; drawbars heard
  while a note sounds; the Leslie's brake.
- **TinyTB303**: the locks drawn as the knob's curve over the bar; a
  lock cleared per knob; patterns chained into a song; the gate length
  and slide time as knobs (the Devil Fish); swing.
- **TinyDX7**: the Reface DX's mode (four operators, twelve
  algorithms, a feedback per operator); the operators switched on and
  off.
- **TinyRhodes**: the tine's arc and its two polarisations (Pfeifle's
  model); a sustain pedal; the 88-key range.
- **TinyCS80**: the initial touch's pitch bend; portamento; the four
  memories.
- **TinyJuno**: the 128 patches as banks; the hold button; the Juno-60's
  arpeggiator.
- **TinyTR808**: patterns chained into a song (its A/B variations, the
  rhythm track mode); swing; the congas, clave and maracas.
- **TinyReBirth**: the song mode; a pattern edited in place; each
  machine's effects sends.
- **TinyReface**: the YC's transistor organs (dividers and squares: a
  subtractive organ beside the additive one); the CS's five oscillator
  types; the looper (`Tape`, one track).

## Out of scope (as before)

- A modular synthesizer with patch cords (the Moog modular, VCV Rack):
  an exercise once the blocks exist, its lesson the patch as a graph.
- Circuit-level modelling (the schematics solved as equations,
  SPICE-like).
- Plugin formats (VST, AU, LV2), audio input.
