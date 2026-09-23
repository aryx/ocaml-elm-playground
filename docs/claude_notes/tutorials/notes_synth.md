# Synthesizers, from scratch: a tutorial for virtual analog and effects

How the classic analog synthesizers made their sound, how a program
reproduces them (what Arturia's V Collection, Native Instruments'
Monark or u-he's Diva do), and what a sound goes through on its way
out: drive, EQ, chorus, delay, reverb, compression. The sequel of
[`notes_audio.md`](notes_audio.md), whose ideas it takes as known:
samples and Nyquist (§2), band-limited oscillators (§3, §6), ADSR (§4),
decibels and tanh (§5), the spectrum (§6), filters and resonance (§7),
delay lines (§8).

Written before the code, as its specification (see
[`plan_synth_teaching.md`](../plans/plan_synth_teaching.md)), like
`notes_audio.md` was; the numbers of what is built are the tests', the
others are to check when their module is written. Companion:
[`notes_audio_related_work.md`](../related-work/notes_audio_related_work.md),
Part 7 (the synthesizers, their virtual versions, the books).

The thread through it: **an analog synthesizer is a signal path you
can read**, left to right on its panel -- something makes a sound,
something shapes it, something decides how loud it is, and slower
things turn the knobs of the fast ones. Each box is one idea, and one
module here.

## 0. Where the code is, and a reading order

| module | what | section | status |
|---|---|---|---|
| `audio/Instrument` | a sound played live: events, blocks, ramps | §1 | done |
| `audio/Oscillator` (extended) | the pulse and its width | §3 | done |
| `audio/Vco` | the live oscillator: a frequency and a width per sample, hard sync | §3 | done |
| `audio/Drift` | analog imprecision | §3 | done |
| `audio/Lfo` | the slow oscillators that turn knobs | §4 | done |
| `audio/Noise` (extended) | random numbers for them: a linear congruential generator | §4 | done |
| `audio/Envelope` (extended) | gated, exponential | §4 | done |
| `audio/Voicing` | keys to a voice: priority, legato, glide (stealing: later) | §5 | done (mono) |
| `audio/Moog_ladder` | the Moog filter: naive, zero-delay, nonlinear | §6 | done |
| `audio/Svf` | the state-variable filter: Chamberlin's, zero-delay | §7 | done |
| `audio/Drive`, `Filter` (EQ) | gain, waveshaping, oversampling; shelves, peaks | §8 | |
| `audio/Modulated_delay`, `Phaser` | chorus, flanger; phaser | §8 | |
| `audio/Delay`, `Reverb` | echoes in time with the music; rooms | §8 | |
| `audio/Dynamics` | compressor, limiter, gate | §8 | |
| `audio/Rack` | effects in an order | §8 | |
| `apps/music/Minimoog_voice` | the Model D's signal path | §2 | done |
| `apps/music/TinyMinimoog` | its panel | §9 | done (patches not saved yet) |
| `playground/Audio`'s instruments, `Mixer.instrument` | playing one from a game or an app | §1 | done |

## 1. Playing live: instruments

Everything in `notes_audio.md` is **rendered ahead**: a sound is a
value, its samples computed whole when played (`Synth.render`), a
tune rendered once and looped. A synthesizer can't work that way:
when a key goes down, nobody knows yet when it will come up, and
someone may turn the filter's knob in between. So an **instrument** is
a *process*: a state, changed by events, asked for its next block of
samples (`Instrument.mli`):

```
   update, 60 a second            the sound card, 44,100 a second

   note_on 60 ----.
   set "volume" --+--> [ state ] --fill--> the next n samples --> Mixer
   note_off 60 ---'       ^  |               (a pull: 735 a frame)
                          '--'
                   kept from block to block
```

Three rules, which every live block of this tutorial follows:

- **The state is mutable and private.** The oscillator's phase, the
  filter's memory, the knobs: a record of mutable fields behind the
  `.mli`, as the DSP books write them and as a sound card pulls them --
  a block computed in place, nothing allocated per sample. (The
  functional alternative, a new state returned per block, is a copy of
  every delay line 60 times a second: a reverb's is 100 KB.)
- **An event lands between blocks.** A key pressed during frame k is
  heard from the next block: in a golden run (a pull of 735 samples a
  frame) at exactly sample 735 k -- the test plays A4 at frame 10 and
  finds silence until sample 7350 and the note in frame 10's block.
  Natively the queue kept ~50 ms ahead adds its latency
  (`notes_audio.md` §10); a frame's 16.7 ms granularity is below what a
  keyboard player notices, but not below what a *sequencer* would:
  the TB-303's steps will run in the audio clock, not in `update`.
- **A knob is ramped, not jumped.** A volume going from 0.2 to 0.8 at
  once is a step in the wave, a click; a knob turned slowly is a step
  every block, a click 60 times a second: **zipper noise**, the sound of
  the first digital synthesizers' 7-bit knobs. So a knob's new value is
  reached over the block, sample i (from 0) of a block of 735 getting
  0.2 + 0.6 (i + 1) / 735: neighbours 0.00082 apart instead of 0.6 once
  (tested on C1, whose own slope is at most 0.0047 a sample: the
  largest step measured stays under their sum). A note's **gate** the
  same, over 5 ms: 0 to full in 221 samples, and back when let go.

An instrument is a record of four functions, `note_on`, `note_off`,
`set` and `fill` -- an object in all but name, because the mixer holds
instruments of different kinds and wants the same four things from
each. The mixer sums a block of each into its pull
(`Mixer.instrument`); `stop` fades one out over a pull, like a loop.
A game or an app plays one by name, Evan-style (`Audio.instrument`,
`note_on`, `note_off`, `set`); the smallest one, `Instrument.sine`, a
single sine playing the last key pressed, is the model the others
follow.

The shape is the one every plug-in API has had since Steinberg's VST
(1996; VST 2's `processReplacing`, 1999, a block in and a block out, in
place): a host calling a process with blocks, events between them,
parameters smoothed inside.

## 2. Subtractive synthesis: the Minimoog's signal path

**Subtractive synthesis**: start from a wave rich in harmonics (a
sawtooth has all of them, 1/k loud; a square the odd ones), then
*take away* with a filter whose cutoff moves: bright as a note starts,
darker as it goes on -- the way most acoustic sounds behave, which is
why the recipe works. The Minimoog Model D (Moog Music, 1970), the
first synthesizer made to be played on stage rather than patched in a
studio, fixed the path left to right on its panel:

```
 oscillators --> mixer --> filter --> amplifier --> out
  1  2  3        + noise    ^           ^
  (3 also an LFO)           |           |
                      filter contour  loudness contour
                         (an ADS)       (an ADS)
         ^                  ^
         '------ modulation (osc 3 / noise), by the mod wheel
 keyboard: pitch (monophonic, low-note priority, glide), gate
```

- three **oscillators** (VCOs), each with a range in organ feet (LO,
  32', 16', 8', 4', 2': octaves) and six waveforms (triangle,
  triangle-saw, sawtooth, square, wide and narrow pulses), the second
  and third detunable: two saws a few cents apart **beat**, the thick
  sound;
- a **mixer**: their levels, and noise, white or pink (white only
  here: pink noise is `notes_audio.md` §12's exercise);
- the **ladder filter** (§6), -24 dB per octave, its cutoff, its
  "emphasis" (resonance), its "amount of contour" (how far the filter
  envelope opens it) and its keyboard tracking (a third, two thirds, or
  all of the note: a high note's harmonics not all cut);
- two **contour generators**, attack, decay, sustain, the "decay" switch
  making the release the decay (no release knob), their curves those of
  a capacitor charging (§4);
- **modulation**: oscillator 3 can leave the keyboard and run slowly,
  an LFO; the mod wheel sends it (or noise) to the oscillators' pitch
  (vibrato, trills) and the filter's cutoff (growl, wah).

`Minimoog_voice` is that path, over the blocks of the sections below,
and the knobs' ranges are the Model D's where its documentation gives
them: the contours' attack from 1 ms to 10 s and decay from 4 ms to 35
s, the glide from 1 ms to 10 s, the filter contour up to 4 octaves,
the tracking a third, two thirds or all of the note, oscillators 2 and
3 detuned 7 semitones either way, the pitch wheel a fifth. Oscillator
3's six waveforms swap the shark tooth for a *reverse* sawtooth, the
ramp an LFO wants; the shark tooth itself is a passive mix on the
waveform switch, about three quarters triangle and a quarter
sawtooth. The modulation mix blends oscillator 3 and noise (the 2016
reissue adds a separate LFO, the filter contour as a source, and a
choice of note priority: not here). Where nothing says, the choices
are ours and said so in its `.mli`: LO 6 octaves under 8' (oscillator
3 off the keyboard then runs at 2.7 to 6.1 Hz, a vibrato's rates), the
wide and narrow rectangles 30% and 10% wide, the modulation's depths
(12 semitones and 3 octaves at the wheel's top), the emphasis knob
reaching k = 4.5 (oscillating from 0.89 of its turn).

Two measurements that are the Model D's character: the **mixer
overloads the filter** -- three sawtooths at a tenth of their level
come out at 0.123, at a third 0.312, at full 0.446: 3.3 times the input
gives 2.5 times the output, the next 3 times only 1.4, the ladder's
tanh (§6) thickening the sound instead; and with no oscillator at all,
the **filter played as an oscillator** -- the "whistle" preset, the
emphasis at its top and the tracking full, sings C3, C4, G4 and C5
within 2.5 cents. A patch is the panel's positions, written as "name =
value" lines, and the presets (bass, lead, brass, flute, whistle,
wind) are our settings in that text; each plays the same riff as a
golden WAV (`apps/music/tests/`).

## 3. Oscillators for a synthesizer

`Oscillator` already has the four waveforms, naive and band-limited
(`notes_audio.md` §6: PolyBLEP corrects each jump over two samples).
A synthesizer needs three more things, and an oscillator run live,
`Vco` (the voltage-controlled oscillator), a block at a time with a
frequency *per sample*, so a vibrato or a glide moves it smoothly
within a block.

**The pulse, and its width.** A pulse is high for a fraction w of its
period: w = 0.5 is the square. Its harmonic k is (4 / (pi k)) |sin(pi
k w)| loud, so a width of 1/3 has no 3rd, 6th, 9th harmonics (the
others 1.103, 0.551, 0.276 for the 1st, 2nd, 4th: measured within 3%,
PolyBLEP dulling the 4th by 2.7%), a width of 1/2 no even ones: the
width *is* the timbre, thin and nasal as it narrows. Band-limited, it
is two PolyBLEPs, at phase 0 (up) and at w (down). Its average, 2w - 1,
is taken away, as an analog oscillator's output capacitor does: so
that **pulse-width modulation** (PWM), w moved by an LFO, shifts the
harmonics' recipe all the time -- a single oscillator sounding like
several (the Juno's strings) -- without moving the whole wave up and
down (tested: w swept from 0.1 to 0.9, every period's average within
0.02 of 0).

**Hard sync.** Oscillator 2 restarted whenever oscillator 1 starts a
period: its pitch is then oscillator 1's, and moving its own frequency
changes only its *shape*, the harmonics sweeping like a vowel (the
Prophet-5's sync lead). The teaching point: the restart is a jump, and
it falls *between* two samples, at a fraction of one; the BLEP
correcting it has to be placed at that fraction, or the aliases come
back. The master says, at each sample, whether and where in the step
to the next one it wraps, so the slave corrects the sample before the
jump too. Measured: a sawtooth synced to a 1001 Hz master at 2.37 times
its frequency, the loudest alias below 5 kHz -29.5 dB naive, -69.9 dB
corrected at the fraction -- and -30.7 dB corrected as if the jump fell
on the next sample: hardly better than nothing, since a jump moved in
time is itself an error the ear hears as an alias.

**Drift.** An analog oscillator is never quite in tune: its frequency
wanders by a few cents as the circuit warms. Two "perfect" detuned saws
beat at a fixed rate, mechanically; with drift, the beating breathes.
`Drift`: a random walk pulled back towards 0 (an Ornstein-Uhlenbeck
process), one per oscillator, from a seeded generator (deterministic,
testable), switchable, the first of the "simple vs better" switches
that are really "exact vs analog". Its spread is set in cents (3 by
default: measured 2.97 over 290 s) and its wandering time tau (2 s:
two seconds apart the values are correlated by 0.35, e^-1 being
0.37). It steps every 64 samples *of the audio clock*, whatever the
size of the blocks it is advanced by, so a golden run's pulls of 735
and SDL's give the same drift.

## 4. Modulation: the knobs turned by the synthesizer itself

A **modulator** is a signal too slow to hear, used to turn a knob.

**The LFO** (low-frequency oscillator), 0.1 to 20 Hz: sine, triangle,
square, sawtooth up and down, and **sample-and-hold** (a random value
held for each period: the "computer" burble of 1970s films). Its random
values are not the NES's LFSR's, whose successive states are each other
shifted by a bit (16384, 8192, 4096...), but a linear congruential
generator's (`Noise.lcg`, Numerical Recipes' constants: from 0,
-0.528, -0.443, 0.639 as numbers from -1 to 1). Naive
waveforms are right here: a 5 Hz square's harmonics are far below
Nyquist, so PolyBLEP (needed at 440 Hz) has nothing to correct. Worked
example, a vibrato of 6 Hz and 0.3 semitone: the frequency f
2^(0.3 sin(2 pi 6 t) / 12), between 0.983 f and 1.017 f. (`Effect`'s
`Vibrato` is this, rendered ahead; `Lfo` is the live one.) Its rate can
follow a tempo (a quarter note at 120 BPM: 2 Hz).

**Envelopes, live and exponential.** `Envelope` was straight lines,
computed over a known length (`level`, `apply`). Live, an envelope is a **state
machine** driven by a gate:

```
   gate on             gate off
      |                   |
 idle --> attack --> decay --> sustain --> release --> idle
             ^                                |
             '---- a key pressed again: the attack, from where it is
```

The attack restarts from the current level, not from 0: a note played
again while the last one dies away rises from there, without a jump.
The ADSR's knobs are read at every block; turning the sustain while a
note is held moves the level to it at the decay's pace.

and an analog one's segments are a capacitor charging through a
resistor: exponential, each a one-pole towards a target. The catch:
an exponential never arrives. So the attack aims *past* 1 and stops
when it gets there: aiming at 1.5, the level 1.5 (1 - e^(-t/tau))
reaches 1 at t = tau ln 3, so tau = attack / ln 3 = 0.91 attack. The
decay and release fall towards their targets for good; "their time"
is the time to fall most of the way (to 1/1000 of the distance, -60
dB: tau = time / 6.91). Heard, the exponential attack is punchier and
the release more natural: our ears hear loudness in decibels, and an
exponential is a straight line in decibels -- the straight release
loses little at first and plunges at the end (-6 dB at half its time,
-20 at 90%, -40 at 99%): it sounds cut off. Measured on the ADSR of
`notes_audio.md` §4 (A 0.01, D 0.1, S 0.5, R 0.2, let go at 0.5 s):
0.633 halfway up (the straight one: 0.5), 1 after 441 samples, 0.5158
halfway through the decay, 0.5005 at its end, 0.0158 halfway through
the release, 0.0005 at its end (-60 dB). The straight live envelope
gives `Envelope.level`'s values within one sample's step. Heard side by
side: the golden WAV `envelope_linear_vs_exponential`.

**Where modulation goes.** The Minimoog's mod wheel scales one source
into two destinations; later synthesizers (the ARP 2600's patch
points, the Oberheim Matrix-12's matrix) let any source reach any
knob. Here: the Minimoog's routing, fixed, in `Minimoog_voice`; a
general **modulation matrix** is the modular synthesizer's lesson, an
exercise.

## 5. Playing: keys to voices

**Monophonic** (the Minimoog, the TB-303): one voice, and a stack of
the keys held. Which one sounds is the **priority**: holding C4 and
pressing E4, *low-note* priority (the Minimoog) stays on C4, *last-note*
(most monosynths since) goes to E4 and, E4 released, back to C4, the
stack remembering; *high-note* keeps E4. Trills are played that way:
hold one key, tap another. **Legato**: a new key while one is held
changes the pitch without restarting the envelopes; **retrigger**
reopens their gate at every change. `Voicing` turns each key into an
event for the voice -- a note begins (the gate opens), changes (the
gate stays), or ends -- so the voice itself knows nothing of keys.

**Glide** (portamento): the pitch not jumping to the new note but
moving there, a one-pole in *semitones* (not in hertz: a glide of an
octave up and one down take the same time, as the ear expects). Its
knob is a time constant tau: after tau, 63% of the way; after 5 tau,
99.3%. C4 to C5 with tau = 0.1 s: 67.59 after 0.1 s (405.5 Hz, between
G4 and G#4), 71.92 after 0.5 s (8 cents flat). The pitch is kept when
the keys come up, so the next note glides from the last one, as the
Minimoog's did. A phrase put together from the blocks, legato with a
50 ms glide: the golden WAV `mono_legato_glide`.

**Polyphonic** (the Juno, later): N voices, and which one a new note
takes -- a free one, else one **stolen**: the oldest, the quietest (in
its release), or the one already playing that note. A classic
visible-in-the-code decision, and the voice limit `notes_audio_midi.md`
left open for the MIDI player.

## 6. The ladder filter

Robert Moog's filter (patented 1969), the sound people mean by "Moog":
four one-pole low-passes in a row, made of transistor pairs stacked
like the rungs of a ladder, and the output fed back to the input,
inverted, times k (the "emphasis").

```
  x --(+)--> [pole] --> [pole] --> [pole] --> [pole] --+--> y
       ^-                                               |
       '------------------ k -------------------------- '
```

**Without feedback** each pole loses 3 dB at the cutoff and 6 dB per
octave above: four lose 12 dB at the cutoff, **24 dB per octave**
above -- steep, the brightness taken away fast.

**The resonance, worked out.** At the cutoff each pole also delays the
phase by 45 degrees: four by 180. The inverted feedback, delayed by
half a period, arrives *in* phase: it reinforces the frequencies near
the cutoff, the resonant peak. How much? The loop's gain at the cutoff
is k (1/sqrt 2)^4 = k / 4: at **k = 4** the loop sustains itself, the
filter oscillates on its own, a sine at the cutoff -- playable, the
cutoff following the keyboard. And the price, the Minimoog's known
character: below the cutoff the feedback *subtracts*, the gain at 0 Hz
is 1 / (1 + k), a fifth (-14 dB) at k = 4: the bass thins as the
resonance rises. (Some later ladders add the input back to compensate:
an option, `~compensation`.) Below k = 4 the analog peak lies a little
*under* the cutoff: 59 cents under and 9.25 dB up at k = 3.5, 11 cents
at 3.9 (from its transfer function, 1 / ((1 + s)^4 + k)) -- a number
the digital versions below are measured against.

**Three versions**, switchable:

1. **Naive**: the four poles as one-poles (`y += g (x - y)`), and the
   feedback from the *previous* sample, since the current output isn't
   known yet. That one-sample delay adds phase (8.2 degrees at 1 kHz,
   44,100 Hz: 360 x 1000 / 44,100), a different amount at every cutoff
   (Stilson and Smith, 1996). Measured at k = 3.5: the peak 36 cents
   under the cutoff at 440 Hz, 10 under at 1 kHz, 129 *over* at 5 kHz;
   oscillation from k = 4.06 at 110 Hz, 4.26 at 440, 4.64 at 1 kHz, and
   not at all up to k = 8 at 5 kHz. The resonance knob means something
   else at every note.
2. **Zero-delay feedback** (Zavalishin's topology-preserving
   transform): the loop solved instead of delayed -- each pole a
   trapezoidal integrator whose output is a known multiple of its input
   plus a part of its state, so the four in a row give y = G^4 u +
   sigma, and with u = x - k y, y = (G^4 x + sigma) / (1 + k G^4),
   computed *before* the poles run. Measured: the analog numbers -- the
   peak 59 cents under at 440 Hz and 1 kHz (55 at 5 kHz, the trapezoid
   shifting the frequencies around its pre-warped cutoff a little near
   Nyquist), oscillation from k = 4.000 at every cutoff.
3. **Nonlinear**: the zero-delay loop with a tanh at its input and at
   each pole's, the transistors saturating (after Huovilainen, 2004,
   whose own model runs on the naive loop): the linear solution predicts
   the loop's input, the saturation bends it. Driven harder, it gets
   thicker, not just louder: a 108 Hz sine's 3rd harmonic 50 dB under
   the fundamental at an amplitude of 0.1, 24 at 0.5, 17 at 1, 11 at 4.
   Past k = 4 it oscillates without running away, the tanh holding its
   level, within 1 cent of the cutoff -- the golden WAV
   `ladder_self_oscillation` plays C4, E4, G4, C5 on the cutoff alone,
   each within 0.3 cents. And `ladder_sweep`, a sawtooth under a
   sweeping cutoff at k = 0, 2.5 and 3.8.

## 7. The state-variable filter

Why a second filter: `Filter`'s biquad is fine for a fixed cutoff, but
a synthesizer moves the cutoff *every sample* (an envelope, an LFO at
audio rate), and a biquad's coefficients recomputed that fast zip,
or, fast enough, blow up: they were designed for one frequency, not a
moving one. The **state-variable filter** (Hal Chamberlin, *Musical
Applications of Microprocessors*, 1980) has the cutoff and the
resonance themselves as its parameters, and gives low-pass, band-pass,
high-pass and notch at once from one structure (two integrators in a
loop). Chamberlin's first, integrators as running sums: cheap, right
at low cutoffs, and unstable high up -- at Q = 0.707 stable only up to
7,637 Hz (about a sixth of the rate, the classic warning; higher Qs a
little further, 18.9 kHz at Q = 20). Then its zero-delay version
(Zavalishin; Andrew Simper's form, 2013), stable at any cutoff: -3.01
dB at the cutoff in its low, band and high outputs at 1, 5, 8 and 12
kHz alike. The test that says why a synthesizer wants it: a 110 Hz
sawtooth low-passed at Q = 5, the cutoff swept between 125 Hz and 8 kHz
at audio rate. Swept 5 times a second, the loudest sample is 2.38
through the SVF and through the biquad recomputed at each sample; 500
times a second, 2.40 through the SVF, 6.66 through the biquad; 3,000
times, 2.33, and the biquad blown up.

## 8. Effects: what a sound goes through on its way out

Effects are live blocks like the rest (§1), in stereo where it matters,
each with a dry/wet mix. The order they go in matters (§8.7).

### 8.1 Gain and drive

Gain is a multiplication (in decibels, `Mix.decibels`). **Drive** is a
*waveshaper*: each sample through a curve that flattens the loud parts
-- hard clip, tanh, a cubic, an asymmetric one like a diode (which adds
even harmonics, "warmer"). The lesson: a waveshaper **adds harmonics**
-- that's the point -- and some go above Nyquist and fold back
(`notes_audio.md` §2). A 5 kHz sine through tanh gets its 3rd harmonic
at 15 kHz (fine), its 5th at 25 kHz (folded to 19.1 kHz), its 7th at 35
(to 9.1 kHz) and its 9th at 45 kHz, folded to **0.9 kHz**: a tone
below the note, not a harmonic of it, the ugly sound of cheap
distortion. The cure is **oversampling**: up to 4 x 44,100 (Nyquist at
88.2 kHz), shape there, low-pass, back down: the harmonics that would
have folded are filtered out on the way. Seen on the spectrum, switched
on and off.

### 8.2 EQ

The rest of Bristow-Johnson's cookbook, next to `Filter`'s low- and
high-pass: the **peaking** filter (a bell: +6 dB around 1 kHz) and the
**shelves** (the lows or the highs raised or lowered from a frequency
on). A three-band EQ is three biquads in a row; `Filter.response` draws
its curve.

### 8.3 Chorus and flanger: a delay that moves

A delay line read at a length an LFO moves. Reading a delay that
shortens is reading *faster*: a pitch up; lengthening, down. A **chorus**
(10 to 25 ms, no feedback, mixed with the dry sound): 15 ms +- 3 ms at
0.5 Hz changes by at most 2 pi 0.5 x 0.003 = 0.0094 seconds per second,
a pitch wobbling by +- 16 cents -- the dry note and a slightly out of
tune copy, the "several players" sound (the Juno-60's chorus, 1982).
The read falls between samples: `Resample`'s interpolation, linear
then an all-pass. A **flanger** is the same with a short delay (1 to 5
ms) and feedback: the dry sound plus itself delayed by d cancels at
the frequencies where the delay is half a period, (2k + 1) / (2 d): at
1 ms, 500, 1500, 2500 Hz..., notches **evenly spaced**, sweeping as d
moves: the jet plane (first done with two tape machines, a thumb on a
reel's flange: the name).

### 8.4 Phaser

Notches without a delay line: the signal through all-pass filters (each
passes every frequency at the same level but delays the phase, 0 to 180
degrees around its frequency), added to the dry one; where the phase
has turned by 180 degrees, cancellation. Four all-passes turn up to 720
degrees: two notches, *not* evenly spaced -- the difference from the
flanger, heard (a softer, vocal sweep) and seen. An LFO moves the
all-passes' frequency (the MXR Phase 90, 1974).

### 8.5 Delay

`Effect.echo`, live: the time in seconds or **in beats** (at 120 BPM a
dotted eighth is 0.375 s: the echoes fall between the notes), the
feedback through a **low-pass** so each repeat is darker than the one
before (the tape echo's sound: the Roland Space Echo, 1974), and
**ping-pong**, the repeats alternating left and right.

### 8.6 Reverb

`Effect.reverb` is Schroeder's (1962): four combs, two all-passes; its
echoes are too regular, metallic on a drum. **Freeverb** (Jezar
Wakefield, 2000, public domain): eight combs, each with a low-pass in
its feedback (a room's air and walls absorb the highs, so a hall's tail
darkens), four all-passes, the two channels' delays slightly different
so the room is wide. Then **Dattorro's plate** (1997): a figure-eight
of delays and all-passes, the smooth, dense reverb of 1980s studio
hardware. Tested as Schroeder's was: the decay to -60 dB against the
setting.

### 8.7 Dynamics: compressor, limiter, gate

A **compressor** turns down what is loud: an **envelope follower**
(the level, a one-pole on the absolute value or the square, fast to
rise -- the attack -- slow to fall -- the release), a **gain computer**
in decibels (above the threshold, the level rises only 1/ratio as
fast), and a make-up gain. Worked example, the static curve: threshold
-20 dB, ratio 4:1, an input at -8 dB is 12 dB over, which becomes 3:
out at **-17 dB**, a gain reduction of 9 dB, the needle every
compressor has. A **soft knee** rounds the corner over a few dB. The
same machine with other settings: a **limiter** (ratio infinite, a
look-ahead so nothing gets through before it reacts; `Mix.limit`'s
tanh, at the end of the mixer, is a limiter's crude cousin) and a
**noise gate** (below the threshold, silence). With a **side-chain**,
the follower listening to another sound: a kick drum turning a bass
down on each beat, the "pumping" of dance music.

**The order.** A chain's usual order is drive, EQ, chorus, delay,
reverb, compressor: distort a clean signal (a reverb's tail driven is
mud), color it, widen it, put it in a space, and control the level of
the whole. `Rack` is that list, each effect bypassable.

## 9. The panel

**Knobs**, dragged *vertically* (up is more) rather than turned around
their center: a mouse moves in straight lines, and a circular drag
needs aiming; every software synthesizer since the 1990s does this,
often with a modifier key for fine moves. And *relatively*: each
frame's move of the mouse turns it by that much (`Gui.knob`: 200
pixels for the whole range), so pressing a knob doesn't make it jump
to the mouse, as a slider's does -- the tests turn one from 0.3 by
20 pixels a frame: 0.3 at the press, then 0.4, 0.5, 0.6. A knob's
value is not always linear: the cutoff over ~10 octaves is exponential
(each equal turn an equal *ratio*), times are too -- that is the
voice's business (`Minimoog_voice`'s laws), the knob only turns from
0 to 1. **Rocker switches**, rocked by a click, and **rotary switches**
(`Gui.selector`) for the oscillators' range and waveform, a position
per 24 pixels of drag or the next on a click. They needed two more
kinds of paint in the toolkit (`gui/Widget.mli`), a disc and a
segment -- a knob's face and its pointer -- where it had drawn
everything with rectangles and text. The keyboard: the computer's
letters (two rows as a piano's white and black keys, as
`examples/AudioPiano.ml`), a drawn one of two octaves, played with
the mouse too (sliding from key to key), and the two wheels, pitch
(springing back) and modulation (staying). Under the panel, what the
voice just played (its last 2048 samples, kept by the voice itself so
every backend can show them): an oscilloscope and a spectrum.

**A patch** is the panel's positions -- a record, and on paper, a
Minimoog "patch chart": knobs drawn with their pointers marked. Presets
of our own (a bass, a lead, a brass, a flute, a whistle, a wind);
saved and opened through the File menu every app shares
(`appkits/file_menu`), exported as text: an exercise left in
`TinyMinimoog.ml`'s header.

## 10. After the Minimoog

- **TB-303** (Roland, 1981): one oscillator, a filter of its own (a
  diode ladder, softer than 24 dB per octave), an **accent** that
  shortens the envelope and opens the filter, **slide** between steps,
  and a 16-step sequencer inside: a machine that plays itself. Failed
  as a bass guitar replacement, became acid house.
- **DX7** (Yamaha, 1983): no filter at all -- `notes_audio.md` §7's FM
  with six operators wired by 32 **algorithms** (small graphs: which
  operator modulates which), feedback on one, envelopes of four rates
  and four levels. Its cartridges are a documented format (a 4096-byte
  bank of 32 voices): readable, the user bringing their own.
- **Juno-106** (Roland, 1984): polyphony (six voices, §5), one
  digitally controlled oscillator per voice with a sub-oscillator and
  PWM, and the stereo chorus of §8.3 that *is* its sound.

## 11. Exercises

- A **modular synthesizer**: the blocks with patch cords, a source to
  any destination (the modulation matrix of §4), the patch a graph --
  cycles allowed, and why a one-sample delay has to break them.
- **Oversampling** the ladder (its tanh adds harmonics too, §8.1).
- **Unison**: one key, several detuned oscillators spread across the
  stereo field (the "supersaw" of the Roland JP-8000, 1996).
- **Velocity** and **aftertouch** from a real MIDI keyboard to the
  filter.
- A **vocoder**: a filter bank's levels on one sound (a voice) imposed
  on another (a synthesizer).

## Glossary

- **Subtractive synthesis**: a rich wave, filtered. **VCO, VCF, VCA**:
  voltage-controlled oscillator, filter, amplifier: the boxes of the
  signal path.
- **Instrument**, **block**, **event**: a live process, the samples it
  computes at a time, what changes it in between.
- **Zipper noise**: a parameter changing in steps, heard; cured by a
  **ramp** or a smoothing one-pole.
- **Pulse width**, **PWM**: the fraction of the period a pulse is high,
  and its modulation. **Hard sync**: one oscillator restarting another.
- **LFO**: a low-frequency oscillator, a modulator. **Sample and hold**:
  a random value held for a period.
- **Gate**: a key held, on or off. **Legato**, **retrigger**. **Glide**,
  **portamento**: the pitch moving between notes. **Priority**: which
  held key sounds.
- **Ladder filter**, **emphasis**, **self-oscillation**. **Zero-delay
  feedback**: a loop solved instead of delayed.
- **Waveshaper**, **drive**, **oversampling**. **Shelf**, **peaking** EQ.
- **Chorus**, **flanger**, **phaser**: a moving delay, a short moving
  delay with feedback, moving all-passes.
- **Compressor**: **threshold**, **ratio**, **knee**, **attack**,
  **release**, **make-up gain**, **side-chain**. **Limiter**, **gate**.

## References

(To check against the sources when writing each module.)

- Robert A. Moog, "A Voltage-Controlled Low-Pass High-Pass Filter for
  Audio Signal Processing", Audio Engineering Society convention, 1965;
  US patent 3,475,623, 1969.
- Moog Music, *Minimoog Model D* owner's manual, 1970s.
- Hal Chamberlin, *Musical Applications of Microprocessors*, Hayden,
  1980 (2nd ed. 1985): the state-variable filter.
- Tim Stilson, Julius O. Smith, "Analyzing the Moog VCF with
  Considerations for Digital Implementation", ICMC 1996; and
  "Alias-Free Digital Synthesis of Classic Analog Waveforms", ICMC 1996.
- Jon Dattorro, "Effect Design, Part 1: Reverberator and Other
  Filters" and "Part 2: Delay-Line Modulation and Chorus", Journal of
  the Audio Engineering Society 45(9) and 45(10), 1997.
- Jezar Wakefield, Freeverb, 2000 (public domain source).
- Antti Huovilainen, "Non-Linear Digital Implementation of the Moog
  Ladder Filter", DAFx 2004.
- Vesa Välimäki, Antti Huovilainen, "Oscillator and Filter Algorithms
  for Virtual Analog Synthesis", Computer Music Journal 30(2), 2006.
- Udo Zölzer (ed.), *DAFX: Digital Audio Effects*, 2nd ed., Wiley,
  2011.
- Dimitrios Giannoulis, Michael Massberg, Joshua D. Reiss, "Digital
  Dynamic Range Compressor Design -- A Tutorial and Analysis", Journal
  of the Audio Engineering Society 60(6), 2012.
- Vadim Zavalishin, *The Art of VA Filter Design*, Native Instruments,
  2012 (rev. 2018).
- Will Pirkle, *Designing Software Synthesizer Plug-Ins in C++*, Focal
  Press, 2014; *Designing Audio Effect Plugins in C++*, 2nd ed.,
  Routledge, 2019.
- Trevor Pinch, Frank Trocco, *Analog Days: The Invention and Impact of
  the Moog Synthesizer*, Harvard University Press, 2002 (the history).
- Julius O. Smith III, *Physical Audio Signal Processing*, online,
  CCRMA, Stanford (delay lines, chorus, flanging).
