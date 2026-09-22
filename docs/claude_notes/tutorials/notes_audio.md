# Sound, from scratch: a tutorial for `audio/`

How a computer makes sound: the few ideas every synthesizer, from Max
Mathews's first computer music at Bell Labs (1957) to the NES's five
channels and a modern game's mixer, is built from, where they came
from, and what goes wrong when they're done naively. It was written
before `audio/`, as its specification (see
[`plan_audio_teaching.md`](../plans/plan_audio_teaching.md)), and has
been kept in step with it since: its numbers are the tests'. Companions:
[`notes_2d.md`](notes_2d.md) (pictures), [`notes_2d_physics.md`](notes_2d_physics.md)
(motion), and [`notes_audio_related_work.md`](../related-work/notes_audio_related_work.md).

The parallel with pictures runs all the way through, and is worth
keeping in mind: a picture is a grid of pixels, a sound is a row of
samples; drawing a circle computes pixels from a formula, playing a
tone computes samples from one; jagged edges and aliased tones are the
same mistake, with the same cure.

## 0. Where the code is, and a reading order

| module (`audio/`) | what | section |
|---|---|---|
| `Signal` | samples, sample rate, time | §1, §2 |
| `Oscillator` | sine, square, triangle, sawtooth; naive and band-limited | §3, §6 |
| `Noise` | random signals; the NES's LFSR | §3 |
| `Envelope` | ADSR: how a sound starts and ends | §4 |
| `Mix` | adding sounds, volume, decibels, clipping | §5 |
| `Spectrum` | the Fourier transform: which frequencies a sound has | §6 |
| `Filter` | low-pass, high-pass, resonance | §7 |
| `Fm` | FM synthesis: sidebands from two sines | §7 |
| `Synth` | a sound as a tree of voices, rendered; slides | §8 |
| `Effect`, `Sfx` | vibrato, jump, arpeggio, echo, reverb; sfxr's parameters and buttons | §8 |
| `Pluck` | a plucked string: Karplus-Strong, tuned | §8 |
| `Space` | stereo: the pan laws, the ears' delay, distance, air, Doppler | §5 |
| `Music`, `Abc`, `Doremi`, `Midi` | notes, equal temperament, tunes as text, MIDI files | §9 |
| `Resample` | recordings at other pitches and rates: nearest, linear, cubic | §9 |
| `Mixer` | the sounds playing, pulled by the sound card | §10 |
| `Instrument` | a sound played live, a block at a time: see [`notes_synth.md`](notes_synth.md) | §10 |
| `Wav` | samples in a file, written and read | §2, §9 |
| `playground/Audio`, `Audio3d`, `Audio_debug` | the Evan-style API over all of it; heard from a 3D camera; the sound seen | §13 |

## 1. What a sound is

Sound is air pressure going up and down, fast: a string or a speaker
pushes the air, the ear feels the pressure change. A **pure tone** is a
pressure following a sine wave; how many times per second it goes up
and down is its **frequency**, in hertz (Hz), which we hear as pitch
(the A a piano tunes to is 440 Hz; human hearing goes from about 20 Hz
to 20,000 Hz); how far it goes is its **amplitude**, which we hear as
loudness.

```
  pressure
     ^     one period = 1/440 s
     |    .--.       .--.
     |  /      \   /      \           a sine wave: a pure tone
   0 +-/--------\-/--------\--> time
     |           '          '
```

A **digital** sound is that curve **sampled**: its value measured at
regular instants, the **sample rate** times per second, each measure a
number (here a float from -1. to 1.). CDs and most games use 44,100
samples per second: a 440 Hz tone is then one period every
44,100 / 440 = **100.23 samples**. One second of mono sound is 44,100
numbers (88,200 bytes as 16-bit integers): sound is a lot of data,
computed fast, but each number is simple.

## 2. Sampling, and the Nyquist limit

How many samples per second are enough? **Nyquist and Shannon**
(Harry Nyquist, 1928; Claude Shannon, 1949): a sample rate of F can
represent every frequency *below F/2*, exactly, and nothing above. At
44,100 Hz, the limit (the **Nyquist frequency**) is 22,050 Hz, just
above what we hear -- which is why CDs chose it.

A frequency *above* the limit doesn't just vanish: sampled, it looks
exactly like a lower one, **folded back** below the limit, an
**alias**. At 44,100 Hz, a 30,000 Hz tone gives the same samples as a
44,100 - 30,000 = **14,100 Hz** tone, and that's what you hear:

```
   the 30,000 Hz tone, sampled (the dots), and the 14,100 Hz tone
   the same dots also fit -- the samples can't tell them apart

   .    '.     .'   '.    .'     '.
        :   .   :     : .  :       :
```

That's the same phenomenon as the jagged edges of `notes_2d.md` §10
(detail finer than the pixel grid, turned into a coarse pattern), and
the wagon wheels turning backwards in films. The cure is the same too:
remove what's too fine *before* sampling (§6).

(A sound is written to a file with `Wav`: a small header -- sample
rate, channels, bits per sample -- then the samples: the PPM of sound,
and what the golden tests compare. Read back, a file from elsewhere
may have other chunks, two channels, another rate: §9.)

## 3. Oscillators: the waveforms

An **oscillator** computes a periodic wave. Its heart is the **phase
accumulator**: a number going from 0 to 1 over one period, advanced by
frequency / sample_rate at each sample, and wrapped back to 0 -- then
the waveform is a function of the phase:

```
   sine       sin(2 pi phase)           smooth, a flute; one frequency
   square     phase < 0.5 ? 1 : -1      hollow, a clarinet; NES pulses
   triangle   up, then down             soft, the NES's bass channel
   sawtooth   2 phase - 1               buzzy, brassy; strings
   noise      random                    wind, explosions, drums

     sine        square       triangle     sawtooth
    .-.         .--.          /\            /|  /|
   /   \   .    |  |  |      /  \  /       / | / |
        '-'        '--'          \/       /  |/  |
```

Why do they sound different at the same pitch? Every periodic wave is
a sum of sines at multiples of its frequency, its **harmonics** (Joseph
Fourier, 1822, §6): the sine has only the first, the square only the
odd ones (1, 3, 5, ... at amplitudes 1, 1/3, 1/5, ...), the sawtooth
all of them (1/n). The recipe of harmonics is the **timbre**.

**Noise** is random samples. A computer's randomness is a
pseudo-random generator; the NES's noise channel used a 15-bit
**linear-feedback shift register** (shift the bits, feed back the XOR
of two of them): cheap, deterministic (so testable), and its "short
mode", a 93-step loop, is the metallic noise of 8-bit games. **White
noise** has all frequencies equally; **pink** noise less of the high
ones: rain vs hiss.

## 4. Envelopes: how a sound starts and stops

A tone switched on and off at once starts and ends with a click (a
jump in the waveform is a burst of every frequency), and a real
instrument never does that: a piano's note starts sharply and dies
slowly, a violin's swells. An **envelope** is the volume over time,
multiplied into the wave; the classic one, from the 1960s analog
synthesizers, is **ADSR**:

```
  volume
    ^   /\
    |  /  \_______________         A: attack   time to full volume
    | /                   \        D: decay    time down to...
    |/                     \       S: sustain  ...this level, held
    +-A--D-------S---------R--> t  R: release  time to silence
```

A percussive sound (a blip, a drum) is a short attack and a decay to 0
with no sustain; an organ is all sustain. Most game sounds are one
oscillator and one envelope.

## 5. Mixing: adding sounds, and not too much

Two sounds at once are simply **added**, sample by sample (air
pressures add up the same way). The problem is the result's range: two
sines at full volume add up to 2, out of the -1..1 the sound card
accepts, and the peaks are cut flat -- **clipping**, a harsh
distortion. The fixes: lower each sound's volume (the mixer's gain),
or bend the curve smoothly near the limits (**soft clipping**, e.g.
tanh), which distorts gently, like an overdriven tube amplifier.

Loudness is measured in **decibels** (dB), a logarithm: 20 log10 of the
amplitude ratio. Halving the amplitude is **-6.02 dB**, a tenth is -20
dB; the ear perceives ratios, not differences, so a volume slider in dB
feels even (and one in plain amplitude doesn't).

### Two ears: stereo, and where a sound comes from

The ear finds a sound's direction mostly from two differences between
the ears: the **level** (the head shadows the far ear) and the **time**
(the far ear hears it later). Two channels, left and right, can give
both. The level: each sound played gets a **pan**, from -1 (left) to 1
(right), turned into two gains. The
obvious gains, linear, `1 - p` and `1 + p`, have a **hole in the
middle**: what we hear as loudness is the power, the sum of the gains'
squares, 2 in the middle but 4 at a side, so a sound crossing from left
to right dips by 3 dB as it passes the centre. The **constant power
law** puts the two gains on a quarter circle, `sqrt 2 cos a` and `sqrt 2
sin a`, the squares always adding up to 2 (`Space.pan`). The time:
around the head, a sphere of 8.75 cm, the far ear's path is longer by
r (theta + sin theta) at an angle theta (Woodworth, 1938), 0.656 ms,
29 samples, for a sound straight to the side; a sound played once has
its far channel that much later (`Space.interaural_delay`), a
continuous one, whose pan moves every frame, only its level (a moving
delay needs a fractional delay line: §12).

Where a sound is also says how loud it is: it spreads over a sphere,
its amplitude falling as 1 / distance -- half as loud (-6 dB) each time
the distance doubles, the **inverse distance law**. The air takes the
highs too, a far thunder a rumble: fitted to the standard's table (ISO
9613-1), a loss of 0.0766 (f / 8000)^1.75 dB a meter, so 3 dB at 8 kHz
after 40 m, at 4 kHz after 130 m -- a low-pass whose cutoff falls with
the distance (`Space.air_cutoff`). And how it moves
says its pitch: a source coming towards you squeezes its waves, higher;
going away, lower -- the **Doppler** effect, `f' = f (c - v_listener) /
(c - v_source)`, each speed along the line between them. A car at 30
m/s: 1.096 coming, 0.920 going, a drop of 3 semitones as it passes, the
"neeee-owww" (`Space.doppler`: the formulas as OpenAL 1.1 has them).

In the code, sounds stay mono -- voices, filters, effects -- and a pan
is a node of the tree (`Synth.Panned`), which only `Synth.render_stereo`
hears: a tree with no pan in it renders once, the same array in both
channels. The mixer, the platforms and the dumped WAVs carry two
channels; the software backend's `m` key mixes them back down to one,
to hear what panning does. In a 3D world the listener is the camera:
its eye the ears, the direction it looks straight ahead, its right the
right ear's side, and a sound made anywhere is panned, delayed, faded,
dulled and Doppler-shifted from there (`playground/Audio3d`;
`TinyStarFox.ml`'s enemies go by a tenth higher coming, a tenth lower
gone, the stage carrying you at 34 m/s).

## 6. The spectrum: which frequencies a sound contains

The **Fourier transform** turns a sound (samples over time) into its
**spectrum** (amplitudes over frequency): the recipe of §3, measured.
The discrete version (DFT) on N samples costs N^2 operations; the
**fast Fourier transform** (Cooley and Tukey, 1965; Gauss knew it in
1805) does it in N log N, by splitting the samples into even and odd
halves, recursively: the algorithm that made digital signal processing
possible. For N = 1024: a million operations vs ten thousand.

```
  amplitude          a 1000 Hz square wave: odd harmonics, 1/n
      ^
      | |
      | |   |
      | |   |   |   |   |   ...
      +-1---3---5---7---9-------> kHz
```

What it's for here: tests (a 440 Hz sine's spectrum has one peak, at
440 Hz, and nothing else), the debug display (the spectrum drawn over
the frame), and seeing aliasing. A **naive square wave** at 1000 Hz,
sampled at 44,100 Hz, has harmonics at 23, 25, 27 kHz, above the
Nyquist limit: they fold back to 21.1, 19.1, 17.1 kHz, frequencies that
are *not* harmonics of 1000 Hz, heard as a harsh, out-of-tune fizz,
worse for higher notes. **Band-limited** oscillators avoid it:
synthesize the wave without the harmonics above the limit, e.g. by
smoothing each jump of the square with a small polynomial correction
(PolyBLEP): the audio twin of antialiasing a polygon's edge.

## 7. Filters: shaping the spectrum

A **filter** lets some frequencies through and weakens others. The
simplest **low-pass** (keeps the lows, softens the highs) is one line,
a running average that follows the input slowly:

```
   y := y + a * (x - y)          with a = 1 - exp(-2 pi fc / F)
```

where fc is the "cutoff" frequency: for fc = 1000 Hz at F = 44,100 Hz,
a = 0.133. Subtract it from the input and it's a **high-pass**. The
**biquad** (two samples of memory, five coefficients: Robert
Bristow-Johnson's cookbook gives them for every classic filter) adds
**resonance**: a boost just at the cutoff, the "wah" of a synthesizer
whose cutoff moves. Filtered noise is most of the sound effects there
are: a low-passed noise is a rumble, an explosion, a ship's engine; a
high-passed one a hiss, a cymbal.

The other way to a rich spectrum is to make one rather than carve one:
**FM synthesis** (John Chowning, Stanford, 1973), one sine wobbling
the phase of another at an audio rate, `sin(2 pi fc t + I sin(2 pi fm
t))`. The spectrum gets sidebands at fc +- k fm, of amplitude the
Bessel function J_k(I): the ratio fm / fc says where they fall (a whole
number on the harmonics, an instrument; otherwise between them, a bell),
the index I how many are loud (the brightness), and making the index
follow the envelope gives a note bright when struck and darker as it
dies. Two sines and three numbers: the Yamaha DX7's sound, and the Sega
Genesis's and the Sound Blaster's (`audio/Fm`).

## 8. Game sounds from a few parameters

Most arcade sounds are one oscillator, one envelope, and a few
modulations: a **pitch slide** (a jump goes up, a laser goes down, a
coin jumps up by a step), a **vibrato** (the pitch wobbling), an
**arpeggio**, filtered noise for impacts. **sfxr** (Tomas Pettersson,
2007, written for game jams) turned that into a generator: a dozen
parameters, a "random laser" button. `audio/Sfx` is the same, readable:
a record of sfxr's sliders -- `jump` is a square from 300 Hz sliding to
650, held 0.04 s, decaying over 0.14 s; the explosion noise sliding
from 1500 steps a second to 150 under a low-pass falling from 4000 Hz
to 150 -- and `Sfx.vary`, sfxr's "mutate", nudges every number for a
family of sounds from one; `Sfx.random`, sfxr's other buttons, draws a
new sound within a category's ranges -- the ranges are what make it that
kind of sound (a laser always slides down, a jump up, an explosion is
noise getting duller: 200 seeds each, checked). `playground/Audio`'s
ready-made sounds are its presets.

The **arpeggio** is worth a second look: the notes of a chord one after
the other, every 1/60 s, around and around -- the chiptune trick of the
NES and the C64, whose two or three voices were too few to play chords:
fast enough, the notes blur into one warbling chord (`Effect.Arpeggio`,
and a tracker's `0xy` effect).

An **echo** is the sound plus itself delayed and quieter, fed back so
that the echo echoes too, `y[n] = x[n] + g y[n - D]`, from a **delay
line** (a circular buffer of the last D samples): a feedback comb
filter, the simplest effect with memory, and the start of
reverberation. Its echoes die away geometrically, the sound lasting
until they fall below -60 dB: 2.5 s for a delay of 0.25 s and a
feedback of 0.5 (`Effect.echo`, `Effect.tail`).

A **reverb** is a room's thousands of echoes, too many and too close to
hear one by one. Manfred Schroeder (Bell Labs, 1962) built one from the
echo: four feedback combs in parallel, their delays 30 to 45 ms and
mutually prime so their echoes never line up, then two **all-pass**
filters, which multiply the echoes and let every frequency through at
the same level. Each comb's feedback comes from the reverberation time
T asked for, 10^(-3 D / T), and on an impulse the tail is indeed 30 dB
down T / 2 later, 1552 of the samples of a tenth of a second non-zero
where one comb would have 3: a wash, not echoes (`Effect.reverb`,
`Audio.reverb`).

A delay line makes an instrument too. **Karplus-Strong** (Kevin Karplus
and Alex Strong, Stanford, 1983): fill a delay line one period long
with noise, read it around and around, and write each sample back as
the average of it and the one before. Going round, the line repeats, so
the noise becomes a periodic wave with every harmonic, like a string
just plucked; the average, a gentle low-pass once a period, kills the
high harmonics first and the low ones last, which is what a real
string does -- the twang turning into a hum. Nothing in it models a
string, and it sounds like one (`Pluck`, `Audio.pluck`: an A3's
brightness falls from 3803 Hz at the pluck to 642 Hz 1.5 s later).
Three details make it right. The noise must add up to 0, or the
averaging, which lets 0 Hz through, keeps its offset forever: a first
version, filled from the NES's shift register started at 1, was three
quarters -1s. The average is of a sample and the one *before* it, half a
sample late, so the period is the line plus a half: a first version
averaged with the one after, half a sample early, and was 12.5 cents
sharp at 440 Hz where the formula said 4.7 flat -- a test measuring the
pitch to a tenth of a cent caught it. And the line is a whole number of
samples, so the pitch is off by up to half a sample, 35 cents at 2 kHz:
a first-order all-pass in the loop adds the missing fraction (David
Jaffe and Julius Smith, 1983), and every note is then within a quarter
of a cent.

### The ready-made sounds, three generations

`playground/Audio`'s `blip`, `coin`, `laser`, `explosion`... were
written three times, each generation keeping the sounds' idea and
changing only how they were made, so the difference each technique
makes can be measured on the same sounds (`Unit_effect`'s "three
generations" test keeps the first generation's recipes as the record,
and checks every number below):

1. **Recipes on naive oscillators** (phase 3): each sound a few verbs,
   `square 880 |> lasting 0.06 |> fading`, the coin two voices `after`
   each other, the explosion noise sliding from 1500 steps a second to
   150. The oscillators the formulas of §3.
2. **The same recipes, band-limited** (phase 6): not a character
   changed in them; the oscillators underneath smooth their jumps
   (PolyBLEP) and the triangle its corners (PolyBLAMP, §6).
3. **sfxr's numbers** (phase 7): `audio/Sfx`'s presets, the same
   sounds re-made with what the recipes couldn't say -- an envelope
   with a held part, a jump inside one voice, filters.

What each step bought (brightness is the spectrum's centroid, its
centre of mass, over 2048 samples at the start and near the end):

| sound | generation 1 | 2: band-limited | 3: sfxr's numbers |
|---|---|---|---|
| blip | brightness 7623 Hz | 4476 Hz: 1.6% of its energy taken out (-18 dB) | the same, a held part |
| coin | 7869 Hz; its level falls to 0.045 where its notes meet (0.391 held) | 4656 Hz (-16 dB taken out) | one voice, a jump: 0.389 where the notes meet |
| laser | 7839 Hz | 6568 Hz, 4150 at 0.15 s (-16 dB taken out) | 3803 Hz, 1066 at 0.15 s: the low-pass following it down |
| hit | 4725 Hz | the same (noise: no jumps to smooth) | 1755 Hz |
| explosion | 4027 Hz, 3078 near the end | the same | 1196 Hz, 109 near the end: a burst, then a rumble |
| step | 422 Hz | -69 dB taken out: inaudible | the same |

The lessons, one per row:

- **Aliases are a small part of the energy and a large part of the
  sound.** Band-limiting took only 1.6% of the naive blip's energy out
  (-18 dB), yet its brightness fell from 7623 Hz to 4476: most of what
  was *high* in it were aliases, harmonics above 22,050 Hz folded back
  (§2), out of tune with the note, heard as a thin whistle over it. The
  higher the note, the worse: the coin's 1568 Hz loses as much. The
  triangle's corners (the step's) were never much of a problem: PolyBLAMP
  takes out -69 dB, a correct fix nobody can hear -- the triangle's
  harmonics already fall as 1/n^2 (§3).
- **Two notes in one voice, not two voices.** The first coin was two
  sounds one `after` the other, each fading in and out over 5 ms so
  as not to click (§4): where they met, the level dropped to a ninth,
  an audible hiccup between the notes. sfxr's "change" -- a jump in
  pitch inside a single voice, `Effect.Jump` -- keeps the phase and the
  envelope going: 0.389 where it was 0.045.
- **Slowing noise doesn't darken it; a filter does.** The first
  explosion slid its noise from 1500 steps a second to 150, meaning a
  rumble, and hardly got darker (4027 Hz to 3078): each step of the
  shift register is a jump from -1 to 1 or back, a square edge, and its
  harmonics are there however slow the steps. Only a low-pass falling
  with it (4000 Hz to 150) takes them away: 1196 Hz, then 109. That's
  subtractive synthesis (§7), and why sfxr has a low-pass slider: the
  same filter makes the laser go darker as it falls (1066 Hz at 0.15 s,
  where the band-limited recipe was still at 4150) and the hit a thud
  rather than a hiss.
- **A held part.** A linear fade from the start (the recipes'
  `fading`) makes every sound a pluck; sfxr's attack, sustain, decay
  lets a blip or a coin *hold* its note before dying, which is what
  makes it read as a note rather than a click.

The first two steps are general (every sound played gets them); the
third is the sound designer's, a dozen numbers per sound -- which is
why sfxr, for all its sliders, sounds like sfxr. Hear the difference
with the software backend's `l` key (generation 2 back to 1, on any
game), and `examples/AudioSfx.ml` for the third's numbers.

## 9. Notes and music

Western music divides each octave (a doubling of the frequency) into
12 **semitones**, all the same ratio: 2^(1/12) = **1.0595** (**equal
temperament**, standard since the 18th-19th centuries). From A4 = 440
Hz, A5 is 880 Hz, and C4 (middle C, nine semitones below A4) is 440 x
2^(-9/12) = **261.63 Hz**. `Music.note "C4"` is that formula.

A **sequencer** plays notes at times: a pattern of steps, like the
**trackers** of the Amiga (1987) and the NES's music drivers -- rows
of notes, one per channel, played at a tempo. Tetris's theme (the
Russian folk song Korobeiniki) is a few such rows: `Tetris.ml` has it
in ABC, two voices, 8 bars, 12.8 s. The standard way to store and send
notes, MIDI, has its own note: [`notes_audio_midi.md`](notes_audio_midi.md).

The **tempo** is how many notes a second, and changing it is dividing
every duration by the same number, the pitches kept (`Synth.faster`).
Games learned to play with it: Space Invaders' four-note march speeds
up as the invaders get fewer (1978), Tetris's theme as the level rises.
The music has to *go on* faster, not start again from its first note,
so a playing loop is swapped for the faster one at the same point of
the tune, the same fraction of the way through (`Mixer.change`,
`Audio.change_loop`).

### Samples: playing recordings

A recording has no frequency to change: to play it an octave up, read
it twice as fast -- and it lasts half as long. Pitch and time go
together, as on a tape, and the samplers (the Fairlight, 1979) and the
Amiga's trackers made every note of an instrument from one recording
that way (`Audio.pitched` on a `wav` or a `recorded` sound;
`examples/AudioSampler.ml`). Read a fifth faster, most reads land
between two samples, and making one up is the audio twin of scaling an
image (`notes_2d.md`'s image filtering), with the same answers: the
nearest sample, a line between the two neighbours, a curve through four
(Catmull-Rom). A sine read a fifth faster shows each one's error: to
689 Hz, -39.6, -79.2 and -112.5 dB; to 2756 Hz, -27.3, -54.6 and -75.3
-- the higher the note, the fewer samples a period to guess between
(`Resample`). The same reading converts a file's rate: a WAV recorded at
22,050 is read at half speed to play at 44,100 (`Wav.of_string` also
walks the file's chunks and mixes stereo down).

## 10. The audio loop: latency and the two clocks

The sound card pulls samples at its own steady rate, in **blocks** (say
1024 samples: 23 ms at 44,100 Hz); if a block isn't ready in time, it
plays silence or garbage, an audible **click** -- unlike a late frame,
which just shows a bit later. The game, meanwhile, runs at 60 frames
per second, 735 samples per frame. So the two run apart: each frame
decides *what* should be playing (the sounds its `update` starts or
keeps), and the audio side renders it ahead, into a queue, a few
blocks in advance. The
cost of the margin is **latency**, the time from a key press to its
sound: a couple of blocks is 50 ms, noticeable in a rhythm game, fine
for a shoot-em-up. The same trade-off as the physics' fixed time step
(`notes_2d_physics.md` §7): determinism and safety vs responsiveness.

## 11. Compared with SDL_mixer, Web Audio, and SuperCollider

**Playing vs synthesizing.** SDL_mixer, OpenAL, FMOD and elm-audio
*play recordings*: they decode WAV, OGG or MP3 files, resample them,
mix channels, stream music from disk, and (OpenAL, FMOD) place them in
3D. Synthesis is the exception there, and the rule here: nearly every
sample is computed, a WAV file played the exception (§9), and the only
thing we ask of SDL is a queue of samples (`SDL_QueueAudio`, topped up
to 3 frames, 50 ms, ahead). Stereo, 3D positioning (OpenAL's own
distance model and Doppler, §5) and a reverb (§8) we have now; what
they have that we don't: compressed formats, streaming from disk, a
real-time audio thread, and HRTFs.

**Web Audio.** The browser's API is a graph of nodes
(`OscillatorNode`, `BiquadFilterNode`, `GainNode`) run in native code
on the browser's audio thread: §3 and §7 without computing a sample.
The web backend uses none of them, only an `AudioBuffer` per frame
filled with our samples, so a sound is the same, sample for sample, as
natively and in the golden WAVs. The price: synthesis in JavaScript on
the main thread, and about 100 ms of latency.

**Synthesis languages.** SuperCollider, Csound or Pure Data compute
their unit generators a block at a time (64 samples, say) on a
real-time thread, and the graph can change while it plays. `Synth`
renders a one-shot whole, the frame it's played (`Synth.render`), and a
loop once; only the continuous voices (`keep_playing`) are computed a
pull at a time. Simple, and deterministic, but a two-second bell costs
its two seconds of samples in one frame. The whole landscape (the
chips, the languages, the middleware):
[`notes_audio_related_work.md`](../related-work/notes_audio_related_work.md).

## 12. What's missing, and exercises

In rough order of difficulty:

- **pink noise** (§3): white noise through a few one-pole low-passes
  summed, or Voss's algorithm; in `Noise`, next to the LFSR;
- **a wah on a continuous sound**: `keep_playing` filters with the
  cutoff of the frame (the biquad's memory carried from pull to pull),
  but ignores a `wah`'s sweep, which is over a sound's length;
- **a low-pass before reading faster** (§9): a recording read an octave
  up has its highs above Nyquist, folded (§2); a good sampler filters
  first, as a good image scaler does;
- **the ears' delay on a moving sound** (§5): a continuous voice's pan
  changes every frame, and so would its delay -- a fractional delay
  line per ear, read with the interpolation of §9, or it clicks; then
  HRTFs, the ear's own filtering, which also tell front from back and
  above from below;
- **a better reverb** (§8): Schroeder's is metallic, its echoes too
  regular; Freeverb (2000) uses eight combs with low-passes in them, and
  a convolution with a real room's recorded echo is exact;
- **a MOD player**: Amiga music is recordings played at the notes of a
  tracker's patterns -- §9's sampler with a sequencer
  (`notes_audio_midi.md` §9);
- **the audio off the frame**: rendering in an OCaml 5 domain, a block
  at a time, so a long sound doesn't cost the frame it starts;
- **Web Audio's own nodes** (the plan's phase 4, left): a web backend
  building an `OscillatorNode` and a `BiquadFilterNode` per voice
  instead of our samples, and the two compared, by ear and in latency.

## 13. In the playground

The API is `playground/Audio.mli`, in the `elm_playground` library, so
every backend has it. A **sound** is a value, like a shape: made from a
few numbers (`tone`, `square`, `triangle`, `sawtooth`, `noise`, §3;
`fm`, §7; `pluck`, §8; `note "C4"`, §9) or a recording (`wav`, a
file's; `recorded`, a sound frozen; §9), shaped by verbs like `move`
and `scale` (`lasting`, `fading`, §4; `louder`, §5; `naive`, §6;
`low_pass`, `high_pass`, `wah`, §7; `sliding`, `vibrato`, `arpeggio`,
`echo`, `reverb`, §8; `faster`, §9; `pan`, `from`, `pitched`, §5), and
combined with `after` and `together`, Euterpea's two operators;
`blip`, `coin`, `jump`, `laser`, `hit`, `explosion`, `step` and
`powerup` are ready-made, `audio/Sfx`'s presets (§8), `varied` nudges
one (a new seed, a new shot), `random_sound` draws a new one of a
kind, and `sfx` makes one's own from sfxr's numbers. Tunes are text,
`abc` and `doremi`, or a MIDI file, `midi` (§9, `notes_audio_midi.md`).
In a 3D world, `Audio3d.heard` places a sound as the camera hears it
(§5).

Unlike pictures, sounds are *commands*: `Audio.play` in `update`, when
the ball bounces, fire and forget -- the one impure call of the
playground (the `.mli` says why, and what elm-audio does instead).
`keep_playing name s`, called every frame, is a continuous sound whose
pitch and volume change smoothly (§10's two clocks: the phase goes on
between pulls); `loop` and `stop` are the music, `change_loop` the
same music changed (faster) from the same note, `loop_from` a tune
from a file or a URL, and `position` the music's own clock, what a
rhythm game judges a step by (§10). Underneath, `Mixer` sums them
through tanh (§5), and the platform pulls its samples: the SDL queue
natively, an `AudioBuffer` in the browser (§10).

The examples, one idea each: `examples/AudioTheremin.ml`, the whole
instrument one `keep_playing` line; `examples/AudioPiano.ml`, the
keyboard as a piano, space switching the waveform (and a fifth
timbre, the plucked string) (§3's timbre, §9's
notes); `examples/AudioAliasing.ml`, a square's spectrum, its aliases
in red, space switching naive and band-limited (§2, §6);
`examples/AudioSpace.ml`, a car going by, panned, fading with the
distance and Doppler-shifted, each of the three switchable (§5);
`examples/AudioSampler.ml`, one recording played at every key, the
three ways of reading between samples on space (§9);
`examples/AudioSfx.ml`, the ready-made sounds on keys 1 to 8, their
numbers and their shape on screen, `r` a variation, `n` a random one,
`c` a cave (§8). The games:
`TinyBreakout.ml` (a brick's pitch from its row), `TinyMario.ml` (a
jump, footsteps, coins, a flag's arpeggio made with `after`, and its
music, an ABC tune or `music=` a `.mid` file), and the rhythm games,
`TinyDDR.ml`, `TinyGuitarHero.ml`, `TinyRockBand.ml`, judged by
`Audio.position`; `Asteroid.ml` (a bang by size, the thrust as
noise through a low-pass brightening with speed, kept playing, the
heartbeat speeding up as the asteroids get fewer), `Pong.ml` and
`TinyPong.ml` (Pong's three blips, TinyPong's rising with the rally's
speed), `Snake.ml` (a crunch, a fall), `Tetris.ml` (Korobeiniki,
faster at each level, `change_loop`; a thud, a chime, a "Tetris"),
`TinyStarFox.ml` (every sound heard from the camera, the enemies'
engines Doppler-shifted as they go by); about thirty more play the
ready-made sounds. No game uses `fm` or `wah` yet: only the golden WAVs
do.

To see the sound: the software backend's `v` key (with `-debug-keys`)
draws `Audio_debug`'s oscilloscope, then spectrum (§6), over the frame;
its `l` key turns the band-limited oscillators off, the aliases back,
and its `m` key stereo off, one channel.
To test it: the golden WAVs of `audio/tests/`, compared sample by
sample, and `-dump-audio file` with `-dump-frame` writing a game's
sound to a WAV.

## Glossary

- **Sample**: one measure of a sound's pressure, a number; **sample
  rate**: how many per second (44,100 Hz here).
- **Frequency**: periods per second, in Hz; heard as pitch.
  **Amplitude**: heard as loudness.
- **Nyquist frequency**: half the sample rate, the highest representable
  frequency. **Aliasing**: a higher one folded back below it.
- **Oscillator**: a periodic wave generator; **phase accumulator**: its
  0-to-1 counter.
- **Harmonics**: the multiples of a sound's frequency it contains;
  their recipe is the **timbre**.
- **Envelope**, **ADSR**: the volume over time.
- **Clipping**: samples cut at the limits; **soft clipping**: bent
  smoothly instead.
- **Decibel (dB)**: 20 log10 of an amplitude ratio.
- **Spectrum**, **Fourier transform**, **FFT**: a sound's frequencies,
  and how to compute them fast.
- **Band-limited**: without harmonics above Nyquist.
- **Filter**: low-pass, high-pass, resonance, cutoff.
- **Equal temperament**: 12 equal semitones per octave, 2^(1/12).
- **Sequencer**, **tracker**: notes played at times, in patterns.
- **Latency**: the delay from an event to its sound; **buffer**, **block**:
  the samples handed to the sound card at a time.
- **Pan**: where between the left and the right a sound is; **pan law**:
  how that becomes two gains (constant power: as loud everywhere).
- **Doppler shift**: the pitch changed by a source's or a listener's
  motion.
- **Delay line**: a circular buffer of the last samples; **comb**,
  **all-pass**: filters built on one; **reverb**: many echoes, a room.
- **Resampling**: reading a recording at another rate; **interpolation**:
  guessing between its samples (nearest, linear, cubic).

## References

- Joseph Fourier, "Théorie analytique de la chaleur", Firmin Didot,
  1822.
- H. Nyquist, "Certain Topics in Telegraph Transmission Theory",
  Transactions of the AIEE, 1928.
- Robert S. Woodworth, "Experimental Psychology", Holt, 1938 (the time
  between the ears).
- C. E. Shannon, "Communication in the Presence of Noise", Proceedings
  of the IRE, 1949.
- M. R. Schroeder, "Natural Sounding Artificial Reverberation", Journal
  of the Audio Engineering Society, 1962.
- M. V. Mathews, "The Digital Computer as a Musical Instrument",
  Science, 1963.
- James W. Cooley, John W. Tukey, "An Algorithm for the Machine
  Calculation of Complex Fourier Series", Mathematics of Computation
  19(90):297-301, 1965.
- Solomon W. Golomb, "Shift Register Sequences", Holden-Day, 1967 (the
  LFSR).
- John M. Chowning, "The Synthesis of Complex Audio Spectra by Means of
  Frequency Modulation", Journal of the Audio Engineering Society
  21(7), 1973.
- Edwin Catmull, Raphael Rom, "A Class of Local Interpolating Splines",
  1974.
- Kevin Karplus, Alex Strong, "Digital Synthesis of Plucked-String and
  Drum Timbres", Computer Music Journal 7(2), 1983; David Jaffe, Julius
  O. Smith, "Extensions of the Karplus-Strong Plucked-String
  Algorithm", same issue (the tuning).
- ISO 9613-1:1993, "Attenuation of sound during propagation outdoors,
  part 1: calculation of the absorption of sound by the atmosphere".
- Tim Stilson, Julius O. Smith, "Alias-Free Digital Synthesis of
  Classic Analog Waveforms", International Computer Music Conference
  (ICMC), 1996 (BLIT, the band-limited impulse train).
- Curtis Roads, "The Computer Music Tutorial", MIT Press, 1996.
- Robert Bristow-Johnson, "Cookbook formulae for audio EQ biquad filter
  coefficients" (the Audio EQ Cookbook), 1998.
- James McCartney, "Rethinking the Computer Music Language:
  SuperCollider", Computer Music Journal 26(4), 2002.
- Creative Labs, "OpenAL 1.1 Specification and Reference", 2005 (the
  distance models, Doppler).
- Vesa Välimäki, Antti Huovilainen, "Antialiasing Oscillators in
  Subtractive Synthesis", IEEE Signal Processing Magazine, 2007
  (PolyBLEP).
- Miller Puckette, "The Theory and Technique of Electronic Music",
  World Scientific, 2007.
- Julius O. Smith III, "Mathematics of the Discrete Fourier Transform"
  and "Introduction to Digital Filters", W3K Publishing, 2007; online
  at https://ccrma.stanford.edu/~jos/ with his other books.
- Andy Farnell, "Designing Sound", MIT Press, 2010.
- Paul Hudak, Donovan Quick, "The Haskell School of Music: From Signals
  to Symphonies", Cambridge University Press, 2018 (`after` and
  `together`).
- W3C, "Web Audio API", W3C Recommendation, 2021.
