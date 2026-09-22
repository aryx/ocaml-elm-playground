# Sound, from scratch: a tutorial for `audio/`

How a computer makes sound: the few ideas every synthesizer, from Max
Mathews's first computer music at Bell Labs (1957) to the NES's five
channels and a modern game's mixer, is built from, where they came
from, and what goes wrong when they're done naively. It's also the
specification of `audio/` (see
[`plan_audio_teaching.md`](../plans/plan_audio_teaching.md)): written before the
code, its pointers name the planned modules. Companions:
[`notes_2d.md`](notes_2d.md) (pictures), [`notes_2d_physics.md`](notes_2d_physics.md)
(motion), and [`notes_audio_related_work.md`](../related-work/notes_audio_related_work.md).

The parallel with pictures runs all the way through, and is worth
keeping in mind: a picture is a grid of pixels, a sound is a row of
samples; drawing a circle computes pixels from a formula, playing a
tone computes samples from one; jagged edges and aliased tones are the
same mistake, with the same cure.

## 0. Where the code is, and a reading order

| module (`audio/`, planned) | what | section |
|---|---|---|
| `Signal` | samples, sample rate, time | §1, §2 |
| `Oscillator` | sine, square, triangle, sawtooth; naive and band-limited | §3, §6 |
| `Noise` | random signals; the NES's LFSR | §3 |
| `Envelope` | ADSR: how a sound starts and ends | §4 |
| `Mix` | adding sounds, volume, decibels, clipping | §5 |
| `Spectrum` | the Fourier transform: which frequencies a sound has | §6 |
| `Filter` | low-pass, high-pass, resonance | §7 |
| `Fm` | FM synthesis: sidebands from two sines | §7 |
| `Effect`, `Sfx` | slides, vibrato, echo; game sounds from parameters | §8 |
| `Music` | notes, equal temperament, a sequencer | §9 |
| `Wav` | writing samples to a file | §2 |
| `playground/Sound` | the Evan-style API over all of it | §11 |

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
and what the golden tests compare.)

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
parameters, a "random laser" button. `Sfx` is the same, readable:
`jump` is a square wave, an attack of 0, a decay of 0.2 s, a slide up
-- five numbers.

An **echo** is the sound plus itself delayed and quieter, from a
**delay line** (a circular buffer of the last samples): the simplest
effect with memory, and the start of reverberation.

## 9. Notes and music

Western music divides each octave (a doubling of the frequency) into
12 **semitones**, all the same ratio: 2^(1/12) = **1.0595** (**equal
temperament**, standard since the 18th-19th centuries). From A4 = 440
Hz, A5 is 880 Hz, and C4 (middle C, nine semitones below A4) is 440 x
2^(-9/12) = **261.63 Hz**. `Music.note "C4"` is that formula.

A **sequencer** plays notes at times: a pattern of steps, like the
**trackers** of the Amiga (1987) and the NES's music drivers -- rows
of notes, one per channel, played at a tempo. Tetris's theme (the
Russian folk song Korobeiniki) is a few such rows. The standard way
to store and send notes, MIDI, has its own note:
[`notes_audio_midi.md`](notes_audio_midi.md).

## 10. The audio loop: latency and the two clocks

The sound card pulls samples at its own steady rate, in **blocks** (say
1024 samples: 23 ms at 44,100 Hz); if a block isn't ready in time, it
plays silence or garbage, an audible **click** -- unlike a late frame,
which just shows a bit later. The game, meanwhile, runs at 60 frames
per second, 735 samples per frame. So the two run apart: each frame
decides *what* should be playing (the game's `sounds`), and the audio
side renders it ahead, into a queue, a few blocks in advance. The
cost of the margin is **latency**, the time from a key press to its
sound: a couple of blocks is 50 ms, noticeable in a rhythm game, fine
for a shoot-em-up. The same trade-off as the physics' fixed time step
(`notes_2d_physics.md` §7): determinism and safety vs responsiveness.

## 11. In the playground

The API (`playground/Sound.mli`, planned) follows Evan's rule, values
not commands: a **sound** is a value (`tone 440`, `note "C4"`, `blip`,
`explosion`) shaped by functions (`lasting`, `fading`, `louder`,
`sliding`, `together`), and a game says **what's playing**, as a
function of its model, beside its `view`: each sound *since* the time
of its event (`blip |> since game.last_bounce`). The backend compares
with what's already playing and starts or stops sounds -- the game
never says "play". Pong gets its beeps in two lines, a theremin fits in
three, and every sound is reproducible, so testable.

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
