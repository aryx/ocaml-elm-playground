# Plan: sound for the playground, from scratch, for teaching (`audio/`)

## Context

`graphics/` teaches how pictures are computed, `physics/` (planned,
[`plan_physics_teaching.md`](plan_physics_teaching.md)) how motion is;
this plan does the same for **sound**: a small software synthesizer
under `audio/` that computes every sample itself, and teaches **digital
audio and signal processing** -- sampling and the Nyquist limit,
oscillators, envelopes, mixing, filters, the spectrum -- the audio
twin of a software rasterizer. And, like `Playground` for pictures, a
small **Evan-style API** over it, so that a game says what it sounds
like, not how: a beep when the ball hits the paddle, an explosion when
an asteroid breaks.

Today the playground is silent: no backend plays sound, and Evan's
elm-playground has none either. The games are waiting for it: Pong's
beeps, Asteroid's thrust, shots and explosions (and the arcade game's
famous two-note heartbeat, speeding up), Spacewar!'s torpedoes,
Slingshot's impacts, Snake's crunch, Tetris's theme.

Companions: [`notes_audio.md`](notes_audio.md), the tutorial (written
ahead of the code, as its specification),
[`notes_audio_midi.md`](notes_audio_midi.md) (music as data: MIDI), and
[`notes_audio_related_work.md`](notes_audio_related_work.md) (the
chips, the synthesizers, the languages and libraries, and the teaching
lineage).

## Principles (the same as `graphics/` and `physics/`)

- **Independent of the Playground.** `audio/` knows samples, signals,
  frequencies and seconds: no `computer`, no SDL. `playground/Sound.ml`
  is the adapter.
- **Generated, not loaded.** Like the playground's shapes (no image
  needed for a circle), sounds are made from a few parameters -- a
  frequency, a waveform, an envelope -- not from files: the 8-bit,
  sfxr-style sound of arcade games, and every sound readable as code.
  (Loading WAV/OGG files: later, like `image` for pictures.)
- **One idea per module, the simple and the better version side by
  side** (e.g. a naive square wave, and a band-limited one that
  doesn't alias), switchable, so the difference can be *heard* -- and
  *seen*: an oscilloscope and a spectrum drawn over the frame, the
  magnifier of sound.
- **Every `.mli` explains its idea** with a diagram of the waveform, a
  worked example with numbers, and its reference; `audio/tests/`
  checks the examples.
- **Deterministic, so testable.** Synthesis is a pure function of time
  (noise from a seeded generator, like the NES's), so a sound can have
  a **golden WAV**, compared sample by sample, like the golden frames;
  and the spectrum lets tests check what a sound *contains* (a 440 Hz
  sine has one peak, at 440 Hz).
- **Comments describe the code as it is**; long explanations in the
  `.mli`s and the notes.

## The Playground API, Evan-style

Sound doesn't fit the Model-View-Update loop as easily as pictures:
`view` returns shapes to draw *now*, but a sound lasts, and starts at
an event (a collision) that happened in `update`. The Elm way, following
elm-audio's design: **describe what should be playing, as a function of
the model, like `view` describes what's on screen**; each sound says
*when it started*; the backend compares with what's already playing,
starts the new ones, stops the ones that disappeared. No "play" command,
no handles, no callbacks -- and it replays, pauses and tests like
pictures do.

Tentative (`playground/Sound.mli`), to be refined by writing the games
with it:

```ocaml
type sound                                  (* a value, like a shape *)

(* making sounds *)
val tone : number -> sound                  (* a sine wave, in Hz *)
val note : string -> sound                  (* "C4", "A4" (440 Hz), "F#5" *)
val square : number -> sound                (* the retro ones *)
val triangle : number -> sound
val noise : sound                           (* explosions, drums, wind *)

(* shaping them, like move/scale/fade for shapes *)
val lasting : number -> sound -> sound      (* seconds *)
val fading : sound -> sound                 (* an envelope: attack, decay *)
val louder : number -> sound -> sound       (* volume, 0. to 1. *)
val sliding : number -> sound -> sound      (* pitch sweep: lasers, jumps *)
val together : sound list -> sound          (* mixing, like group *)
val after : sound -> sound -> sound         (* one, then the other *)

(* ready-made game sounds, sfxr-style *)
val blip : sound  val jump : sound  val coin : sound
val laser : sound  val explosion : sound  val hit : sound

(* when: since a time, like computer.time *)
val since : time -> sound -> sound

(* the app: a game with sounds, [sounds] beside [view] *)
val game_with_sounds :
  (computer -> 'model -> shape list) ->      (* view *)
  (computer -> 'model -> sound list) ->      (* sounds: what's playing *)
  (computer -> 'model -> 'model) -> 'model -> ('model, msg) app
```

Pong then says:

```ocaml
let sounds computer game =
  [ blip |> since game.last_bounce;
    explosion |> since game.last_point ]
```

and a theremin, in a few lines (`examples/Theremin.ml`):

```ocaml
let sounds computer () =
  [ tone (200 + computer.mouse.x) |> louder (0.5 + computer.mouse.y / 1000) ]
```

Open questions, to settle by writing the games with it: `since` with
the model storing event times (declarative, elm-audio's way) vs sounds
returned by `update` as events (simpler for beginners, less Elm-like);
continuous sounds (the theremin, a ship's thrust) vs one-shots; how a
sound keeps its identity from frame to frame, so the backend knows it's
the same one still playing.

## Target layout

```
audio/                    (audio, private, package elm_playground: pure
                          OCaml, rendered to samples; both backends use it)
  Signal                  samples, sample rate, buffers; time <-> samples
  Oscillator              sine, square, triangle, sawtooth, noise; the
                          phase accumulator; naive vs band-limited
                          (PolyBLEP)
  Noise                   pseudo-random generators: an LFSR like the NES's,
                          white vs pink noise
  Envelope                ADSR: attack, decay, sustain, release
  Mix                     summing, gain, decibels, clipping vs soft clipping
  Filter                  one-pole low/high-pass, the biquad (resonance)
  Effect                  pitch slide, vibrato, echo (delay line)
  Sfx                     the sfxr-style generator: presets and parameters
  Music                   notes and equal temperament, a small sequencer
  Midi                    MIDI messages and Standard MIDI Files (see
                          notes_audio_midi.md)
  Spectrum                the DFT, then the FFT; for tests and the display
  Wav                     writing (and later reading) WAV files
  Resample                changing sample rates (for loaded sounds, later)
audio/tests/              the worked examples; golden WAVs
playground/Sound.ml       the Evan-style API above, over audio/
native: SDL audio         a callback/queue feeding audio/'s samples
web: two ways             Web Audio's own oscillators and gain nodes (the
                          "SVG" way: the browser synthesizes), or audio/'s
                          samples in an AudioBuffer (the "software" way)
```

## Groundwork decisions

### The audio loop is not the frame loop

Pictures are computed 60 times a second; sound is 44,100 samples a
second, pulled by the sound card in blocks (e.g. 1024 samples, 23 ms),
on its own clock, and a late block is an audible click, not a slower
frame. So synthesis runs apart from `update`/`view`: each frame, the
game's `sounds` say what should be playing; between frames, the audio
side renders those sounds, sample by sample, ahead of time into a queue
(SDL's `SDL_QueueAudio` on native: no callback on another thread to
synchronize with OCaml). The notes explain latency, buffers and the
two clocks.

### Where synthesis happens on the web

Two backends in one, like 3D's SVG and WebGL: the browser's Web Audio
nodes (an `OscillatorNode`, a `GainNode` for the envelope: no samples
computed by us, low CPU, the "reuse the platform" way) or our
`audio/` samples copied into an `AudioBuffer` (exactly the native
sound, every sample ours). The second first: the same sound
everywhere, and the teaching one; the first as a comparison.

### Debugging sound by looking at it

With `-debug-keys`: an oscilloscope (the waveform of the last frame's
samples) and a spectrum (its FFT) drawn over the frame, a mute key, and
keys switching the simple and better versions (naive vs band-limited
oscillators, hard vs soft clipping). Sound's magnifier.

## The modules, with their references

(To double-check against the sources when writing each `.mli`.)

- **Signal**: sampling; Nyquist (1928) and Shannon (1949): a sample
  rate of 44,100 Hz can represent frequencies up to 22,050 Hz.
- **Oscillator**: the phase accumulator; aliasing of the naive square
  and sawtooth; band-limited synthesis (Stilson and Smith,
  "Alias-Free Digital Synthesis of Classic Analog Waveforms", ICMC
  1996; PolyBLEP, Välimäki et al., 2007). Worked example: a 1000 Hz
  naive square at 44,100 Hz has odd harmonics at 23, 25, 27 kHz, above
  Nyquist, which fold back to 21.1, 19.1, 17.1 kHz: audible, and not
  harmonics of 1000 Hz.
- **Noise**: linear-feedback shift registers (the NES APU's 15-bit
  LFSR); white vs pink.
- **Envelope**: ADSR (the Moog and ARP synthesizers, 1960s-70s).
- **Mix**: decibels (halving the amplitude is -6.02 dB); clipping and
  soft clipping (tanh).
- **Filter**: the one-pole low-pass (y += a (x - y)); the biquad
  (Robert Bristow-Johnson, "Audio EQ Cookbook").
- **Sfx**: sfxr (Tomas Pettersson, "DrPetter", 2007): a few parameters
  (waveform, frequency, slide, envelope, noise) cover most game sounds.
- **Music**: equal temperament (A4 = 440 Hz, a semitone is 2^(1/12));
  trackers (Ultimate Soundtracker, Amiga, 1987).
- **Spectrum**: the discrete Fourier transform; the FFT (Cooley and
  Tukey, 1965).
- **FM**, maybe (John Chowning, "The Synthesis of Complex Audio
  Spectra by Means of Frequency Modulation", 1973: the Yamaha DX7's
  sound in one formula).

## New examples

- `examples/Theremin.ml`: the mouse is the pitch and the volume; the
  oscilloscope shows the sine (with `-debug-keys`).
- `examples/Piano.ml`: the keyboard's letters play notes (equal
  temperament), each with an envelope; switch the waveform.
- `examples/Sfx.ml`: the sfxr presets on keys, and their parameters on
  screen: the teaching sfxr.
- `examples/Aliasing.ml`: a sweep up past Nyquist, naive vs
  band-limited: aliasing, heard and seen on the spectrum.

## Games with sound

Pong (a blip per bounce, a lower one per point), Asteroid (shots,
explosions by size, thrust as filtered noise, the heartbeat speeding
up), Snake, Tetris (Korobeiniki, the folk tune, on the sequencer: the
music demo), then the physics plan's Spacewar! and Slingshot (impacts
louder when harder: the collision impulse as the volume -- physics and
audio meeting).

## Phasing

0. **Groundwork**: `audio/` and `audio/tests/` skeletons; `Wav`
   (write), so every later phase can produce and compare golden WAVs;
   SDL audio opened in `Native_loop_2d` (a queue, off by default in
   the golden-frame dump mode).
1. **Signals and oscillators**: `Signal`, `Oscillator` (naive), `Noise`;
   tests: a 440 Hz sine's period (100.23 samples at 44,100 Hz), the
   LFSR's sequence, golden WAVs.
2. **Envelopes and mixing**: `Envelope`, `Mix` (clipping vs soft).
3. **The Playground API, v1 and the native backend**: `Sound` (`tone`,
   `note`, `square`, `noise`, `lasting`, `fading`, `louder`,
   `together`, `since`), `game_with_sounds`; the audio queue; Theremin,
   Piano; Pong's beeps. Settle the open questions by writing them.
4. **The web backend**: our samples in an `AudioBuffer`, then Web
   Audio's nodes as a comparison.
5. **Spectrum and the debug display**: `Spectrum` (DFT, then FFT, the
   simple one kept), the oscilloscope and spectrum overlay; tests on
   spectra (a sine's one peak, a square's odd harmonics).
6. **Band-limited oscillators and filters**: PolyBLEP vs naive (the
   Aliasing example), `Filter`; tests: aliases gone from the spectrum,
   a low-pass's attenuation at a known frequency.
7. **Game sounds**: `Effect`, `Sfx` (presets, sfxr's parameters), the
   Sfx example; Asteroid's and Snake's sounds.
8. **Music**: `Music` (notes, a sequencer of patterns); Tetris's
   theme. Then MIDI (`audio/Midi`, see `notes_audio_midi.md`): reading
   Standard MIDI Files into the sequencer, played by our synthesizer
   (General MIDI's programs mapped to our waveforms), and a real MIDI
   keyboard for the Piano example (ALSA/PortMidi on native, Web MIDI in
   the browser).
9. **Docs**: `notes_audio.md` checked against the code, numbers
   filled in.
10. *(later)* Loaded sounds (WAV, then OGG via stb_vorbis, the same
   stb as the images), `Resample`; positional sound for 3D (panning,
   distance, Doppler: from the physics bodies' positions and
   velocities).

## Status

- **The API decision (2026-09-19), before phase 3**: `playground/Audio.mli`
  (not `Sound`), Evan-style like `Physics.mli`, but **stateful** for
  the triggering, as the user suggested: sounds fit the pure
  Model-View-Update loop badly, and a beginner's "play a blip when the
  ball bounces" should be one call in `update`. Two kinds: one-shots,
  `Audio.play sound` (fire and forget, like PICO-8's `sfx(n)`); and
  continuous sounds, `Audio.keep_playing name sound`, called every frame
  while it should sound (a theremin, a ship's thrust), stopping the
  first frame it isn't, its phase kept from frame to frame. The sounds
  themselves stay values, composed like Paul Hudak's Euterpea (the
  library of The Haskell School of Music, Hudak and Quick, 2018; after
  his Haskore): `after` and `together` as its `:+:` and `:=:`, `line`
  and `chord` for music -- credited where borrowed. Determinism kept:
  the backend stamps each call with its frame, so a scripted run plays
  the same samples (golden WAVs of a game). The caveat for the `.mli`:
  an `update` run twice for a frame (a time-travel debugger) plays its
  sounds twice, the price elm-audio's declarative design avoids. To
  revisit (the user: "we can always revisit and find a more Evan-like
  API later").
- **Phase 0, DONE except the SDL side**: `audio/` (library `audio`,
  package elm_playground), `audio/tests/` (`Unit_signal`, and
  `Golden_wav`: sounds written to `actual/*.wav`, compared sample by
  sample with `golden/*.wav`, `make approve-golden-audio`, checked by
  plotting their waveforms when not by ear), `Wav` (the 44-byte header,
  16-bit mono PCM, writing and reading back). SDL's audio queue comes
  with phase 3, when there is something to play.
- **Phase 1, DONE**: `Signal` (the rate, Nyquist, `alias`: 30,000 Hz
  heard at 14,100), `Oscillator` (the phase accumulator, the four naive
  waveforms; a 1000 Hz square's 23rd, 25th, 27th harmonics at 21.1,
  19.1, 17.1 kHz), `Noise` (the NES's 15-bit LFSR: 32,767 steps in its
  long mode, 93 in its short one, from 1: checked by simulation before
  writing it). Golden WAVs: the four waveforms at 440 Hz and both
  noises, a quarter second each.
- **Phase 2, DONE**: `Envelope` (ADSR, straight lines; `percussive`;
  the release from wherever the level was when let go) and `Mix`
  (`add`, `gain`, `delay`, `then_`, decibels, hard vs soft clipping
  with tanh). Tests (`Unit_envelope`): the `.mli`'s ADSR example
  (0.5, 0.75, 0.5, 0.25, 0 at 0.005, 0.06, 0.3, 0.6, 0.7 s), the click
  (a tone cut near a peak ends high, an enveloped one silent), -6.02
  dB, two full sines peaking at 2, cut to 1 or bent to tanh 2. Golden
  WAVs: three beeps cut vs enveloped, a chord (A4 and E5) clipped hard
  vs soft (their waveforms plotted before approving).
- **Phase 3, DONE (native)**: the engine got `Music` (the notes'
  frequencies, equal temperament: the start of phase 8), `Synth`
  (sounds as values: voices at the leaves, `Together` and `After`
  above, Euterpea's (:=:) and (:+:), credited; each voice rendered with
  5 ms ramps, so nothing clicks; a voice's slide; the continuous
  voices' `continue` and `release`, their volume ramped over each
  pull, no zipper noise) and `Mixer` (the stateful part: one-shots read
  through, continuous voices kept by name, the sum through tanh, at
  most 32 one-shots so a backend not pulling can't pile them up).
  `playground/Audio.mli`, Evan-style: `tone`, `square`, `triangle`,
  `sawtooth`, `noise`, `note`; `lasting`, `fading`, `louder`,
  `sliding`, `together`, `after`; ready-made sounds after sfxr's
  categories (our own recipes, credited to sfxr's idea; the coin C6 then
  G6, not Nintendo's); `play`, `keep_playing`; `pull` for the
  platforms. Native: `Native_loop_2d.run ~pull_audio ~dump_audio`, an
  SDL audio device (S16, mono, 44,100), its queue topped up to 3
  frames (50 ms) ahead each frame, the two clocks never drifting; with
  -dump-frame, no device, 735 samples a frame, and `-dump-audio file`
  writing them as a WAV (both native backends). Tests (`Unit_synth`):
  the notes (A4 69, C4 261.63 Hz), durations, no clicks, a slide's
  periods (275 then 385 over two half seconds), the mixer's
  one-shots and continuous voices, the 32 cap. A race fixed on the way:
  the golden WAV tests run in parallel, and two of them could both
  create `actual/` (an intermittent failure under `make test`).
  TinyMario plays: a jump, steps (a foot every 30 pixels), coins, a
  fall, an arpeggio at the flag, checked by dumping a scripted run's
  sound (steps every 5 frames, the jumps at the script's frames).
  `examples/Theremin.ml` (keep_playing: the mouse's x the pitch over
  three octaves, y the volume) and `examples/Piano.ml` (a s d f g h j k
  and w e t y u, C4 to C5; space the waveform; the notes measured in
  the dumped sound: 260, 330, 390 Hz for C4, E4, G4); golden frames.
  The web and 3D backends don't pull yet: silent (phase 4). Left for
  later: golden WAVs of whole game runs (the golden runner would pass
  -dump-audio), a mute key.
- **Phase 8's music, DONE (moved up)**: tunes in ABC notation (Chris
  Walshaw, 1991; the 2.1 standard cited), not an invented format: a
  standard, plain text, and thousands of tunes typed in it.
  `audio/Abc`, a subset parser (notes and octaves, accidentals lasting
  to the bar line, lengths, rests, chords, triplets, dotted pairs, the
  major and minor key signatures, voices, inline fields; skipping
  comments, chord names, decorations, grace notes, ties, repeats);
  `Music.to_sound`, a tune on the NES's band (the first voice a square,
  the last the triangle, notes sounding 90% of their length).
  `Audio.abc`, `Audio.loop` (a named loop, rendered once, asking again
  harmless) and `Audio.stop`; `Mixer.loop`, `stop`, `looping`. Tests
  (`Unit_abc`): each piece of the notation (the `.mli`'s example first
  had =F for 0.5 s where it's 0.25, fixed to =F2), Frere Jacques as a
  two-voice round (16 s, the second voice at 4 s); a golden WAV of 3 s
  of the round, where the voices overlap. TinyMario: an original tune
  (8 bars, arpeggios over C F Dm G, a triangle bass) looping from the
  first frame, stopped at the flag; `music=file.abc` for the user's own
  tune (the decision: the user's choice 3, an original by default and
  their own file by the flag, the famous theme never in the
  repository), `music=off`. Checked by dumping a run's sound: the
  melody's first bar E4 E4 G4 G4 C5 C5 G4 G4. Then, the user's idea,
  `audio/Doremi`: tunes in solfège (do ré mi fa sol la si, fixed do;
  # and b; a sticky octave digit; lengths in beats after a colon; -
  a rest; tempo; voix), parsed into the same Abc.tune; its `.mli`
  tells Guido d'Arezzo's Ut queant laxis. Tests (`Unit_doremi`): the
  example, Au clair de la lune (16 beats: 8 s; a first count of 12
  beats was wrong), two voices, errors. `Audio.doremi`; TinyMario's
  music= file in solfège unless it ends in .abc; its sounds and music
  gathered in an Audio section.
- **MIDI, DONE (files; not yet a live keyboard)**: `audio/Midi`,
  Standard MIDI Files read (formats 0 and 1: chunks, variable-length
  quantities, running status, note offs as note ons of velocity 0,
  program changes, meta events, the tempo map walked from ticks to
  seconds; sysex skipped) and written (`of_tune`: ABC or solfège to
  MIDI, format 1, a track per voice). A MIDI score's notes overlap
  freely, so `Music.render_score` renders into one buffer, each note
  added at its start (`Synth.Samples`, a new leaf: a tree of a song's
  notes would allocate a song-length array per note), General MIDI's
  families mapped to our waveforms and channel 10 to noise drums.
  `Audio.midi`; TinyMario's music= takes a .mid too. Tests
  (`Unit_midi`): the VLQ table both ways, a chord in 7 bytes with
  running status, a tempo change mid-song, Frere Jacques from ABC to
  MIDI and back note for note; a golden WAV of that round played as
  MIDI. The trackers' formats (MOD, S3M, XM, IT) discussed in
  `notes_audio_midi.md` section 9 at the user's request, not parsed.
  Left: a real MIDI keyboard (ALSA/CoreMIDI/PortMidi need bindings;
  Web MIDI in the browser), MIDI's control changes and pitch bend,
  a voice limit.
- **Phase 4, DONE (our samples; the browser's own nodes left for the
  comparison)**: `playground/web/Playground_platform.ml` feeds Web
  Audio: each frame, `Audio.pull`'s samples into an AudioBuffer (mono,
  44,100, resampled by the browser), a buffer source started right
  after the previous one, ~100 ms ahead of the AudioContext's clock;
  resumed on the first input event (the autoplay policy), the samples
  dropped until then; through Ojs, vdom's JavaScript layer. And files
  by URL, the user's request: `Audio.loop_from name source` (a file or
  a URL; .mid, .abc, else solfège), through a fetcher each platform
  installs (`Audio.set_fetcher`): natively `Download.local_file` (a
  path read, a URL through curl, blocking once), in a browser an
  XMLHttpRequest (a plain name from the page's server; elsewhere if
  CORS allows). TinyMario's music= uses it (a URL works; no famous
  theme's URL in the repository, the user brings their own). Checked
  in a headless Chrome (a test page wrapping
  AudioBufferSourceNode.start, with the autoplay policy lifted):
  buffers started, no error, the original tune's samples (peak 0.66),
  and with ?music=test.mid the file fetched and its notes playing
  (peak 0.156, the computed 0.25 x 100/127 x 0.8); natively the same
  MIDI through loop_from (C4 then E4, measured). Not checked: hearing
  it in a real browser (headless Chrome's audio clock barely moves).
  Left: the Web Audio nodes version (OscillatorNode, GainNode) for
  comparison; the 3D backends' sound (their loops don't pull yet).
- **Phase 5, DONE**: `audio/Spectrum`: the DFT by its definition (N^2)
  and the FFT (Cooley-Tukey, radix 2, recursive: the butterfly), both
  kept, the first checking the second; magnitudes scaled so a sine of
  amplitude A reads A; the Hann window; bins to frequencies. Tests
  (`Unit_spectrum`): the `.mli`'s examples (an impulse's flat
  spectrum, a cosine's bins 1 and 7 at 4), FFT = DFT on random
  signals, a sine on a bin's one peak at its amplitude, a square of
  period 64's odd harmonics within 2% of 4 / (pi k) and its even ones
  at 0, Parseval. The debug overlay, `playground/Audio_debug`
  (Playground shapes, so any backend could draw it): the software
  backend's "v" key (with -debug-keys) cycles an oscilloscope (the
  last 23 ms, triggered on a rising zero crossing) and a spectrum (an
  FFT of the last 2048 samples, 90 bars on a log axis from 20 Hz to
  20 kHz, -80 to 0 dB); the platform records every pulled sample.
  Golden frames: TinyMario's music as both (and the "h" help's new
  line). The naive squares' aliasing is already visible on it, spread
  over the high frequencies: phase 6's subject.
- **Asked by the user (phase 5)**: subtractive synthesis (a rich wave
  through a resonant low-pass: phase 6's Filter), FM synthesis
  (Chowning: to add with phase 6, small, and made for the spectrum),
  additive (already: together of tones), vibrato and echo (phase 7's
  Effect).
- **The goal set by the user**: TinyMario with music and sounds when
  moving. So after phases 2 and 3, phase 8's `Music` (notes, the
  sequencer) comes before phases 5-7. The classic Super Mario Bros.
  theme (Koji Kondo, 1985) is Nintendo's copyrighted melody: not
  transcribed into the repository; either a tune file the user provides
  locally (a flag), or an original tune in its style -- the user's call.

## Verification

- `make test`: the worked examples, the spectra (frequencies present
  and absent), and golden WAVs of the examples' sounds, compared
  sample by sample (and a `make approve-golden-audio`).
- By ear: each example and game, on native and in the browser.
- Latency: a key press to a sound, under a frame or two.

## Out of scope

- Sampled instruments, music files (MOD, MIDI), recording from a
  microphone.
- Reverb beyond a simple echo, 3D audio beyond panning (phase 10).
- Real-time safety beyond the queue (no audio thread in OCaml).
