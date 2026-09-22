# audio/ vs. the rest of the computer sound world

The sound twin of [`notes_playground_related_work.md`](notes_playground_related_work.md)
(2D graphics) and [`notes_physics_related_work.md`](notes_physics_related_work.md)
(physics): where the planned `audio/` synthesizer and its Evan-style
`Sound` API (see [`plan_audio_teaching.md`](../plans/plan_audio_teaching.md))
come from, and where they sit among the chips, synthesizers, languages
and libraries that make computer sound. The same through-line: **the
real systems are designed to sound as good, or run as fast, as
possible; `audio/` is designed to be as *legible* as possible**, and
`Sound` as *simple* as possible.

## The one-line version

| | What it optimizes for | What you see / write |
|---|---|---|
| Sound chips (Atari TIA, NES APU, C64 SID, Yamaha FM) | Game sound from a few registers, in 1977-1990 hardware | Register writes: frequency, volume, waveform |
| MUSIC-N, Csound, SuperCollider, Pure Data, Max, ChucK | Composing and synthesizing music, any sound imaginable | A language of unit generators (oscillators, filters) patched together |
| Sonic Pi, TidalCycles/Strudel, Euterpea | Live coding and teaching music with code | `play 60`, patterns as strings, music as data |
| Web Audio, SDL/SDL_mixer, OpenAL, FMOD, Wwise | Playing and mixing sounds in apps and games | A graph of nodes or channels, sound files, handles |
| sfxr and its heirs, PICO-8 | Making retro game sounds in seconds | A dozen sliders, a "random" button |
| elm-audio | Sound in Elm's architecture | Sounds described from the model, like `view` |
| `audio/` + `Sound` | Every sample readable, and sounds as values | `blip \|> since game.last_bounce` over one module per idea |

## Part 1: the chips -- where game sound came from

Early game consoles had no samples to play; they had tiny
**synthesizers** in silicon, and the programmer wrote their registers:

- **Atari 2600's TIA** (1977): two channels, a handful of waveforms
  from shift registers, and notoriously out-of-tune frequencies.
- **The NES's 2A03 APU** (Nintendo, 1983): two square ("pulse") waves
  with four duty cycles, a triangle for the bass, a noise channel (the
  15-bit LFSR of `notes_audio.md` §3), and low-quality samples. Mario's
  and Zelda's music, from five channels; the reference for `audio/`'s
  retro sound.
- **The Commodore 64's SID** (Bob Yannes, 1982): three oscillators,
  ADSR envelopes, and a real analog filter with resonance: a small
  analog synthesizer on a chip, loved by musicians to this day.
- **Yamaha's FM chips** (the OPL in PC sound cards, the Sega Mega
  Drive's YM2612), from John Chowning's frequency modulation (Stanford,
  1973, licensed to Yamaha: the DX7, 1983): one sine modulating
  another's frequency, rich timbres from two oscillators.
- **MIDI** (1983): notes as messages between instruments, then as
  files; see [`notes_audio_midi.md`](../tutorials/notes_audio_midi.md).
- **Trackers** (Ultimate Soundtracker, Amiga, 1987): music as rows of
  notes playing short samples, the MOD format; the ancestor of the
  sequencers in PICO-8 and every chiptune tool.

## Part 2: synthesis languages and environments

- **MUSIC I-V** (Max Mathews, Bell Labs, 1957-): the first computer
  music, and the idea everything since uses: **unit generators** --
  oscillators, envelopes, filters -- connected into instruments, then
  played from a score. `audio/`'s modules are unit generators.
- **Csound** (Barry Vercoe, MIT, 1986): MUSIC-N's heir, still used.
- **Max** and **Pure Data** (Miller Puckette, 1980s and 1996): patching
  boxes with wires, visually; Puckette's free book, *The Theory and
  Technique of Electronic Music* (2007), is one of the best textbooks.
- **SuperCollider** (James McCartney, 1996): a language and a real-time
  synthesis server; **ChucK** (Ge Wang and Perry Cook, 2003):
  "strongly-timed", sample-accurate timing in the language.
- **Sonic Pi** (Sam Aaron, Cambridge, 2012): live coding music, built
  for teaching programming in schools (on the Raspberry Pi) --
  `play 60`, `sleep 0.5` -- the closest in spirit to the playground's
  audience. **TidalCycles** (Alex McLean, Haskell, 2009) and its
  browser port **Strudel**: patterns of notes as a small language.
- **Euterpea** (Paul Hudak, *The Haskell School of Music*): music and
  sound synthesis as functional programming, music as data -- the
  functional ancestor of `Sound`'s sounds-as-values.

## Part 3: libraries for playing sound

- **The Web Audio API** (W3C, first in Chrome, 2011): a graph of nodes
  (`OscillatorNode`, `GainNode`, `BiquadFilterNode`, ...) the browser
  runs; enough to synthesize without computing a sample. The web
  backend's two ways (`plan_audio_teaching.md`) are: use its nodes, or
  fill its buffers with our samples.
- **SDL audio** (1998-): a device, a format, and a callback or queue of
  samples -- nothing more; the native backend's layer, like SDL's
  pixel buffer for graphics. **SDL_mixer**: channels and music files on
  top.
- **OpenAL** (Loki Software, 2000): 3D positional sound, OpenGL's
  counterpart; **FMOD** and **Wwise**: the game industry's middleware,
  with editors for sound designers, events, 3D mixing.
- **In OCaml**: **Liquidsoap** (Savonet: a whole language, written in
  OCaml, for audio streaming, radio stations), and bindings to
  PortAudio, SDL audio, ALSA. No small synthesizer written to be read
  that I know of.

## Part 4: game sound tools

- **sfxr** (Tomas Pettersson, "DrPetter", 2007): a dozen parameters
  (waveform, frequency and its slide, envelope, vibrato, filters), and
  buttons -- "pickup/coin", "laser/shoot", "explosion", "jump" -- that
  randomize them in a sensible range; made for game jams, and cloned
  everywhere (bfxr, jsfxr, ChipTone). `Sfx`'s model, and its
  presets are `Sound`'s `blip`, `laser`, `explosion`.
- **PICO-8** (Lexaloffle, 2015): a fantasy console with its own sound
  and music editors -- 8 waveforms, 64 sounds, patterns: game audio
  made small enough to learn in an afternoon.
- **Andy Farnell, *Designing Sound*** (2010): procedural sound, every
  effect (footsteps, fire, engines) synthesized from its physics in
  Pure Data -- sounds computed, not recorded, the principle of `audio/`.

## Part 5: in Elm

Evan's elm-playground has no sound, and Elm itself none in its core:
sound is an effect, and Elm's architecture has no obvious place for
it. **elm-audio** (Martin Stewart) found one: an `audio` function from
the model to the sounds that should be playing, each with the time it
started, and the library diffs them against what's playing -- sound
described like `view` describes the screen. `Sound.since` and
`game_with_sounds` follow that design; the difference is underneath:
elm-audio plays loaded sound files through Web Audio, `audio/`
synthesizes every sample itself.

## Part 6: the teaching lineage

- **Nyquist (1928) and Shannon (1949)**: the sampling theorem.
- **Fourier (1822)**, **Cooley and Tukey (1965)**: the spectrum and its
  fast computation.
- **Miller Puckette, *The Theory and Technique of Electronic Music*
  (2007)**; **Julius O. Smith III's online books** (Stanford's CCRMA:
  digital filters, physical modeling); **Curtis Roads, *The Computer
  Music Tutorial* (1996)**, the encyclopedia; **Richard Boulanger and
  Victor Lazzarini, *The Audio Programming Book* (2010)**, in C.
- **Stilson and Smith (1996)**, **Välimäki et al. (PolyBLEP)**:
  aliasing-free oscillators.
- **Robert Bristow-Johnson's "Audio EQ Cookbook"**: the biquads.
- **Sonic Pi's tutorials** and **Euterpea**: teaching with sound, for
  beginners and for functional programmers.

## Where `audio/` and `Sound` actually sit

Two levels, like graphics and physics:

- **`audio/`, the synthesizer**, at the legible end: unit generators
  (Mathews's idea) one per module, each `.mli` with its waveform
  diagram, worked example and reference; the naive and the band-limited
  oscillator side by side, and an oscilloscope and a spectrum to *see*
  the difference, not only hear it; golden WAVs and spectra as tests.
  The NES's palette of sounds, computed by code a student can read.
- **`Sound`, the API**, at the simple end: sounds as values, shaped
  like shapes, and a game describing what's playing from its model,
  elm-audio's idea, in Evan's vocabulary: `blip |> since
  game.last_bounce`.

The ceiling, deliberate: no sampled instruments or music files (yet),
no real reverb, no 3D audio beyond panning -- enough for every game in
this project to sound like an arcade game, and small enough to read.

## Postscript: the numbers (to come)

Once built: lines of code of `audio/`, the CPU time of synthesis per
second of sound (vs real time), and the latency from a key press to its
sound, native and web.

Sources: from memory, to be checked before relying on them for
teaching -- the books and papers named above, the documentation of the
Web Audio API, SDL, sfxr, PICO-8, Sonic Pi and elm-audio, and general
knowledge of the chips' history.
