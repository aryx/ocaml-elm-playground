# audio/: a software synthesizer, from scratch, for teaching

One idea per module, each `.mli` with its diagram, worked example and
references; independent of the Playground (`Audio.ml`, in
`playground/apis/`, is the Evan-style API over it, `Audio3d.ml` hears it from a 3D camera,
`Audio_debug.ml` shows it). Pure OCaml, so every backend, the web
included, has it. The tutorials are `docs/claude_notes/tutorials/`'s
`notes_audio.md` (digital audio, and a sound rendered ahead),
`notes_synth.md` (a synthesizer played live, and its effects) and
`notes_audio_formats.md` (the files).

## The folders

A library per folder, in layers, each on the ones before it:

    signal --> synthesis --> engine --> instruments / effects
       \
        '--> formats

| folder (library) | what | modules |
|---|---|---|
| `signal/` (`audio_signal`) | the samples, and what's computed on them | `Signal`, `Resample`, `Mix`, `Spectrum` |
| `formats/` (a library each) | the files sounds and songs are kept in | WAV, MIDI, ABC and solfège, MOD; MP2 and MP3 decoded (`mpeg_audio/`: `Mpeg_audio`, `Layer2`, `Layer3`, `Imdct`, `Polyphase`) |
| `synthesis/` (`audio_synthesis`) | the building blocks of a sound | `Oscillator`, `Noise`, `Fm`, `Pluck`, `Envelope`, `Filter` |
| `audio/` itself (`audio`), the engine | a sound as a tree rendered ahead; tunes; stereo and 3D; the sounds playing, pulled by the sound card; the interfaces of what's played live | `Synth`, `Sfx`, `Pitch_effect`, `Music`, `Space`, `Mixer`, `Tape` (a multitrack recorder), `Instrument`, `Control` |
| `instruments/` (`audio_instruments`) | the live blocks of a synthesizer's voice | `Vco`, `Lfo`, `Drift`, `Voicing` (a monophonic voice's keys), `Polyphony` (a voice per key), `Sequencer` (steps in the audio clock), `Moog_ladder`, `Diode_ladder` (the TB-303's), `Svf`, `Dx_envelope` and `Fm_algorithm` (the DX7's), `Modal` (a struck resonator) |
| `effects/` (`audio_effects`) | the live effects, and the rack that chains them | `Effect` (the interface), `Drive`, `Eq`, `Modulated_delay` (chorus, flanger), `Phaser`, `Modulation` (the rack's slot for the three), `Delay`, `Reverb`, `Dynamics` (compressor, limiter, gate), `Leslie` (the rotating speaker), `Rack` |

Why these layers:

- `signal/` is below everything, the video formats' sound included:
  samples, and arithmetic on them, know nothing of music.
- `synthesis/` is what the engine renders with; its modules are
  formulas and small states over samples, usable ahead (`Synth`) or
  live (`instruments/`).
- The engine holds the *interfaces* of what is played live,
  `Instrument.t` and the `Control` of a knob, because its `Mixer` plays
  instruments; the instruments and effects themselves sit above it.
- `instruments/` needs only `signal/` and `synthesis/`: an oscillator
  run block after block, a filter moved all the time, the keys to a
  voice. The voices built from them are the apps' (a Minimoog is
  `apps/music/`'s `Voice_minimoog`).
- `effects/` needs the engine only for `Control`. Each effect has its
  own typed interface (its tests and worked example use it) and an
  `Effect.t`, the same kind of record of functions as `Instrument.t`,
  for the `Rack` to hold effects of every kind in an order it can
  change.

A new module goes in the lowest layer it can: a filter that any sound
might use in `synthesis/`, one that only makes sense live in
`instruments/`, a processor with knobs in `effects/`.

## Tests

`audio/tests/` (`make test`): each `.mli`'s worked example measured,
and golden WAVs compared sample by sample (`make approve-golden-audio`
after listening to `_build/default/libs/audio/tests/actual/*.wav`); the
formats' in `formats/tests/`.
