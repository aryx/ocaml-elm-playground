# Plan: what's left for sound

The plan is done: see [`done/plan_audio_teaching.md`](../done/plan_audio_teaching.md)
-- `audio/`, a software synthesizer computing every sample itself:
`Signal`, `Oscillator` (naive, PolyBLEP, PolyBLAMP), `Noise` (the NES's
LFSR), `Envelope`, `Mix`, `Filter`, `Fm` (Chowning, 1973), `Pluck`
(Karplus-Strong, tuned), `Effect` (vibrato, arpeggio, echo, Schroeder's
reverb), `Sfx` (sfxr, 2007), `Music` (ABC, solfège, MIDI files, drums),
`Spectrum` (the DFT and the FFT), `Space` (pan laws, the ears' delay,
air, Doppler), `Resample`, `Synth` and `Mixer`; the Evan-style
`Audio.mli` over it, SDL natively and an AudioBuffer in a browser;
`Audio3d` for the 3D games; the examples AudioTheremin to
AudioSampler; TinyMario, Asteroid, Tetris, TinyStarFox and some thirty
more games heard.

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli` and its test.

## 1. A live MIDI keyboard (phase 8)

- A real keyboard for AudioPiano (and the Tiny synthesizers):
  ALSA, CoreMIDI or PortMidi natively, which need bindings; Web MIDI
  in the browser.
- MIDI's **pitch bend** and **control changes** heard, in files as
  well as live (a channel's volume and pan first).
- A voice limit for a score's notes.

## 2. Web Audio's own nodes (phase 4)

- The other web backend, the "SVG" way: an `OscillatorNode`, a
  `GainNode` for the envelope and a `BiquadFilterNode` per voice, the
  browser synthesizing, no samples of ours.
- The two compared, by ear, in CPU and in latency: the reason the
  phase kept it.

## 3. The exercises of `notes_audio.md` §12

- **Pink noise**: white noise through a few one-pole low-passes summed,
  or Voss's algorithm; in `Noise`, next to the LFSR.
- **A wah on a continuous sound**: `keep_playing` filters with the
  frame's cutoff, and ignores a `wah`'s sweep.
- **A low-pass before reading faster**: a recording read an octave up
  folds its highs (`Resample` and `Paula` say so, and don't).
- **The ears' delay on a moving sound**: a fractional delay line per
  ear, or it clicks; then HRTFs, which tell front from back and above
  from below.
- **A convolution reverb**, a real room's recorded echo (Schroeder's
  and Freeverb are built).
- **The audio off the frame**: rendering in an OCaml 5 domain, a block
  at a time, so a long sound doesn't cost the frame it starts.

## 4. The API, revisited

- The decision of 2026-09-19 made `Audio` stateful (`play`,
  `keep_playing`), not elm-audio's declarative sounds of the model.
  The price, in the `.mli`: an `update` run twice for a frame (a
  time-travel debugger) plays its sounds twice. The user: "we can
  always revisit and find a more Evan-like API later".

## 5. Out of scope, still

- Recording from a microphone.
- OGG Vorbis (MP3 is decoded now: `formats/mpeg_audio/`).
- Real-time safety beyond the queue (no audio thread in OCaml).

## 6. Checks not yet made

- By ear: each example and game, natively and in a browser. The
  checks were on dumped WAVs (-dump-audio), their pitches and levels
  measured; headless Chrome's audio clock barely moves.
- Latency, a key press to a sound, under a frame or two: only
  measured through TinyDDR's calibration.
- Golden WAVs of whole game runs (the golden runner passing
  -dump-audio), and a mute key among the debug keys.
