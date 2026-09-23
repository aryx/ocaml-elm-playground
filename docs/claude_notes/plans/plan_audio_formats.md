# Plan: sound and music formats, from scratch, for teaching (`audio/formats/`)

## Context

`graphics/images/` now reads the image formats with our own code, one
directory per format (`xpm/`, `png/` over `deflate/`, `gif/`, `jpeg/`),
each a classic of how to make a picture small
([`plan_images_teaching.md`](plan_images_teaching.md)). This plan does
the same for sound and music: **the files a sound or a song is kept
in**, each read (and, where it teaches something, written) by code
meant to be read, under `audio/formats/`.

`audio/` already has four, written for the audio plan and sitting flat
among the synthesizer's modules: `Wav` (RIFF chunks, 16-bit PCM),
`Midi` (Standard MIDI Files), `Abc` and `Doremi` (tunes as text). They
move under `audio/formats/` (phase 0), and the new ones join them,
chosen, as the images were, for being **famous, simple and elegant**,
and for what each one teaches:

| format | year | what it teaches |
|---|---|---|
| **MOD** (Ultimate Soundtracker) | 1987 | a song that carries its own instruments: samples, patterns, an order list; the tracker |
| **IFF** (8SVX, AIFF) | 1985 | chunks, the idea every container since reuses: WAV's RIFF is IFF little-endian |
| **AU** (Sun), and **mu-law** | 1992, 1972 | the smallest header; a logarithmic curve that makes 8 bits sound like 14 |
| **IMA ADPCM** | 1992 | lossy compression in forty lines: predict the next sample, code the error in 4 bits |
| **FLAC** | 2001 | lossless: linear prediction and Rice codes -- the PNG of sound, beside `deflate/` |
| **MML** (BASIC's `PLAY`) | 1978-81 | a score as a string, smaller than ABC |
| MIDI, ABC, solfège, WAV | (existing) | -- |

And two programs, the first users of MOD and MIDI beyond tests:

- **TinySoundtracker**, after Ultimate Soundtracker (Karsten Obarski,
  Amiga, 1987): the tracker, patterns of notes in a grid scrolling up
  as the song plays, played by our MOD player -- the editor whose
  interface is still Renoise's and OpenMPT's;
- **TinyMediaPlayer**, a MIDI player after Windows' Media Player
  (Microsoft, 1991, playing `.mid` files through MCI), with a piano
  roll (Cakewalk's, 1987): each channel a row of bars, the playhead on
  the music's clock (`Audio.position`).

Companions: [`notes_audio.md`](../tutorials/notes_audio.md) §2, §9 and
§12 (a MOD player is its exercise), and
[`notes_audio_midi.md`](../tutorials/notes_audio_midi.md) §9 (the
trackers' formats, discussed, not parsed). A tutorial,
`notes_audio_formats.md`, written ahead of the code as its
specification (phase 0).

## Principles (the same as `graphics/images/`)

- **One directory per format, one library each** (`audio/formats/mod/`
  is `audio_mod`, ...), each with a `.mli` that explains the format --
  its layout drawn, a worked example with bytes and numbers, its
  history and reference -- and nothing else: a reader gives samples
  (`Signal.t`) or notes (`Abc.tune`, `Midi.score`, a `Mod.song`), and
  playing them is `audio/`'s business.
- **Pure OCaml, both backends**: in the `elm_playground` package like
  `audio/`, so a browser plays a MOD as natively does (and 32-bit
  integers in a browser remembered: `Int32` where a format wraps).
- **Readers first, writers where they teach**: WAV and MIDI already
  write; MOD writes (the tracker saves, and the tests make their own
  modules); AU and IMA ADPCM write (a round trip shows what the lossy
  one loses); FLAC reads, and writes with its fixed predictors only
  (the simplest encoder that is still FLAC).
- **Our own test files**: no module or song by someone else in the
  repository -- a module's samples and patterns are its author's work
  (as the Mario theme is Nintendo's: `plan_audio_teaching.md`). The
  tests write their files with our writers (a MOD made by `Mod.write`,
  a FLAC by our encoder, checked against the reference decoder when
  one is installed, never needed by the tests); the apps take the
  user's own files by a flag or a URL, as TinyMario's `music=` does.
- **Golden WAVs** for what plays (a module's first seconds), **worked
  examples** for what reads (a header's bytes), as `audio/tests/` does.

## Target layout

```
audio/formats/            the files sounds and songs are kept in
  wav/                    Wav (moved): RIFF, PCM
  midi/                   Midi (moved): Standard MIDI Files
  abc/                    Abc, Doremi (moved): tunes as text
  iff/                    Iff (the chunks), 8SVX and AIFF readers
  au/                     Au (Sun's .snd), Mulaw (G.711)
  adpcm/                  Ima_adpcm
  flac/                   Rice, Lpc, Flac
  mml/                    Mml (BASIC's PLAY)
  mod/                    Mod (the file), Paula (the Amiga's four
                          channels: periods, volumes, the stereo), Mod_player
  tests/                  their worked examples and golden WAVs
apps/music/               TinySoundtracker, TinyMediaPlayer
```

`Wav.of_string`'s other rates (`Resample`) and `Audio.wav`,
`Audio.midi`, `loop_from`'s extensions stay the entry points games use;
`loop_from` learns `.mod`, `.au`, `.aiff`, `.flac` as they come.

## The formats

(To check against the sources when writing each `.mli`.)

### MOD: a song that carries its instruments

Ultimate Soundtracker (Karsten Obarski, Amiga, 1987) wrote the first
modules; Noisetracker and ProTracker (1990) extended them into the
format everyone used. The file, big-endian:

```
  20   song title
  30 x 31 instruments: name (22), length in words (2), finetune (1),
       volume 0-64 (1), loop start in words (2), loop length in words (2)
   1   song length: how many positions play (1-128)
   1   (restart; ignored)
 128   the order list: a pattern number per position
   4   "M.K." (at byte 1080: 1084 bytes of header)
1024 x the patterns: 64 rows x 4 channels x 4 bytes a cell
     then the samples, 8-bit signed, one after the other
```

A cell's 4 bytes pack an instrument (8 bits, split in two nibbles), a
**period** (12 bits: the Amiga's Paula chip counts down from it at
3,546,895 Hz, PAL, so period 428, the tracker's C-2, plays a sample at
8,287 Hz), and an **effect** (4 bits) with its parameter (8 bits):
arpeggio (0xy), slides up and down (1, 2), slide to a note (3),
vibrato (4), volume slide (A), jump and break (B, D), volume (C),
speed and tempo (F), and the E-commands. Ultimate Soundtracker's own
files have 15 instruments and no "M.K." (a 600-byte header): both read.

Time is **ticks**: 50 a second at the default tempo of 125 (a tick is
2.5 / tempo seconds), a row every 6 ticks (the speed): 120 ms a row.
Effects act per tick. Paula's four channels are panned hard, 1 and 4
left, 2 and 3 right -- why headphones make old Amiga music strange.

`Paula` models the chip (a channel: a sample, a period, a volume, the
loop), `Mod_player` the player (the order list, rows, ticks, effects),
as an `Instrument.t`-like source the `Mixer` pulls (plan_synth's
phase 0), so a module plays live, loops, and can be followed by a
display. Two teaching switches: Paula's samples read as the hardware
did (nearest, the aliases and all: the Amiga's sound) or interpolated
(`Resample`'s linear or cubic, with the low-pass before reading
faster that `notes_audio.md` §12 asks for), and the Amiga's hard
stereo or a softer mix.

### IFF: chunks

Electronic Arts' Interchange File Format (Jerry Morrison, 1985): a file
is a `FORM` chunk -- 4 letters, a 4-byte big-endian size, a 4-letter
type -- holding chunks, each 4 letters and a size, padded to an even
length. A reader skips what it doesn't know: the whole trick of a
format that lasts. **8SVX** ("8-bit sampled voice", the Amiga's
instruments: `VHDR`, `BODY`) and **AIFF** (Apple, 1988: `COMM` with its
sample rate as an 80-bit float, `SSND`). And the lesson: Microsoft and
IBM's RIFF (1991), WAV's container, is IFF with its sizes little-endian
-- `Wav` and `Iff` share the chunk walk.

### AU and mu-law

Sun's `.au` (1992): ".snd", then the data's offset, size, encoding,
rate and channels, 24 bytes big-endian. Its default encoding, **mu-law**
(ITU G.711, 1972, the telephone's): 8 bits a sample, logarithmic, so
quiet sounds keep their detail -- 14 bits' worth of range in 8. Worked
example to compute when writing: a sample's segment and step, encoded
and decoded, and the error, small near 0 and large near full scale.

### IMA ADPCM

The Interactive Multimedia Association's ADPCM (1992): each sample
predicted from the last, the difference coded in 4 bits against a
**step** that grows when the differences are large and shrinks when
they are small (a table of 89 steps, an index moved by the code). 4:1,
lossy, in about forty lines: the round trip's error measured, on a
sine and on noise.

### FLAC

The Free Lossless Audio Codec (Josh Coalson, 2001): blocks of samples,
each channel predicted -- a constant, verbatim, a **fixed** polynomial
predictor (orders 0 to 4: the next sample from the last few by fixed
coefficients) or an **LPC** one (coefficients computed and stored) --
and the prediction's error coded with **Rice codes** (a quotient in
unary, a remainder in k bits, k chosen per partition). The same idea
as PNG's (predict, then code what's left: the filters and DEFLATE,
`graphics/images/`), for sound. A reader; a writer with the fixed
predictors and one Rice parameter per block; the size a sine and a
noise compress to, measured.

### MML

The Music Macro Language: `PLAY "T120 O4 L4 CDEF G2"` in Microsoft BASIC
(the IBM PC, 1981; NEC's PC-8001 before it), notes as letters, octaves,
lengths, tempo, rests. Parsed into the same `Abc.tune` as ABC and
solfège, so everything that plays a tune plays it.

## The programs

- **TinySoundtracker**: the pattern grid, four channels, the rows
  scrolling up as the song plays; typing notes with the keyboard (two
  rows as a piano, as TinyMinimoog), an instrument per cell, effects
  typed as hex; the order list; play the song or the pattern; the
  instruments' samples drawn; saving through `appkits/file_menu`, as a
  MOD our player and every tracker reads. Its default song made by us
  (instruments synthesized by `audio/`, written into the module:
  a MOD's samples can be anything), `mod=file` or a URL for the
  user's own.
- **TinyMediaPlayer**: open a `.mid` (the user's, by a flag or a URL;
  a default tune of ours, from ABC through `Midi.of_tune`), play,
  pause, stop, a position slider, the channels muted one by one; a
  piano roll following the playhead. And the MIDI leftovers of the
  audio plan where they fit: pitch bend and the control changes that
  matter for playback (volume, pan).

## Phasing

0. **The notes and the move**: `notes_audio_formats.md` written ahead;
   `Wav`, `Midi`, `Abc`, `Doremi` moved under `audio/formats/` (the
   user's go-ahead, 2026-09-23), `audio/` depending on them, nothing
   else changed (test-lite, the golden WAVs untouched).
1. **MOD, read and written**: `Mod` (both headers, the cells, the
   samples), `Mod.write`; tests: a module made by `Mod.write` read back
   byte for byte, the worked example's bytes, the 15-instrument header.
2. **Paula and the player**: `Paula`, `Mod_player` (ticks, rows, the
   order list, the effects), `Audio.mod_` and `loop_from`'s `.mod`;
   tests: a period's frequency, a row's duration, each effect on a
   one-row module, golden WAVs of our module (nearest vs cubic).
3. **TinySoundtracker**: the grid, editing, playing, saving; golden
   frames, CATALOG.md's row, its web page.
4. **TinyMediaPlayer**: the player and the piano roll; pitch bend and
   control changes; golden frames, its row, its page.
5. **IFF and AU**: `Iff` (the chunk walk, shared with `Wav`), 8SVX,
   AIFF (the 80-bit float), `Au`, `Mulaw`; worked examples, round trips.
6. **IMA ADPCM**, then **FLAC** (`Rice`, `Lpc`, `Flac`): round trips,
   sizes measured.
7. **MML**, and the docs: `notes_audio_formats.md` checked against the
   code.

## Status

- **Phase 0, the move, DONE (2026-09-23)**: `audio/formats/wav/`
  (`audio_wav`), `midi/` (`audio_midi`, over `audio_abc`), `abc/`
  (`audio_abc`: `Abc`, `Doremi`), the files moved unchanged. `Wav`
  reads other rates through `Resample`, and the synthesizer uses the
  formats (`Music` plays `Abc` tunes and `Midi` scores), so `Signal` and
  `Resample` became a library of their own, `audio_signal`, below both --
  in `audio/dune`, their files not moved. `Abc` and `Doremi` name
  `Music` only in comments: no cycle. The existing tests stay in
  `audio/tests/` (they play the tunes through `Music`); the new
  formats' go in `audio/formats/tests/`. test-lite passing, the golden
  WAVs untouched. Left in phase 0: `notes_audio_formats.md`.

## Verification

- `make test-lite` as the gate (the golden WAVs are cheap: audio, not
  pixels), plus only the new programs' golden frames.
- By ear: a module and a MIDI file each, natively and in a browser --
  the user's job; the checks here are on dumped WAVs, measured.
- Where a reference exists and is installed (`flac`, `sox`), checked
  against it once, by hand, not in the tests.

## Out of scope

- MP3, AAC, Ogg Vorbis, Opus: the psychoacoustic codecs, a plan of
  their own (the MDCT, the ear's masking).
- S3M, XM, IT: the trackers after MOD (`notes_audio_midi.md` §9), an
  exercise once MOD plays.
- NSF, SID, VGM: music as a sound chip's program, which needs the
  chip's CPU (a 6502) -- the ix plan's territory.
