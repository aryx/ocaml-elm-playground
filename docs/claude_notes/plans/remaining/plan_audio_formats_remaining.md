# Plan: what's left for the sound and music formats

The plan is done: see [`done/plan_audio_formats.md`](../done/plan_audio_formats.md)
-- `audio/formats/`, one library per format: `Wav`, `Midi`, `Abc` and
`Doremi` moved there; MOD (Ultimate Soundtracker, 1987; ProTracker,
1990) read and written by `Mod`, played by `Paula` (the Amiga's four
channels, held or interpolated) and `Mod_player` (ticks, rows, the
order list, the effects) through `Audio.play_module`; MP2 and MP3
decoded (`formats/mpeg_audio/`); TinySoundtracker (`apps/music/`), the
tracker; and TinyMediaPlayer, grown into a small VLC in its own
category, `apps/media/` (`Media`, `Our_media`).

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli` and its test.

## 1. MIDI heard whole in TinyMediaPlayer

- **Pitch bend and the control changes** that matter for playback: a
  channel's volume (7) and pan (10). `Midi` reads the events and skips
  them today; phase 4 asked for them and they became an exercise in
  the player's header.
- The channels of a tune muted one by one from the piano roll.
- A playlist saved and opened.

## 2. The formats dropped

Phases 5 to 7 were dropped (2026-09-23, the user's call): formats few
people meet, whose ideas the repository already teaches elsewhere
(chunks in WAV, prediction in PNG's filters, a score as text in ABC).
`notes_audio_formats.md` §2 to §6 describe them, marked "not built". If
one is ever wanted, in this order:

- **IFF** (Electronic Arts, 1985): the `FORM` chunk walk shared with
  `Wav`'s RIFF, 8SVX (the Amiga's instruments) and AIFF (Apple, 1988,
  its rate an 80-bit float). 8SVX first: TinySoundtracker's instruments
  read from the Amiga's own files, an exercise in its header.
- **AU** (Sun, 1992) and **mu-law** (G.711, 1972): 24 bytes of header,
  and 14 bits' worth of range in 8; its cousin A-law, Europe's
  telephone.
- **IMA ADPCM** (1992): the step table and the round trip's error, on a
  sine and on noise.
- **FLAC** (Josh Coalson, 2001): Rice codes, the fixed predictors, a
  reader and the simplest writer; then LPC's coefficients computed
  (Levinson-Durbin), not just read.
- **MML** (BASIC's `PLAY`, the IBM PC, 1981): parsed into an `Abc.tune`.
- Each one then a line in `Media.ml`, and `loop_from` learning its
  extension.

## 3. MOD's rest

- The effects read and ignored: E6 (the pattern loop), ED and EE (the
  note and pattern delays), E0 (the Amiga's filter).
- **S3M, XM, IT**, the trackers after MOD (`notes_audio_midi.md` §9).
- TinySoundtracker's exercises: the instruments edited (a sample drawn
  with the mouse, or one of the synthesizer's sounds recorded,
  `Audio.recorded`); an edit step other than 1; a block of a pattern
  copied and pasted; the channels muted one by one.

## 4. MP2 and MP3's rest

- Layer III's intensity stereo (no file of ours has it: LAME doesn't
  write it), Layer I, the free format.
- The LAME tag's gapless trimming (the encoder's delay and padding),
  and the Xing tag's frame count.
- An encoder.

## 5. Out of scope, a plan each

- AAC, Ogg Vorbis, Opus: the psychoacoustic codecs after MP3.
- NSF, SID, VGM: music as a sound chip's program, which needs the
  chip's CPU (a 6502) -- the ix plan's territory.

## 6. Checks not yet made

- By ear: a module and a MIDI file each, natively and in a browser.
  The checks were on dumped WAVs, measured, not listened to.
- A `song.mod` exported by TinySoundtracker opened in a real tracker
  (OpenMPT, ProTracker): it was only reopened by our own player.
