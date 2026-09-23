# MIDI: music as messages, a tutorial

A companion to [`notes_audio.md`](notes_audio.md), independent of it:
that note is about *sound* (samples, oscillators, filters); this one is
about *music as data* -- not what a note sounds like, but which note,
when, how hard, on which instrument. That's MIDI, and it's small,
old, everywhere, and a good lesson in designing a binary protocol and a
file format. Where it fits in the playground: `Midi` (in
`audio/formats/midi/`, beside the other formats) reads and
writes MIDI files, and `audio/Music` plays them with `audio/`'s own
synthesizer (§12; see
[`plan_audio_teaching.md`](../plans/plan_audio_teaching.md)); a real
MIDI keyboard playing the `AudioPiano` example is planned (§11).

## 1. What MIDI is (1983)

The **Musical Instrument Digital Interface**, proposed by Dave Smith
(Sequential Circuits) and Ikutaro Kakehashi (Roland), standardized in
1983 by the synthesizer makers together: a way for one instrument to
tell another "play this note now", over a 5-pin cable at 31,250 bits
per second. It carries **no sound at all**: only events, a few bytes
each. A keyboard sends "note 60 on, velocity 100" when you press middle
C; whatever receives it -- a synthesizer, a computer -- decides what
that sounds like. So a whole song is a few kilobytes, the same file
sounds different on every synthesizer, and a computer can edit the
music note by note: sequencers, then every music program since.

## 2. Notes as numbers

MIDI numbers the keys: **middle C is 60**, each semitone is one, so the
88 keys of a piano are 21 (A0, 27.5 Hz) to 108 (C8, 4186 Hz), and
**A4, 440 Hz, is 69**. The frequency of note n in equal temperament
(`notes_audio.md` §9):

```
   f(n) = 440 * 2^((n - 69) / 12)      f(60) = 261.63 Hz, middle C
```

A note also has a **velocity**, 1 to 127, how hard the key was hit:
usually loudness, sometimes brightness (a harder piano note has more
high harmonics).

## 3. Messages

Every message is a **status byte** (high bit 1: what, and on which of
16 **channels**, one per instrument), then one or two **data bytes**
(high bit 0: values from 0 to 127):

```
   9n kk vv     note on, channel n, key kk, velocity vv
   8n kk vv     note off
   Bn cc vv     control change: controller cc (7 volume, 64 sustain pedal, ...)
   Cn pp        program change: choose instrument pp
   En ll mm     pitch bend: a 14-bit value, 8192 in the middle
```

Example: `90 3C 64` is "note on, channel 1, key 60 (middle C),
velocity 100"; `80 3C 40` releases it (a note on with velocity 0 is
also a note off, which matters for the next trick).

The high bit is the whole framing: a receiver that joins mid-stream,
or loses a byte, resynchronizes at the next byte with its high bit
set. **Running status**: when several messages in a row have the same
status byte, it's sent only once -- `90 3C 64 40 64 43 64` is three
notes on (C, E, G: a chord) in 7 bytes instead of 9; with "velocity 0
means off", a whole melody on one channel is one status byte and pairs
of data bytes. At 31,250 bits per second (about 1000 messages a
second), a third saved was worth the complication.

## 4. Standard MIDI Files (1988)

A `.mid` file stores messages with their times. It's a sequence of
**chunks**, each a 4-letter tag and a 32-bit big-endian length: one
header, `MThd` (the format, the number of tracks, and the **division**:
ticks per quarter note), then the tracks, `MTrk`, each a list of events
with the **delta time** since the previous one, in ticks.

Delta times are stored as **variable-length quantities**: 7 bits per
byte, most significant first, the high bit set on every byte but the
last. Small numbers, the common case, take one byte:

```
     0  ->  00          127  ->  7F         128  ->  81 00
   200  ->  81 48       480  ->  83 60      16383  ->  FF 7F
   16384 -> 81 80 00
```

(128 = 1 x 128 + 0: a first byte 0x80 + 1, then 0x00; the same idea as
UTF-8's continuation bytes, or Protocol Buffers' varints.)

Besides MIDI messages, a track has **meta events** (`FF type length
data`): the track's name, lyrics, the time signature, the end of the
track, and the **tempo**, in microseconds per quarter note.

## 5. From ticks to seconds

A delta time is in ticks, abstract; the tempo makes it time. With a
division of 480 ticks per quarter note and a tempo of 500,000
microseconds per quarter note (**120 beats per minute**), one tick is
500,000 / 480 = **1.042 ms**, and 960 ticks (two beats) are one second.
The tempo can change in the middle of the song (a tempo meta event), so
converting ticks to seconds means walking the tempo map, adding up each
stretch at its own tempo: the one subtle computation of a MIDI player.

## 6. General MIDI (1991)

Since MIDI carries no sound, "program 1" meant a different instrument
on every synthesizer, and a song written for one sounded like nonsense
on another. **General MIDI** fixed a list: 128 programs (1 is the
grand piano, 57 the trumpet, 81 a square-wave "lead"), and **channel
10 is always drums**, each key a drum (36 bass drum, 38 snare, 42
closed hi-hat). With it, a `.mid` file sounded roughly right anywhere:
the 1990s web pages' background music, and the soundtracks of PC games
on their sound cards. `audio/Music` can map the programs to its few
waveforms (a square for leads, a triangle for bass, noise for drums):
an NES-like rendering of any MIDI file.

## 7. Playing MIDI with a synthesizer

To play a MIDI file: parse the chunks and events, convert delta times
to seconds (§5), then for each "note on", start a voice -- an
oscillator at f(n) with an envelope (`notes_audio.md` §3-4), louder with
the velocity -- and release it at the matching "note off". A
**voice** is one playing note; a synthesizer has a limited number
(**polyphony**) and must steal one when they run out. A **SoundFont**
(E-mu and Creative, 1990s) replaces the oscillators by recorded
samples of real instruments, one per range of keys: what FluidSynth
and TiMidity do.

In the other direction, a MIDI keyboard's messages arrive in real time:
on native through the OS (ALSA on Linux, CoreMIDI, Windows' MIDI API,
or PortMidi over all three), in the browser through the **Web MIDI
API**. The `AudioPiano` example with a real keyboard: each "note on" an
`Audio.play` of its note, louder with the velocity (not written yet:
§11).

## 8. A few histories

- **iMUSE** (Michael Land and Peter McConnell, LucasArts, 1991: Monkey
  Island 2): game music as MIDI, rearranged live as the player walks
  from place to place -- only possible because the music was notes, not
  a recording.
- **Doom** (1993) shipped its music as MIDI (in its own MUS variant),
  played by whatever sound card the player had: the same song in AdLib
  FM, Sound Blaster or Roland.
- **MIDI 2.0** (2020): higher resolution (32-bit velocities and
  controllers), two-way negotiation between devices, compatible with
  1.0. The 1983 protocol is still what most instruments speak.

## 9. MIDI's cousins: the trackers' modules (MOD, S3M, XM, IT)

Where a MIDI file says *what* to play and leaves the sound to whatever
synthesizer reads it, a **module** carries its own instruments: short
recorded **samples**, inside the file, next to the notes. The same song
sounds the same everywhere -- on the Amiga, whose Paula chip played
four sampled channels in hardware, that was the whole point.

- **MOD** (Karsten Obarski's Ultimate Soundtracker, Amiga, 1987; the
  format everyone used through ProTracker, 1990): 31 instruments of
  8-bit samples, **patterns** of 64 **rows** by 4 channels (each cell a
  note, an instrument, and an effect: a slide, a vibrato, a volume
  change, an arpeggio), and an **order list** saying which pattern
  plays when. Written by editing the grid, rows scrolling upward in
  time: the **tracker** interface, still used today (Renoise,
  OpenMPT, and in hardware, the Polyend Tracker).
- **S3M** (Scream Tracker 3, Future Crew, PC, 1994): up to 32 channels,
  and AdLib FM instruments beside the samples.
- **XM** (FastTracker 2, Triton, PC, 1994): instruments made of several
  samples across the keyboard, with volume and panning envelopes.
- **IT** (Impulse Tracker, Jeffrey Lim, PC, 1995): new note actions
  (a note keeps sounding when the next starts), resonant filters.

They were the music of the **demoscene** (Future Crew's Second
Reality, 1993) and of many 1990s games -- Epic's Unreal (1998) shipped
its music as modules (UMX) -- small enough for a floppy, and sounding
better than the General MIDI of the era's sound cards. Players today: libxmp,
libopenmpt (OpenMPT's), MikMod.

Their place here: a module is our `audio/` in miniature -- samples,
a pitch per note (a sample played faster for higher notes: resampling,
the plan's phase 10), envelopes, effects like `sliding` -- driven by a
sequencer; a tracker is the music's editor in the same grid spirit as
`Tilemap` is a level's. Not read by `audio/` (ABC, solfège and MIDI
cover the notes; a module's samples would need `Resample`): a
possible exercise, MOD first, the simplest.

## 10. Compared with FluidSynth and TiMidity

**What it ignores.** Of the messages of §3, `Midi.parse` keeps the
notes, the program changes and the tempo, and reads past the rest:
control changes (the sustain pedal, the volume, the pan), pitch bend,
aftertouch, system exclusive messages; a file timed in SMPTE frames
instead of ticks is refused. A real player -- FluidSynth, TiMidity, a
Roland Sound Canvas -- obeys all of them, and plays SoundFont samples
(§7) rather than a square wave: the same file, a piano instead of an
NES.

**Rendered ahead, not played live.** A player like FluidSynth is a
real-time sequencer: its voices start and stop as the events come,
with a limited polyphony and a voice stolen when it runs out (§7).
`Music.render_score` renders the whole score into one buffer before it
plays, each note added at its start: unlimited polyphony, but a long
song costs its whole rendering at once, and it can't react to anything
-- no iMUSE (§8). The wider landscape, the trackers and the sound
chips: [`notes_audio_related_work.md`](../related-work/notes_audio_related_work.md).

## 11. What's missing, and exercises

In rough order of difficulty:

- **running status on write**: `Midi.of_tune` writes the status byte
  of every message; drop the repeated ones (§3), and count the bytes
  saved on a tune (`Midi.parse` already reads them);
- **controllers**: the volume (7) and the sustain pedal (64): a note
  released while the pedal is down lasts until it's up -- in
  `Midi.parse`'s `0xB0` case, now skipped;
- **pitch bend**: a note's frequency moving while it plays, `Synth`'s
  `sliding` from the bend's 14-bit value (§3, the `0xE0` case);
- **polyphony**: at most N voices at once in `Music.render_score`,
  stealing the oldest (§7), and hear what a 1980s synthesizer's limit
  did to a busy song;
- **a live player**: the events handed to the `Mixer` as their time
  comes instead of one rendering, so that a game can change the music
  as it plays (a tempo, a muted voice: the iMUSE of §8);
- **a MIDI keyboard for `AudioPiano`**: the Web MIDI API in the
  browser, ALSA or PortMidi natively (§7); each note on, an
  `Audio.play`;
- **a MOD player** (§9): the samples in the file, played at each note's
  pitch -- which needs `Resample` first.

## 12. In the playground

`playground/Audio.mli`'s `midi bytes` is a sound made from a Standard
MIDI File's bytes (`Midi.parse`, then `Music.render_score`), played
like any other: `Audio.loop "music" (Audio.midi bytes)`. `loop_from`
fetches it too, from a path or a URL, as a MIDI file when the name ends
in `.mid`: `TinyMario.ml`'s `music=` flag
(`dune exec games/platform/TinyMario.exe -- music=song.mid`) replaces
its own ABC tune with any MIDI file. The other way round,
`Midi.of_tune` writes an ABC or solfège tune as a MIDI file (format 1,
a track per voice), and `audio/tests/`'s golden WAV
`frere_jacques_midi.wav` is the round trip: Frère Jacques written as
MIDI, read back, and played.

## Glossary

- **MIDI**: a protocol of musical events, not sound.
- **Note number**: 0-127, 60 = middle C, 69 = A4 (440 Hz).
- **Velocity**: 1-127, how hard a note is played.
- **Channel**: 1-16, one per instrument; 10 is drums in General MIDI.
- **Status byte / data byte**: high bit 1 / 0.
- **Running status**: a repeated status byte, omitted.
- **SMF**: Standard MIDI File; **chunk**: `MThd` or `MTrk`.
- **Tick**, **division**, **tempo**: MIDI's time, per quarter note, and
  microseconds per quarter note.
- **Variable-length quantity**: 7 bits per byte, the high bit meaning
  "more follows".
- **General MIDI**: the standard list of 128 instruments and the drum
  map.
- **Voice**, **polyphony**: a playing note, and how many at once.
- **SoundFont**: recorded samples of instruments, for playing MIDI.
- **Module**, **tracker**: a song file carrying its own samples (MOD,
  S3M, XM, IT), and the grid editor that writes it; **pattern**,
  **row**, **order list**: its structure.

Sources: from memory, to be checked before relying on them for
teaching -- the MIDI 1.0 specification and the Standard MIDI File
specification (MIDI Manufacturers Association), General MIDI's
instrument list, and general knowledge of the history.

## References

- Dave Smith, Chet Wood, "The 'USI', or Universal Synthesizer
  Interface", Audio Engineering Society Convention, 1981 (MIDI's
  precursor proposal).
- MIDI Manufacturers Association, Japan MIDI Standards Committee,
  "MIDI 1.0 Detailed Specification", 1983.
- MIDI Manufacturers Association, "Standard MIDI Files 1.0", 1988.
- MIDI Manufacturers Association, "General MIDI System Level 1", 1991.
- Chris Walshaw, "The abc music standard 2.1", 2011
  (https://abcnotation.com/wiki/abc:standard:v2.1; what `Midi.of_tune`
  writes as MIDI).
- MIDI Manufacturers Association, Association of Musical Electronics
  Industry, "MIDI 2.0" specifications, 2020.
- The MIDI specifications today: https://midi.org/specifications
