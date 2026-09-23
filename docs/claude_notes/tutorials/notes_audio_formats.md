# Sound in files: a tutorial for `audio/formats/`

How a sound or a song is kept in a file, told by the formats that are
famous, simple and elegant enough to be read in an afternoon: what each
one stores, how its bytes are laid out, and what idea it brought. The
sound twin of `graphics/images/` (see
[`plan_images_teaching.md`](../plans/plan_images_teaching.md)); written
before the code, as its specification
([`plan_audio_formats.md`](../plans/plan_audio_formats.md)), the
numbers of what is built the tests'. Companions:
[`notes_audio.md`](notes_audio.md) (what the samples mean) and
[`notes_audio_midi.md`](notes_audio_midi.md) (MIDI, the notes as
messages).

The thread: **a file is samples or notes, and the formats differ by
which, and by how they squeeze it**.

```
                    samples (the sound itself)        notes (what to play)
  plain             WAV, AIFF, 8SVX, AU              ABC, solfege, MML (text)
  squeezed, lossy   AU's mu-law, IMA ADPCM          MIDI (events, binary)
  squeezed, exact   FLAC
  both at once      MOD: notes, and the samples they play
```

## 0. Where the code is

| directory (`audio/formats/`) | modules | section | status |
|---|---|---|---|
| `wav/` | `Wav` | §2 | done (the audio plan) |
| `midi/` | `Midi` | `notes_audio_midi.md` | done |
| `abc/` | `Abc`, `Doremi` | `notes_audio.md` §9 | done |
| `mod/` | `Mod`, `Paula`, `Mod_player` | §1 | done (`Audio.play_module`) |
| `iff/` | `Iff`, 8SVX, AIFF | §2 | |
| `au/` | `Au`, `Mulaw` | §3 | |
| `adpcm/` | `Ima_adpcm` | §4 | |
| `flac/` | `Rice`, `Lpc`, `Flac` | §5 | |
| `mml/` | `Mml` | §6 | |

## 1. MOD: a song that carries its own instruments

**The idea.** A MIDI file says *play a C on instrument 1* and leaves the
sound to whatever synthesizer reads it: the same file sounds different
on every computer. A **module** carries its instruments inside, as
short recordings (**samples**), next to the notes that play them: the
same song sounds the same everywhere. On the Amiga (1985), whose Paula
chip played four sampled channels in hardware, that was the whole
point, and Karsten Obarski's **Ultimate Soundtracker** (1987) made the
format and the editor at once: the **tracker**, the song as a grid of
notes scrolling up the screen.

**The file**, all numbers big-endian (the Amiga's 68000):

```
offset  size
     0    20   the song's title
    20  30x31  the instruments, 30 bytes each:
                 22  name
                  2  length, in words (x 2: bytes)
                  1  finetune, a signed nibble: -8..7 eighths of a semitone
                  1  volume, 0-64
                  2  loop start, in words
                  2  loop length, in words (1: no loop)
   950     1   the song's length: how many positions play
   951     1   (restart position, ignored)
   952   128   the order list: which pattern plays at each position
  1080     4   "M.K." -- 1084 bytes of header
  1084  1024 x the patterns (as many as the order list's highest + 1):
               64 rows x 4 channels x a 4-byte cell
        then   the samples, one after the other, 8-bit signed
```

The initials are Mahoney & Kaktus's, whose NoiseTracker (1989) made the
tag, and 31 instruments, the standard. Ultimate Soundtracker's own
files have 15 instruments and no tag: a 600-byte header (20 + 15 x 30 +
1 + 1 + 128), told apart by the missing tag. Later trackers put the number of channels
in the tag ("6CHN", "8CHN"): the same format, wider rows.

**A cell**, 4 bytes for a note, an instrument and an effect, packed as
the 68000 liked:

```
  byte 0          byte 1          byte 2          byte 3
  iiii pppp       pppp pppp       iiii eeee       xxxx yyyy
  instrument's    period's        instrument's    the effect's
  high nibble,    low 8 bits      low nibble,     parameter
  period's                        the effect
  high 4 bits
```

Worked example (the `.mli`'s, checked by the tests): instrument 1 playing
C-2 (period 428 = 0x1AC) with effect C (set the volume) to 0x20 is `01
AC 1C 20`; the same with instrument 17 (0x11), `11 AC 1C 20`.

**Periods, not notes.** A cell holds no note name: it holds a **period**,
what the Amiga's Paula chip counts down from, at 3,546,895 Hz (half the
PAL machine's clock), before reading the next byte of the sample. So a
period *is* a sample rate: 3,546,895 / 428 = 8,287 Hz for C-2. ProTracker's
table gives three octaves, C-1 (856) to B-3 (113), each semitone the
last period divided by 2^(1/12), rounded -- the rounding is the music's
tuning, and the finetune picks one of 16 slightly different tables.
What pitch a note sounds depends on the sample: a recording of a C
played at C-2 sounds C.

**Time is ticks.** 50 ticks a second at the default tempo of 125 (a tick
is 2.5 / tempo seconds: the PAL screen's 50 Hz), a row every 6 ticks
(the speed): 120 ms a row, a pattern of 64 rows in 7.68 s. Effects act
on every tick of a row: a slide moves the period each tick, a vibrato
swings it, an arpeggio cycles three notes one tick each (the chiptune
chord, `notes_audio.md` §8). The main effects, the ones the player will
do:

| | effect | |
|---|---|---|
| 0xy | arpeggio | the note, +x, +y semitones, a tick each |
| 1xx, 2xx | slide up, down | the period -xx, +xx each tick |
| 3xx | slide to the note | towards the cell's period by xx a tick |
| 4xy | vibrato | the period swung at speed x, depth y |
| Axy | volume slide | +x or -y each tick |
| Bxx, Dxx | position jump, pattern break | the song's shape |
| Cxx | volume | 0-64 |
| Fxx | speed (under 32) or tempo | |
| 9xx | sample offset | start xx x 256 bytes in |
| Exy | the "extended" ones | fine slides, retrigger, note cut, delay |

**Paula's stereo**: channels 1 and 4 on the left, 2 and 3 on the right,
hard -- why Amiga music in headphones sounds so strange, and why players
offer a softer mix.

**Playing it** is `notes_audio.md` §9's sampler with a sequencer: each
channel reads its sample at the period's rate (`Resample`), loops it if
it loops, at its volume; the sequencer walks the order list, rows and
ticks. The teaching switch: the samples read as Paula did (the nearest
byte, and no filter: the aliases are the Amiga's sound) or
interpolated and low-passed first (§12's exercise, done).

## 2. IFF and RIFF: chunks

Electronic Arts' **Interchange File Format** (Jerry Morrison, 1985): a
file is a `FORM` chunk, and a chunk is four letters, a size, and that
many bytes (padded to even):

```
  "FORM" <size> "8SVX"
     "VHDR" <20>  the voice header: lengths, rate, octaves, volume
     "NAME" <n>   ...
     "BODY" <n>   the samples, 8-bit signed
```

A reader walks the chunks and skips the ones it doesn't know: that is
how a format grows for thirty years without breaking its readers.
**8SVX** was the Amiga's instruments (a MOD's samples, before they were
put in the MOD); **AIFF** (Apple, 1988) the Mac's recordings, its rate an
80-bit extended float (the 68881's native size). And Microsoft and IBM's
**RIFF** (1991), WAV's container, is IFF with its sizes little-endian,
for the Intel processors -- `Wav` and `Iff` walk the same chunks.

## 3. AU and mu-law

Sun's `.au` (1992, for the SPARCstation's speaker): ".snd", then the
data's offset, its size, the encoding, the rate and the channels, six
32-bit numbers, big-endian: the smallest header there is. Its first
encoding, **mu-law** (the telephone's, ITU G.711, 1972): 8 bits a
sample, but logarithmic, a sign, 3 bits of segment (a power of two)
and 4 of step within it -- small samples finely, large ones coarsely,
as the ear hears loudness: the range of 14 bits in 8.

## 4. IMA ADPCM

Predict the next sample from the last, and store only the error, in 4
bits against a **step** that grows when the errors are large (a loud,
changing sound) and shrinks when they are small: 89 steps in a table, an
index moved by each 4-bit code. The Interactive Multimedia
Association's (1992), 4:1, lossy, about forty lines both ways -- the
games of the 1990s' voices. The tests measure what it loses, on a sine
and on noise.

## 5. FLAC: lossless

Josh Coalson's Free Lossless Audio Codec (2001): the same idea as PNG's
(`graphics/images/`: filter to make it predictable, then code what's
left), for sound. A block of samples is **predicted** -- by a fixed
polynomial of the last samples (order 0: 0; 1: the last one; 2: 2x[n-1]
- x[n-2], a straight line continued; up to 4) or by stored LPC
coefficients -- and the **residual**, the prediction's error, small and
mostly near 0, is written with **Rice codes**: a number's high part in
unary (that many 1s, a 0), its low k bits as they are, k chosen per
partition. A sine predicts almost perfectly; noise doesn't at all: the
sizes measured say how much.

## 6. MML

The Music Macro Language: notes as letters, in BASIC's `PLAY` statement
(Microsoft BASIC on the IBM PC, 1981; NEC's PC-8001 before it), `T120 O4
L8 CDEFGAB > C` -- tempo, octave, length, the notes, an octave up.
Smaller than ABC, parsed into the same `Abc.tune`.

## Exercises

- **S3M, XM, IT**, the trackers after MOD (`notes_audio_midi.md` §9).
- **A MOD's samples from 8SVX files**, as the Amiga's composers built
  their instrument disks.
- **FLAC's LPC coefficients computed** (Levinson-Durbin), not just read.
- **mu-law's cousin A-law**, Europe's telephone.

## References

(To check when writing each module.)

- Karsten Obarski, Ultimate Soundtracker, EAS Computer Technik, 1987;
  the ProTracker file format as documented by its players (MikMod,
  libxmp, OpenMPT's wiki).
- Jerry Morrison, "EA IFF 85: Standard for Interchange Format Files",
  Electronic Arts, 1985; "8SVX IFF 8-Bit Sampled Voice", same; Apple,
  "Audio Interchange File Format", 1989; Microsoft and IBM, "Multimedia
  Programming Interface and Data Specifications 1.0", 1991 (RIFF, WAV).
- ITU-T G.711, "Pulse code modulation of voice frequencies", 1972.
- Interactive Multimedia Association, "Recommended Practices for
  Enhancing Digital Audio Compatibility in Multimedia Systems", 1992.
- Josh Coalson, the FLAC format specification, 2001 (and RFC 9639,
  2024).
- Microsoft, GW-BASIC's PLAY statement, 1983.
