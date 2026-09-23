# Video, from scratch: a tutorial for `graphics/videos/`

How a computer keeps moving pictures, told by the formats that found
each idea: raw frames and the color model every codec works in, frames
stored as what changed, a container holding pictures and sound in step,
and motion compensation, the idea that makes video small. Written
before the code, as its specification
([`plan_video_teaching.md`](../plans/plan_video_teaching.md)); the
numbers of what is built will be the tests'. Companions:
[`notes_images.md`](notes_images.md) (the pictures a video is made of:
JPEG's DCT comes back in §5), [`notes_audio.md`](notes_audio.md) (the
sound, and the audio clock of §4), and
[`notes_video_related_work.md`](../related-work/notes_video_related_work.md).

The thread: **consecutive frames are nearly the same.** Every format
after the first is a way of not storing the same thing twice.

## 0. Where the code is, and a reading order

| directory (`graphics/videos/`) | modules | section | status |
|---|---|---|---|
| `movie/` | `Movie`, a video as a player sees it | §3, §4 | done |
| `yuv/` | `Yuv`, `Psnr` | §2, §6 | done |
| `y4m/` | `Y4m` | §2 | done |
| `fli/` | `Fli` | §3 | |
| `avi/` | `Avi` | §4 | |
| `mpeg1/` | `Bits`, `Vlc`, `Mpeg1`, later `Motion` | §5, §7 | |
| `apps/media/` | TinyMediaPlayer's `Movie` kind (done: GIFs), the analyzer | §8 | |

## 1. How big video is

A frame is a picture; a video is 24 to 60 of them a second. Raw, a
minute of 640 x 480 at 30 frames a second, 3 bytes a pixel:

    640 x 480 x 3 x 30 x 60 = 1,658,880,000 bytes: 1.6 GB

A DVD holds that minute in about 40 MB (MPEG-2 at 5 Mbit/s), a video
site in less. The factor of 40 and more comes from three places, each a
section below: the eye's weakness for color (§2, a factor of 2 before
anything clever), the frames' likeness to each other (§3, §5), and what
JPEG already does to one picture (the DCT and its quantization, which
§5 reuses on what is left).

## 2. YCbCr, 4:2:0, and raw video (Y4M)

The eye sees brightness finely and color coarsely. So video doesn't
keep a pixel as red, green and blue but as **luma** Y (the brightness)
and two **color differences** Cb and Cr (blue and red, less the
brightness), BT.601's recipe (1982, for digital television):

    Y  =  0.299 R + 0.587 G + 0.114 B
    Cb = (B - Y) x 0.564 + 128
    Cr = (R - Y) x 0.713 + 128

Worked example: pure red (255, 0, 0) is Y = 76, Cb = 85, Cr = 255 --
dark (the eye finds red dim next to green) and all red difference.
(Video proper squeezes Y into 16-235 and Cb, Cr into 16-240, the
"studio range", and MPEG-1 does; JPEG uses the full 0-255. To check,
and say, in `Yuv.mli`.)

Then the color is **subsampled**: **4:2:0** keeps one Cb and one Cr for
each 2 x 2 square of Y. A frame of W x H is W x H bytes of Y and W x H /
4 of each color: 1.5 bytes a pixel instead of 3, half the data, and
very hard to see -- except at a sharp edge between two colors: a 2 x 2
checker of red and blue keeps its four brightnesses and becomes one
purple (Cb 170, Cr 181, `Yuv.mli`'s worked example). The tests measure
it by the PSNR (§6) of a picture through 4:2:0 and back: over 35 dB
for a smooth gradient, under 15 for that checker.

**Y4M** (YUV4MPEG2, mjpegtools, 2001) is exactly this and nothing more,
the raw format video tools pass each other:

```
YUV4MPEG2 W320 H240 F25:1 Ip A1:1 C420\n     a text header
FRAME\n  then 320 x 240 bytes of Y, 160 x 120 of Cb, 160 x 120 of Cr
FRAME\n  ...
```

320 x 240 at 25 frames a second: 115,200 bytes a frame, 2.9 MB a
second. Our first clip is written this way, rendered by our own 2D
rasterizer: the repository filming itself.

## 3. Delta frames: FLI and FLC

Jim Kent's Autodesk Animator (1989), 320 x 200 in 256 colors, the
format of a thousand DOS games' cut-scenes, stores the first frame
whole and every other **as what changed**. A file is frames, a frame
is chunks:

```
  COLOR_256    the palette (only the colors that changed)
  BRUN         the first frame: each line as runs -- n times one color,
               or n literal bytes
  LC (FLI)     a later frame: lines skipped, then in each changed line
  DELTA_FLC    runs of pixels to skip and runs to write
  (FLC)
  BLACK, COPY  a frame all color 0, a frame stored raw
```

A still background costs nothing; a moving sprite costs its outline
(the pixels it left and the ones it reached). Worked example, to
compute when writing: a 16 x 16 ball moving 2 pixels across a still
320 x 200 background -- the raw frame's 64,000 bytes against the
delta's few hundred. And the limit: a camera that pans changes every
pixel, and the delta is the whole frame again. FLC (Animator Pro, early
1990s) widened the idea to bigger screens and word-sized runs.

## 4. Container and codec: AVI and Motion JPEG

A video file is two things that are easy to confuse: the **codec**, how
one frame (or one stretch of sound) is compressed, and the
**container**, the file that holds the compressed frames and the sound,
says when each plays, and lets a player find frame 1000 without
reading the 999 before. AVI (Microsoft's Video for Windows, 1992) is a
RIFF file -- WAV's container (`notes_audio_formats.md` §2) -- holding:

```
  RIFF "AVI "
    LIST "hdrl"   the header: the frame rate, the size, a header per stream
    LIST "movi"   the data, interleaved: 00dc a frame, 01wb some sound,
                  00dc, 01wb, ...  (so a player reading in order has both)
    idx1          the index: where each chunk is
```

Here the codec is **Motion JPEG**: every frame a JPEG, alone
(`graphics/images/jpeg` reads it), no delta -- what webcams and the
first digital cameras recorded, simple to cut anywhere, big. The sound
is PCM, WAV's samples.

**Keeping them in step.** The sound card pulls samples on its own clock
and cannot wait (`notes_audio.md` §10); the screen can skip a frame and
nobody notices. So the **audio clock drives the video**: the frame
shown is the one at the sound's position, frame = position x frame rate,
and a slow frame is dropped rather than the sound delayed -- the
music's clock of `Audio.position`, which TinyDDR judges steps by, again.

## 5. Motion compensation: MPEG-1

MPEG-1 (ISO/IEC 11172-2, 1993, the Video CD's) cuts a frame into
**macroblocks** of 16 x 16 pixels -- four 8 x 8 blocks of Y and one each
of Cb and Cr (4:2:0) -- and codes each in one of two ways:

- **intra**, alone, as JPEG would: each 8 x 8 block's DCT, quantized,
  in zigzag order, as run-and-level pairs in variable-length codes;
- **predicted**: copied from a frame already decoded, **moved** by a
  motion vector, plus the DCT of what still differs, the **residual** --
  often nothing at all, a macroblock that costs a few bits.

The frames come in three kinds:

```
  display order:   I0  B1  B2  P3  B4  B5  P6 ...
  decode order:    I0  P3  B1  B2  P6  B4  B5 ...

  I   intra: every macroblock alone -- the frame a player can start at
  P   predicted from the last I or P, forward
  B   bidirectional: from the I or P before, the one after, or their
      average -- the best predictions, and so stored after the P they
      need: the decoder reorders
```

**Half a pixel.** A vector's unit is half a pixel: between two pixels,
the prediction is their average, (a + b + 1) / 2, rounded up; between
four, the average of four. Worked example, to compute when writing: a
row 10, 20, 30, 40 moved by +1.5 pixels.

**The bitstream**, layer inside layer: a sequence header (the size,
the frame rate, the quantization matrices), groups of pictures, a
picture header (its kind, its place in display order), slices (a row
of macroblocks, where a decoder can resynchronize after an error),
macroblocks (the address skipped to, the kind, the vector, which
blocks are coded), blocks (the coefficients). Its variable-length codes
come in tables in the standard; reading them is `Vlc`, the bits `Bits`.

That is the skeleton of every codec since: MPEG-2 (DVDs) added
interlace, H.264 (2003) smaller and many-shaped blocks, several
reference frames, an in-loop filter and an arithmetic coder, AV1 more
of each -- but a frame is still blocks predicted from blocks moved,
plus a transformed residual.

## 6. Measuring: PSNR

How good is a lossy frame? Against the original, the **mean squared
error** of the pixels, and in decibels the **peak signal-to-noise
ratio**:

    PSNR = 10 log10 (255^2 / MSE)

Worked example: every pixel off by 2 gives MSE 4 and PSNR 42.1 dB; off
by 16, 24.0 dB. Around 30 dB a picture is watchable, past 40 it looks
the same. It is not how the eye judges (a slight shift everywhere
scores worse than a blotch the eye catches at once), and newer measures
try to be (SSIM, 2004; VMAF, 2016), but it is the field's common ruler,
and the tests' here: each decoded frame against the frame it was
encoded from.

## 7. The encoder's side: motion estimation

The standard fixes what a decoder does with a vector, not how an
encoder finds it: that is where encoders compete. The simplest is
**full search**: for each macroblock, try every vector within a window
(+-16 pixels: 1,089 of them), and keep the one whose moved block
differs least, by the **sum of absolute differences** (SAD) of its 256
pixels -- 278,784 subtractions a macroblock, the encoder's whole cost.
Faster searches look at fewer places: the three-step search (Koga et
al., 1981) tries 9 around the center, halves the step, 9 around the
best, and so on. Our own encoder, when written, measured both ways:
its time and the size and PSNR it reaches.

## 8. Seeing what the file says: the analyzer

TinyMediaPlayer's analyzer view draws the file's own decisions over the
picture: the frame kinds as a strip of colored cells (I, P, B), the
macroblock grid, the motion vectors as arrows (a pan: all the arrows the
same; a ball: a few arrows on the ball), the residual alone (the
prediction switched off: what the encoder actually sent, mostly grey),
the Y, Cb and Cr planes apart; for FLI, the pixels a delta frame
touched. The trade's own tools do this; here it is the lesson made
visible.

## Exercises

- **Interlace**: two fields a frame, as television had them, and what
  MPEG-2 added for it.
- **A faster motion search**: the diamond search, and the size it
  loses against full search.
- **MP4's boxes**: the container of today, IFF's chunks once more.
- **Scene cuts**: an encoder that puts an I frame where the picture
  changes entirely, found by the SAD of the whole frame.

## Glossary

- **Frame**, **frame rate**. **Luma** (Y), **chroma** (Cb, Cr),
  **4:2:0**: chroma at a quarter resolution.
- **Delta frame**: a frame stored as its changes. **Run**: repeated or
  literal bytes counted.
- **Container** (AVI, MP4) vs **codec** (MJPEG, MPEG-1). **Interleaving**,
  **A/V sync**.
- **Macroblock**, **motion vector**, **motion compensation**,
  **residual**. **I, P, B frames**, **GOP**, **display** vs **decode
  order**.
- **PSNR**, **MSE**. **Motion estimation**, **SAD**, **full search**.

## References

(To check when writing each module.)

- ITU-R BT.601, "Studio encoding parameters of digital television",
  1982 (YCbCr).
- Charles Poynton, *Digital Video and HD: Algorithms and Interfaces*,
  Morgan Kaufmann, 2003 (2nd ed. 2012).
- Jim Kent, the FLI format (Autodesk Animator, 1989), as documented in
  *Dr. Dobb's Journal*, March 1993.
- Microsoft, "AVI RIFF File Reference" (Video for Windows, 1992).
- ISO/IEC 11172-2, "Coding of moving pictures and associated audio for
  digital storage media at up to about 1,5 Mbit/s -- Part 2: Video",
  1993.
- Didier Le Gall, "MPEG: a video compression standard for multimedia
  applications", Communications of the ACM 34(4), 1991.
- Joan L. Mitchell, William B. Pennebaker, Chad E. Fogg, Didier J.
  LeGall, *MPEG Video Compression Standard*, Chapman and Hall, 1996.
- T. Koga, K. Iinuma, A. Hirano, Y. Iijima, T. Ishiguro, "Motion
  compensated interframe coding for video conferencing", NTC 1981 (the
  three-step search).
- Dominic Szablewski, pl_mpeg (2019) and jsmpeg (2013): readable MPEG-1
  decoders, in C and JavaScript.
