# Plan: video, from scratch, for teaching (`graphics/videos/`)

## Context

`graphics/images/` reads pictures with our own code
([`plan_images_teaching.md`](plan_images_teaching.md)); `apps/media/`'s
TinyMediaPlayer shows them, plays sounds and tunes, and plays a GIF's
frames as the only "video" it has
([`plan_audio_formats.md`](plan_audio_formats.md), phase 4). This plan
adds **video**: pictures in time, the most data a computer has to move
and the place where compression's best ideas were found.

The subject in one line: a minute of 640 x 480 at 30 frames a second
is 1.6 GB raw, and a DVD's MPEG-2 stores it in about 40 MB (5 Mbit/s),
because
**consecutive frames are nearly the same** -- store what changed, and
where things moved, not the pictures. The formats below are chosen as
the images were, famous and simple enough to read, each bringing one
idea, in the order history found them:

| format | year | the idea it teaches |
|---|---|---|
| GIF | 1987, 1989 | done: frames with delays and a disposal, the web's first video |
| **Y4M** (YUV4MPEG2) | 2001 | raw video: frames in **YCbCr** with the color **subsampled** (4:2:0), the pixels every codec works in, and how big video is uncompressed |
| **FLI / FLC** (Autodesk Animator) | 1989, early 1990s | **delta frames**: store only the pixels that changed, as runs -- temporal compression at its simplest; the DOS games' cut-scenes |
| **AVI** with **Motion JPEG** | 1992 | the **container** apart from the **codec**: RIFF chunks (WAV's own) interleaving video and audio; every frame a JPEG (`graphics/images/jpeg`); keeping picture and sound in sync |
| **MPEG-1** video | 1993 | **motion compensation**: a frame predicted from others by moving 16 x 16 blocks, only the difference's DCT sent; I, P and B frames, decoded out of display order -- the structure of every codec since (MPEG-2, H.264, HEVC, AV1 refine it) |

TinyMediaPlayer plays each as it comes, and gets a **codec analyzer**
view: what the file actually says, drawn over the picture.

Companions: `docs/claude_notes/tutorials/notes_video.md`, written ahead
of the code as its specification, and
`docs/claude_notes/related-work/notes_video_related_work.md` (the
codecs before and after, the players, the books).

## Principles (the same as `graphics/images/`)

- **One directory per format, one library each**
  (`graphics/videos/mpeg1/` is `graphics_mpeg1`, ...), each `.mli`
  explaining its format -- its layout drawn, a worked example with
  bytes and numbers, its history, its reference -- pure OCaml, in the
  `elm_playground` package, so a browser plays what native does.
- **Frames on demand, not all at once**: a decoder is a state that
  gives the next frame (a video of minutes doesn't fit in memory as
  pictures, as a module's song didn't as samples); a player asks for
  the frame the clock says.
- **The audio clock drives the video**: when a file has sound, the
  frame shown is the one at the sound's position (the music's clock of
  `Audio.position`, again), never the other way round -- a late frame
  is skipped, a late sample would click.
- **Our own clips**: no film by someone else in the repository. The test
  clips are **rendered by our own renderers** (a scene of the 2D
  rasterizer, a spinning shape of the 3D one: the repository filming
  itself) and written by our own writers; where our writer doesn't
  exist yet (an MPEG-1 encoder, until phase 5), the clip is encoded once
  by a committed script from our own frames (as `make_demo_pictures.py`
  made the demo pictures), and never needed by the tests.
- **Measured, not eyeballed**: each decoder checked against the source
  frames it was encoded from, by **PSNR** (the signal-to-noise ratio in
  decibels, the field's own measure: 30 dB watchable, 40 near
  identical) and each format's size against the raw one.
- **The simple version next to the better one**, as elsewhere: the
  prediction switched off (see only the residual the encoder sent), the
  motion vectors drawn, the frame types shown.

## Target layout

```
graphics/videos/          video formats, one library each
  movie/                  Movie: what every reader gives, frames decoded
                          on demand at their times (done, phase 0)
  yuv/                    Yuv: RGB <-> YCbCr (BT.601), 4:2:0 and back;
                          Psnr (done, phase 1; JPEG keeps its own
                          conversion, Jpeg.mli's same formulas)
  y4m/                    Y4m: YUV4MPEG2 read and written (done, phase 1)
  fli/                    Fli: FLI and FLC read and written (done, phase 2)
  avi/                    Avi: the RIFF walk (Wav's), MJPEG frames, PCM
                          audio; written with Jpeg_encode (done, phase 3)
  mpeg1/                  Bits (the bit reader), Vlc (the variable-length
                          code tables), Mpeg1 (headers, macroblocks,
                          motion, B-frame reordering), later Motion (an
                          encoder's motion estimation)
  tests/                  worked examples, PSNR against our frames
apps/media/               TinyMediaPlayer: a Movie kind, the analyzer
```

## The formats

(To check against the specifications when writing each `.mli`.)

### Y4M and YCbCr

The eye sees brightness finely and color coarsely, so every video codec
stores a picture as **Y** (luma) and two color differences **Cb, Cr**,
the color at a quarter of the resolution (**4:2:0**: one Cb and one Cr
for each 2 x 2 block of Y) -- half the data before any compression, and
hard to see. YUV4MPEG2 (mjpegtools, 2001; what ffmpeg and x264 read)
is that and nothing else: a text header (`YUV4MPEG2 W320 H240 F25:1
C420`), then per frame `FRAME` and the three planes. The size lesson:
320 x 240 x 1.5 bytes x 25 = 2.9 MB a second.

### FLI and FLC: delta frames

Jim Kent's Autodesk Animator (1989): 320 x 200, 256 colors, a frame is
chunks -- the palette (`COLOR_64`, `COLOR_256`), the first frame as
byte runs (`BRUN`: a run of one color, or a run of literal bytes), and
every other frame as **what changed**: `LC` (FLI) and `DELTA_FLC`
(FLC, Animator Pro), lines skipped, then in each line runs of pixels to skip
and runs to write. A still background costs nothing; a small moving
sprite costs its pixels. Our clip quantized to 256 colors (a small
median cut, or the 2D renderer's own palette), written and read back;
its size against the raw frames'.

### AVI and Motion JPEG

Microsoft's Audio Video Interleave (Video for Windows, 1992): a RIFF
file (`RIFF ... AVI `) holding a header list (`hdrl`: the frame rate,
the size, a stream header per stream), the data (`movi`: chunks `00dc`
a frame, `01wb` audio samples, interleaved so a player reading in
order has both at once) and an index (`idx1`). The lesson is the
**container**: it knows where each frame is and when it plays, nothing
of what's inside -- here JPEGs (**Motion JPEG**: each frame alone, no
delta, what webcams and early digital cameras recorded), read by
`graphics/images/jpeg`, and PCM sound, WAV's samples. The player plays
the sound and shows the frame its clock says.

### MPEG-1 video: motion compensation

ISO/IEC 11172-2 (1993), the Video CD's: a sequence header (the size,
the frame rate, the quantization matrices), groups of pictures, and
three kinds of picture:

```
  display order:   I  B  B  P  B  B  P ...
  decode order:    I  P  B  B  P  B  B ...    (a B needs the P after it)

  I  intra: every 16 x 16 macroblock coded like JPEG -- 8 x 8 DCT,
     quantized, zigzag, run-length and Huffman-like codes
  P  predicted: each macroblock copied from the last I or P, moved by a
     motion vector (half a pixel precise), plus the DCT of what still
     differs -- often nothing
  B  bidirectional: from the one before, the one after, or their average
```

The decoder is fixed by the standard; the **encoder** is where the
cleverness is (finding the vectors: motion estimation, phase 5). The
readable reference to follow, not copy: Dominic Szablewski's pl_mpeg
(2019, one file of C, MIT), after jsmpeg. Parts: a bit reader, the
standard's variable-length code tables, the macroblock layer, motion
compensation with half-pel averaging, the IDCT (`graphics/images/jpeg`'s
if it fits), the reordering. The elementary video stream (`.m1v`)
first; the system stream's packets (`.mpg`) after, their MP2 audio out
of scope (a psychoacoustic codec: another plan).

### The codec analyzer

TinyMediaPlayer's view of what a file says, over the picture: the frame
types as a strip of colored cells (I red, P green, B blue), the
macroblock grid, the **motion vectors** as arrows, the **residual**
alone (the prediction switched off: what the encoder sent, mostly grey),
the Y, Cb, Cr planes apart; for FLI, the pixels a delta frame changed.
The tools of the trade do this (ffmpeg's `-flags2 +export_mvs`, the
commercial stream analyzers); here it is the lesson made visible.

## Phasing

0. **The notes and the groundwork** (done): `notes_video.md` and
   `notes_video_related_work.md` written ahead; `graphics/videos/movie/`
   (`Movie`: the frames' start times, a frame decoded when asked, and
   `Movie.sequential` for the decoders that only go forward, a seek back
   starting over) and `graphics/videos/tests/`; `Media`'s `Movie` kind
   in TinyMediaPlayer, the GIF animation moved onto it. The sound of a
   movie, and the audio clock driving it, wait for phase 3's AVI, the
   first format with both.
1. **Y4M and YCbCr** (done): `Yuv` (BT.601, both ranges, 4:2:0 and
   4:4:4, round trips measured by `Psnr`), `Y4m` read and written (its
   frames at known places: any one decoded at once, no
   `Movie.sequential`); our first clip, `ball_and_square.y4m`, 160 x 120
   for 2 s, drawn by the 2D rasterizer in `Our_media` and written as
   Y4M when the player starts (1.4 MB: no file in the repository); the
   player plays it, a golden frame showing 4:2:0's fringes.
2. **FLI/FLC** (done): `Fli` read (BRUN, LC, DELTA_FLC, the palettes,
   BLACK, COPY) and written (FLC by DELTA_FLC, FLI by LC), decoded
   forward only through `Movie.sequential` (which now keeps the frame
   before the last, for the comparison below); `ball_and_square.flc` in
   the player, the same frames as the Y4M. Our clip has 5 colors, so no
   quantization: more than 256 is refused, a median cut left as an
   exercise. Sizes of the 2 s clip: raw RGB 2,880,000 bytes, Y4M
   1,440,363, FLC 19,818, FLI 13,314 -- FLI smaller, its byte packets
   fitting edges that FLC's word packets (faster on a 386) round up to
   pairs of pixels. The analyzer's first view, `d` in the player, for
   any movie: what changed from the frame before, the rest dimmed.
3. **AVI with Motion JPEG and PCM** (done): `Avi` read (the RIFF walked,
   "rec " lists opened; MJPG only; PCM 8 or 16 bits, mono or stereo,
   mixed and resampled as `Wav` does) and written (Motion JPEG through
   `Jpeg_encode`, 16-bit mono PCM interleaved a frame at a time, the
   index), ffprobe and ffmpeg reading it whole. The RIFF walk is its own
   dozen lines, not shared with `Wav`'s (sharing it means a library
   below both, and changing `Wav`: not worth it for a dozen lines).
   `Media`'s movies got their sound (`Movie of { movie; sound }`), and
   TinyMediaPlayer its audio clock: a movie with a sound plays once, its
   frame the sound's position's, so pausing and seeking move the picture
   with the sound. `ball_and_square.avi`: the clip as JPEGs at quality
   75 with a blip at each landing, 263,050 bytes: 176,400 of sound (2 s
   of 16-bit samples), about 86,600 of JPEGs (Y4M's frames 1,440,363,
   FLC's 19,818: every frame whole, so bigger than the deltas, but any
   frame at once). Found on the way: the playlist box doesn't scroll (a
   window of 12 rows now, scrolled to the item playing), and every
   process linking `Our_media` built the whole playlist when it started
   (the tests' 64 workers: 143 s of CPU; now lazy, 2 s).
4. **MPEG-1 video**: I frames (a stream of them decodes like a JPEG
   sequence), then P (motion compensation, half-pel), then B and the
   reordering; our clip encoded once by a committed script (ffmpeg, from
   our frames), decoded, PSNR against our frames; the analyzer's frame
   types, vectors and residual.
5. **Motion estimation**: our own MPEG-1 encoder (I and P frames,
   full-search block matching by the sum of absolute differences, then
   a faster search), so the clips are ours end to end and ffmpeg is no
   longer needed; its size and PSNR against ffmpeg's.
6. **Docs**: `notes_video.md` checked against the code.

## Verification

- `make test-lite` as the gate, plus the player's new golden frames:
  PSNR and sizes checked in `graphics/videos/tests/`, a few frames of
  each clip compared with golden PNGs.
- By eye: each clip in the player, native and in a browser (decoding
  speed in js_of_ocaml measured, the frame rate it keeps).

## Out of scope

- H.264 and after (CABAC, intra prediction, in-loop filters): a plan of
  their own, MPEG-1 being their skeleton.
- MP2, MP3, AAC audio (psychoacoustic codecs).
- MP4, Matroska, Ogg containers (an exercise: MP4's boxes are IFF's
  chunks again).
- Streaming over a network, hardware decoding.
