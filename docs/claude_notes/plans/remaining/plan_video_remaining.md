# Plan: what's left for video

The formats are done: see
[`done/plan_video_teaching.md`](done/plan_video_teaching.md) --
`graphics/videos/`'s own code for raw video (`yuv/`: `Yuv`, `Psnr`;
`y4m/`: `Y4m`), frames on demand (`movie/`: `Movie`), delta frames
(`fli/`: `Fli`, FLI and FLC read and written), the container (`avi/`:
`Avi`, Motion JPEG through `Jpeg_encode` and PCM sound), and MPEG-1
(`mpeg1/`: `Bits`, `Vlc`, `Mpeg1` decoding I, P and B pictures,
`Motion` and `Mpeg1_encode` encoding I and P); TinyMediaPlayer
(`apps/media/`) playing our clip in every one of them, a movie's
sound driving its picture, and the analyzer's three views (`d` what
changed, `a` the macroblocks and vectors, `r` what was sent); the
tutorials [`notes_video.md`](../tutorials/notes_video.md) and
[`notes_video_related_work.md`](../related-work/notes_video_related_work.md).

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli`, its test, and the
simpler version kept beside the better one, so the difference is a
number you can watch.

## 1. The encoder

- **B pictures** in `Mpeg1_encode`: a search backwards as well as
  forwards (`Motion.estimate` against the next reference), the average
  of the two predictions tried as a third, the macroblock types of a B
  (`Vlc.mb_type_b`), and the frames written out of display order (the
  reordering the decoder undoes, `Mpeg1.mli`'s diagram). Then our clip's
  size against ffmpeg's, which has them (36,487 bytes at quantizer 4;
  ours without, 28,091 at 5): the gap that's left.
- **Rate control**: a bitrate asked for, the quantizer chosen per frame
  -- or per macroblock, through the macroblock types' "quant" -- to
  meet it, the encoder's buffer model (the `vbv_buffer_size` it writes
  and doesn't honor). The knob every real encoder exposes.
- **Scene cuts**: an I picture where the picture changes entirely,
  found by the whole frame's SAD, instead of every 12.
- **Encoding speed**: `Dct.fdct` is the formula (4,096 products a
  block); a separable or AAN forward transform, beside it behind a
  switch, timed. Encoding our clip takes 2 to 3 s; the transform is the
  likely cost (the logarithmic search's 34 SADs a macroblock are
  cheap), to be measured first.

## 2. The analyzer

- **The planes apart**: Y, Cb and Cr side by side, the color's quarter
  resolution seen (`plan_images_remaining.md` §5 wants it for JPEG too:
  one view, both).
- **The bits per macroblock**: each macroblock's cost in bits, a heat
  map -- where the encoder spent the stream (the decoder counts its
  `Bits.position` around each).
- **FLI's chunks**: which frames are BRUN, LC or DELTA_FLC, and their
  sizes, as the strip does for I, P and B.

## 3. More formats

- **More than 256 colors in FLI**: a median cut (Heckbert, 1982),
  shared with `plan_images_remaining.md`'s GIF writer -- one palette
  chooser for both, and for TinyAseprite's GIF export.
- **The MPEG system stream** (`.mpg`): the packs and packets around
  the video elementary stream, and a second stream beside it. Its sound
  is MP2, a psychoacoustic codec: a plan of its own, or PCM in a
  private stream to begin with.
- **An AVI's other codecs**: uncompressed frames (`DIB `), and RLE8
  (Microsoft's run-length, FLI's cousin) -- small, and what old AVIs
  hold.
- **MPEG-2** (out of scope before, still): interlace, the field
  pictures, larger ranges. MP4 and its boxes, IFF's chunks once more.

## 4. On the web

The player builds for the browser (`apps/media/web/`), but nobody has
watched it there: the decoders' speed in js_of_ocaml (MPEG-1 at 160 x
120, 25 frames a second: does it keep up?), and the encoding at an
item's first play (seconds natively: longer in a browser, a progress
shown, or the encoding moved to build time).

## 5. Settled, and not to be re-opened

The reasoning is in `done/plan_video_teaching.md`.

- **Our own clips**: every clip is our own frames (`Our_media.clip`,
  drawn by graphics/2d); ffmpeg made the one MPEG-1 clip with B pictures
  (`graphics/videos/tests/clips/make_clips.sh`), kept as the decoder's
  independent check, beside ours.
- **ffmpeg as the oracle**, not the standard's text: the decoder's
  tables were written from the standard and checked by ffmpeg's
  decoding, within the IDCT's rounding (65-70 dB on our clip, 57 on
  harder streams).
- **The IDCT is `Dct.idct_aan`**, floating point: a pixel can differ by
  one from another decoder's, as the standard allows; the encoder uses
  the same one, so it and our decoder never drift.
- **`Avi`'s RIFF walk is its own**, not `Wav`'s: a dozen lines, not
  worth a library below both.
- **The playlist's items are made when played**
  (`Our_media.playlist`): making them all at start cost the tests'
  workers minutes of CPU.
