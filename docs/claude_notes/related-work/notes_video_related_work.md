# graphics/videos/ vs. the rest of the video world

The video twin of [`notes_images_related_work.md`](notes_images_related_work.md)
(pictures) and [`notes_audio_related_work.md`](notes_audio_related_work.md)
(sound): where the planned decoders of `graphics/videos/` (see
[`plan_video_teaching.md`](../plans/plan_video_teaching.md) and the
tutorial [`notes_video.md`](../tutorials/notes_video.md)) come from,
and where they sit among the codecs, players, libraries and books. The
same through-line as the other notes: **the real codecs are designed to
be small and fast, the players to read every file ever made; ours are
designed to be *legible***, and complete only for the files the
playground meets.

(From memory, to check against the sources before relying on it for
teaching.)

## The one-line version

| | What it optimizes for | What you see |
|---|---|---|
| libavcodec (ffmpeg), the hardware decoders | Every codec, as fast as the machine allows | Millions of lines of C and assembly, fixed-function silicon |
| x264, x265, libaom, SVT-AV1 | The smallest file for a quality, at a speed | Encoders tuned for years: rate control, psychovisual tricks |
| VLC, mpv, the browsers' `<video>` | Playing anything, in sync, anywhere | A demuxer per container, a decoder per codec, a clock |
| pl_mpeg, jsmpeg | One codec (MPEG-1), short, readable | A single file each, written to be understood |
| `graphics/videos/` | Every step readable, with its history | One idea per module, its `.mli` telling the story |

## Part 1: before the standards -- a codec per program

The first video on personal computers was each program's own:
**Autodesk Animator's FLI** (1989) and FLC, the delta frames of DOS
cut-scenes; Apple's **QuickTime** (1991), a container with its codecs
(Road Pizza, then **Cinepak**, SuperMac's, 1991-92, the CD-ROM era's
workhorse); Microsoft's **Video for Windows** and its AVI container
(1992), Intel's **Indeo**; and for games RAD's **Smacker** (1994) and
**Bink** (1999), made to decode fast on the CPUs games left free.
Their lesson is the container and the delta frame; their codecs
(vector quantization, in Cinepak's case) were dead ends the standards
overtook.

## Part 2: the standards -- one skeleton, refined

- **H.261** (ITU, 1988-90), for videoconferencing over ISDN: the
  skeleton appears -- 16 x 16 macroblocks, 8 x 8 DCT, motion
  compensation by whole pixels.
- **MPEG-1** (ISO, 1993), the Video CD at 1.5 Mbit/s: B frames and
  half-pixel vectors. Its audio layer III is MP3.
- **MPEG-2** / H.262 (1995): interlace, higher rates -- DVDs, digital
  and satellite television; for a decade the video most people watched.
- **H.263** (1996) and **MPEG-4 Part 2** (1999, DivX and Xvid's).
- **H.264 / AVC** (2003): the leap -- intra prediction, a 4 x 4 integer
  transform, blocks of many sizes, several reference frames, an in-loop
  deblocking filter, CABAC's arithmetic coding: half MPEG-2's size.
- **VP8** (On2, bought and opened by Google, 2010), **VP9** (2013),
  **HEVC** / H.265 (2013), **AV1** (the Alliance for Open Media, 2018),
  **VVC** / H.266 (2020): bigger blocks, more predictions, and the
  patent licensing that split the field in two.

Each keeps MPEG-1's frame: blocks predicted from moved blocks, a
transformed residual. That is why MPEG-1 is the one to learn from.

## Part 3: players and libraries

- **ffmpeg** (Fabrice Bellard, 2000) and its **libavcodec**: nearly every
  codec, the library under most players and services.
- **VLC** (the VideoLAN project, from students of the École Centrale
  Paris, 1996; free software 2001): a demuxer per container, a decoder
  per codec, the clock that keeps them in step -- the architecture
  TinyMediaPlayer copies in miniature.
- **mjpegtools** (Y4M's home), **x264** (2004), the reference software
  of each standard (JM for H.264, HM for HEVC), readable in places and
  huge.
- In the browser, `<video>` and Media Source Extensions; **jsmpeg**
  (Dominic Szablewski, 2013) decoding MPEG-1 in JavaScript where
  `<video>` couldn't, and **pl_mpeg** (2019), the same in one file of C:
  the readable decoders this plan follows.

## Part 4: the teaching lineage

- Charles Poynton's *Digital Video and HD* (2003, 2012): color, gamma,
  YCbCr, the sampling structures, from the engineer who explains them
  best.
- Didier Le Gall, "MPEG: a video compression standard for multimedia
  applications" (CACM, 1991): the design, by its committee's chair.
- Mitchell, Pennebaker, Fogg and LeGall's *MPEG Video Compression
  Standard* (1996): the standard explained.
- Iain Richardson's *Video Codec Design* (2002) and *The H.264 Advanced
  Video Compression Standard* (2010); John Watkinson's *The MPEG
  Handbook* (2001, 2004).
- Online: the MultimediaWiki's format pages (FLI, AVI, Cinepak...),
  and the "digital video introduction" tutorials that walk from pixels
  to H.264.

## Where `graphics/videos/` actually sits

At the legible end, with pl_mpeg and jsmpeg, and further: one idea per
module, the simple version beside the better one (the prediction
switched off to see the residual), each decoder measured against the
frames it was made from, and an analyzer that draws the file's own
decisions. The ceiling, deliberate: MPEG-1 and the formats before it,
not the codecs of today -- which are the same frame, refined past what a
reader can follow in an afternoon.
