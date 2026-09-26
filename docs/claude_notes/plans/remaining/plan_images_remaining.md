# Plan: what's left for the image formats

The formats are done: see
[`done/plan_images_teaching.md`](done/plan_images_teaching.md) --
`graphics/images/`'s own decoders for PNG (`deflate/`: `Crc32`,
`Adler32`, `Huffman`, `Inflate`, `Deflate`, `Zlib`; `png/`: `Png`), GIF
(`gif/`: `Lzw`, `Gif`) and baseline JPEG (`jpeg/`: `Dct`, `Jpeg`), a
PNG writer, stb_image and imagelib gone from the repository, the codecs
right in JavaScript too (`Int32` checksums, `graphics/tests/js/` under
node), the three demos `examples/ImageJpeg.ml`, `ImagePng.ml`,
`ImageLzw.ml`, and the tutorials
[`notes_images.md`](../tutorials/notes_images.md) and
[`notes_images_related_work.md`](../related-work/notes_images_related_work.md).

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli`, its test, and --
where it replaces something -- the simpler version kept beside it
behind a switch (a parameter: the pure libraries can't use
graphics/core's `Opti`), so the difference is a number you can watch.

## 1. What stb_image read and we don't: the regressions

Dropping stb_image took away formats a user of `Playground.image` could
have been relying on. Nothing in the repository uses them, which is
why phase 5 went ahead, but a program loading a photograph from a URL
may meet the first one.

- **Progressive JPEG** (SOF2), the web's favorite: refused today, with a
  message. The coefficients come in several scans, the DC ones first,
  then bands of the zigzag, then refinements a bit at a time
  (successive approximation), with an end-of-band run that spans
  blocks. About 300 lines over `Jpeg`'s baseline: the coefficients
  kept per block across scans instead of transformed at once, and the
  four scan kinds (DC first, DC refine, AC first, AC refine). The
  fixture: PIL writes them (`progressive=True`, graphics/tests/jpegs/'s
  `make_jpegs.py` already makes one, to be refused). And `ImageJpeg`
  could show it: the picture after each scan.
- **BMP**, and while at it **PPM/PGM** (what `-dump-frame` writes):
  next to no compression, an hour each -- BMP's bottom-up rows padded to
  4 bytes, its palette and bit depths, its optional RLE.
- TGA and PSD, which stb_image also read: only if someone asks.

## 2. The encoder: smaller files

`Deflate` is greedy, with one block of fixed codes. Its golden frames
are already 10% smaller than imagelib's, so this is for the lesson,
and for photographs.

- **Lazy matching** (RFC 1951, section 4; zlib's levels 4 and up): before
  taking a match at i, look for a longer one at i + 1, and if there is
  one send i as a literal. Measure on the golden frames and on
  PngSuite.
- **Dynamic Huffman blocks**: codes built from the block's own counts --
  Huffman's algorithm run forward, where `Inflate` only runs it back --
  with the lengths limited to 15 (package-merge, Larmore and Hirschberg
  1990, or zlib's simpler heuristic), the code lengths themselves sent
  run-length coded (16, 17, 18). Blocks cut every 16K symbols or so.
- **Trying the filters for real**: `ImagePng` found the spec's heuristic
  wrong on a drawing (no filter, 595 bytes; adaptive, 728). Encoders
  that care (pngcrush, zopfli's PNG mode) compress each choice and keep
  the smallest; per row, or per image with a handful of strategies.
  `Png.encode ?filter` already has the switch.

## 3. More formats

- **A GIF writer**: LZW encoding (`Lzw.mli`'s encoder column, run
  forwards), and the interesting half, choosing 256 colors -- median
  cut (Paul Heckbert, "Color Image Quantization for Frame Buffer
  Display", SIGGRAPH 1982), then dithering (Floyd-Steinberg, 1976). The
  user: `TinyAseprite` exporting an animation.
- **QOI** (Dominic Szablewski, 2021): lossless, a one-page
  specification (runs, an index of 64 recent colors, small
  differences), about PNG's size and much faster. An afternoon, and a
  good last page for the tutorial: what a format designed in 2021 for
  speed looks like next to one designed in 1995 for size.
- ~~**A JPEG writer**~~ (done, `Jpeg_encode`: baseline, 4:2:0 or
  4:4:4, T.81 Annex K's tables and Huffman codes, the IJG's quality
  scaling; on the fixtures' picture at quality 75, within a few bytes
  and 0.1 dB of libjpeg's own, and libjpeg reads it). Left: Huffman
  codes built for the picture (libjpeg's -optimize), gray, restart
  markers.
- **APNG** (in PNG since its third edition): `acTL`, `fcTL`, `fdAT`,
  and GIF's disposal rules, which `Gif.animation` already composes.
- **Arithmetic coding**: in JPEG's standard (and refused), and the
  other answer to Huffman's whole bits; worth a module of its own
  before any format uses it.

## 4. The decoders: robustness and speed

- **Fuzzing**. The `.mli`s promise `Failure` on a corrupt file, and
  PngSuite's corrupt files and a few cut ones are tested; but not every
  read is checked -- `Jpeg`'s segment readers index a DQT or DHT
  segment without checking its length, for one, and would raise
  `Invalid_argument`. `Image_decode` catches everything, so no program
  crashes; the promise is still broken. A test mutating the fixtures
  (a byte flipped, a length changed, a cut) and accepting only
  `Failure` or a picture would find the rest.
- **Huffman by table**: `Huffman.decode` reads a bit at a time (puff.c's
  way): 0.3 s for a 1024 x 768 JPEG, seconds for a camera's 12
  megapixels. zlib and libjpeg look up 8 or 9 bits at once, a second
  table for the longer codes. Beside the bit-at-a-time version, behind
  a switch, and timed.
- **The untested path**: a baseline JPEG with one scan per component
  (non-interleaved) goes through the code a gray scan uses, but no
  fixture has one -- PIL can't write it; libjpeg's `cjpeg -scans` can.
- **16 bits a channel** keep their high byte (as stb_image did);
  rounding (v * 255 + 32767) / 65535 would be exact, and the PngSuite
  table would move by a unit here and there.

## 5. Seeing it

- `ImageJpeg`'s **quality slider** (done: keys 1-9 and 0, our writer's
  bytes and PSNR shown) and the
  **Y, Cb, Cr planes** apart -- why color can be kept at half size.
- **LZ77 drawn**: a back-reference as an arrow from the bytes copied to
  the bytes written, over a line of text or a row of pixels -- the one
  idea of `notes_images.md` no demo shows yet.
- A **progressive JPEG filling in**, scan by scan (§1).

## 6. On the web

The web backends let the browser decode, from an `<img>`, which is
right for drawing a picture. But our decoders now run there too
(`graphics/tests/js/`), which opens two things the browser's `<img>`
can't do: a picture's pixels in the program (`Sprite.of_rgba` already
draws one on every backend, as the demos do), and textures for the
3D SVG backend, which has no per-pixel access and draws
`Playground3d.placeholder_texture_color` instead -- an embedded
texture could be decoded and sampled like the software backend's.

## 7. Settled, and not to be re-opened

The reasoning is in `done/plan_images_teaching.md`.

- **stb_image as an oracle**: the version bundled with the OCaml binding
  gets 9 of PngSuite's files wrong (interlaced 16-bit, tRNS colors) and
  returns a GIF's first frame only. The references are pypng, PIL and
  libjpeg-turbo, whose pixels are in the tests as checksums or PNGs.
- **Disposal 2 clears to transparent**, as browsers do, not to the
  background color, as the GIF spec says and PIL does (`Unit_gif`'s
  test says so on the pixel).
- **The demo GIF is ImageMagick's**: PIL writes a 16-color GIF's codes
  for 256 colors, which hides the width growing.
- **Binary data embedded as escaped strings**
  (`scripts/build/files_to_string_ml.ml`), not base64: the web has no
  base64 decoder of ours, and the compiler reads either.
