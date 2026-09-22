# Plan: image formats, from scratch, for teaching (`graphics/images/`)

## Context

`graphics/` teaches how pictures are *computed*; this plan teaches how
they are *stored*: our own readers for the three formats the
playground actually meets -- **PNG, GIF and JPEG** -- replacing
stb_image, the C library (through an old OCaml binding) that decodes
them today. XPM (`graphics/images/xpm/`, `Xpm.mli`) was the first
format done this way; this plan does the other three.

Each format is also a classic of compression, and they happen to
cover the field in three steps:

- **GIF (1987): LZW**, a dictionary built while reading (Welch 1984),
  lossless, 256 colors;
- **PNG (1996): DEFLATE**, LZ77 back-references then Huffman codes
  (Ziv-Lempel 1977, Huffman 1952), lossless, plus the *filters* that
  make rows compressible;
- **JPEG (1992): the DCT**, a transform to frequencies, quantization
  (throwing away what the eye doesn't see), then Huffman again --
  lossy.

So this is a teaching subject in its own right, like sound
(`plan_audio_teaching.md`) or deciding (`plan_ai_teaching.md`), and
not only the removal of a dependency: **how to make a picture small**,
losslessly and not, taught by the three formats everyone has met. The
dependency goes away as a consequence -- pure OCaml builds anywhere (no
C stub, no Makefile-built binding) and could run on the web backend
too.

Companions: [`notes_images.md`](../tutorials/notes_images.md), the
tutorial (written ahead of the code, as its specification, its worked
examples the tests to come): entropy, Huffman codes, LZW, LZ77 and
DEFLATE, PNG's filters, the DCT, quantization and chroma subsampling;
and [`notes_images_related_work.md`](../related-work/notes_images_related_work.md):
the formats before GIF, the Unisys LZW patent that made PNG happen, the
JPEG committee, the libraries (zlib, libpng, libjpeg, stb_image, Go's,
Wuffs, the OCaml ones), decoders as an attack surface, the books and
the code written to be read, and the formats after them.

## What stb_image does today

All in `graphics/images/` (library `graphics_images`):

- `Image_decode`: `Stb_image.load` of a (downloaded) file into the 2D
  `image` type; the GIF animation code (`gif_frames`,
  `animation_of_gif`) already parses GIF's blocks itself, and only
  calls stb_image to decode each frame's pixels, after rebuilding a
  standalone one-frame GIF around them.
- `Texture_decode`: `Stb_image.load`/`decode` for the 3D textures.
- `Rgba`: works around the pinned binding's bug with `~channels:4`
  (expands 1-3 channels to 4 by hand). Disappears with stb_image.

The type `Stb_image.int8 Stb_image.t` (width, height, channels,
offset, stride, and a `Bigarray` of bytes) leaks into
`playground/native/Image_native.ml` (Cairo surfaces),
`playground/native/Playground3d_platform.ml` (GL textures),
`playground/software/Shape_render_software.ml` and
`Shape3d_render_software.ml`, and `graphics/tests/Unit_rgba.ml`; and
`stb_image` is a dependency of four packages in `dune-project`
(`elm_playground_native`, `elm_playground_software`,
`elm_playground_3d_software`, `elm_playground_3d_opengl`).

What is actually decoded: `examples/checker.png`,
`games/adventure/tomb.png`, `games/fps/minecraft.png` (8-bit RGB, not
interlaced), elm-lang.org's GIFs (`Mario.ml`'s sprites, animated,
`Turtle.ml`), and whatever URL a user passes to `image`, which may be
a JPEG. The web backends are unaffected: the browser decodes.

Separately, `tests/common/` uses **imagelib** to read and write the
golden frames' PNGs. A PNG writer of our own removes that dependency
too (a second one; imagelib's GIF bug is what sent us to stb_image,
see the survey in `Image_decode.ml`).

Not in scope: `Download` and **curl** (fetching URLs) -- another C
dependency, but a networking question (see
`plan_networking_teaching.md`).

## Principles (the same as the rest of `graphics/`)

- **One idea per module**, its `.mli` explaining it: a diagram of the
  bytes, a worked example with numbers, the reference (the spec, and
  the paper).
- **Independent of the Playground**: bytes in, an RGBA buffer out.
- **Tested worked examples**, and real files: a few tiny images
  checked in as fixtures, decoded and compared pixel by pixel.
- **Readable first, fast enough second**: the images are small
  (sprites, 256x256 textures), decoded once, at load time. No SIMD
  tricks; the one place speed matters (JPEG's IDCT) gets the naive
  version and the fast one side by side, switchable, like the
  rasterizer's toggles.
- **Fail clearly** on what we don't read (a progressive JPEG, a
  16-bit PNG if we skip it): `Error "progressive JPEG: not supported"`,
  and the playground's usual behavior for a broken image (a warning,
  the image skipped).

## The image type

Ours, replacing `Stb_image.int8 Stb_image.t` everywhere:

```ocaml
(* graphics/images/rgba/Rgba_image.mli *)
type t = {
  width : int;
  height : int;
  (* row-major, top-to-bottom, 4 bytes per pixel, R G B A,
   * straight (not premultiplied) alpha; no offset, no stride *)
  rgba : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}
```

A `Bigarray` rather than `Bytes`, to keep what the consumers do today
unchanged: the OpenGL backend uploads it as is, the software backends
already use such a buffer (their stride-copy disappears, since there
is no stride any more). Always 4 channels: the decoders expand gray,
palette and RGB themselves, which is what `Rgba` does today. The field
is `rgba`, as in `Blit.image` and `Texture.image`, the same layout: the
software backends go from one to the other by renaming the record.

## Target layout

Like `xpm/`: private, pure-OCaml libraries, installed as part of
`elm_playground` (so the web backend could use them), unwrapped.

```
graphics/images/
  rgba/     Rgba_image              the type above
  deflate/  Crc32, Adler32          checksums (PNG chunks, zlib stream)
            Huffman                 canonical codes: lengths -> table
            Inflate                 DEFLATE decoding (RFC 1951)
            Deflate                 encoding: stored, then LZ77 + fixed codes
            Zlib                    the 2-byte header and the Adler32 trailer
  png/      Png                     chunks, filters, color types, Adam7; read and write
  gif/      Lzw                     GIF's variable-width LZW decoding
            Gif                     blocks, palettes, frames, disposal (moved
                                    from Image_decode), interlaced rows
  jpeg/     Jpeg_huffman            JPEG's Huffman tables and bit reader
            Dct                     the 8x8 IDCT, naive and fast (AAN)
            Jpeg                    markers, quantization, zigzag, upsampling, YCbCr
  xpm/      Xpm                     (exists)
  Image_decode, Texture_decode      (exist) dispatch on the magic bytes:
                                    \x89PNG, GIF8, \xFF\xD8
```

`Huffman` is shared by DEFLATE and JPEG (both canonical codes, built
from a list of code lengths): the same idea met twice, which is worth
saying in its `.mli`.

## Each format's story

Every format's main `.mli` (`Gif.mli`, `Png.mli`, `Jpeg.mli`, and
`Inflate.mli`, `Lzw.mli`, `Dct.mli` for the ideas under them) opens as
`Xpm.mli` does: who made it, when, for what, a diagram of the file
with a tiny example, what subset is read here and what is not, and the
references -- the spec *and* the paper the idea comes from. The
tutorial tells the same stories at more length, in order, as one
history of compression.

- **Entropy and Huffman.** Shannon, "A Mathematical Theory of
  Communication" (*Bell System Technical Journal*, 1948): the limit on
  how small data can get. Huffman, "A Method for the Construction of
  Minimum-Redundancy Codes" (*Proc. IRE*, 1952), a term paper for a
  class taught by Robert Fano, that beat his professor's own method.
  Used by both DEFLATE and JPEG (`Huffman.mli`).
- **LZ77 and LZ78.** Ziv and Lempel, "A Universal Algorithm for
  Sequential Data Compression" (*IEEE Trans. Information Theory*,
  1977): back-references into what was already seen; and "Compression
  of Individual Sequences via Variable-Rate Coding" (1978): a
  dictionary of phrases instead.
- **GIF (1987) and LZW.** Welch, "A Technique for High-Performance
  Data Compression" (*IEEE Computer*, 1984), a speed-up of LZ78, made
  at Sperry. CompuServe's GIF87a (Steve Wilhite's team), for its
  online service's pictures, 256 colors, LZW; GIF89a (1989) added
  transparency and delays; Netscape Navigator 2.0's looping extension
  (1995) made GIF the web's animation format. Then the Unisys patent on
  LZW (US 4,558,302), enforced from late 1994, expired 2003-2004 --
  the reason PNG exists. Spec: *Graphics Interchange Format, Version
  89a* (CompuServe, 1990).
- **DEFLATE and zlib.** Phil Katz's PKZIP 2 (1993): LZ77 plus Huffman,
  patent-free; zlib by Jean-loup Gailly and Mark Adler (1995), which
  nearly everything since uses. Specs: RFC 1950 (zlib) and RFC 1951
  (DEFLATE), Peter Deutsch (1996). Reading: Adler's `puff.c`.
- **PNG (1996).** Started in January 1995 on Usenet (Thomas Boutell's
  draft, "PNG's Not GIF") as the patent-free GIF replacement, with
  truecolor and alpha GIF never had; W3C Recommendation 1996, RFC 2083
  (1997), ISO/IEC 15948 (2003), a third edition (2025) adding APNG
  and HDR. The filters' Paeth predictor: Alan Paeth, "Image File
  Compression Made Easy" (*Graphics Gems II*, 1991). Reading:
  Roelofs, *PNG: The Definitive Guide* (1999, online); lodepng.
- **The DCT and JPEG (1992).** Ahmed, Natarajan and Rao, "Discrete
  Cosine Transform" (*IEEE Trans. Computers*, 1974). The Joint
  Photographic Experts Group, formed in 1986 by ISO and the CCITT; the
  standard, ITU-T T.81 (1992) / ISO/IEC 10918-1 (1994), and its
  explanation, Wallace, "The JPEG Still Picture Compression Standard"
  (*CACM*, 1991); the file format everybody actually writes, JFIF
  (Eric Hamilton, C-Cube, 1992); the Independent JPEG Group's libjpeg
  (Tom Lane, 1991), which made it universal. The fast IDCT: Arai, Agui
  and Nakajima, "A Fast DCT-SQ Scheme for Images" (*Trans. IEICE*,
  1988). Books: Pennebaker and Mitchell, *JPEG Still Image Data
  Compression Standard* (1993). Reading: NanoJPEG.
- **After them**, for the tutorial's last page only (not implemented):
  JPEG 2000 (wavelets), WebP (VP8's intra frames), AVIF, JPEG XL --
  and QOI (2021), a lossless format whose whole spec is one page,
  a candidate exercise.

## The modules, with their references

**Deflate (the hard part of PNG).** `Inflate` is the heart: the three
block types (stored, fixed Huffman, dynamic Huffman), the code-length
code that encodes the Huffman tables themselves, and the LZ77 copy
(length, distance) -- including the overlapping copy (distance 1,
length 10: ten copies of the last byte), the classic subtlety. ~250
lines. Reference: RFC 1951 (Deutsch, 1996); and Mark Adler's `puff.c`
(in zlib's `contrib/`), a DEFLATE decoder written *to be read* -- the
model for this module. `Deflate`, the encoder, for the PNG writer:
stored blocks first (correct, big), then LZ77 with a hash of 3-byte
prefixes and the fixed Huffman codes (small enough for golden frames,
which are mostly flat colors). Ziv & Lempel 1977; Huffman 1952.

**Png.** Chunks (length, type, data, CRC), `IHDR`, `PLTE`, `tRNS`,
`IDAT` (concatenated, then inflated), `IEND`; the five filters (None,
Sub, Up, Average, Paeth -- Paeth 1991, with its worked example); the
color types (gray, RGB, palette, gray+alpha, RGBA) at bit depths
1/2/4/8, 16 reduced to 8; Adam7 interlacing (seven passes, a picture
of the 8x8 pattern). ~250 lines. Writing: RGBA 8-bit, filter chosen
per row (the "minimum sum of absolute differences" heuristic), one
`IDAT`. ~80 lines. Reference: *PNG Specification*, W3C / ISO 15948
(2003); Roelofs, *PNG: The Definitive Guide* (1999). Test corpus:
Willem van Schaik's PngSuite (public domain; a subset checked in).

**Gif.** `Lzw`: variable-width codes (from `lzw_min_size + 1` bits up
to 12), the clear code (`1 lsl lzw_min_size` -- exactly what imagelib
gets wrong, see `Image_decode.ml`; a test for it), the end code, the
KwKwK case (a code not yet in the dictionary). ~80 lines. `Gif`: the
block parsing and frame composition already in `Image_decode` move
here, now decoding pixels directly instead of rebuilding one-frame
GIFs for stb_image; interlaced frames (rows 0, 8, ...; 4, 12, ...).
~150 lines. Reference: *GIF89a Specification* (CompuServe, 1990);
Welch, "A Technique for High-Performance Data Compression", *IEEE
Computer* (1984).

**Jpeg.** Baseline sequential only, which is what cameras, the web
and image editors write by default: the markers (`SOI`, `DQT`, `SOF0`,
`DHT`, `SOS`, `DRI`, `EOI`, the `APPn` skipped), the bit reader with
its byte stuffing (`FF 00`), DC coefficients as differences, AC as
(run of zeros, size) pairs, dequantization, the zigzag order, the 8x8
inverse DCT, chroma upsampling (4:4:4, 4:2:2, 4:2:0), restart markers,
YCbCr to RGB; grayscale too. ~450 lines. `Dct`: the naive 8x8 IDCT
from its formula (64 multiplications per pixel), and Arai-Agui-
Nakajima's fast one (1988), side by side; a test that they agree.
Progressive and arithmetic-coded JPEGs: refused, clearly (progressive
is another ~300 lines: a later exercise). CMYK: refused. Reference:
ITU-T T.81 (1992); Wallace, "The JPEG Still Picture Compression
Standard", *CACM* (1991); Martin Fiedler's NanoJPEG, a decoder
written to be short and clear.

## Phasing

Each phase keeps `make test` green, and stb_image stays until the
last one, **as the oracle**: during the transition a test decodes
every fixture with both and compares pixel by pixel.

1. **The type.** `Rgba_image.t`; `Image_decode`, `Texture_decode` and
   the four backends move to it (converted from stb_image's at the one
   place it's decoded). No behavior change; the golden frames prove
   it.
2. **Deflate + PNG reader.** `Crc32`, `Adler32`, `Huffman`, `Inflate`,
   `Zlib`, `Png`; PngSuite subset and the three textures as tests;
   `Image_decode` and `Texture_decode` use it for `\x89PNG`.
3. **GIF.** `Lzw`, `Gif`; `animation_of_gif` on it; the Mario and
   turtle GIFs as fixtures (or GIFs of our own, made with GIMP, if
   elm-lang.org's shouldn't be checked in).
4. **JPEG baseline.** `Jpeg_huffman`, `Dct`, `Jpeg`; fixtures at
   several subsamplings and with restart markers; a textured example
   using a JPEG.
5. **Drop stb_image.** Remove `Rgba`, the dependency in
   `graphics/images/dune`, the two backends' dune files and the four
   `dune-project` packages; regenerate the `.opam` files; rewrite the
   survey in `Image_decode.ml` to say where we ended up.
6. **PNG writer + drop imagelib.** `Deflate`, `Png.write`;
   `Testutil_golden` reads and writes with `Png`; `-dump-frame` can
   write PNG instead of PPM. Existing golden PNGs stay as they are
   (only their pixels are compared).

Size, all told: ~1,400 lines of OCaml plus their `.mli`s and tests.
Difficulty, in order: GIF is easy (a day's afternoon); PNG is
medium, `Inflate` being the only tricky part and `puff.c` a precise
guide; JPEG is the biggest (Huffman bit reading, subsampling layouts,
restart intervals), but baseline-only keeps it bounded. The migration
itself is small: the consumers already take a Bigarray of RGBA bytes.

## Status

- **Phase 1: done.** `graphics/images/rgba/` (library `graphics_rgba`,
  in `elm_playground`); `Rgba.of_stb_image` now gives an
  `Rgba_image.t`, and is the only code that sees stb_image's type
  (with `Unit_rgba`, which checks the binding's bug); the software
  backends' stride copy, the software 3D backend's layout assert and
  the OpenGL backend's `gl_format_of_channels` are gone (always RGBA);
  the two 3D packages no longer list stb_image. Golden frames
  unchanged (TexturedCube3d, TinyMinecraft, TinyTombRaider); Mario's
  animated GIFs and the turtle checked by hand (they need the network,
  so have no golden frame).

## Demos: seeing the compression

Part of the plan, not an extra: every idea gets something to look at,
the way the rasterizer's toggles show each step.

- `examples/ImageLzw.ml`: a small GIF decoded step by step, the codes
  read and the dictionary growing beside the pixels they produce;
  where the code width goes from 9 to 10 bits.
- `examples/ImagePng.ml`: a picture with each row colored by the
  filter PNG chose for it, and the bytes before and after filtering
  drawn as gray levels (filtered rows are mostly zero: why they
  compress); the LZ77 back-references drawn as arrows.
- `examples/ImageJpeg.ml`: the 8x8 blocks, a slider keeping only the
  first *n* DCT coefficients of each (why JPEG blurs edges and rings
  around text), a quality slider (the quantization tables scaled), the
  64 basis patterns, and the Y, Cb, Cr planes apart (why color can be
  stored at half resolution).
- A size readout on each: the same picture as raw RGBA, GIF, PNG and
  JPEG, in bytes.

Native only (they need the decoders' insides); the modules expose
those steps (a decoder with hooks, or a step-by-step variant) as far
as it stays readable.

## Verification

- Unit tests per module (`graphics/tests/`), worked examples from the
  `.mli`s: a hand-built DEFLATE stream, the Paeth example, a 4-pixel
  LZW stream, an 8x8 block through the IDCT.
- Fixtures: PngSuite subset, the repository's own PNGs and GIFs, a few
  JPEGs; decoded, compared with stb_image (phases 1-4), then with
  checked-in expected pixels (phase 5 onwards).
- Round trip: `Png.read (Png.write img) = img`.
- `make test`: the golden frames unchanged at every phase (the
  textures and Mario's GIFs must decode to the same pixels).

## Out of scope

- Progressive, arithmetic-coded, CMYK and 12-bit JPEGs; JPEG writing.
- APNG, PNG's color management chunks (`gAMA`, `iCCP`: ignored, as
  most decoders do), writing GIF.
- BMP, PPM, TGA, WebP (PPM and BMP are trivial and could follow, as
  exercises).
- Decoding in the browser: the web backends keep the browser's
  decoders.
- curl (see Context).
