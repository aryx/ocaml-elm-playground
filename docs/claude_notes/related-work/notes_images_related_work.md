# graphics/images/ vs. the rest of the image-file world

The file-format twin of [`notes_playground_related_work.md`](notes_playground_related_work.md)
(drawing) and [`notes_audio_related_work.md`](notes_audio_related_work.md)
(sound): where the planned PNG, GIF and JPEG decoders of
`graphics/images/` (see [`plan_images_teaching.md`](../plans/plan_images_teaching.md)
and the tutorial [`notes_images.md`](../tutorials/notes_images.md))
come from, and where they sit among the formats, libraries and books.
The same through-line as the other notes: **the real decoders are
designed to be fast, complete and hardened against hostile files;
ours are designed to be *legible***, and complete only for the files
the playground meets.

## The one-line version

| | What it optimizes for | What you see |
|---|---|---|
| zlib, libpng, libjpeg(-turbo), giflib | Every file ever written, fast, for 30 years | Tens of thousands of lines of C, SIMD, decades of fixes |
| stb_image | One header, no dependency, the common cases | ~7,000 lines of dense C, every format in one file |
| Browsers' decoders | Speed, streaming, security (sandboxed, fuzzed) | Invisible: `<img src>` |
| Go's `image/*`, Wuffs | Readable (Go) / proven memory-safe (Wuffs) | Clean code in a safe language |
| lodepng, puff.c, NanoJPEG | One format, short, readable | A single file each, written to be understood |
| imagelib, camlimages, decompress | Images and compression in OCaml | Pure OCaml (imagelib, decompress) or C bindings (camlimages) |
| `graphics/images/` | Every step readable, with its history | One idea per module, its `.mli` telling the story |

## Part 1: before GIF -- every program its own format

The first bitmap formats belonged to one program or one machine, and
most used nothing smarter than **run-length encoding**:

- **MacPaint** (Apple, 1984): 576 x 720 black and white, compressed
  with **PackBits** (a count byte, then literal bytes or one repeated
  byte), later reused by TIFF.
- **Targa / TGA** (Truevision, 1984), for their video capture boards:
  true color and alpha early, optional RLE; still a game-asset format.
- **IFF ILBM** (Electronic Arts, 1985), the Amiga's pictures, and
  Deluxe Paint's: bitplanes and RLE; the chunked layout (a type, a
  length, data) that PNG's chunks descend from.
- **PCX** (ZSoft, PC Paintbrush, 1985): RLE, DOS's picture format.
- **TIFF** (Aldus, 1986): tags for everything, several compressions
  (PackBits, LZW, later JPEG, DEFLATE): the professional archive
  format, and the reason "TIFF reader" means "a reader of some TIFFs".
- **BMP** (Microsoft, Windows and OS/2): uncompressed rows, bottom to
  top, padded to 4 bytes; optional RLE.
- **PBM / PGM / PPM** (Jef Poskanzer's Pbmplus, 1988, later netpbm): a
  text header and raw pixels, no compression -- a format to convert
  *through*, and still the simplest one to write (the software
  backends' `-dump-frame` writes PPM).

## Part 2: GIF and the patent wars

CompuServe, the online service, needed pictures that its members'
different computers could all show over slow modems: **GIF87a** (June
1987, Steve Wilhite's team), LZW-compressed, 256 colors, interlaced for
the modem. **GIF89a** (1989) added the graphic control extension --
transparency, delays, disposal -- meant for slideshows. **Netscape
Navigator 2.0** (1995) added the looping application extension, and
the animated GIF became the web's animation format for the next thirty
years (an 8-frame Mario included).

LZW turned out to be patented: Welch's employer Sperry, merged into
**Unisys**, held US 4,558,302 (1985), and at the end of 1994 Unisys and
CompuServe announced that GIF software would need a license. The web
reacted with a replacement (Part 3) and, in 1999, a "Burn All GIFs
Day". The patent expired in the US in 2003, elsewhere in 2004, and GIF
lived on -- by then mostly for its animations.

## Part 3: PNG -- a format designed in the open

Within weeks of the Unisys announcement, in January 1995, Thomas
Boutell posted a draft on Usenet for a patent-free replacement; a
group formed on comp.graphics and the mailing lists, named it **PNG**
("PNG's Not GIF", officially Portable Network Graphics), and fixed
what GIF lacked while they were at it: true color, 16 bits per
channel, a real alpha channel, gamma, a signature that detects broken
transfers, a CRC per chunk, and DEFLATE instead of LZW -- compression
from **zlib** (Jean-loup Gailly and Mark Adler, 1995), itself built on
Phil Katz's patent-free DEFLATE from PKZIP 2 (1993). PNG became a W3C
Recommendation in 1996, RFC 2083 in 1997, ISO/IEC 15948 in 2003;
the **third edition** (2025) finally takes in **APNG**, Mozilla's
animated PNG (2004), long a de facto standard in browsers, and HDR.

Its library, **libpng** (Guy Eric Schalnat, then Andreas Dilger, Glenn
Randers-Pehrson and others), is in almost every program that shows a
picture. Greg Roelofs's *PNG: The Definitive Guide* (1999) tells the
story from the inside.

## Part 4: JPEG -- a committee and a transform

The **Joint Photographic Experts Group**, formed in 1986 by ISO and the
CCITT (now ITU-T), compared a dozen proposals and chose the **discrete
cosine transform** (Ahmed, Natarajan and Rao, 1974) with quantization
and Huffman coding; the standard is ITU-T T.81 (1992) = ISO/IEC
10918-1 (1994), explained for everyone by Gregory Wallace in the
*Communications of the ACM* (1991). The standard defines the coding,
not a file: the file everybody writes is **JFIF** (Eric Hamilton,
C-Cube, 1992), and cameras write **Exif** (1995) around the same data.

What made JPEG universal was a free implementation: the **Independent
JPEG Group**'s libjpeg (Tom Lane and others, first release 1991),
which Netscape and Mosaic used, which is why the web's photographs are
JPEGs; **libjpeg-turbo** (2010, Darrell Commander), the same API with
SIMD, is what runs today. Parts of the standard went unused because of
patents (arithmetic coding) or complexity (hierarchical and lossless
modes); progressive JPEG, rare at first, became a web favorite.

## Part 5: libraries, and decoders as an attack surface

- **zlib** (1995): DEFLATE everywhere -- ZIP, gzip, PNG, HTTP, PDF, git.
  **miniz** (a single-file zlib), **zopfli** (Google, 2013: slower,
  3-8% smaller DEFLATE), **libdeflate** (faster) are its descendants.
- **giflib** (from Gershon Elber's original code, 1989), **libpng**,
  **libjpeg**: the reference C libraries of the three formats.
- **stb_image** (Sean Barrett, 2009 on): every common format in one
  public-domain header, no dependencies, the game world's default --
  and what this repository uses until this plan is done (see the survey
  in `Image_decode.ml` for why it replaced imagelib).
- **Go's `image/png`, `image/gif`, `image/jpeg`** (Go's standard
  library, from 2010, largely by Nigel Tao): complete decoders in a
  memory-safe language, and the most readable production decoders
  there are.
- **Wuffs** (Nigel Tao, Google, 2017 on: "Wrangling Untrusted File
  Formats Safely"): a language whose compiler *proves* the decoders
  can't overflow a buffer, compiled to C; as fast as the C libraries.
- **In OCaml**: **camlimages** (Jun Furuse; bindings to the C
  libraries), **imagelib** (Rodolphe Lepigre; pure OCaml, whose GIF bug
  sent us to stb_image), **decompress** (Romain Calascibetta, for
  MirageOS; pure-OCaml zlib, gzip and more -- the one to compare
  `Inflate` with, and to measure against).

Why "hardened": an image decoder reads bytes from strangers -- every
web page, every message -- and a C decoder that trusts a length field
writes past its buffer. The history is long: libpng's and libjpeg's
CVEs, the Windows GDI+ JPEG overflow (MS04-028, 2004), ImageMagick's
"ImageTragick" (2016), and FORCEDENTRY (2021), an iMessage exploit
hidden in a file posing as a GIF. Browsers answer with fuzzing,
sandboxes and, lately, rewrites in memory-safe languages. Ours are in
OCaml: a bad length raises `Invalid_argument`, it doesn't overwrite
memory -- a correct-by-construction property worth a sentence in the
`.mli`s, if not a reason for the plan.

## Part 6: the teaching lineage

- **Books**: Mark Nelson and Jean-loup Gailly, *The Data Compression
  Book* (2nd ed., 1995), code for every method; David Salomon, *Data
  Compression: The Complete Reference* (many editions), the
  encyclopedia; Khalid Sayood, *Introduction to Data Compression*, the
  textbook; Timothy Bell, John Cleary and Ian Witten, *Text Compression*
  (1990), the theory; David MacKay, *Information Theory, Inference, and
  Learning Algorithms* (2003, free online), entropy done properly;
  Colt McAnlis and Aleks Haecky, *Understanding Compression* (2016), for
  programmers.
- **Code written to be read**: Mark Adler's `puff.c` (a DEFLATE
  decoder whose comments are a course, the model for `Inflate`); Lode
  Vandevenne's lodepng (PNG, both ways, one file); Martin Fiedler's
  NanoJPEG (baseline JPEG, one short file, the model for `Jpeg`); the
  PNG and GIF specifications themselves, which are short and clear.
- **Test corpora**: Willem van Schaik's **PngSuite**, every PNG
  variant in a small file each (the plan checks a subset in).
- Nobody we know of teaches the three formats *together*, as one
  history of compression, from one small codebase, with the steps drawn
  on screen: that is the gap `graphics/images/` and its demos fill.

## Part 7: after them

The three formats won and stayed; their successors each improve
compression by a third to a half, and each fight for adoption:

- **JPEG 2000** (2000): wavelets instead of 8 x 8 blocks (no blocking
  artifacts, one file at every resolution); digital cinema and
  medicine, but never the web.
- **WebP** (Google, 2010): lossy from the VP8 video codec's intra
  frames; its lossless mode (Jyrki Alakuijala) beats PNG.
- **HEIF / AVIF** (2015 / 2019): a still frame of a video codec (HEVC,
  AV1) in a container; the iPhone's HEIC.
- **JPEG XL** (2021-2022, from Google's PIK and Cloudinary's FUIF): the
  most capable, and it can re-encode an existing JPEG losslessly
  20% smaller.
- **QOI** (Dominic Szablewski, 2021): the opposite direction -- a
  lossless format whose specification is one page, nearly PNG's size,
  many times faster; the tutorial's suggested exercise.

## Where `graphics/images/` actually sits

Closest to puff.c and NanoJPEG in spirit (one format, short, read top
to bottom), to Go's decoders in safety (a memory-safe language, no
`unsafe`), and to nothing in scope: the subset the playground needs
(baseline JPEG, the common PNGs, GIF with animation), each gap refused
with a clear message rather than decoded wrongly. What it adds that
none of them have: the three formats as one story, each module's
history and paper in its `.mli`, and demos that draw what the
compression did.
