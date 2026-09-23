# Pictures in files, from scratch: a tutorial for `graphics/images/`

How a picture is made small enough to keep and to send: the few ideas
behind the three formats everybody has met -- GIF, PNG and JPEG --
where they came from, and what goes wrong when they're done naively.
It is written before the code, as its specification (see
[`plan_images_teaching.md`](../plans/plan_images_teaching.md)): its
worked examples are the tests' to come. Companions:
[`notes_2d.md`](notes_2d.md) (how pictures are computed),
[`notes_audio.md`](notes_audio.md) (the same story for sound: samples,
frequencies, what the ear ignores), and
[`notes_images_related_work.md`](../related-work/notes_images_related_work.md).

The three formats are also three chapters of one story, the history of
**compression**, and they are told here in that order:

```
  1948 Shannon: how small can it get?     (entropy)
  1952 Huffman: short codes for common symbols
  1977 Ziv & Lempel: "the same as 3 bytes back"   (LZ77)
  1978 Ziv & Lempel, 1984 Welch: a dictionary     (LZ78, LZW)
  1974 Ahmed et al.: the cosine transform         (DCT)
        |                  |                         |
       GIF 1987 (LZW)     PNG 1996 (LZ77 +        JPEG 1992 (DCT +
       lossless,          Huffman = DEFLATE)      quantization + Huffman)
       256 colors         lossless, any color     lossy, photographs
```

## 0. Where the code is, and a reading order

| module (`graphics/images/`) | what | section |
|---|---|---|
| `rgba/Rgba_image` | the decoded picture: width x height x RGBA bytes | §1 |
| `deflate/Huffman` | canonical Huffman codes, from their lengths | §3 |
| `gif/Lzw` | GIF's LZW: a dictionary built while reading | §4 |
| `gif/Gif` | blocks, palettes, frames, disposal, interlacing | §5 |
| `deflate/Crc32`, `Adler32` | checksums | §6 |
| `deflate/Inflate`, `Deflate`, `Zlib` | LZ77 + Huffman, decoding and encoding | §6 |
| `png/Png` | chunks, filters, color types, Adam7 | §7 |
| `jpeg/Dct` | the 8x8 cosine transform, naive and fast | §8 |
| `jpeg/Jpeg_huffman`, `Jpeg` | quantization, zigzag, runs, YCbCr, subsampling | §9 |
| `Image_decode`, `Texture_decode` | which format a file is, and the playground's cache | §12 |

Read §1-3 first (they're about all three formats), then any of GIF
(§4-5), PNG (§6-7), JPEG (§8-9) on its own.

## 1. What a picture is, and why it must shrink

Decoded, a picture is what every backend here draws: a grid of pixels,
row by row from the top, each pixel four bytes, red, green, blue and
alpha (opacity), 0 to 255:

```
  width = 3                               bytes (row 0, then row 1):
  +-----+-----+-----+                     FF 00 00 FF  00 FF 00 FF  00 00 FF FF
  | red |green|blue |   height = 2        00 00 00 00  FF FF FF FF  FF FF FF 80
  +-----+-----+-----+                     ^ transparent             ^ half-opaque white
  |     |white|white|
  +-----+-----+-----+
```

That is `Rgba_image.t`. It's big: `games/fps/minecraft.png`, 256 x 256,
would be 196,608 bytes as RGB; the file is 5,257 bytes, 37 times
smaller. `examples/checker.png`, 64 x 64: 12,288 bytes of pixels, a
263-byte file, 47 times smaller. A 12-megapixel photograph is 36 MB of
RGB; as a JPEG, about 3 to 5 MB, and it looks the same.

Every file starts with a few **magic bytes** saying what it is -- which
is how `Image_decode` chooses a decoder, never from the file's name:

| starts with | format |
|---|---|
| `47 49 46 38` (`GIF8`) | GIF (then `7a` or `9a`) |
| `89 50 4E 47 0D 0A 1A 0A` | PNG (§7 says why those eight) |
| `FF D8 FF` | JPEG |
| `/* XPM */` | XPM (`Xpm.mli`: text, not compressed at all) |

Two families of compression:

- **lossless**: the decoder gives back exactly the pixels encoded (GIF,
  PNG). For drawings, sprites, text, screenshots: anything with sharp
  edges and flat colors, where one wrong pixel shows.
- **lossy**: the decoder gives back pixels *close* to the originals
  (JPEG). For photographs, where the eye doesn't see the difference,
  and the file is ten times smaller again.

## 2. How small can it get: entropy

Shannon (1948) answered first: data can be compressed as far as it is
**predictable**, and no further. If a source emits symbols with
probabilities *p*, each costs at least *-log2 p* bits, and the average,
the **entropy**, is

```
  H = - sum over symbols of  p * log2 p      bits per symbol
```

Worked example (the one `Huffman`'s tests use): the eight symbols
`AAAABBCD`. A is 1/2 of them, B 1/4, C and D 1/8 each:

```
  H = 1/2 * 1  +  1/4 * 2  +  1/8 * 3  +  1/8 * 3  =  1.75 bits per symbol
```

With four symbols, the obvious code takes 2 bits each (16 bits for the
string); 1.75 x 8 = 14 bits is the limit.

Two consequences worth knowing before writing any compressor:

- **Random data doesn't compress.** There are 2^n files of n bits and
  fewer than 2^n shorter ones, so no program shortens every file:
  compression is a bet that the input is predictable, and it loses on
  noise (and on already-compressed files: zipping a PNG gains nothing).
- **The model is everything.** "Probability" means probability *as the
  compressor sees it*. A photograph's pixels, byte by byte, look nearly
  random; seen as "this pixel is close to its neighbor", they are very
  predictable. PNG's filters (§7) and JPEG's transform (§8) are both
  ways of *changing the model* so that a simple coder then wins.

## 3. Huffman codes: short codes for common symbols

Huffman's algorithm (1952): take the two least likely symbols, join
them under a node whose probability is their sum, repeat until one
tree is left; each symbol's code is its path from the root (0 left, 1
right).

```
  AAAABBCD:  A 4, B 2, C 1, D 1

  join C+D (2), then B+(CD) (4), then A+(B(CD)) (8):

        (8)
       0/ \1
       A  (4)            A = 0
         0/ \1           B = 10
         B  (2)          C = 110
           0/ \1         D = 111
           C   D

  AAAABBCD = 0 0 0 0 10 10 110 111 = 14 bits: exactly the entropy
```

It hits the entropy exactly here because every probability is a power
of 1/2; otherwise it's within one bit per symbol of it (arithmetic
coding does better, §11). The code is **prefix-free**: no code is the
start of another, so a decoder reads bits one by one, walking down the
tree, and knows when a symbol ends without any separator.

**Canonical codes.** To decode, the reader needs the tree. DEFLATE and
JPEG both send only each symbol's **code length**, and both sides
rebuild the same codes by one rule: shorter codes first, and within a
length, in symbol order, counting up (RFC 1951, §3.2.2):

```
  lengths:  A 1, B 2, C 3, D 3

  count per length:   1 -> 1,  2 -> 1,  3 -> 2
  first code of each length:
    length 1: 0
    length 2: (0 + 1) << 1 = 2  = 10
    length 3: (2 + 1) << 1 = 6  = 110
  then counting up within a length:  A 0, B 10, C 110, D 111
```

That is `Huffman.of_lengths`, shared by `Inflate` and `Jpeg_huffman` --
the same idea met twice, sent a little differently (DEFLATE lists a
length per symbol, §6; JPEG lists how many codes of each length, then
the symbols in order, §9).

## 4. Dictionaries: LZW, GIF's compression

Huffman codes single symbols. Pictures repeat whole *sequences*: a row
of sky, a sprite's outline. Lempel and Ziv's second method (LZ78), made
fast by Welch (1984) as **LZW**, gives a code to every sequence already
seen, and builds that dictionary *while reading* -- the decoder builds
the same one from the codes alone, so it's never sent.

GIF's version, for pictures of up to 256 colors:

- the dictionary starts with one entry per color (for 4 colors, codes 0
  to 3), then two special codes: **clear** = `1 lsl min_code_size` (4:
  restart with a fresh dictionary) and **end** = clear + 1 (5); new
  entries start at 6;
- codes are written with a **variable width**: `min_code_size + 1` bits
  at first (3), one more each time the dictionary fills that width, up
  to 12 bits (4096 entries; then the encoder sends a clear);
- bits are packed **least significant first**, in sub-blocks of at
  most 255 bytes.

The encoder: keep the longest sequence *w* still in the dictionary;
when *w* + next pixel isn't, emit *w*'s code, add *w* + pixel as a new
entry, restart *w* from that pixel. Worked example (`Lzw`'s test), six
pixels of color 1, 4 colors (`min_code_size` 2):

```
  encoder                               emits  adds
  w = 1
  pixel 1:  "11" not known              1      6 = "11"    w = 1
  pixel 1:  "11" known                                     w = 11
  pixel 1:  "111" not known             6      7 = "111"   w = 1
  pixel 1:  "11" known                                     w = 11
  pixel 1:  "111" known                                    w = 111
  end of pixels                         7
  stream: clear(4) 1 6 7 end(5)
```

The decoder rebuilds the dictionary one step behind: each code read
(but the first after a clear) adds *previous sequence + first pixel of
this one*. And here's the one subtle case, called **KwKwK**: reading
code 6, the decoder hasn't added 6 yet -- the encoder used it the very
step it was created. That only happens when the new sequence is the
previous one plus *its own* first pixel, so that's what the decoder
takes:

```
  decoder         width  code  output   adds
  (after clear)   3      1     1
                  3      6     1 1      6 = "1"+"1"   (6 unknown: KwKwK)
                  3      7     1 1 1    7 = "11"+"1"  (7 unknown: KwKwK)
     next code is now 8 = 2^3: the width grows to 4
                  4      5     (end)
  output: 1 1 1 1 1 1
```

Packed LSB first -- 4 (`100`, 3 bits), 1, 6, 7, then 5 in 4 bits -- that's
16 bits, the two bytes `8C 5F`: GIF's image data is `02` (the
min_code_size), `02 8C 5F` (a sub-block of 2 bytes), `00` (the end of
sub-blocks). Twelve bytes of RGB in two.

Where LZW decoders go wrong, all worth a test:

- the clear code is `1 lsl min_code_size`. imagelib (the pure-OCaml
  library this repository once used) computes `1 lsl (min_code_size -
  1)`, and fails on real GIFs -- the survey in `Image_decode.ml`;
- when the width grows: when the *next* code to add reaches `2^width`,
  after adding (the encoder is one step ahead and must agree; an
  "early change" by one is the classic off-by-one, and TIFF's LZW
  famously does it the other way);
- the KwKwK case;
- a full dictionary (4096) without a clear: keep decoding, add nothing.

## 5. GIF: the file around the pixels

```
  "GIF89a"
  logical screen: width(2) height(2) flags(1) background(1) aspect(1)
  [global palette: up to 256 x (R, G, B)]
  then blocks, each starting with one byte:
    0x21 0xF9  graphic control: disposal, delay (1/100 s), transparent index
    0x21 0xFF  application: "NETSCAPE2.0", how many times to loop
    0x21 ...   other extensions (comments, text): skipped
    0x2C       a frame: x(2) y(2) width(2) height(2) flags(1)
               [local palette] min_code_size(1) sub-blocks of LZW data
    0x3B       end
```

(All numbers little-endian. `Image_decode.gif_frames` already walks
this; `Gif` takes it over.)

What the pixels are: **indices into a palette** of at most 256 colors,
so a GIF photograph has to choose its 256 colors (and dithers); a
sprite never notices. One index may be declared **transparent** -- on
or off, no half-opacity (PNG's alpha is what GIF lacked).

**Animation** was an afterthought: a GIF89a file may hold several
frames, each a *patch* (Mario's walking frames are 16 x 26 pixels at
an offset inside a 35 x 35 picture) drawn over the previous picture,
then **disposed** of before the next: left there (0, 1), cleared to
transparent (2), or restored to what was there before (3). The delay
is in hundredths of a second, and browsers turn a delay of 0 or 1 into
1/10 s -- old GIFs rely on it, Mario's among them, so we do too.
Netscape Navigator 2.0 (1995) added the looping extension, and the web
got its animation format for the next thirty years.

**Interlacing**: a frame's rows may be stored in four passes -- every
8th row from 0, every 8th from 4, every 4th from 2, every 2nd from 1 --
so a slow modem showed a blurry picture first. The decoder puts row *k*
of the data at its real place.

## 6. Back-references and DEFLATE

The other Ziv-Lempel method (LZ77): instead of a dictionary, say "copy
*length* bytes from *distance* bytes back" into what's already decoded
-- a window of the last 32 KB, for DEFLATE.

```
  abcabcabcabc   ->   a  b  c  (copy 9 from 3 back)

  output so far:  a b c
  copy from 3 back, one byte at a time:
                  a b c a            (the a just copied...)
                  a b c a b c a b c a b c
                        ^-----^ ...is copied again: length > distance
```

The copy **overlaps** what it's writing: a length of 9 from a distance
of 3 repeats "abc" three times, and a distance of 1 is run-length
encoding for free. So `Inflate` copies byte by byte, never with a
block copy (`Bytes.blit` gives the wrong answer here: the classic bug).

**DEFLATE** (Phil Katz, PKZIP 2, 1993; RFC 1951) writes the result
with Huffman codes. One alphabet for literals *and* lengths (0-255 a
byte, 256 end of block, 257-285 a length from 3 to 258, some with
extra bits), another for distances (30 codes, 1 to 32768, with extra
bits). A stream is a series of blocks, each:

- **stored** (type 0): raw bytes, for incompressible data;
- **fixed Huffman** (type 1): codes given by the RFC, no table sent --
  literals 0-143 in 8 bits, 144-255 in 9, 256-279 in 7, 280-287 in 8;
  distances in 5;
- **dynamic Huffman** (type 2): the block sends its own code lengths --
  themselves Huffman-coded, with a small third alphabet (0-15 a
  length, 16 repeat the previous, 17 and 18 runs of zeros) sent in a
  shuffled order (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13,
  2, 14, 1, 15), the most useful first, so trailing ones can be left
  out.

Worked example (`Inflate`'s test), `abcabcabcabc` in one fixed block:

```
  header    BFINAL 1, BTYPE 01 (fixed)             3 bits
  'a' 'b' 'c'  literals 97-99: 0x30+97.. in 8 bits  24 bits
  257+6 = 263  length 9 (257 is 3, 258 is 4, ...)   7 bits: 0000111
  dist code 2  distance 3 (0 is 1, 1 is 2, 2 is 3)  5 bits: 00010
  256          end of block                        7 bits: 0000000
  = 46 bits = 6 bytes:  4B 4C 4A 86 23 00
```

(Header fields and extra bits are packed least significant first, but
Huffman codes most significant first -- the other classic bug.) zlib at
its best compression writes 7 bytes for the same string, `4B 4C 4A 4E
84 21 00`: four literals, then (8, 3). Encoders choose; any decoder
reads both. That freedom is why `Deflate`, our encoder, can start with
stored blocks (correct, big), then add LZ77 with fixed codes -- the
golden frames are mostly flat colors, where that is plenty.

**zlib** (RFC 1950) wraps a DEFLATE stream in two header bytes (method
8 = deflate and window size, then flags such that the 16-bit header is
a multiple of 31: `78 01`, `78 9C`, `78 DA`) and a trailer, the
**Adler-32** checksum of the decoded bytes -- two running sums, fast to
compute:

```
  a = 1 + sum of bytes,   b = sum of the successive a's   (both mod 65521)
  "hi" (104, 105):  a = 1 + 104 + 105 = 210,  b = 105 + 210 = 315
  Adler-32 = b * 65536 + a = 0x013B00D2
  zlib stream of "hi", stored:  78 01 | 01 02 00 FD FF 68 69 | 01 3B 00 D2
                                       ^ final, stored; LEN 2, NLEN = not LEN
```

PNG protects each chunk with **CRC-32** instead (the polynomial
0xEDB88320, a table of 256 entries), whose standard check is
`CRC-32("123456789") = 0xCBF43926`: the first test `Crc32` has.

## 7. PNG: predicting pixels, then DEFLATE

```
  89 50 4E 47 0D 0A 1A 0A      the signature
  then chunks:  length(4) type(4) data(length) CRC-32(4 of type+data)
    IHDR  width(4) height(4) bit depth(1) color type(1)
          compression 0, filter 0, interlace 0 or 1
    PLTE  the palette (color type 3)
    tRNS  transparency, for palette or single-color keys
    IDAT  compressed pixels; several IDATs are one zlib stream, cut
    IEND  the end
```

(All numbers big-endian, unlike GIF.) The signature is a small lesson
in 1990s file transfer: `89` has the high bit set, caught by a link that
strips it (7-bit email); `PNG` for humans; `0D 0A` (CR LF) and `0A`
(LF) caught by a transfer that converts line endings; `1A` stops the
DOS `type` command from printing the rest. A chunk whose type starts
with an upper-case letter is **critical** (a decoder must understand
it), lower-case **ancillary** (`gAMA`, `tEXt`, ...: skip it) -- so old
decoders read new files.

**Color types**: 0 gray, 2 RGB, 3 palette, 4 gray + alpha, 6 RGBA; bit
depths 1, 2, 4, 8 (several pixels per byte for small ones) and 16 (the
high byte kept here). `Png` expands them all to `Rgba_image.t`.

**Filters: the model.** PNG's real idea is before DEFLATE: each row
starts with a byte choosing a **filter**, which replaces each byte by
its *difference from a prediction* made from neighbors already decoded:

```
        c  b         c: up-left   b: up
        a  x         a: left      x: the byte being decoded

  0 None     x
  1 Sub      x - a
  2 Up       x - b
  3 Average  x - floor((a + b) / 2)
  4 Paeth    x - (whichever of a, b, c is closest to a + b - c)
```

On a smooth gradient `10 20 30 40 50`, Sub gives `10 10 10 10 10`:
DEFLATE sees one repeated byte instead of five different ones. On a
flat area, every filter gives zeros. The neighbors are the *same
channel* of the previous pixel (3 bytes back in RGB, 4 in RGBA), and
the arithmetic is mod 256.

**Paeth** (Alan Paeth, *Graphics Gems II*, 1991) guesses a plane
through the three neighbors, *p = a + b - c*, then picks the neighbor
nearest to it (ties: a, then b, then c) -- an edge detector in three
lines:

```
  a = 100 (left)   b = 120 (up)   c = 90 (up-left)
  p = 100 + 120 - 90 = 130
  |p - a| = 30   |p - b| = 10   |p - c| = 40   ->  predict b = 120
  (the values grow downward: up is the better guess)
```

Which filter per row is the *encoder's* choice; the PNG spec suggests
the one whose outputs, read as signed bytes, have the smallest sum of
absolute values. `Png.write` does that; the demo colors each row by
its choice.

**Adam7 interlacing** (Adam Costello, 1995): seven passes over an 8 x 8
pattern, each a smaller picture filtered and compressed on its own, so
a browser shows a coarse picture after 1/64 of the data:

```
  1 6 4 6 2 6 4 6
  7 7 7 7 7 7 7 7
  5 6 5 6 5 6 5 6
  7 7 7 7 7 7 7 7
  3 6 4 6 3 6 4 6
  7 7 7 7 7 7 7 7
  5 6 5 6 5 6 5 6
  7 7 7 7 7 7 7 7
```

Rare now (it compresses worse), but every decoder must read it.

## 8. The cosine transform: pictures as frequencies

JPEG gives up exactness for size, and the question is what to give up.
The answer comes from the eye: it sees **slow changes** (the shading
of a face) much better than **fast ones** (the texture of skin,
noise), and **brightness** much better than **color**. So JPEG
rewrites each 8 x 8 block of pixels as a sum of 64 patterns, from flat
to finely striped, and keeps the slow ones precisely and the fast ones
roughly -- or not at all.

The 64 patterns are cosines, horizontally *u* and vertically *v* times
half a period across the block:

```
  u = 0      u = 1      u = 2         ...  u = 7
  ########   ####....   ##....##           #.#.#.#.      (one row of each,
  (flat)     (a slope)  (a bump)           (finest)       v = 0; '#' light)
```

The **forward DCT** (Ahmed, Natarajan and Rao, 1974) measures how much
of each pattern a block has; the **inverse** (what a decoder computes)
adds them back:

```
  f(x, y) = 1/4  sum over u, v of  C(u) C(v) F(u, v)
                                    cos((2x + 1) u pi / 16) cos((2y + 1) v pi / 16)

  C(0) = 1 / sqrt 2,  C(k) = 1 otherwise;   x, y, u, v in 0..7
```

*F(0, 0)*, the **DC** coefficient, is the block's average; the 63
others, **AC**, its details. Worked example (`Dct`'s test): a block
whose only coefficient is F(0, 0) = 80 is flat, every pixel 1/4 x
1/2 x 80 = 10; plus 128 (JPEG shifts pixels from 0..255 to -128..127
before the transform) = 138.

The formula costs 64 multiplications per pixel, 4096 per block.
Separating it (8 one-dimensional transforms on rows, then 8 on
columns) brings it to 16 per pixel; Arai, Agui and Nakajima (1988)
factor the 1D transform down to 5 multiplications, and fold 8 more into
the quantization table. `Dct` has the formula and AAN side by side,
switchable, and a test that they agree to within rounding.

## 9. JPEG: the pipeline

The encoder, which the decoder runs backwards:

```
  RGB --> YCbCr --> subsample --> 8x8 blocks --> DCT --> quantize --> zigzag --> runs --> Huffman
          (brightness   (color at      (-128)            (divide,     (low          (of
          and color)    half size)                       round: the   frequencies   zeros)
                                                         loss)        first)
```

**YCbCr.** Brightness (Y) and two color differences, as in color TV
(JFIF's constants, from ITU-R BT.601):

```
  Y  =  0.299 R + 0.587 G + 0.114 B
  Cb = -0.1687 R - 0.3313 G + 0.5 B + 128
  Cr =  0.5 R - 0.4187 G - 0.0813 B + 128

  back:  R = Y + 1.402 (Cr - 128)
         G = Y - 0.344136 (Cb - 128) - 0.714136 (Cr - 128)
         B = Y + 1.772 (Cb - 128)
```

**Chroma subsampling**: Cb and Cr at half the width (4:2:2) or half the
width and height (4:2:0, the usual): a 16 x 16 area -- a **minimum
coded unit**, MCU -- is then 4 Y blocks + 1 Cb + 1 Cr, 384 samples
instead of 768, before anything else. The decoder must upsample
(nearest, or smoother: libjpeg interpolates) and know each component's
sampling factors, from the frame header.

**Quantization, the loss.** Each coefficient is divided by an entry of
an 8 x 8 **quantization table** and rounded; the decoder multiplies
back. Big entries for high frequencies, where the eye won't miss the
error. The first row of the example luminance table (T.81 Annex K):

```
  16  11  10  16  24  40  51  61      <- low frequencies divided by ~10
  ...                                    high ones by ~100: mostly 0 after rounding
```

"Quality" in image editors scales these tables (the IJG's rule: below
50, times 5000/Q percent; above, times 200 - 2Q percent) -- the file
only carries the tables, never the quality.

**Zigzag and runs.** After quantization most of the 63 AC coefficients
are 0, the more so the higher the frequency. Read in a zigzag from the
top-left, the zeros come last, in a run:

```
   0  1  5  6 14 15 27 28          order of the 64 coefficients
   2  4  7 13 16 26 29 42
   3  8 12 17 25 30 41 43
   9 11 18 24 31 40 44 53
  10 19 23 32 39 45 52 54
  20 22 33 38 46 51 55 60
  21 34 37 47 50 56 59 61
  35 36 48 49 57 58 62 63
```

The **DC** coefficient is coded as the difference from the previous
block's (neighboring averages are close). Each nonzero **AC** is a
Huffman symbol (*run of zeros before it*, *size in bits*) then its
bits; `0x00` means "the rest are zeros" (end of block), `0xF0` sixteen
zeros. A value of size *s* is sent in *s* bits, negatives shifted --
size 2 covers -3, -2, 2, 3 as `00`, `01`, `10`, `11`:

```
  decode (s bits read as v):  if v < 2^(s-1) then v - 2^s + 1 else v
```

**The file.** A series of markers, `FF` then a byte:

| marker | |
|---|---|
| `FF D8` SOI | start |
| `FF E0` APP0 | JFIF (other `APPn`: Exif, ICC profiles; skipped) |
| `FF DB` DQT | quantization tables |
| `FF C0` SOF0 | baseline frame: size, components, sampling factors |
| `FF C2` SOF2 | progressive: refused here (§11) |
| `FF C4` DHT | Huffman tables: 16 counts per length, then symbols (§3) |
| `FF DD` DRI | a restart every *n* MCUs |
| `FF DA` SOS | start of scan: the entropy-coded data follows |
| `FF D0`-`D7` RSTn | restart: DC predictions reset, bits realigned |
| `FF D9` EOI | end |

Inside the scan, a data byte `FF` is written `FF 00` (**byte
stuffing**) so no marker can appear by accident; the bit reader drops
the `00`. Restart markers let a decoder resynchronize after a
corruption (and decode in parallel); miscounting them is a common
decoder bug, so the fixtures include a file that has them.

## 10. The three side by side

| | GIF | PNG | JPEG |
|---|---|---|---|
| year | 1987 / 1989 | 1996 | 1992 |
| loss | none | none | yes: quantization, subsampling |
| colors | 256, a palette | anything, 1 to 16 bits a channel | 8-bit YCbCr (or gray) |
| alpha | one transparent index | full, 8 or 16 bits | none |
| model | none | filters: predict from neighbors | DCT: frequencies |
| coder | LZW | LZ77 + Huffman (DEFLATE) | runs + Huffman |
| animation | yes | APNG (§11) | no |
| good at | small sprites, old animations | drawings, screenshots, sprites | photographs |
| bad at | photographs (256 colors) | photographs (big files) | sharp edges, text (ringing) |
| byte order | little-endian | big-endian | big-endian |

The sizes, measured on one picture, the demos' (examples/
`demo_picture.*`, 64 x 48, a drawing: a sky gradient, a sun, hills, a
house):

| | bytes | |
|---|---|---|
| raw RGB | 9,216 | 64 x 48 x 3 |
| PNG, no filter (our `Deflate`) | 595 | flat colors repeat: LZ77 alone does it |
| PNG, adaptive filters (ours) | 728 | the spec's heuristic guesses wrong here |
| PNG (PIL, zlib) | 735 | |
| GIF, 16 colors (ImageMagick) | 429 | the smallest -- and the sky banded |
| JPEG, quality 75, 4:2:0 (PIL) | 1,379 | the biggest, and blurred: not a photograph |

A drawing, so JPEG loses and the filters don't help; on a photograph
the order turns round (JPEG several times smaller than PNG, GIF's 256
colors visibly wrong). `examples/ImagePng.ml` and `ImageJpeg.ml` show
these numbers, and why.

## 11. What's missing, and exercises

Not read here, with how far each is:

- **Progressive JPEG** (SOF2): the coefficients sent in several scans,
  low frequencies first, then refinements bit by bit -- a web
  favorite. Another ~300 lines on top of baseline: the best next
  exercise.
- **Arithmetic-coded JPEG**: in the standard, patent-encumbered until
  the 2000s, so nobody wrote it; libjpeg reads it now. Arithmetic
  coding itself (a symbol costs a fraction of a bit) is worth a module
  of its own.
- **APNG**: animated PNG (Mozilla, 2004; in the PNG spec since its
  third edition): `acTL`, `fcTL`, `fdAT` chunks and GIF's disposal
  rules. Small, given `Gif`'s composition code.
- **Writing GIF**: LZW encoding (the table above, run forwards) plus
  choosing 256 colors (median cut, Heckbert 1982) -- the second half is
  the interesting one.
- **Writing JPEG**: forward DCT, quantization, Huffman codes (the
  example tables of Annex K).
- **A better `Deflate`**: lazy matching (look one byte ahead before
  taking a match), dynamic Huffman tables built from the block's own
  counts; how much each gains on the golden frames.
- **QOI** (2021): a lossless format whose whole specification fits on
  a page (runs, an index of recent colors, small differences), about
  as good as PNG and much faster. An afternoon.
- **BMP, PPM, TGA**: next to no compression at all; an hour each.
- **Color management** (`gAMA`, `iCCP`, `sRGB` chunks, ICC profiles in
  JPEG): ignored, as the playground's colors are assumed sRGB.

## 12. In the playground

`Playground.image w h url` (2D) and textures (3D) end in
`Image_decode.image_of_url` / `Texture_decode.load`: fetch the bytes
(`Download`), look at the magic bytes (§1), decode to
`Rgba_image.t`, cache by url. A GIF with more than one frame becomes an
animation (`Image_decode.animation_of_url`), the frame chosen by the
time, looping, like a browser. Each backend turns `Rgba_image.t` into
what it draws with: a Cairo surface (premultiplied alpha), the software
rasterizer's `Blit`, a GL texture. The web backends never see any of
it: the browser decodes the `<img>`.

What a user of the playground sees change: nothing -- the same
pictures, the golden frames identical. What they gain: the decoders to
read, and the demos to watch them work.

## Glossary

- **alpha**: opacity, 0 transparent to 255 opaque; *premultiplied* when
  the color bytes are already multiplied by it (Cairo), *straight*
  otherwise (PNG, `Rgba_image`).
- **canonical Huffman code**: a Huffman code rebuilt from code lengths
  alone, by a fixed rule (§3).
- **chroma subsampling**: color (Cb, Cr) stored at lower resolution than
  brightness (Y).
- **DC / AC**: a block's average (the first DCT coefficient) / the 63
  others.
- **DCT**: discrete cosine transform, a block as a sum of cosine
  patterns.
- **DEFLATE**: LZ77 then Huffman codes; zlib, gzip, ZIP and PNG.
- **entropy**: the average information per symbol, the limit of
  lossless compression.
- **filter** (PNG): a row's bytes replaced by their difference from a
  prediction.
- **LZ77 / LZ78 / LZW**: back-references into a window / a dictionary
  of phrases / LZ78 with a dictionary initialized with every symbol.
- **MCU**: minimum coded unit, the group of blocks covering one area of
  every component.
- **palette**: a table of colors; pixels are indices into it.
- **quantization**: dividing and rounding; the step that loses.
- **zigzag**: the order of a block's coefficients, low frequencies
  first.

## References

- Claude Shannon, "A Mathematical Theory of Communication", *Bell
  System Technical Journal* 27 (1948).
- David Huffman, "A Method for the Construction of Minimum-Redundancy
  Codes", *Proceedings of the IRE* 40 (1952).
- Jacob Ziv and Abraham Lempel, "A Universal Algorithm for Sequential
  Data Compression", *IEEE Transactions on Information Theory* 23
  (1977); "Compression of Individual Sequences via Variable-Rate
  Coding", same journal, 24 (1978).
- Terry Welch, "A Technique for High-Performance Data Compression",
  *IEEE Computer* 17 (1984).
- CompuServe, *Graphics Interchange Format, Version 89a* (1990).
- Peter Deutsch, RFC 1951, *DEFLATE Compressed Data Format
  Specification version 1.3*; Deutsch and Jean-loup Gailly, RFC 1950,
  *ZLIB Compressed Data Format Specification version 3.3* (1996).
- Mark Adler, `puff.c`, in zlib's `contrib/puff/`: an inflater written
  to be read.
- *Portable Network Graphics (PNG) Specification*, W3C Recommendation
  (1996; second edition 2003 = ISO/IEC 15948; third edition 2025).
- Alan Paeth, "Image File Compression Made Easy", in *Graphics Gems
  II*, ed. James Arvo (1991).
- Greg Roelofs, *PNG: The Definitive Guide* (O'Reilly, 1999; free
  online).
- Nasir Ahmed, T. Natarajan and K. R. Rao, "Discrete Cosine
  Transform", *IEEE Transactions on Computers* C-23 (1974).
- ITU-T Recommendation T.81 = ISO/IEC 10918-1, *Digital Compression and
  Coding of Continuous-Tone Still Images* (1992).
- Gregory Wallace, "The JPEG Still Picture Compression Standard",
  *Communications of the ACM* 34 (1991).
- Eric Hamilton, *JPEG File Interchange Format, Version 1.02* (C-Cube
  Microsystems, 1992).
- Yukihiro Arai, Takeshi Agui and Masayuki Nakajima, "A Fast DCT-SQ
  Scheme for Images", *Transactions of the IEICE* E71 (1988).
- William Pennebaker and Joan Mitchell, *JPEG Still Image Data
  Compression Standard* (Van Nostrand Reinhold, 1993).
- Martin Fiedler, NanoJPEG: a baseline JPEG decoder in one short C
  file.
