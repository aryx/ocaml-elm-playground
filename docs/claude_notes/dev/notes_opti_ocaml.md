# Making OCaml number crunching fast, and keeping it readable

The same handful of mistakes, found again and again in this
repository's inner loops (decoders, rasterizers, synthesizers), each
invisible in the source and obvious in a profile. This note lists them:
how each shows up, why OCaml does it, the fix, where it was applied,
and what it bought. The per-renderer logs are `notes_opti.md` (2D) and
`notes_3d_opti.md`; this one is about the language.

The rule for the fixes, as everywhere here (`guide-principles.md`):
**the old code stays in a comment next to the new**, saying what it
cost, so the reader learns the trick where it is used; and a fix is
small and local, not a restructuring. Only a measured hot spot gets
one.

## How to measure

- **Wall clock**, for "is it fast enough": a small driver timing just
  the thing (`Unix.gettimeofday` around it), run twice or three times.
  This machine is shared with other builds: a run can take twice as
  long as the next one. Examples: `Mpeg_to_wav.exe` (it prints the
  decoding time), `Mpg_bench.exe` (an .mpg's demultiplexing, sound and
  video frames timed apart; `NOAUDIO=1` for callgrind).
- **Instructions, with callgrind**, for "what did this change buy",
  deterministic to the instruction whatever the load (no `perf`
  here):

  ```
  valgrind --tool=callgrind --callgrind-out-file=cg.out ./bench.exe input
  callgrind_annotate cg.out | head -30              # the top functions
  callgrind_annotate --tree=caller cg.out | grep -B5 "do_compare_val"  # who calls it
  ```

  ~50 times slower than the real run: decode 60 frames, not 2000.
- **In the program**: a view drawing more than what's on the screen, a
  game falling below 60 fps -- the `-uncapped` flag, `-dump-frame n`
  timed, the fps counter.

Names to recognize in a profile, each a section below:

| in the profile | means | section |
|---|---|---|
| `do_compare_val`, `caml_lessequal`, `caml_greaterequal`, `caml_compare`, `Stdlib.min`/`max` | polymorphic comparison | 1 |
| `caml_alloc*`, `caml_call_gc`, a `fun_NNNN` closure high up | allocation, closures in the loop | 2, 3 |
| `caml_modify` | a polymorphic array function storing ints through the write barrier | 3 |
| `caml_round`, `caml_c_call` | a C external per element | 4 |
| `caml_apply2`, `caml_apply3` | a call through a closure or a partial application | 5 |
| a small function's own name, called millions of times | not inlined | 5 |
| one function's cost growing with the file, not the screen | work redone each frame | 7 |
| the renderer's fills, the decoder fast | a picture drawn as shapes | 8 |

## 1. Polymorphic comparison: `min`, `max`, `compare`, `=`

**Symptom**: `do_compare_val`, `caml_lessequal`, `caml_greaterequal`,
`Stdlib.min` in the profile; in `Mpeg1`, 40% of the decoding.

**Why**: `min`, `max` and `compare` are polymorphic functions, compiled
once for every type: each call goes to the runtime's generic comparison,
which walks the values' representations, even for two ints. The
compiler specializes `<`, `=`, `compare` to machine instructions only
where it *knows* the type at that place (an annotated `(v : int)`); a
call to `min` never gets that.

**Fix**: a clamp on ints, with the types written.

```ocaml
(* was: max 0 (min 255 v) *)
let clamp (lo : int) (hi : int) (v : int) : int = if v < lo then lo else if v > hi then hi else v
```

**Where, and what**: `Mpeg1.clamp` (motion compensation, the pixels
written, the coefficients): an .mpg's 352 x 288 frames from 65 ms to 24
ms each. `Yuv.clamp`, `Jpeg.clamp`, `Signal.to_int16` (2 calls of
`caml_lessequal` per sample written to a WAV).

Also: `=` on a variant, an option or a list (`r = []`, `code = Some
0xB5`) is a C call too -- harmless once per slice, costly per pixel;
there, `match`.

## 2. A float `ref` captured by a closure is boxed

**Symptom**: allocation (`caml_alloc_small`, the GC) in a tight sum;
the loop 10 times slower than its arithmetic.

**Why**: OCaml keeps a local `float ref` unboxed, in a register, only
if nothing but the function itself uses it. Captured by a closure
(`Array.iteri (fun k x -> sum := !sum +. ...)`), it becomes a real
heap cell holding a boxed float: each `:=` allocates a new float.

**Fix**: a `for` loop, the ref never escaping.

```ocaml
(* was: Array.iteri (fun k x -> sum := !sum +. (x *. row.(k))) coefficients *)
for k = 0 to n - 1 do sum := !sum +. (coefficients.(k) *. row.(k)) done
```

**Where**: `Imdct.output` -- 20 s of MP3 decoded in 6.3 s, then 0.69 s:
nine times faster, from this alone.

## 3. `Array.init`, `Array.iteri`, `Array.map` per element

**Symptom**: `fun_NNNN` (the closure) and `caml_modify` high in the
profile; `Array.init` itself.

**Why**: three costs per element. The closure call. Often a division
and a modulo, `k / size` and `k mod size`, to get back the row and the
column the loop had. And, for an `Array.init` of ints, `caml_modify`:
Stdlib's `Array.init` is polymorphic, compiled not knowing whether its
elements are pointers, so each store goes through the garbage
collector's write barrier.

**Fix**: `Array.make`, then two `for` loops over the rows and columns;
for bytes copied row by row, `Bytes.blit` (a `memcpy`).

```ocaml
(* was: Bytes.init (w * h) (fun i -> Bytes.get plane ((i / w * stride) + (i mod w))) *)
let out = Bytes.create (w * h) in
for y = 0 to h - 1 do Bytes.blit plane (y * stride) out (y * w) w done
```

**Where**: `Mpeg1.prediction`, `write_macroblock`, `to_image`'s crop,
the residual's additions.

## 4. A C external per element: `Float.round`

**Symptom**: `caml_round` (or `caml_c_call`) called once per pixel.

**Why**: `Float.round` is an `external`, a C function: never inlined,
a call per value. (`Float.of_int`, `int_of_float`, the arithmetic are
instructions; `Float.round`, `Float.rem`, `**` aren't.)

**Fix**: when the result is clamped to 0..255 anyway, `int_of_float
(v +. 0.5)` gives the same integers (for v >= 0 both round half away
from zero; below 0 both clamp to 0). For values of either sign:

```ocaml
let[@inline] round (v : float) : int = if v >= 0. then int_of_float (v +. 0.5) else -int_of_float (0.5 -. v)
```

**Where**: `Yuv.clamp`, `Jpeg.clamp`, `Mpeg1.round` (a block's 64
values).

## 5. Inlining: closures, partial applications, size

**Symptom**: a tiny function (`get`, `clamp`) with its own line in the
profile, called millions of times; `caml_apply2`.

**Why**: without flambda (this repository's switch), ocamlopt inlines
only a *known*, *top-level* function, *fully applied*, and *small* (its
body under `-inline`'s size, 10 by default). So:

- a local closure capturing variables (`let get px py = ... plane ...
  in`) is never inlined: move it to top level, passing what it
  captured;
- a partial application (`let g = get plane stride rows in g px py`)
  builds a closure again: write the full application at each call;
- a top-level function just over the size (two clamps) isn't:
  `let[@inline] get ...` asks, and ocamlopt obeys even without flambda.

**Where**: `Mpeg1.get` (a pixel of the reference picture, 1 to 4 per
pixel predicted): 5%.

## 6. A float crossing a call is boxed

**Symptom**: allocation in a loop of float arithmetic calling a small
helper (`clamp (y +. t.r_cr.(cr))`).

**Why**: floats are passed to and returned from functions boxed
(allocated), unless the function is inlined. A helper taking a float,
called 3 times per pixel, is 3 allocations per pixel.

**Fix**: `[@inline]` on the helper (section 5); then the float stays in
a register.

**Where**: `Yuv.clamp`: `Yuv.to_image` 6%.

## 7. Work proportional to the file, not the screen

**Symptom**: a program fine on small inputs, slow on real ones; one
function's cost growing with the data's size.

**Why**: a view recomputing from the whole data, every frame, what only
changes when the data does.

**Fix**: compute once, keep, with the data it came from (physical
equality, `==`, tells whether it is still the same).

**Where**: TinyMediaPlayer's waveform scanned all its samples, in every
frame: nothing for our 2 s bell, 8 million samples 60 times a second for
a song -- the frames late, the sound card starved, the sound cut. Now
its 350 columns' peaks are computed once per recording.

## 8. The wrong drawing primitive

**Symptom**: the decoding fast, the program still slow; the time in the
renderer (Cairo's fills, or ours).

**Why**: a picture drawn as shapes. `Sprite.of_rgba` makes a rectangle
per run of one color in a row: for pixel art a few hundred, for a
video's frame, where neighbours differ, one per pixel -- 100,000 for
352 x 288, each filled by Cairo.

**Fix**: the primitive made for it, `Playground.bitmap w h img`, pixels
from memory, which each backend draws its own way (a Cairo image
surface, our `Blit`, a PNG data: URL on the web), the last one
converted kept. Pixel art (64 x 64 pixels or fewer) keeps its squares,
crisp.

**Where**: TinyMediaPlayer's movies: 300 frames of an .mpg in 57 s of
wall time, then 7.3 (5 of them the playing itself): 6 frames a second
to real time.

## 9. The mathematics: symmetry, zeros, tables

Not OCaml's fault, but the same spirit: don't compute what is known.

- **Symmetry.** `Polyphase`'s matrixing: V[32 - i] = -V[i] and V[96 -
  i] = V[i], from the cosines' identities -- 32 rows instead of 64.
  `Imdct`: the outputs' halves mirror each other (the aliases the
  overlap cancels) -- a quarter computed. MP3 decoding: 7.06 to 4.83
  billion instructions for 25 s of stereo (-32%).
- **Zeros.** An IMDCT of 18 zeros (most of the high subbands) is zeros:
  nothing computed.
- **Tables.** A term depending on one byte has 256 values: look it up.
  `Yuv.to_image`'s color conversion (libjpeg's `jdcolor.c` trick), 3
  additions per pixel instead of 6 products and 2 divisions, and no
  tuples allocated for `to_rgb`'s argument and result -- the same
  colors, bit for bit, since the additions keep their order.
- **Shifts.** `px / side`, with side 1 or 2, is `px lsr shift`; a
  division per pixel is tens of cycles.
- **Work not asked for.** `Mpeg1` built the analyzer's residual picture
  for every frame, doubling the writing; now only when the `r` key
  asks.

## 10. Reject before you match: the ancestor filter

A style sheet's selectors are matched right to left, and a descendant
selector walks up the element's ancestors, compound by compound, with
backtracking. On a Wikipedia article (5,637 elements, 1,557 rules,
most of them long chains like `html.skin-theme-clientpref-night
.mw-parser-output ...`), the rules left after the index (filed by their
rightmost id, class or name) still cost 0.95 s: each walked some twenty
ancestors to fail. WebKit's answer, its "selector filter": while the
tree is walked, keep the ancestors' ids, classes and names (a Bloom
filter there, a counted `Hashtbl` here: push on the way down, pop on the
way up); give each rule the keys its left compounds need (those joined
by descendant or child combinators: a sibling is not an ancestor); a
rule whose keys are not all among the ancestors' is rejected before any
matching. The cascade went from 0.95 s to 0.54 s, its results
identical (`Cascade.ml`, the old line in a comment).

## 11. Measure first, then memoize what does not change

TinyChrome on a saved Wikipedia article (4,961 words, 13 pictures)
took 7.7 s to read and lay out, and 8.2 s again for each relayout --
once per picture and sheet that arrives, so about two minutes for the
page. A probe timing each stage (the best of 3 runs: other builds share
the machine) found three things, none a matter of OCaml's code
generation:

- **Shrink-to-fit measured the same subtrees again and again**
  (`Box_layout.shrink`): 89,903 blocks laid out for one page, 88,200 of
  them while measuring -- a flex item measures its content, which holds
  a table, whose cells are measured, each a flex row... each level
  measuring everything below it again. A measure depends only on the
  element, its display and the width asked for (unlimited or 0), not on
  where it is: memoized per layout (by `==`), 2.4 s to 0.1 s.
- **The sheets were parsed at each relayout** (`Browser_page.parsed`):
  memoized by address and text (a page's `<style>`s share its address,
  so the text is in the key).
- **The cascade ran at each relayout** although a picture changes
  neither the tree nor the sheets (`Browser_page.styles_of`): the last
  styles kept with the tree and the sheets' rule lists they came from,
  compared by `==` -- which asked the sheets without `@import` to keep
  their parsed list as it is, not rebuilt by `concat_map` each time.

A memo is only right if nothing it leaves out changes the answer: the
layout of both pages was compared before and after (an MD5 of every
fragment's text and position: identical, 4,961 and 1,215 fragments).
Result: the article read and laid out in 0.7 s, a relayout 0.3 s
(GitHub's repository page: 0.05 s); what is left of a relayout is
building every shape of the page (0.25 s), the next thing to make lazy.
The old lines are in comments beside the memos.

## 12. The inner loop allocates: TLS's big numbers

Our TLS 1.3 (plan_tls.md) checks a certificate chain with ECDSA over
P-256 and P-384: two scalar multiplications a signature, each a few
hundred point doublings and additions, each a dozen Montgomery
multiplications and as many modular additions (`Bignum`). Measured
with a small program timing one check of a real root's self-signature
(`X509.signed_by` on GTS Root R4), and `tls_get` fetching github.com
(two handshakes and its 580 KB page; user time):

| change | ECDSA P-384 | `tls_get https://github.com/` |
|---|---|---|
| first version | 41 ms | 0.36 s |
| `redc_mul`'s modulus padded once, in the `modulus` record, not at each product; `mont_add`, `mont_sub` as loops on the fixed n limbs, not add/sub/compare on normalized copies padded again (three arrays each) -- both together | 31 ms | |
| a chain already checked in this program not checked again (`Tls_client.verified`, until its first certificate expires): the second handshake checks only CertificateVerify | 31 ms | 0.26 s |

The lesson is section 1's again: the arithmetic was right and simple,
and each call made new arrays -- in a loop run thousands of times a
signature. The cache is section 11's: a page's twenty pictures from
one host were twenty identical chains checked.

Not done: a windowed scalar multiplication (a quarter of the additions),
Shoup's 4-bit tables for GCM's GHASH (it is a bit at a time, 300 ms for
500 KB; ChaCha20-Poly1305, which we offer first and every server we
tried chose, is 31 ms), the P-256 prime's special reduction. Each
would cost the readable version its place in the code.

## The .mpg decoder, step by step

60 frames of a 352 x 288 VCD .mpg (`albator_78_debut.mpg`), video only,
in instructions (callgrind), each line with the changes above it:

| change | instructions |
|---|---|
| clamp on ints (1), prediction's loops (3) | 9.13 G |
| write_macroblock's loops, crop's blits (3), the residual only when asked (9) | 6.99 G |
| `get` at top level, fully applied, `[@inline]` (5) | 6.56 G |
| YUV by tables, shifts (9) | 6.40 G |
| `Float.round` replaced (4) | 6.26 G |
| `Yuv.clamp` inlined, floats unboxed (6) | 5.88 G |

(Before the first line, in wall time: 65 ms a frame; after it, 24;
at the end, 13, where 25 frames a second need 40. Then the drawing,
section 8.)

## Not done, deliberately

- `-unsafe` or `Bytes.unsafe_get`: bounds checks are cheap next to the
  above, and an out-of-bounds read in a decoder of files from anywhere
  should stay an exception.
- flambda, `-O3`: a switch of the compiler, not of the code; the code
  should be fast on the default one.
- A fast DCT for `Polyphase` (Byeong Gi Lee's): the matrixing's formula
  stops being readable; the symmetries above keep it.
