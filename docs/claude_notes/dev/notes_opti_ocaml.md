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
- claude: **Per frame, without a window**: a driver that builds the
  program's model and calls its `update` and `view` in a loop, timing
  each, clicks and keys at given frames, printing only the frames over
  1/30 s. `launcher/codemap/bench/codemap_bench.exe <dir> x,y@frame
  key@frame` does it for the code map (`IDLE=n` more quiet frames
  averaged, `OPTI=off` the simple code, section 18); it found every
  case of section 17.
- claude: **On macOS, no valgrind**: `sample <pid> 5 -file out.txt`
  samples a running program (1 ms), its "Sort by top of stack" the
  hottest functions. Its call tree stops at `caml_c_call` (it cannot
  walk past OCaml 4.14's frames), and `atos` finds no lines (no dSYM),
  so two more tools:
  - **who calls it**: `lldb -p <pid> -b -o "br set -r 'camlCode_anatomy__found_at'"
    -o c -o "bt 12" -o "process detach"` -- lldb does walk OCaml's
    stack (the unwind tables): a breakpoint on the hot function, a
    backtrace, a few times;
  - **which closure is `camlMap_v2__fun_4678`**: `otool -tV -p
    _camlMap_v2__fun_4678 prog.exe`, the functions it calls
    (`outside`, `text`, `wrap`: the labelling loop of `Map_names.names`).
  - a quicker `lldb`: `Printexc.get_callstack` printed once inside the
    expensive function (a `lazy`'s first force), removed after.

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
| `mark_slice`, `sweep_slice` (the major GC) high, the program's own code not | a minor heap too small for the program's short-lived data | 16 |
| `Hashtbl.replace`, `insert_all_buckets`, `List.sort` high while nothing happens | a table or a sort rebuilt every frame from data that did not change | 17 |
| `Hashtbl.fold`, `List.find`, `Array.iteri` over everything, to find one | a scan where an index would do | 17 |
| `String.sub`, `Bytes.sub` in a search | a copy made to compare | 17 |

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
`caml_lessequal` per sample written to a WAV). claude: and, found with
`lldb` (above), the Playground's own: `Color.color_clamp` (typed `int
-> int`, but calling the polymorphic `Basics.clamp`: the type written
on the outside does not reach inside a function of another module),
the native loop's audio `int16` (twice a sample, 88,200 samples a
second, every program), Cairo's alpha (`Shape_render_native`), the code
map's `clip` (4 a unit a frame). `Int.min`, `Int.max`, `Float.min`
compare inline.

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

## 13. One general pixel loop for everything: memdraw in OCaml

**Symptom**: ix's mini-9pi (kernel/9pi, PIXEL=ocaml: Plan 9's memdraw
ported to OCaml) boots to rc's prompt under mini-qemu in 323 s, the C
memdraw it replaces in 17 s: the console's drawing.

**Why**: the port's one general loop (lib_graphics/ocaml/Memdraw.ml's
`general`) does every drawing pixel by pixel: the source, the mask and
the destination read into 8-bit channels (a list of channels walked, a
tuple a pixel), composed, written back. Right for every chan and op,
and the reference; but a background filled, a window copied to its
screen, a character drawn are most of the pixels, and memdraw's C
never does those the general way either (its memoptdraw, chardraw).

**Fix**: a separate, switchable section (`Memdraw.fast`, the general
loop's pixels, sooner): a 1x1 colour through an opaque mask, its bytes
made once and blitted row after row; an image copied to one of its
chan, `String.blit` a row; a colour through a 1-bit mask (a
character), the colour's bytes where the bit is set. The general loop
stays, and still does the rest.

**Where**: ix's kernel/9pi/lib_graphics/ocaml/Memdraw.ml, 2026-09-27:
323 s to the prompt, then 17.6 (the C: 17.2); rio's 11 screens the C
9pi's with either path.

## 14. No divide instruction: division is a function call

mini-9pi's OCaml pixels after section 13 were still 6 times slower
than the C ones: 61 s for `ls -l /bin` on the drawn console, the C
10 s. A sampling profiler in mini-qemu (every 1024th instruction's PC,
mapped to the kernel ELF's symbols) gave:

| function | share |
|---|---|
| `__aeabi_idivmod` | 38% |
| `memmove` | 18% |
| the major GC (`mark_slice`, `sweep_slice`) | 14% |

The Pi1's ARMv6 has no divide instruction: every `/` and `mod` is a
call to libgcc's division loop, tens of instructions. `Memimage.byteaddr`
divided by 8 (`fdiv (x * depth) 8`); it ran per row of every draw
and per pixel of every character. The row patterns were built a byte
at a time with `i mod n`, and `fill` did two `mod`s a byte.

The fixes, each with its `old:` code kept in a comment:

1. **Shifts, not divisions.** Every divisor was a power of 2 (8 bits a
   byte, 32 a word), so `a asr 3` replaces `fdiv a 8`. `asr` floors, as
   `fdiv` did, negative numbers included. `units` takes a shift, not
   a divisor.
2. **Hoist the address out of the pixel loop.** The character path now
   computes each row's mask and destination addresses once. A pixel
   is then an offset: `mrow + (lx asr 3)`, `drow + i * n`.
3. **Build a row by doubling.** `Memimage.repeat pat len` copies the
   pattern, then the bytes already there after themselves, so it
   takes log2 of the repetitions blits.
4. **No copy to flush.** `Phys.write_sub pa s off n` writes a row from
   where it is. Before, `String.sub` made a 1280-byte row per row,
   which went to the major heap, then was marked and swept.
5. **memmove a word at a time whenever both ends share an
   alignment.** Before, it copied words only if both ends and the
   length were all aligned. Rows of 16-bit pixels at odd pixels
   went a byte at a time.
6. **`max` and `min` on ints.** The Stdlib's are polymorphic: each is a
   call to `compare_val`.

The result: 61 s, then 45 (1), 32 (2), 28.6 (3, 4, 6); the C takes
14 s under the profiler. Lesson: on a CPU without a divider, a
division in a per-pixel or per-row helper costs more than the whole
pixel operation. The profile names it at once (`__aeabi_idivmod` is
not in your code). Look at the target's instruction set before
optimizing the algorithm.

The full case, with the tools behind these numbers (mini-qemu's
`-prof`, `pcprof.py`, `timecmd.py`), is in ix's
`docs/notes_performance.md`.

## 15. On the web: let the browser do it

tinybox's code map in a browser (js_of_ocaml, plan_tinybox_web.md):
zooming ran at 2 frames a second, and the page froze 1.4 s at start.
Measured in headless Chrome through its DevTools protocol: frame times
from `requestAnimationFrame`, freezes from the `longtask` observer,
and the sampling profiler's self and inclusive times by function (the
development build: release-js minifies the names away).

| where the time went | fix | |
|---|---|---|
| a PNG per new bitmap, our encoder compiled to JavaScript (`filter_row`, `compress`, base64): 1 s for 1738 by 838 | the browser's: the bytes on a canvas (`putImageData`, the Bigarray *is* a typed array, no copy), `toDataURL` | 28 ms |
| 10 MB of fetched bytes to a string, `String.init` over a `Uint8Array`: 10 million calls, the garbage collector | `String.fromCharCode` of 32 KB slices, joined, `Js.to_bytestring` | 1.4 s to ~0.1 s |
| a second picture (the magnifying glass) repainted every frame of a zoom | none while the camera moves | half a frame |
| `fill` writing 4 bytes a pixel | its first row, then `Bigarray.Array1.blit` (a typed array's `set`) | a quarter of a frame |

Zooming went from 2 to 30 frames a second. The lesson: compiled to
JavaScript, a loop over every byte is a loop of function calls; what
the browser already does in native code (encoding a PNG, building a
string, copying memory) should be left to it, and on the web the
profile shows at once which of our loops is doing the browser's job.

## 16. The collector's parameters: a minor heap too small

**Symptom**: ix's mini-9pi (kernel/9pi, a Plan 9 kernel in OCaml,
ocaml-light's runtime) boots to rc's prompt under mini-qemu in 13.5 s,
30 to 50% of it in the major collector (`mark_slice`, `sweep_slice`,
by mini-qemu's `-status`), none of it in one function of the kernel's.

**Why**: the runtime's defaults (ocaml-light's config.h, 1997's
machines): a minor heap of 32k words (128 KB on a 32-bit Pi1), a 42%
space overhead, a heap grown by 62k words. A boot allocates ~13 MB of
mostly short-lived data: the small minor heap fills 100 times, each
time promoting what is still alive only because it is recent, and each
promotion is major work: 43 major cycles, each marking and sweeping the
whole (small) heap. Counted with the runtime's own trace
(CAMLRUNPARAM's v, which a kernel had to be given: its getenv).

**Fix**: no code, a parameter. Each tried alone, then together (a
script, kernel/9pi/tests/perf/gc_boot.sh, the median of 3 boots):

| CAMLRUNPARAM | boot | minor | major |
|---|---|---|---|
| (the defaults) | 13.5 s | 100 | 43 |
| `s=256k` (the minor heap, 1 MB) | 9.4 s | 12 | 6 |
| `o=200` (the space overhead) | 10.2 s | 100 | 48 |
| `h=1M,i=1M` (the heap, its increment) | 12.4 s | 102 | 55 |
| all four | 9.1 s | 13 | 7 |
| `s=1M,o=200,h=4M,i=1M` | 8.6 s | 4 | 2 |

The minor heap is nearly all of it: most of a boot's data dies young,
and a nursery big enough lets it die there. The rest is noise
against a floor near 8 s, the boot's own work. On the 64-bit Pi4,
whose default minor heap is already twice as big in bytes, `s=256k`
took the boot from 9.7 s to 8.1 (61 minor collections to 7, 42 major
cycles to 4). The cost is memory: 1 MB of a kernel's 91 (2 on the
Pi4). It is the kernels' default now, a switch (`make CAMLRUNPARAM=`
for the runtime's own).

**Where**: ix's kernel/lib (libc.c's getenv, kernel.mk's CAMLRUNPARAM),
2026-09-28. The general lesson, for any OCaml program that allocates
much and keeps little (a compiler's pass, a parser, a kernel's boot):
try the minor heap's size before any code change
(OCAMLRUNPARAM=s=256k, or Gc.set at start), and count the collections
(OCAMLRUNPARAM=v=0x400 in today's OCaml prints them at exit) before
profiling code that is not the cost.

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

## 17. A frame's work: what does not change, done once

claude: tinybox's code map on principia (2,213 C files), measured by the
per-frame driver (above): a click froze it 7 s, the X-ray half a
minute, and with nothing happening it used 60% of a CPU. The web page
of the same map was fast: its bundle brings the uses counted by
`make_codemap_data`. Six cases, all the same mistake, work redone that
could be kept:

- **A count redone for each new map** (`Code_rank.compute`, 9 s): a
  click on a folder makes a map, and each counted the uses of every
  definition again. Counted once for a set of sources (a `lazy` in a
  list keyed by `==` on the sources, `Codemap.rank_of_sources`), as the
  web's bundle gives them once. 7 s a click to 20 ms.
- **A scan for every include of every file**
  (`Code_names.resolve_include`): the header named by `#include
  "dat.h"` was searched among all 2,200 files, for each of the ~20
  includes of each file. Only the files of that base name can be it:
  an index by base name, built in the table's own order so that ties
  fall as before, and each include resolved once per directory, what
  its answer depends on. The count 9.4 s to 1.1 s, its result the same
  byte for byte (the bundle compared).
- **Per frame, what does not depend on the camera** (`Map_names.capitals`,
  `unit_ties`): the capitals chosen among the configs' (a sort, a
  table of the 2,200 units) and a hovered unit's ties (every link gone
  through) were computed every frame; kept, keyed by `==` on the
  layout, the configs and the unit looked at. The camera-dependent part
  (positions) stays per frame.
- **A scan to find one** (`entry_of`, `spot`, `unit_spot`): for each of
  a skeleton's bones, every frame, the list of all entries copied
  (`@`) and searched, the array of units gone through. A table by path,
  one per layout.
- **A copy to compare** (`Code_anatomy.found_at`): a line searched for
  15 words by `String.sub s i n = word` at every character: a string
  allocated per character per word. Compared in place, and only where
  a word can start (after no letter of a name: the check that was last,
  done first). 15 ms a file to ~1.
- **A budget in items, not time** (`Map_anatomy.facts_of`): 30 files' facts
  a frame, when one costs from nothing to milliseconds: frames of half
  a second. 8 ms a frame, as the background lexing. And the hover no
  longer starts work it cannot finish in a frame (the uses counted
  after the background reading, `rank_if_counted`).

Result: a click 7 s to 20 ms, the X-ray's frames 480 ms to 5 ms, the
map at rest 63% of a CPU to 44% (the rest is Cairo drawing 2,000 labels
60 times a second: a map that redraws only when something changes
would be the next step, the Playground's, not the map's).

The memos' keys are `==`: the layout, the entries, the configs are
values made once and replaced, not mutated, so a new one is a new key.
A memo is only right if nothing it leaves out changes the answer:
`OPTI=off` (section 18) runs the old code, to compare.

## 18. Keeping the simple code: the `Opti` switch

claude: the rule for the fixes (top of this note) is the old code in a
comment. When the fast path is really more complex than the old one
(an index and a memo instead of a fold, a cache around a function), the
old one is better kept *runnable*: in its own function, the fast one in
another, and a dispatch on `Opti.enabled` (`libs/graphics/core/Opti.mli`,
which lists them all; the optimized path by default):

```ocaml
let resolve_include_simple ix from inc = nearest_include from inc (every C file)
let resolve_include_opti ix from inc = (* memo, by base name *) ...
let resolve_include ix from inc =
  if !Opti.enabled then resolve_include_opti ix from inc else resolve_include_simple ix from inc
```

The reader reads the simple one to understand, the `_opti` one to learn
the trick; the switch (`o` in the software platforms, `opti=off` for the
code map, `OPTI=off` for its driver) shows what each buys, and checks
the two agree. Not for everything: a one-line change (`Int.max` for
`max`, a test moved first) keeps the old line in its `opti:` comment,
no switch. The comments of either kind say `claude: opti:` (`grep -rn
"opti:"`).

## 19. An object a call: the Smalltalk interpreter's contexts

Smalltalk's contexts are objects (`St_interp.mli`): every send made a
MethodContext, `Array.make` then an entry in the object table. The
table is old, so OCaml's collector promoted each array to its major
heap (`caml_modify`, `do_some_marking`, `caml_oldify_one` high in the
profile), and our own collector swept them later. Measured with
`St_bench.exe` (`languages/smalltalk/tests/bench/`: a recursive
`benchFib`, a loop of `inject:into:`, a sieve, a Dictionary filled, 300
factorial printed, the Pen's dragon), millions of bytecodes a second,
natively then under node, the Blue Book's kernel / Squeak's (closures):

| change | sends | blocks | arrays | dictionary | pen |
|---|---|---|---|---|---|
| first version, native | 11.2 / 11.9 | 16.7 / 12.4 | 25.0 / 16.4 | 13.2 / 13.5 | 11.1 / 11.7 |
| contexts recycled | 17.6 / 17.4 | 17.2 / 14.8 | 26.3 / 23.8 | 18.3 / 17.6 | 13.9 / 13.9 |
| first version, node | 3.7 / 3.7 | 4.6 / 3.5 | | 3.8 / 4.1 | 3.0 / 3.3 |
| contexts recycled, the header read once, node | 5.0 / 4.9 | 4.5 / 4.0 | 6.5 / 5.9 | 4.9 / 5.1 | 3.7 / 4.0 |

- **Contexts recycled** (Deutsch and Schiffman's observation, 1984:
  most contexts are never looked at): a context that returns goes into
  a pool, by size, unless it *escaped* -- a bit of its entry in the
  table, set when the program gets hold of it (thisContext, a block
  made, its sender read, the debugger). The next send takes its
  context from the pool. A send is 1.5 times faster; the loop of
  blocks gains little, each of its methods making a block, whose home
  escapes.
- **The header read once**: a method's header was decoded twice a
  send, into a record each time; now its fields are taken off the
  SmallInteger. Within the noise natively.

The lesson: what is garbage for the interpreted language is garbage
for OCaml too, and worse, since the object table makes every object
old. Reuse beats allocating twice.

Callgrind on the loop of blocks, after: `step` itself 20%, then
`St_memory.body` and `fields` (the table read at each variable), the
primitives called through a closure (`caml_apply2`), `caml_modify`.

Then, the wall clock of a shared machine being too noisy for changes
of a few percent, in instructions (callgrind, the boot subtracted,
millions; both kernels together):

| change | sends | blocks | arrays | pen | dictionary |
|---|---|---|---|---|---|
| contexts recycled, the header read once | 4342 | 16024 | 5190 | 5063 | 3663 |
| `at:`, `at:put:` by their bytecodes, and the receiver's fields in a register | 4580 | 15905 | 4497 | 5179 | 3839 |
| the same without the register | 4350 | 15307 | 4378 | 5028 | 3671 |

- **`at:` and `at:put:` by their bytecodes**, for an Array (and `at:`
  for a String), as the arithmetic selectors are: no lookup, no
  primitive called through its closure. The sieve 16% fewer
  instructions, the loop of blocks (its `do:`) 4%.
- **Tried, not kept: the receiver's fields in a register**, read when a
  context becomes active instead of at each instance variable. More
  instructions everywhere, 5% on the sends: a context becomes active
  twice a send, and reads far fewer instance variables than that. The
  profile had `St_memory.fields` high, but most of its calls were not
  the receiver's. Measure the change, not the hunch.

**BitBlt a byte at a time** (`St_bitblt.mli`): the rule applied to
eight pixels by one and, or and not, the source's bits shifted to line
up with the destination's bytes, a mask at each end of a row. A 640 by
400 rectangle xor-ed, millions of pixels a second: 39 a pixel at a
time, 288 a byte at a time natively; 11 and 68 under node. The pixel
version stays, as the definition the other is tested against.
A fill (no source, no halftone, rule 0 or 15) writes the middle of a
row with one `Bytes.fill`: MiniMorphic's cycle with 10 atoms, mostly
its white background over the whole screen, went from 7.5 to 5.1 ms
under node.

And the interpreted program itself, which no interpreter's speedup
replaces: MiniMorphic's cycle went from 206,000 to 61,000 bytecodes by
remembering damaged rectangles without comparing them
(`notes_squeak.md`, section 2).

## Not done, deliberately

- `-unsafe` or `Bytes.unsafe_get`: bounds checks are cheap next to the
  above, and an out-of-bounds read in a decoder of files from anywhere
  should stay an exception.
- flambda, `-O3`: a switch of the compiler, not of the code; the code
  should be fast on the default one.
- A fast DCT for `Polyphase` (Byeong Gi Lee's): the matrixing's formula
  stops being readable; the symmetries above keep it.
