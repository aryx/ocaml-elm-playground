# Plan: a web browser, for teaching (TinyMosaic first)

## Context

`apps/internet/`'s dune file has imagined "a web browser (WorldWideWeb
1990, Mosaic 1993)" since TinyIRC, and `plan_networking_remaining.md`
section 4 lists it. The groundwork is already in the house:

- `networking/`'s `Url` (RFC 3986, relative references resolved),
  `Http` (a response parsed, its body's four framings), `Http_client`
  (blocking, redirections) and `Http_request` (the same GET as a
  non-blocking state machine stepped each frame), and the Playground's
  `Http.get`, a `Cmd.Http_get` performed natively by `Commands` and on
  the web by an XMLHttpRequest;
- `graphics/images/`: GIF, PNG, JPEG and XPM read by our own code, and
  `Playground.bitmap`, pixels from memory drawn as a shape;
- the typesetting appkits: `typeset/Linebreak` (greedy and
  Knuth-Plass), `richtext/`'s `Style` (a look), `Page` (a text laid out,
  and the way back from a point to a character);
- `apps/office/`'s `Stroke_text`: Hershey's strokes in a look (bold,
  italic, underline), with real widths.

What is missing is the browser itself: **HTML read** (tag soup
included), **a tree**, **looks for its elements**, **layout** of blocks
and lines with images in them, **painting**, and **clicking a link**,
round a **history**. That is this plan, and the two companion
documents written ahead of the code as its specification:

- [`notes_browser.md`](../tutorials/notes_browser.md) -- how a browser
  works, section by section, with the worked examples the tests will
  check;
- [`notes_browser_related_work.md`](../related-work/notes_browser_related_work.md)
  -- the browsers since 1990, the two in the author's other repositories
  (Mothra, MMM), and the teaching lineage (*Web Browser Engineering*
  above all).

## Which original: Mosaic, browsing only

The house rule is a Tiny program after a famous original (TinyIRC after
ircII, TinyMinimoog after the Model D). Three originals were candidates
(the author named Mosaic, then "or WWW itself"):

| | year | what it is | what it would teach |
|---|---|---|---|
| **WorldWideWeb** (Berners-Lee, NeXT) | 1990 | the first browser, and an **editor**: pages edited in place, WYSIWYG, links made by selecting | the web as read-write -- but editing is not browsing, and the author wants browsing only |
| **Line Mode Browser** "www" (Nicola Pellow, CERN) | 1991 | a terminal program: the page as text, links numbered `[3]`, followed by typing `3` | fetch and parse with no layout at all |
| **NCSA Mosaic** (Andreessen and Bina) | 1993 | the browser that made the web popular: **images inline** in the text (its own `<img>`, proposed on www-talk in February 1993), fill-out forms (2.0), one click to follow a link, the grey page, the spinning globe | the whole pipeline a browser still has: parse, style, layout, paint, hit-test |

**TinyMosaic**, as the author proposed. Every
stage of a modern rendering engine is in it, in its smallest form, and
its one invention -- a picture *in* the text, a box among the words
that the lines flow round -- is the moment layout becomes more than
word wrap. It also uses what the house has best: our own GIF and JPEG
decoders, where Mosaic called libraries.

**No editor** (the author's call, 2026-09-24): WorldWideWeb's editing
half stays in the related-work notes, as history.

**The Line Mode Browser is a stage, not a program**: phase 2's debug
view draws the parsed page as "www" did, text and numbered links, in
TinyIRC's terminal look, before any layout exists. (A `TinyLineMode` of
its own is an exercise, fifty lines on the engine.)

The name is settled: `TinyMosaic` (the author's first word was
"TinyBrowser"; the house naming says `Tiny<Original>`).

## Prior art in the house: Mothra and MMM

Two browsers are already in the author's other repositories, each with
a literate book, and they are the two ends of the design space this
plan must choose in:

- **Mothra** (Tom Duff, Plan 9, 1995; `principia-softwarica/browsers/`,
  book `Browser.nw`): 4,850 lines of C, of which `rdhtml.c` (1,350) is
  the whole of HTML. **No tree.** A start tag pushes a *style state*
  (font, indent, link, pre...) on a stack, an end tag pops back to its
  match, and each word goes out as an `Rtext` run -- word, font,
  space, indent, and the link it belongs to -- in one flat list, which
  libpanel's text view wraps into lines and draws. Tables are not laid
  out (`<tr>` is a line break). Everything else is another program:
  webfs fetches, `uhtml`/`tcs` decode charsets and entities, `page`
  and external converters decode images, five at a time.
- **MMM** (François Rouaix, INRIA, 1995-96; `~/github/mmm`, book
  `mmm.nw`): 30,000 lines of OCaml over Tk. **Tokens, not a tree,
  either**: `Lexhtml` (ocamllex) gives tokens, `Html_eval` repairs tag
  soup from **DTDs written as OCaml data** (HTML 2.0, 3.2, 3.2 with
  frames: which tags may omit their start or end), and a *display
  machine* (`Html_disp`) dispatches each token to a per-tag handler
  that drives a stack of *formatters*, the main one filling a Tk text
  widget, with images, forms and tables as embedded widgets. Its fame
  is elsewhere: **Caml applets**, bytecode loaded by `Dynlink` and
  sandboxed by giving it only `Safe*` versions of the standard library
  -- capabilities, in 1996.

Both stream: the page is drawn as it arrives, without ever holding the
document whole. The modern engines do the opposite -- a tree (the
DOM), then styles computed on it, then a *layout tree* of boxes, then
a display list -- because CSS and scripts need the whole tree. This
plan takes the modern shape (see "Groundwork decisions"), and keeps
Mothra's and MMM's ideas where they are the simpler ones: MMM's DTD as
data for tag soup, Mothra's run list as what a paragraph becomes
before it is broken into lines.

## Principles (the house's)

- **The engine in `libs/`, the browser in `apps/internet/`.** Parsing
  bytes into a tree and laying boxes out in rectangles knows no
  `shape` and no `computer` (the `libs/` rule); drawing the boxes as
  shapes, the chrome (buttons, URL field, globe) and the history are
  the application's.
- **One stage, one module, one idea**, each `.mli` opening with the
  idea, a diagram, a worked example the tests check, and its reference
  (the WHATWG parsing chapter, CSS 2.1's visual formatting model, the
  RFCs).
- **The simple version next to the better one, switchable**: greedy
  line breaking and Knuth-Plass (`wrap=pretty`, CSS's own name for it
  since 2023); Mosaic's fixed table of looks and a CSS subset
  (`style=css`); images drawn when they arrive, reflowing the page, and
  images reserved by their `width`/`height` (the reason those
  attributes exist).
- **Seeing each stage**: debug keys show the source (Mosaic had "View
  Source"), the tokens, the tree, the line-mode rendering, and the
  boxes outlined over the page (what every browser's inspector does).
- **Deterministic, so testable**: pages from a built-in site (no
  network in `make test`), fixed metrics in the unit tests (ten units a
  character, as `Page`'s tests do), golden frames of TinyMosaic on its
  home page, after a scripted click, and scrolled.

## The pipeline

```
 URL typed / link clicked
   |  Url.resolve against the page's base
   v
 fetch ---------------- about:  (built-in pages, embedded by dune)
   |                    file:   (natively)
   |                    http:   (Http_request, stepped each frame; on the
   |                             web an XMLHttpRequest, same origin only)
   v bytes + Content-Type
 decode ------------- Latin-1 or UTF-8 to UTF-8; entities (&amp; &eacute; &#233;)
   v
 tokenize ----------- Html_lexer: start tags with attributes, end tags,
   |                  text, comments, <!DOCTYPE>
   v
 tree construction -- Html_tree: tag soup repaired (implied end tags from
   |                  a DTD table, stray end tags ignored, <html><head><body>
   |                  implied) -> the DOM: elements and text nodes
   v
 style -------------- Looks: each element's look (Mosaic's table: h1 big
   |                  and bold, a blue and underlined, pre fixed-width)
   |                  later Css: the cascade, specificity, inheritance
   v
 layout ------------- Html_layout: block boxes stacked, inline content
   |                  broken into line boxes (Linebreak), images as
   |                  boxes among the words, lists with their markers
   v
 paint -------------- (the app) the boxes as shapes, clipped and scrolled
   v
 hit-test ----------- a click -> the box under it -> the link it is in
                      -> a new URL, and round again; the history remembers
 images: each <img> another fetch, decoded by graphics/images, and the
         layout run again when its size is known
```

## Target layout

```
libs/web/                    (umbrella library `web`, like `ai`; private,
  README.md                   package elm_playground, pure OCaml, no shapes)
  html/                      library web_html
    Entities.ml(i)           &name; and &#n; -> UTF-8 (HTML 2.0's Latin-1 set,
                              then the rest of HTML 4's 252)
    Charset.ml(i)            Latin-1 / UTF-8 / Windows-1252 decided and decoded
    Html_lexer.ml(i)         the tokenizer (a state machine, hand-written:
                              the WHATWG's states, the few we need)
    Dtd.ml(i)                MMM's idea: HTML as data -- which elements are
                              blocks, empty, have an optional end tag, and
                              what closes what
    Dom.ml(i)                the tree: elements, attributes, text
    Html_tree.ml(i)          tokens -> tree, tag soup repaired with Dtd
    Line_mode.ml(i)          the tree as the 1991 Line Mode Browser's text
  style/                     library web_style
    Looks.ml(i)              Mosaic's fixed looks per element, inherited
    Css.ml(i)                (phase 8) CSS1's subset: parse, match, cascade
  layout/                    library web_layout
    Html_layout.ml(i)        the box tree: blocks, lines, images, lists, pre
    Hit.ml(i)                a point -> the box, the link, the text
    Table_layout.ml(i)       (phase 10) the auto table layout
  tests/                     worked examples of each .mli
apps/internet/
  TinyMosaic.ml              the app: chrome, history, fetching, painting
  Browser_site.ml(i)         the about: pages, embedded by dune
  site/*.html, *.gif         the built-in site (home, the history of the web,
                              the test pages of each phase)
  web/TinyMosaic.html, software/ (golden frames)
libs/networking/httpd/tiny_httpd.exe    (phase 6) a server of a directory, and CGI
libs/networking/unix/Http_server.ml(i)  its event loop, over Server's
```

Module names checked against the unwrapped libraries: `Layout`
(gui/), `Style`, `Page` and `Flow` (richtext/) are taken, hence
`Html_layout` and `Looks`; `grep` again before each new one.

## Groundwork decisions

### A tree, not a stream

Mothra and MMM never build the document: tokens go straight to
formatting. It is smaller and draws sooner, and it is a dead end
the moment anything needs the whole document -- a style rule about an
element's ancestors (`ul ul li`), a table's column widths (every row
before the first can be drawn), the text re-laid out when the window
is resized or an image arrives (a stream has to be re-parsed). Mosaic
itself re-parsed on resize. The tree costs one module (`Dom`) and is
what every engine since has; the stream's history goes in the notes.

### Tag soup: a table, then the standard's ideas where they matter

Real HTML omits end tags (`<p>` closed by the next `<p>`, `<li>` by
the next `<li>`, both legal in SGML) and gets them wrong (`<b><i></b>
</i>`). The WHATWG's tree construction (HTML5, 2008; 23 insertion
modes) specifies exactly what every browser does with any
input. We take its **shape** -- the stack of open elements, "generate
implied end tags", the "has an element in scope" test -- and **MMM's
table** for the rules (`Dtd`: which elements close which), for HTML
3.2's elements only. The adoption agency algorithm (misnested
formatting) is an exercise, explained in the notes with its worked
example. Consequence, stated in `Html_tree.mli`: our tree equals
html5lib's on well-formed pages and on the listed soup cases (tested),
not on everything.

### Where the engine goes

`libs/web/`, a new folder of libraries (the author's yes, 2026-09-24,
"for now": the place may still change). Painting
stays in the app because it makes `shape`s; if another program (a
help viewer) needs it too, it becomes an appkit then.

### Text: Stroke_text, fixed-width, and its move

The looks need proportional text in bold and italic -- `apps/office/`'s
`Stroke_text` over Hershey -- and a fixed-width face for `<pre>`,
`<tt>`, `<code>` (Hershey's futural, set on a grid of equal cells, as
TinyIRC draws). `Stroke_text` is an office module; CLAUDE.md's rule
("an app of a new category needing a shared office module is the
moment to turn that module into an appkit") says it moves to
`appkits/` -- agreed by the author (2026-09-24), done in phase 0,
TinyBravo and TinyWord then depending on the appkit.

`Style.t` (richtext) has bold, italic, underline, strike, size; a
browser also needs colour, fixed-width, and "a link". `Looks` carries
its own record, and converts to a `Style.t` for measuring and drawing.

### Bytes, not text, over `Cmd.Http_get`

Images are binary. Natively `Http_request` returns bytes already;
on the web `fetch_text` reads `responseText`, which mangles them.
The web backend's audio fetcher already reads an `arraybuffer`; a
`Cmd.Http_get_bytes` (Elm's `Http.expectBytes`), or the one command
always reading bytes, is phase 0. The response's `Content-Type` is
needed too (a GIF from a `.cgi` URL), so the command's answer becomes
the headers and the body.

### What can be browsed

- **The built-in site** (`about:home`, `about:history`, the phase test
  pages), embedded by dune from `apps/internet/site/`: works natively,
  on the web, and in `make test`.
- **`http://`**, natively, through `Http_request`: our own
  `tiny_httpd` (phase 6), and the few real sites still serving plain
  HTTP -- CERN's restored first website, `http://info.cern.ch/`, is
  the obvious first real page (checked in phase 0: it answers without
  a redirection to HTTPS).
- **`https://`, natively, through curl for now** (the author's yes,
  2026-09-24), as `Download` does for images, until TLS is ours
  (`plan_teaching_other.md` 4b, `plan_dependencies_remaining.md`). The
  status line says the page came through a borrowed library, so the
  lesson stays honest; curl there is blocking, a freeze while it
  loads, which the notes point out as what `Http_request` avoids. On
  the web, the outer browser does TLS itself.
- **On the web**, the page is inside a browser, which lets it fetch only
  from its own site (CORS): the built-in site, and the repository's own
  pages beside it. A browser in a browser, sandboxed by the one outside
  -- a paragraph in the notes.

### Coordinates

`Html_layout` works as `Page` does: x right, y *down*, from the page's
top-left; the app turns it over and scrolls (the Playground's origin
is the centre, y up).

## The modules, with their references

- `Entities`, `Charset`: HTML 2.0 (RFC 1866, 1995) section 9's
  Latin-1 entities; HTML 4.01's list; the WHATWG's "decode" (Windows-1252
  for a page labelled Latin-1).
- `Html_lexer`: WHATWG HTML, 13.2.5 "Tokenization" -- the data, tag
  open, tag name, attribute name/value (quoted, unquoted), comment and
  DOCTYPE states; RAWTEXT for `<script>` and `<style>` (skipped, as
  Mothra does).
- `Dtd`, `Html_tree`: HTML 3.2's DTD (W3C, 1997) for the data; WHATWG
  13.2.6 "Tree construction" for the algorithm's shape; MMM's
  `dtd.ml`/`html_eval.ml` as the precedent.
- `Looks`: Mosaic's resources (its X defaults file: the fonts of each
  heading level, the link colours); CSS 2.1's "default style sheet for
  HTML 4" (appendix D) for the numbers, since it wrote Mosaic's habits
  down.
- `Html_layout`: CSS 2.1 chapters 9 and 10 (the visual formatting
  model: block formatting contexts, line boxes, `vertical-align:
  baseline`), simplified; *Web Browser Engineering* chapters 3 to 7 for
  the order of exposition.
- `Hit`: *Web Browser Engineering* chapter 7.
- `Css`: CSS1 (W3C, 1996), selectors of type, class, id and
  descendant; specificity as a triple.
- `Table_layout`: CSS 2.1 17.5.2.2, "automatic table layout"
  (non-normative, and what browsers did): each column's minimum and
  maximum content width, then the space shared out.

## TinyMosaic, on screen

X Mosaic 2's window, simplified:

```
 +------------------------------------------------------------------+
 | File  Navigate  Options                              (globe)     |
 | Title: The World Wide Web                                         |
 | URL:   about:home____________________________________________    |
 +------------------------------------------------------------------+
 |                                                                  |
 |  The World Wide Web                         <- h1, big, bold     |
 |                                                                  |
 |  The WorldWideWeb (W3) is a wide-area [hypermedia]  <- blue,     |
 |  information retrieval initiative ...    +------+     underlined |
 |                                          | GIF  |  <- an image   |
 |  * What's out there?                     |      |     among the  |
 |  * Help                                  +------+     words      |
 |                                                            |#|   |
 +------------------------------------------------------------------+
 | http://info.cern.ch/hypertext/WWW/Help.html   <- the link hovered|
 | [Back] [Forward] [Home] [Reload] [Open...] [Source]              |
 +------------------------------------------------------------------+
```

The globe turns while something is loading (Mosaic's; clicking it
stops the load, as Mosaic's did). The page is grey, text black, links
blue and underlined, visited links purple -- 1993's defaults, with the
`<body bgcolor>` of Netscape 1.1 (1995) honoured from phase 7. Keys:
arrows and Page Up/Down scroll, Backspace or `b` goes back, `u` edits
the URL, and a key per stage's view (plain keys, as "View Source"
was a menu item): `s` source, `t` tokens, `d` the tree, `l` the
line-mode view, `o` the boxes outlined.

## Phasing

0. **Groundwork.** `libs/web/` and its dune files; `Stroke_text`
   moved to an appkit (`appkits/`, its users updated). `Cmd` answers
   with bytes and headers, both backends. `Entities` and `Charset`
   with their tests. The built-in site's first page embedded.
   `TinyMosaic` shows `about:home`'s source as plain text, scrolled.
1. **The tokenizer** (`Html_lexer`), and the `t` view. Tests: the
   worked example of the notes, attributes quoted three ways, a `<`
   that is not a tag, comments, entities in attribute values.
2. **The tree** (`Dom`, `Dtd`, `Html_tree`), and the `d` view, and
   the **line-mode view** `l` (text, `[n]` after each link, as the
   1991 "www"): the first time a link can be followed, by typing its
   number. Tests: the notes' soup cases (`<p>` in `<p>`, `<li>`
   without `</li>`, stray `</b>`, missing `<html>`).
3. **Looks and blocks**: `Looks`; `Html_layout` for blocks only (each
   paragraph one line, clipped), headings, `<hr>`, margins. Painted.
4. **Lines**: inline content broken into lines (`Linebreak.greedy`),
   mixed looks on a line sharing a baseline, `<br>`, `<pre>` set on
   the fixed grid, lists with bullets and numbers, `<blockquote>`,
   `<dl>`. `wrap=pretty` switches to `Linebreak.optimal`. Resizing the
   window re-lays the tree out. Golden frame of `about:home`.
5. **Links and history**: `Hit`; hover shows the URL in the status
   line; a click follows; Back, Forward, Home, Reload; `#fragment`
   scrolls to `<a name>`/`id`; the page's scroll position remembered
   in the history (the notes: two stacks). The `o` view. Golden frame
   after a scripted click.
6. **Fetching for real**: `http://` through the stepped
   `Http_request` (the globe turning, the status line saying what is
   awaited), redirections, errors as pages. `tiny_httpd` in
   `networking/httpd/`, serving `apps/internet/site/`, so TinyMosaic
   browses a real server with no Internet. `info.cern.ch` tried.
7. **Images** (Mosaic's own): `<img>` as an inline box, fetched in
   parallel, decoded by `Image_decode`, `alt` text until it arrives,
   the page re-laid out when it does (or not, when `width` and
   `height` were given), `align=left/right` (Netscape's floats, the
   simplest float). `<body bgcolor text link>`. XBM, the first inline
   format Mosaic read, as a small reader in `graphics/images/xbm/`
   (optional: it is thirty lines, and a lesson in "a C file as an
   image").
8. **CSS, the subset** (`style=css`): `<style>` and `style=`, CSS1's
   selectors, cascade and inheritance, the properties `Looks` already
   has (colour, font weight and style, size, margins, text-align,
   display: none). Mosaic's table becomes the user-agent style sheet,
   which is what it always was.
9. **Forms** (Mosaic 2.0): text fields, checkboxes, radio buttons,
   select, submit -- `gui/`'s widgets inside the page's boxes;
   submitted as a GET with `application/x-www-form-urlencoded`, and
   POST; `tiny_httpd`'s CGI answering (NCSA httpd's invention, 1993,
   from the same lab as Mosaic).
10. **Tables** (Netscape 1.1, HTML 3.2): `Table_layout`, the auto
    algorithm; borders, `colspan`. Tested on the notes' worked example.
11. **Docs**: `notes_browser.md` checked against the code, its numbers
    the tests'; `CATALOG.md` row; `apps/internet/dune`'s comment.

Phases 1-5 are the core (a browser of the built-in site, natively and
on the web); 6-7 make it Mosaic; 8-10 each stand alone.

## Status

**Phase 0 done** (2026-09-24): `Stroke_text` is the appkit
`appkits/stroke_text`; `Cmd.Http_get` answers with a
`Cmd.http_response` (the final URL, the status, the headers, the
body's bytes) on both backends -- one command, always bytes, read as
text by `Playground.Http.expect_string` and whole by the new
`expect_response`; `libs/web/html/` has `Charset` and `Entities`, with
their tests (`libs/web/tests/`); `TinyMosaic` shows a page's source,
`about:home` embedded from `apps/internet/site/`, or `http://` (tried
on `info.cern.ch`: it still answers over plain HTTP, 646 bytes, no
redirection), with its golden frame, web page and catalogue row.

**Phase 1 done** (2026-09-24): `Html_lexer`, the tokenizer, each state
a function and each transition a tail call (a dozen states: tags,
attributes quoted three ways or not at all, comments and bogus ones,
doctypes, RAWTEXT for `script`/`style`, RCDATA for `title`/`textarea`,
a tag cut off by the end dropped), with the notes' worked example and
the `.mli`'s cases as tests. TinyMosaic's views are plain keys, not
`-debug-keys` (a browser's "View Source" is a feature): `s` the source,
`t` the tokens (flag `view=tokens`, its golden frame
`TinyMosaic_tokens.png`); the title field is filled from the tokens.
Seen then: the software backend's font is Hershey's ASCII, so a
decoded "é" is drawn as "?" there (Cairo draws it); phase 3's
`Stroke_text` has the same limit, to be lifted with the font.

**Phase 2 done** (2026-09-25): `Dom` (elements and text, a value),
`Dtd` (void, head, block, `closes`, `stops`: HTML 3.2's rules as
tables), `Html_tree` (the stack of open elements; html, head and body
made at once; `</p>` alone an empty p; a newline after `<pre>`
dropped; misnesting ours, the adoption agency the exercise),
`Line_mode` (the tree as text at 80 columns, links numbered), each with
its tests (14 more, 37 in `libs/web/tests/`). TinyMosaic: `d` the
tree, `l` line mode, a link followed by typing its number and Return
(resolved by `Url.resolve`), `h` home; the title now from the tree;
a second built-in page, `about:history`; golden frames `_tree`,
`_line` and `_follow` (the script `type(2):10,return:12`). Found on the
way: `Sub.on_key_down` gives SDL's lowercased names natively
("pagedown", "return") and the DOM's in a browser, so TinyMosaic
lowercases before matching (phase 0's Page Up/Down worked only on the
web). Next: phase 3, looks and blocks -- the first page drawn.

**Phase 3 done** (2026-09-25): `libs/web/style/` (`web_style`) with
`Looks` (the look inherited down the tree, the box not; CSS 2.1
appendix D's numbers; HTML 3.2's `align=`; a box's `indent` and
`right`, since the body's 8 and a blockquote's 40 are on both sides),
`libs/web/layout/` (`web_layout`) with `Html_layout` (blocks stacked,
siblings' margins collapsed, anonymous blocks around runs of inline
content, words on lines sharing a baseline with CSS's half-leading, a
line broken only at `<br>` and `<pre>`'s newlines), 8 tests (45 in
all), the worked example to the tenth. The notes' §6-§7 numbers were
redone to the code's (body margins, leading 1.2). TinyMosaic: `p`, the
page, the default view -- laid out with Hershey's widths (a fixed-width
look on cells of 0.6 em), drawn by `Stroke_text`, each line's glyphs
made once per page and moved by the scroll; `hr` an inset rule; the
chrome drawn over what overflows. Golden frames: `TinyMosaic.png` the
page, `_source` the old default (pixel-identical). Seen: a link's
underline stops at the spaces between its words (Mosaic's ran on);
the first paragraph runs off the window, as planned. Next: phase 4,
lines broken at the width, lists' markers. Committed with phases 0-2
as 07c69f6.

**Phase 4 done** (2026-09-25): lines broken at the width. What is
broken is a *unit* (words stuck together: a link and its full stop);
`Html_layout` takes a `breaker` (`greedy`, its own, each unit with its
own space's width, the default), since `libs/` must not depend on
`appkits/typeset` -- TinyMosaic passes `Linebreak.optimal` for
`wrap=pretty` (key `w`), ragged right: a stretch, no shrink. List items
get a `marker` (a bullet, or their number in an `<ol>`), drawn in the
list's indent at the item's `first_baseline`. The reflow: `[` and `]`
(flag `width=`) narrow and widen the page, the tree laid out again --
the playground's screen is 1000 wide whatever the window, so a real
window's resize is not what drives it. 5 more tests (49 in all). The
history page got the first web page's URL in its text, the long word
before which greedy leaves a loose line; golden frames `_narrow` and
`_pretty` (the history page at 600, greedy and pretty) show the
difference. Next: phase 5, links clicked and the history. Committed
as 5228bc3.

**Phase 5 done** (2026-09-25): `Hit` (`libs/web/layout/`): `link_at`,
the line holding a point, the fragment on it, its look's link (a space
between two words of one link counts); `anchor`, a name to its y. The
layout now keeps anchors -- `<a name>` and inline `id=` as items of no
width on their line (`line.anchors`), a block's `id` on the block, an
anchor alone as a line of no height -- 4 tests (53 in all).
TinyMosaic: the pointer over a link shows its resolved URL in the
status line, a click visits it; the history as two stacks of entries
(the page kept whole, and its scroll), Back and Forward (buttons, `b`
or Backspace, `f`) giving a page back as it was, a visit emptying
what was ahead; a `#fragment` scrolls, at once on the page shown, once
loaded on another; visited links purple (drawn again on Back, their
colour changed since); the chrome's buttons clickable, lit when they
do something; `o` outlines the layout's boxes and lines. Golden
frames (scripted, the pointer at the home page's "short"): `_hover`,
`_click`, `_back`, `_outline`. Next: phase 6, http:// for real (the
globe turning, redirections, errors as pages, curl for https://) and
`tiny_httpd`. Committed as 5e14700.

**Phase 6 done** (2026-09-25): the server's side of `Http`
(`parse_request`, whole or not yet, `reason`, `response`,
`response_to_string`); `Http_server` (`networking/unix/`), a web
server's event loop, a connection per request, garbage answered 400;
`networking/httpd/tiny_httpd.exe` over it: a directory's files, `../`
refused (403), "Index of" listings, NCSA's log format. Tests: the
server and three `Http_request`s at once in one process (a page, a
redirection followed, a 404), and garbage. `https://` natively by curl
in `native_common/Commands` (blocking, the one request that is), also
when an `http://` page redirects to `https://` (our client stops at
it, its URL handed to curl). TinyMosaic: a `Failed` state no more --
what could not be fetched is an error page through the pipeline
(status 0, `r` reloads it, the history keeps it); what is not HTML is
made a page (text in `<pre>`, as Mosaic; anything else said what it
is). Tried by hand: `tiny_httpd` browsed (its index, its pages),
`https://example.com/`, a refused port, an `.ml` as `text/plain`.
Golden frame `_failed` (port 1). Next: phase 7, images. Committed as
217f863.

**The split, TinyMosaic and TinyNetscape** (the author's call,
2026-09-25, after a question on concurrency: Mosaic had no threads --
blocking reads through libwww, the globe turned by a callback during
them, images fetched one after the other). TinyMosaic keeps what Mosaic
had: images (phase 7, fetched one at a time, the Mosaic way, the page
shown meanwhile), forms and CGI (phase 9), and the docs and the check
in a real browser (phase 11). **TinyNetscape** (1994-96) gets what came
after, its own plan section to write: images fetched several at once
and the page drawn as it arrives, **threads** for what blocks (DNS,
curl's https://; OCaml 4.14's threads share one lock, so concurrency,
not parallelism -- domains are OCaml 5's; the web build keeps the
event loop, behind `Cmd`), `<body bgcolor>` and `align=left/right`
(floats), tables (phase 10), CSS1 (phase 8), maybe cookies and frames.
The code the two browsers share (chrome, drawing, history, fetching)
becomes an appkit, `appkits/browser`, then.

**Phase 7 done** (2026-09-25): images, the Mosaic way. `Html_layout`:
an `<img>` of known size -- its `width=` and `height=`, or the caller's
`picture_size` (the decoded picture's) -- is a word that is a picture
(`fragment.picture`: its src, its height, bottom or `align=middle` on
the baseline, no leading), else its alt text; 3 tests (56 in all), the
`.mli`'s worked example. TinyMosaic: once a page is shown, its
pictures fetched one after the other (a queue, one in flight, the rest
dropped when another page is shown), decoded by their magic numbers
(`Gif`, `Png`, `Jpeg`: our own), the page laid out again as each
arrives; a cache of every page's pictures; drawn by `Playground.bitmap`,
a reserved frame until then, NCSA's broken image if they could not be
had, a linked one with a border of the link's colour; the globe turns
and the status line names the picture while one comes. The built-in
site has the demo picture in its three formats (`site/picture.*`,
embedded by `files_to_string_ml.ml` as `Site_pictures`), the home page
showing them. Tried against `tiny_httpd`: the page at frame 2 with the
alt texts, the pictures fetched in order (its log), all in by frame 60.
Found on the way: the Cairo and web backends kept one converted bitmap
only, so a page of three pictures would convert them again every
frame; they keep the last 32 now. The home page's pictures moved its
links down: the hover/click/back scripts point at the new place.
Deferred to TinyNetscape as planned: `align=left/right`, `bgcolor`.
XBM (Mosaic's other inline format) not done. Next: phase 9, forms.
Committed as 6631901.

**Phase 9 done** (2026-09-25): fill-out forms, both ends.
`networking/protocols/Urlencoded` (a form's fields as one string, both
ways, the notes' example), `web/html/Forms` (the controls of a page,
their initial values, HTML's successful controls, Mosaic's "Submit
Query"; it does not encode, so `web/` still depends on no other
library); `Html_layout`: a control a box in the line (`fragment.control`,
its size by its kind; the internal `Word` carries a picture or a
control); `Hit.fragment_at`. POST through the stack: `Http.post`,
`request_to_string ?body`, `Http_client.prepare ?post`,
`Http_request.start ?post` (a redirection makes it a GET), `Cmd.Http_post`,
`Playground.Http.post`, curl's and the XMLHttpRequest's. `tiny_httpd`:
CGI, `/cgi-bin/NAME` runs root/cgi-bin/NAME (the request in its
environment and on its standard input, its output the answer, a
Status: header honoured); `site/cgi-bin/echo`, a shell script. Tests:
4 (Urlencoded), 5 (Forms), 1 (layout), 1 (Hit), 1 (a POST over
sockets, and 303 then GET); 64 in `libs/web/tests/`. TinyMosaic: the
controls drawn in Motif's look every frame (not in the page's glyphs
made once: their values change as you type), their values kept with
the page, a focus taking the keys (Return sends, Escape gives up),
radio buttons exclusive in their form, a select cycling its options
on a click (Motif popped a menu), reset; `about:form` (site/form.html),
answered by `about:echo`; tried by hand against `tiny_httpd`, its CGI
answering a POST. The Title and URL fields now drawn in cells, cut at
their end (a long URL spilled over the label). Golden frames `_form`,
`_get`, `_post`; every other TinyMosaic frame changed with the fields.
Next: phase 11 (the docs, and the web build tried in a browser), then
TinyMosaic is done. Committed as 0241152.

**Phase 11 done, TinyMosaic finished** (2026-09-25). The web build
tried in a real browser at last (headless Chrome, `--screenshot`,
served as `make serve-build` does): the home page laid out as natively,
its three pictures decoded by our readers inside the browser (drawn as
PNG data URLs), `about:form` drawn, an `http://` page fetched by the
XMLHttpRequest from the same site (its status line: 95 bytes, status
200). Found then: the status line, drawn by `words` from a guessed
width, started 50 units too far right in the browser's narrower font
-- now in cells, as the fields; every golden frame changed in that row
only. The notes' table and exercises (a guest book, Mosaic's frozen
view, a popped-up select, XBM), the related work's last section, the
CATALOG row: brought up to date. Left for another time, as exercises
or TinyNetscape's: the adoption agency, frames, incremental layout,
XBM, a select's menu. **Next: TinyNetscape** -- its plan section first
(the split above), the shared code as `appkits/browser` first of all.
Committed as 10ca292.

**`appkits/browser` done** (2026-09-25, the author's go): what a
browser is made of beyond the engine, out of TinyMosaic, for
TinyNetscape -- `Browser_url` (resolve, the fragment and the query
apart), `Browser_text` (Hershey's widths, cells, escaping),
`Browser_picture` (waiting, arrived, broken; decoded by magic numbers),
`Browser_draw` (a laid-out page as shapes: letters, pictures, rules,
markers, Motif's controls, the inspector's outlines; a control's value
given as a function, so that drawing needs no page), `Browser_page` (a
page read through the whole pipeline, laid out again on a reflow; the
browser's own pages, an error, a form's echo, text; the controls'
values; the model's part as `settings`: width, breaker, visited,
pictures), `Browser_history` (the two stacks, generic), `Browser_forms`
(a click or a key on a control as an `effect`: focus, a change, a
submission as a URL and a POST). TinyMosaic over it: 1135 lines down to
~700, keeping its model, its Motif chrome, its one-at-a-time pictures,
its views, its built-in site; every golden frame pixel-identical.
Tests: `appkits/tests/Unit_browser` (the history's worked example, URLs,
GET and POST requests, a radio's exclusivity, keys). Next: TinyNetscape's
plan section, then TinyNetscape itself.

Written as the specification, with
[`notes_browser.md`](../tutorials/notes_browser.md) and
[`notes_browser_related_work.md`](../related-work/notes_browser_related_work.md)
beside it. Decisions taken, with their reasons, so they are not
re-argued:

- **the author asked for it** (2026-09-24), a "TinyBrowser" in
  `apps/internet/`, pointing at MMM and Mothra as inspiration, then
  at Mosaic as the first model, "or WWW itself";
- **TinyMosaic, browsing only** (the author's call, 2026-09-24): the
  name settled, no WorldWideWeb-style editor; the Line Mode Browser
  as a debug view;
- **a tree, not a stream**, unlike both Mothra and MMM (why: above);
- **MMM's DTD-as-data** for tag soup, in the WHATWG algorithm's shape;
- **no JavaScript**: Mosaic had none; a scripting language is
  `plan_teaching_languages.md`'s business, and a DOM for it would be
  its own plan;
- **`libs/web/`** for the engine, **`Stroke_text` moved to an
  appkit**, **curl for `https://`** until TLS is ours: the author's
  yes to all three (2026-09-24), "for now".

## Verification

- Unit tests per module (`libs/web/tests/`), each `.mli`'s worked
  example first; the soup cases compared once, by hand, with what
  html5lib builds (recorded in the tests, no dependency).
- Layout tests with ten-unit metrics: the boxes' positions of the
  notes' worked example, checked by a person with a ruler.
- Golden frames (`tests/2d/`): TinyMosaic on `about:home`, after a
  scripted click, scrolled, with images, with the boxes outlined.
- By hand: natively against `tiny_httpd` and `info.cern.ch`; in a real
  browser for the web build.

## Out of scope

JavaScript and the DOM API; our own HTTPS until TLS is ours; HTTP caching
(beyond keeping the pages of the history); cookies; frames; CSS
beyond CSS1's subset (floats beyond `align`, positioning, flexbox,
grid); bidirectional text and complex scripts; incremental layout
(the page is laid out whole, each time -- the notes say what
incremental layout is and why the real engines need it).
