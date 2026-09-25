# A web browser, from scratch: a tutorial

How a browser turns a URL into a page you can click: fetching the
bytes, reading HTML (including the HTML people really write), building
a tree, deciding what each element looks like, laying out blocks and
lines with pictures among the words, painting them, and finding what
was clicked. Every stage is the smallest version that still has the
stage's idea, the version NCSA Mosaic (1993) had, with pointers to what
the real engines do instead.

Written before the code, as its specification (see
[`plan_browser_teaching.md`](../plans/plan_browser_teaching.md)), like
`notes_synth.md` was: the worked examples below are the ones the tests
will check, and where the code ends up measuring something else, these
notes will say what it measured. Companion:
[`notes_browser_related_work.md`](../related-work/notes_browser_related_work.md)
(the browsers since 1990, Mothra and MMM, and the books). It takes
`notes_networking.md` as known for TCP and HTTP, and `notes_gui.md`
section 7 for line breaking.

The thread through it: **a browser is a pipeline of translations**,
each from one data structure to the next -- bytes, characters, tokens,
a tree, a tree with looks, boxes, shapes -- and each is simple once you
know what it takes in and what it gives out. The difficulty of real
browsers is not any one stage; it is that the input is anything anyone
ever wrote, and that the pipeline must run again, fast, every time
something changes.

## 0. Where the code is, and a reading order

| module | what | section | status |
|---|---|---|---|
| `networking/Url`, `Http`, `Http_request` | the address, the protocol, the request stepped each frame | §1 | done |
| `web/html/Charset`, `Entities` | bytes to characters; `&eacute;` | §2 | done |
| `web/html/Html_lexer` | characters to tokens | §3 | done |
| `web/html/Dtd`, `Dom`, `Html_tree` | tokens to a tree, tag soup repaired | §4 | done |
| `web/html/Line_mode` | the tree as the 1991 Line Mode Browser showed it | §8 | done |
| `web/style/Looks` | the tree's looks, Mosaic's table | §5 | done |
| `web/layout/Html_layout` | blocks and lines: boxes with positions | §6, §7 | done (greedy lines; Knuth-Plass plugged in by the app) |
| `web/layout/Hit` | a point to a link, a name to its place | §8 | done |
| `apps/internet/TinyMosaic` | the chrome, painting, the history, pictures, forms | §7-§11 | done |
| `web/style/Css` | the cascade | §10 | planned, TinyNetscape's |
| `networking/httpd/tiny_httpd`, `networking/unix/Http_server` | the other end: a server, and CGI | §11 | done |
| `web/html/Forms`, `networking/Urlencoded` | a form's controls, what a submission sends, how | §11 | done |
| `web/layout/Table_layout` | tables | §12 | planned, TinyNetscape's |

## 1. From a URL to bytes

A **URL** (Berners-Lee, 1991; RFC 3986 today) names a resource and
says how to get it: `http://info.cern.ch/hypertext/WWW/TheProject.html`
is a *scheme* (`http`: the protocol), a *host*, and a *path* on it.
A link in a page is usually **relative**, resolved against the page's
own URL (its *base*) exactly as a path in a shell is against the
current directory (`Url.resolve`, whose tests have RFC 3986's own
examples):

```
base      http://info.cern.ch/hypertext/WWW/TheProject.html
"Help.html"                -> http://info.cern.ch/hypertext/WWW/Help.html
"../DataSources/Top.html"  -> http://info.cern.ch/hypertext/DataSources/Top.html
"/"                        -> http://info.cern.ch/
"#people"                  -> the same page, scrolled to "people"
```

The last one is not fetched at all: a **fragment** is for the browser,
never sent to the server.

The bytes come by HTTP (`Http.mli` explains the messages), with a
`Content-Type` saying what they are (`text/html`, `image/gif`). A
browser trusts it, mostly; when it is missing or absurd, it **sniffs**
the first bytes (`GIF89a`, `\x89PNG`, `\xFF\xD8\xFF` for JPEG, `<html`
for a page) -- Mothra runs the Unix `file` command on them, the WHATWG
wrote the rules down ("MIME Sniffing").

A browser that draws 60 frames a second cannot wait for a server, so
the request is `Http_request`'s state machine, stepped once a frame;
a page with ten images is eleven of them stepping side by side, and
the page is drawn in between with what has arrived. Netscape (1994)
made its name that way -- the text first, the pictures filling in over
several connections at once -- where Mosaic fetched one thing at a
time and showed the page when it was all there.

## 2. From bytes to characters

HTML is text, but the bytes do not say which **encoding** they are
in. `é` is one byte, `E9`, in Latin-1 (ISO 8859-1, the web's default
until HTML5), and two, `C3 A9`, in UTF-8. Read UTF-8 as Latin-1 and
`café` becomes `cafÃ©` -- *mojibake*, the web's commonest bug for
twenty years. The encoding comes from the `Content-Type`'s `charset=`,
else a `<meta charset>` near the top, else a guess (`Charset`: valid
UTF-8 is UTF-8; otherwise Windows-1252, a superset of Latin-1 whose
bytes 80-9F are the curly quotes Word typed, which is what "Latin-1"
pages really were -- the WHATWG makes the substitution official).
Everything after this stage is UTF-8.

**Entities** name the characters a keyboard lacks, or that HTML
reserves: `&lt;` is `<`, `&amp;` is `&`, `&eacute;` is `é`, and
`&#233;` or `&#xE9;` is `é` by its Unicode number. HTML 2.0 had
Latin-1's 100 or so; HTML 4 had 252; HTML5 has 2,231. `Entities`
decodes them, and leaves an unknown `&foo;` as it is, which is what
browsers do.

## 3. From characters to tokens

The **tokenizer** reads characters and gives **tokens**:

```
<p class=intro>Caf&eacute; <a href="menu.html">menu</a>

StartTag "p" [class = "intro"]
Text     "Café "
StartTag "a" [href = "menu.html"]
Text     "menu"
EndTag   "a"
EOF
```

It is a state machine, one character at a time: in *data* state, `<`
followed by a letter starts a tag (`<` then a space is just a `<`,
another thing real pages do); in *tag name*, letters are lowercased
(HTML is case-insensitive: `<P>` is `<p>`); then *attribute name*,
*attribute value* (double-quoted, single-quoted, or unquoted up to a
space or `>`); `>` emits the tag. `<!--` enters *comment* until `-->`;
`<!DOCTYPE` is its own token. Inside `<script>` and `<style>`, nothing
is a tag until the matching end tag (*RAWTEXT*): we skip both, as
Mothra does.

The WHATWG's tokenizer has 80 states, because it says what to do with
every broken input (an attribute with no value, a `<` inside a
quoted value, a comment that never ends); ours has about fifteen, and
its `.mli` lists the inputs it treats differently. Mosaic's had none of
this care: its parser was a few hundred lines, and the next decade's
browsers each copied Mosaic's accidents so that the same pages would
look the same -- which is how "tag soup" became a compatibility
requirement.

## 4. From tokens to a tree

The **tree** (the DOM, the *Document Object Model*) is what the rest
works on: elements with attributes and children, and text.

```
<title>Lunch</title>            html
<h1>Menu</h1>                    +- head
<p>Soup of the day               |   +- title: "Lunch"
<p>Salads:                       +- body
<ul>                                 +- h1: "Menu"
<li>tomato                           +- p: "Soup of the day"
<li><b>cucumber</b>                  +- p: "Salads:"
</ul>                                +- ul
                                         +- li: "tomato"
                                         +- li
                                             +- b: "cucumber"
```

(whitespace-only text between blocks left out of the picture; the
tree has it, and layout ignores it). Four repairs made that tree, all
legal in HTML's SGML days and all still in every page:

1. `<html>`, `<head>`, `<body>` are **implied**: `<title>` goes in the
   head, the first thing that is not head material opens the body.
2. The second `<p>` **closes the first**: a paragraph cannot contain
   a paragraph.
3. `<ul>` **closes the open `<p>`**: a paragraph contains no blocks.
4. The second `<li>` **closes the first**, and `</ul>` closes the
   last.

The algorithm is a **stack of open elements**. A start tag first
closes what it cannot be inside (the `Dtd` table: `p` is closed by
any block; `li` by `li`; `dt`, `dd` by either; `option` by `option`;
`tr` by `tr`; `td`, `th` by either), then is added as a child of the
top of the stack, and pushed -- unless it is **empty** (`br`, `hr`,
`img`, `input`, `meta`, `link`), which never has children. An end
tag pops the stack down to its match, if there is one; **if there is
none, it is ignored** (a stray `</b>`). Text is appended to the top.

That is MMM's approach (its `dtd.ml` has the rules of HTML 2.0 and 3.2
as OCaml data) and, in its shape, the WHATWG's (the stack, "generate
implied end tags", "has a `p` element in button scope"). Where the two
differ is **misnested formatting**:

```
<b><i>x</b>y</i>

ours (and Mothra's): </b> pops i and b          b( i("x") ), "y"
the WHATWG's "adoption agency" algorithm:      b( i("x") ), i("y")
```

Every browser shows `y` in italics, because in 1995 one of them did;
the adoption agency (HTML5, 2008) is the page and a half of spec that
reproduces it, and implementing it is the first exercise.

## 5. From a tree to looks

Each element gets a **look**: size, weight, slant, colour, fixed width,
underline, margins, how its lines are broken. In Mosaic these were
fixed (the X resources file named the font of each heading), and
`Looks` is that table:

| element | look |
|---|---|
| `h1` ... `h6` | bold; sizes 2, 1.5, 1.17, 1, 0.83, 0.75 times the text's |
| `p` | a blank line (1.12 em) above and below |
| `b`, `strong` / `i`, `em`, `cite` | bold / italic |
| `tt`, `code`, `pre`, `kbd` | fixed width; `pre` also keeps its spaces and newlines |
| `a` with `href` | blue, underlined (purple once visited) |
| `ul`, `ol`, `blockquote`, `dd` | indented by 40 units |
| `center` | lines centred |

(the numbers are CSS 2.1's appendix D, the "default style sheet for
HTML 4", which wrote down what the browsers had converged on since
Mosaic). A look is **inherited**: text inside `<b>` inside `<h1>` is
bold and heading-sized, so each element's look is its parent's with
its own changes applied, computed going down the tree. Margins are
not inherited -- a paragraph's space above is not its words'.

## 6. Layout: blocks

**Layout** gives every element a box: a position and a size, on a page
whose width is the window's and whose height is whatever it takes. It
starts with the **block** elements (`p`, `h1`, `ul`, `li`, `pre`,
`hr`...): each is as wide as its parent minus its indent, and they
stack downwards, each starting where the last ended (plus the margins
between them -- of which, where two touch, only the larger counts:
CSS's *margin collapsing*, why two paragraphs are one blank line apart,
not two). Worked example (`Html_layout.mli`'s and its tests', every
character as wide as its size, the root's size 10, a page 200 wide,
the margins those of section 5):

```
<h1>Menu</h1><p>Soup of the day</p>

 body   x 8, width 184 (its margins 8), y 8
 h1     y 8 + 13.4 = 21.4        its margin: 0.67 em of its 20
        "Menu" x 8..88, line 21.4..45.4, baseline 39.4
 p      y 45.4 + 13.4 = 58.8     max (13.4, 11.2): collapsed
        "Soup" x 8, "of" 58, "the" 88, "day" 128, baseline 67.8
 body   ends at 70.8 + 11.2 = 82, the page at 90
```

(CSS would also collapse the body's 8 with the h1's 13.4, a parent's
margin with its first child's; ours collapses siblings only.) A block
holding both blocks and text (`<li>text<ul>...`) gets an *anonymous*
block around each run of text, so that a block's children are all
blocks or all lines.

The page's coordinates are the typesetter's: x from the left, y
**down** from the top (`Page.mli`'s convention); TinyMosaic turns them
over to draw, since the playground's y is up and its origin the centre.

## 7. Layout: lines

Inside a block, the **inline** content -- text, `b`, `a`, `img` -- is
cut into words, each word measured in its look, and the words poured
into **line boxes** as wide as the block. Worked example, with the
tests' metrics (a character as wide as its size, a space too), a page
208 wide, so a paragraph 192 (`Html_layout`'s test):

```
<p>Soup of the day and salads</p>

 p (size 10), y 8 + 11.2 = 19.2
   line 1 "Soup of the day and"  x 8..198,  y 19.2..31.2   40+10+20+10+30+10+30+10+30 = 190
   line 2 "salads"               x 8..68,   y 31.2..43.2   190 + 10 + 60 = 260 > 192: broken
```

That is **greedy** breaking, what every browser does by default:
fill the line, break when the next word does not fit. `wrap=pretty`
switches to `Linebreak.optimal`, Knuth and Plass's paragraph-wide
choice (`Linebreak.mli`'s worked example) -- which CSS adopted in 2023
as `text-wrap: pretty`, forty-two years after their paper.

Two things a browser's breaking has that a word processor's does not.
What is broken is not a word but a *unit*, the words stuck together
with no space between them (a link's "home" and the "." after it; the
two halves of `<b>bo</b>ld`), so the breaker sees units. And the text
is **ragged right**, not justified: a line's spaces may be left wide
(the line ends short) but never squeezed (it would end past the
edge), so TinyMosaic's `wrap=pretty` gives Knuth and Plass a stretch
and no shrink. Their breaker then counts a line with no space and room
to spare as infinitely bad, and so avoids the loose line greedy
leaves before a long word -- a URL in the text, a long entry in a
list: the history page at 600 wide shows it (`TinyMosaic_narrow.png`
beside `TinyMosaic_pretty.png`). The last line is free, as in TeX; CSS's
`pretty` mostly cares for that last line (no word alone on it), an
exercise.

On a line, words of different sizes share a **baseline**: each word
has an *ascent* (above the baseline, 0.8 of its size here) and a
*descent* (below, 0.2), and its line height (1.2 of its size, CSS's
"normal") spreads the rest half above, half below (the *half-leading*);
the line is as tall as its parts need, the baseline where the tallest
top puts it:

```
<p>soup SOUP</p>, "SOUP" at size 20:
  "soup" size 10: ascent 8,  descent 2, line 12: 9 above the baseline, 3 below
  "SOUP" size 20: ascent 16, descent 4, line 24: 18 above, 6 below
  the line: 18 above, 6 below, 24 high, its baseline 18 from its top;
  "soup" drawn from 10 to 20, "SOUP" from 2 to 22
```

(`Html_layout`'s test has `<big>`, 1.17 em, with the same rule.) An
**image** is Mosaic's addition: a word that is a picture, a box of its
own width and height, sitting on the baseline (its ascent is its
height, its descent zero, and no leading):

```
<p>A <img src=g.gif width=30 height=50> B</p>
  "A" x 0..10;  image x 20..50;  "B" x 60..70 (from the line's start)
  line height 50 + 3 = 53, baseline at 50: the text sits at the
  image's bottom (Mosaic's default; align=middle and top move it)
```

Until an image has arrived, its size is unknown -- unless the page
gave `width` and `height`, which is what those attributes are for: a
browser that knows them lays the page out once, one that does not lays
it out again when each image arrives, and the text jumps (what Google
now measures as "cumulative layout shift", and what 1994's pages did
all the time). TinyMosaic can do both: `alt` text or an empty box of
the given size until the pixels come.

When they come is the other half. Mosaic fetched a page's pictures
one after the other, after the page, and showed nothing until the
last was in: every request a blocking read, through CERN's libwww (a
callback during the reads turned the globe, and let a click on it
interrupt). TinyMosaic fetches them one after the other too, but shows
the page at once and lays it out again as each arrives, since its
requests never block the frame. Netscape (1994) fetched four at once
and drew the page as it came, which is TinyNetscape's to show. The
pictures are decoded by their first bytes, not by what the server
says they are: `GIF8`, `\x89PNG`, `\xFF\xD8` -- the magic numbers
(TinyMediaPlayer's `Media.sniff` does it for every format).

`<pre>` keeps its spaces and newlines and never breaks: a line per
source line, in fixed-width cells. `<br>` ends a line. A list item's
**marker** (a bullet, or `3.`) is drawn to the left of its first line,
outside its box, in the indent `ul` made.

## 8. Painting and clicking

**Painting** walks the boxes and emits shapes: a rectangle for a
background, a rule for `<hr>`, the words' glyphs (`Stroke_text`, which
knows bold and italic), an image's pixels (`Playground.bitmap`), the
underline under a link. The page is taller than the window, so it is
**scrolled**: an offset subtracted from every y, and what lies outside
the window never emitted (the real engines' *culling*).

**Clicking** is the other way: the point, plus the scroll, is a point
on the page; `Hit` finds the line holding it, and on it the fragment
-- and the fragment's look knows its link, the href of the nearest `a`
around it, inherited down the tree like a colour. A browser finds the
same `a` by walking up the tree from the box's element (which is also
the way a click event *bubbles*, for scripts); ours keeps the link in
the look because a look is all a fragment keeps. A point in the space
between two words of one link is in it too (Mosaic underlined the
spaces). Hovering does the same every frame, to show the link's URL in
the status line.

A **#fragment** is the other lookup: `Hit.anchor` finds the line an
`<a name>` or an `id=` is on (the layout keeps them there, as items of
no width among the words), and the page scrolls to its top -- at once
when the fragment is on the page shown, once it is in when it is on
another.

The **line-mode view** (the 1991 CERN browser, "www") skips layout
altogether: the text of the tree, each link followed by a number, and
the person types the number (`Line_mode`, 80 columns, its tests'
example; the 1991 prompt offered more, "1-2, Back, Up, <RETURN> for
more, Quit, or Help:"):

```
                                      Menu

Soup of the day. See the recipes[1] or go back home[2].

1-2, type a link's number then <RETURN>: _
```

It is layout with the hard part left out -- a character per cell, so
a line breaks after a count, not a measure -- and so the first thing
written once the tree existed (phase 2), before any font.

## 9. Navigating: the history

The history is **two stacks**: pages behind, pages ahead. Visiting a
page pushes the current one on *behind* and **empties *ahead***; Back
moves one from behind to ahead; Forward the other way.

```
visit A, B, C        behind [A B]   current C   ahead []
Back                 behind [A]     current B   ahead [C]
visit D              behind [A B]   current D   ahead []     C is gone
```

That last line surprises everyone once, and every browser does it (MMM's
`history.ml` calls the lost branch "obsolete"; Mothra has no forward
at all, but a list of the last 64 pages to pick from). Each entry keeps
the page's parsed tree and its scroll position, so Back is instant and
lands where you were; Reload fetches again.

## 10. Style sheets: the cascade

Mosaic's looks were the browser's, and a page could only choose among
elements. Pages wanting control used `<font>` and tables for layout
(Netscape's era); **CSS** (Håkon Wium Lie, 1994; CSS1, 1996) put the
looks in rules instead:

```
p        { color: black }       specificity (0, 0, 1)
.intro   { color: green }                   (0, 1, 0)
p.intro  { color: red }                     (0, 1, 1)   <- wins for <p class=intro>
#top     { color: blue }                    (1, 0, 0)   <- would beat all three
```

For each element and each property, the rules whose **selector**
matches are sorted by **specificity** -- (ids, classes, element names)
compared left to right -- then by order in the sheet, the last
winning; `style="..."` on the element beats them all. Where no rule
says, the property is inherited (colour, font) or takes its initial
value (margins). And the table of section 5 becomes the first sheet in
the cascade, the *user-agent style sheet* -- which is what Mosaic's
table always was.

In this repository (TinyNetscape's N5): `Css` parses the page's
`<style>` and matches its rules, the cascade giving each element its
winning declarations; the layout computes a look as before -- the
table's, inherited from the parent -- and then applies them
(`Looks.styled`, `styled_box`). So inheritance comes for free: a rule
changes an element's look, and its children start from that look.
TinyNetscape's `about:css` is the example above in a page, and its key
`c` lays out the same tree without the sheets: the looks are not in the
HTML, which is the whole point.

## 11. The other end: a server

`tiny_httpd` is the smallest useful web server: read a request (`Http`
parses both directions, `Http.parse_request` saying whether the bytes
so far are a whole one yet), map the path to a file under a directory,
send it with its `Content-Type` guessed from its extension, or a 404.
Three things every web server learned early, each a few lines there: a
path that climbs out of the directory (`/../../etc/passwd`, the first
attack) is refused, 403; a directory with no `index.html` is answered
with a page listing its files (NCSA's "Index of /"); and each request
is a line in the log, NCSA's Common Log Format, still every server's:

```
127.0.0.1 - - [25/Sep/2026:07:01:26 +0000] "GET /home.html HTTP/1.1" 200 1411
```

Its event loop is `Http_server`, the shape of `Server`'s (WebSocket)
and of `Http_request`'s (the client): non-blocking sockets, a step
doing what can be done without waiting, select between steps -- so a
slow client holds up no other, where CERN's and NCSA's servers gave
each connection a process of its own. **CGI** (NCSA httpd, 1993,
from the same lab as Mosaic) was the first dynamic page: a URL
names a *program*, run with the request in environment variables
(`QUERY_STRING`, `REQUEST_METHOD`), whose output is the response.
A form's fields reach it **URL-encoded**:

```
q = "café au lait", lang = "fr"
  -> GET /search?q=caf%C3%A9+au+lait&lang=fr
```

(a space is `+`, a byte that is not a letter or a digit is `%` and its
hexadecimal, and `é` is its two UTF-8 bytes: `Urlencoded`, its tests
this example). Which fields a submission sends is HTML's rule of
"successful controls" (`Forms`): each named control of the form --
a field's text, a checkbox or radio button only if checked, a select's
chosen option, the one button clicked and no other, never a reset.
GET puts them in the URL (bookmarkable, and in every log on the way),
POST in the request's body, for a password or an order; answered by a
redirection, a POST's answer is fetched with a GET, as browsers do, so
that reloading it does not post again.

The two ends, in this repository: TinyMosaic's built-in form page is
answered by the browser itself (`about:echo`, the fields decoded), and
the same page from `tiny_httpd` by a CGI program, `cgi-bin/echo`, a
shell script reading `QUERY_STRING` and its standard input. The
controls themselves are the browser's widgets drawn in the page, laid
out as boxes in the line like pictures; their values are the browser's,
kept with the page (Back gives a half-filled form back half filled),
never written into the tree -- changing the tree as you type is what
JavaScript's DOM made browsers do.

## 12. Tables

A table's columns are as wide as their contents need, and a cell's
need depends on the width it is given -- so the **automatic table
layout** (CSS 2.1 section 17.5.2.2, which wrote down what Netscape did)
asks each cell two widths: its **minimum** (its longest word: it can
wrap down to that, not further) and its **maximum** (everything on one
line). Worked example, the tests' metrics:

```
| a            | 3.50 |      col 1: min 60 ("tomato"), max 110 ("tomato soup")
| tomato soup  | 4.20 |      col 2: min 40,            max 40
                              sums: min 100, max 150
width available 200: >= 150, every column at its max:  110, 40 (table 150 wide)
width available 120: between, the 20 beyond the minimums shared in
                     proportion to max - min (50 and 0): 80, 40,
                     and "tomato soup" wraps onto two lines
width available  90: < 100, every column at its min, and the table
                     overflows the window
```

In this repository, `Table_layout` (the grid, and these widths) and
`Html_layout` (the cells laid out) do it for TinyNetscape. A cell's two
widths come from the layout itself, run twice: at width 0 every word is
a line of its own, so the widest line is the minimum; without a limit
nothing wraps, so the widest line is the maximum -- a table in a cell
measured the same way, for free. Each cell is laid out three times, and
a table in a table nine: why real engines cache what they measure.
TinyMosaic, like the Mosaic of 1994, has no tables: its cells are text
run together, the "table soup" every page designer saw in the browsers
left behind.

## 13. What the real engines add

- **Scripts**: JavaScript (Netscape 2, 1995) changes the tree after it
  is built, so every stage above must be *re-run* -- style, layout,
  paint -- on the part that changed. Engines keep **dirty bits** on
  the tree and redo only what depends on what changed (**incremental
  layout**); ours redoes everything, which is fine for a page, and is
  the first thing that breaks at the size of a real one.
- **Compositing**: the page painted in layers the GPU moves without
  repainting (scrolling, animations).
- **Isolation**: a process per tab (Chrome, 2008), per site since
  2018, because a page is a program from a stranger.
- **Security by origin**: a page may read only from its own site
  (CORS) -- which TinyMosaic meets from the other side when it runs
  inside a browser: a browser in a browser, allowed to fetch only the
  pages beside it.
- **TLS** (almost every page is `https://` now), HTTP/2 and 3, caches,
  cookies, fonts downloaded, bidirectional text, accessibility trees.

## 14. Not waiting: four connections, and threads

Mosaic (1993) fetched a page, then each picture, one after the other,
its window frozen in libwww's blocking reads. Netscape (1994) did the
same work without waiting, and every browser since has: TinyMosaic and
TinyNetscape are that difference, over the same engine.

**Four connections.** A picture is a request of its own; four in
flight at once (`connections`, Netscape's default), the next started
as one ends, and the page laid out again as each arrives -- the text
readable before the first picture is in. None of this needs a thread:
`Http_request` is a state machine, stepped once a frame, doing on each
socket only what won't block (select with a timeout of 0). One thread,
many sockets: the **event loop**, as in `Http_server`.

**What an event loop can't hide.** Two calls block and are no state
machine: the host's name resolved (`getaddrinfo`, the C library asking
a DNS server) and curl's `https://`, the whole transfer. Measured on
TinyNetscape, the frame that asks for the page: 64 ms for
`http://info.cern.ch/`'s name, 568 ms for `https://example.com/`,
the window frozen that long. So those go to **threads**, as Netscape
did on NSPR's: `Worker`, a pool of four, each thread taking a job from
a queue, running it, leaving its result; the frame polls the job as it
steps a socket (`Http_request`'s `Resolving` state; `Commands`' curl).
Measured again: no frame over 50 ms.

```
  frame loop                          a thread of the pool
  submit (getaddrinfo host) --queue-> wait on the condition
  poll: nothing yet                   getaddrinfo ... (blocks, the
  poll: nothing yet                     runtime lock released)
  poll: the addresses <-------------- result, under the mutex
  connect (non-blocking), ...
```

**Concurrency, not parallelism.** OCaml 4.14's threads share one
runtime lock: only the thread holding it runs OCaml code, and a thread
gives it up in a blocking system call (the DNS query, a read, curl's
transfer, the frame loop's sleep). That is exactly when another has
something to do, so waiting overlaps; computing does not -- a JPEG
decoded on a thread would take the frame's time all the same. Running
OCaml on several cores at once is OCaml 5's domains. And the queue and
each result, touched by two threads, are read and written under a
mutex: without it the lock would still make each word's write whole,
but not the order of two, and a program should not lean on a runtime's
detail for its correctness.

**And JavaScript kept one thread.** A browser page has no threads:
JavaScript's model is the event loop's -- callbacks when a request
answers, never a blocking call -- and the browser does the waiting
(on its own threads). So the web build of TinyNetscape has no
`threads` flag: the web platform's `XMLHttpRequest` is already the
Worker.

## 15. Extensions: the web's second layer

HTML 2.0 (RFC 1866, November 1995) wrote down what Mosaic read. By
then Netscape had shipped its own additions for a year, without asking
anyone: `<font size=+1 color=red>`, `<center>`, `<body bgcolor=silver>`,
rules of any thickness (`<hr size=4 width=50%>`), and pictures the
text flows around (`<img align=left>`). HTML 3.2 (1997) took most of
them in. A page of 1995 mixes the two, often under a DOCTYPE claiming
HTML 2.0 -- this repository's own home page did, with a `<P
ALIGN=CENTER>`.

So the tree keeps **where each thing comes from** (`Dtd.origin`): the
lexer marks a Netscape element's start tag, and keeps a core element's
Netscape attributes apart; the tree keeps both marks:

```
<body bgcolor=silver>   Start_tag "body" [] {Netscape: bgcolor = "silver"}
<font size=+1>          Start_tag "font" [size = "+1"] {Netscape}
                        the tree:  body {Netscape: bgcolor="silver"}
                                     font size="+1" {Netscape}
```

The core reads the core (`Dom.attribute`); the rest is asked for by
name (`~extensions:true`). One tree for every browser, and each honours
what it knows: TinyMosaic, HTML 2.0, where an extension is an unknown
tag (its content shown, the tag ignored: HTML's rule for what a browser
does not know, which is what let Netscape extend it at all);
TinyNetscape, the extensions too.

**Floats** are the one extension that changed layout itself. A picture
with `align=left` leaves the line, goes against the left edge, and the
lines beside it are shortened until its bottom -- including the next
paragraph's, so the floats are the page's, not a block's. A line's
width now depends on where it is, so lines are filled one at a time,
each as wide as the room at its top (a paragraph scored whole, Knuth
and Plass, needs one width: beside floats, the breaker is greedy).
`<br clear=all>` moves the next line below them. CSS 2.1's section 9.5
wrote down what Netscape did, and added much (see `Html_layout.mli`'s
worked example, and its list of what is not done).

## Exercises

1. The adoption agency algorithm, for `<b><i>x</b>y</i>` (section 4).
2. A `TinyLineMode` program: section 8's view as the whole browser.
3. `<table>` as Mothra does it (a row a line, a cell a space), then as
   section 12 does, side by side.
4. Frames (Netscape 2, 1996): a page split into pages, each with its
   own history.
5. Incremental layout: a dirty bit per block, and only dirty blocks
   laid out again when an image arrives; count the boxes saved.
6. A guest book: a CGI program (in OCaml, over `Urlencoded`) that
   appends what a form sent to a file and answers with the whole book.
7. Mosaic's way with pictures exactly: the page not shown until the
   last picture is in, the view frozen meanwhile (`fetch=mosaic`), and
   the difference felt against a slow `tiny_httpd`.
8. A `<select>` that pops a menu of its options, as Motif's did,
   instead of cycling through them on a click.
9. XBM, the other inline image format Mosaic read: a C file as a
   picture, a reader of thirty lines beside `graphics/images/xpm/`.
10. The pictures decoded on the pool too (section 14), and measured:
    no faster under OCaml 4.14's lock; then with OCaml 5's domains.
11. A slow `tiny_httpd` (`delay=`), to feel four connections against
    one: `Http_server` answering a request later without stopping the
    others (a sleep in the handler would stop the whole server).
12. `<blink>` (Netscape 1.0's most hated tag): a look that the drawing
    turns on and off every half second.
13. A float that does not fit beside another goes below it, as CSS
    says; and a block's background under a float (only its lines are
    shortened).

## Glossary

- **DOM**: the document as a tree of elements and text.
- **tag soup**: HTML as written, with end tags omitted and misnested.
- **user-agent style sheet**: the browser's own looks, the first in the
  cascade.
- **block / inline**: stacked downwards / poured into lines.
- **line box**: a line of a block, as high as its tallest word needs.
- **replaced element**: an element drawn by something else than its
  text (an image, a form field), laid out as a box of its own size.
- **reflow**: layout run again after a change.
- **hit testing**: from a point to what is drawn there.

## References

- Pavel Panchekha and Chris Harrelson, *Web Browser Engineering*
  (browser.engineering): a browser in Python, a chapter a stage; the
  order of this tutorial is theirs.
- Matt Brubeck, "Let's build a browser engine!" (2014): robinson, a
  toy engine in Rust -- HTML, CSS, style, layout, paint.
- Tali Garsiel and Paul Irish, "How Browsers Work: Behind the scenes
  of modern web browsers" (2011).
- WHATWG, *HTML Living Standard*, section 13.2 "Parsing HTML
  documents" (the tokenizer, tree construction); *MIME Sniffing*;
  *Encoding*.
- W3C, *CSS 2.1*, chapters 8-10 (boxes, the visual formatting model)
  and 17 (tables), and appendix D (the default style sheet).
- T. Berners-Lee and D. Connolly, RFC 1866, *Hypertext Markup
  Language - 2.0* (1995); RFC 3986 (URIs).
- Tim Berners-Lee, *Weaving the Web* (1999), for why WorldWideWeb was
  an editor.
- In the author's other repositories: `Browser.nw` (Mothra, webfs,
  hget; principia-softwarica) and `mmm.nw` (MMM).
