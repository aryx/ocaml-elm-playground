# TinyMosaic vs. the browsers since 1990

Where a small teaching browser sits among the ones people used, the
ones people studied, and the two already in the author's other
repositories. What they do that this will not, and which of their
ideas fit in a few thousand readable lines. Companions:
[`notes_browser.md`](../tutorials/notes_browser.md) (how it works) and
[`plan_browser_teaching.md`](../plans/plan_browser_teaching.md) (what
gets built, in what order).

## The one-line version

| | What it optimized for | What it is made of |
|---|---|---|
| WorldWideWeb (1990) | Reading *and writing* hypertext | NeXTSTEP's Text object, edited in place |
| Line Mode Browser (1991) | Any terminal, anywhere | Text and numbered links |
| Mosaic (1993) | Anyone, with pictures | A Motif window, `<img>`, a click per link |
| Netscape, IE (1994-2000) | Winning the market | Tables, frames, `<font>`, JavaScript, each other's bugs |
| Lynx, w3m, Links | The terminal, still | Text layout, tables drawn in characters (w3m) |
| Mothra, Abaco (Plan 9) | Small, composed of other programs | A style stack, a run list, webfs |
| MMM (1996) | A research browser in a typed language | OCaml, Tk, DTDs as data, sandboxed Caml applets |
| WebKit, Gecko, Blink | Every page ever written, fast, safely | Millions of lines, a process per site |
| Servo, Ladybird | A new engine from the standards | Rust and parallel layout / C++ and the specs read line by line |
| *Web Browser Engineering* | Teaching | A Python browser, a chapter a stage |
| TinyMosaic | Seeing each stage, on Mosaic's web | A tree, Mosaic's looks, blocks and lines, our own image decoders |

## Part 1: the first browsers (1990-1993)

- **WorldWideWeb** (Tim Berners-Lee, CERN, late 1990; renamed
  *Nexus*): on a NeXT, in Objective-C, and a **browser-editor** --
  a page opened was a page you could type into, and a link was made by
  selecting text and pointing at another window. Berners-Lee's own
  account (*Weaving the Web*, 1999) regrets that the browsers after it
  dropped the editing half. Reconstructed to run in today's browsers by
  a CERN team in 2019 (worldwideweb.cern.ch).
- **Line Mode Browser** (Nicola Pellow, CERN, 1991): the web for every
  terminal, in portable C: the text of the page, a number in brackets
  after each link, the number typed to follow it. It is what made the
  web reachable from any machine at CERN, and it is the layout-free
  stage of this plan's pipeline.
- **Erwise** (students at Helsinki University of Technology, 1992):
  the first graphical browser for X11; **ViolaWWW** (Pei-Yuan Wei,
  Berkeley, 1992), with style sheets and embedded scripts of its own
  language years before CSS and JavaScript; **Midas** (Tony Johnson,
  SLAC, 1992); **Lynx** (University of Kansas, 1992, the web added in
  1993), still maintained.
- **NCSA Mosaic** (Marc Andreessen and Eric Bina, National Center for
  Supercomputing Applications, 1993): X, then Windows and Macintosh.
  **Images inline** (Andreessen proposed `<img>` on the www-talk list in
  February 1993; Berners-Lee would have preferred a general `<include>`),
  **fill-out forms** (Mosaic 2.0), a friendly installer, and the grey
  page. The web's traffic grew by orders of magnitude within a year.
  Its source is readable today (X Mosaic 2.7's has been put back on
  GitHub by several people) -- one C program, Motif widgets, its HTML
  "parser" a few hundred lines that later browsers had to imitate.

## Part 2: the engines

- **Netscape Navigator** (1994, by much of Mosaic's team): progressive
  rendering, several connections at once, then tables (1.1), frames,
  cookies (Lou Montulli, 1994), SSL, and **JavaScript** (Brendan Eich,
  1995). **Internet Explorer** (1995) was Spyglass's licensed Mosaic;
  IE 3 (1996) was the first commercial browser with CSS. The **browser
  wars** made HTML a pile of vendor tags, which the W3C's HTML 3.2
  (1997) and 4 (1997) then standardized.
- **CSS**: Håkon Wium Lie's proposal (1994), CSS1 (1996, with Bert
  Bos); **Arena** (Dave Raggett, CERN then W3C) and **Amaya** (W3C,
  1996, a browser-editor again) were the testbeds.
- **Gecko** (Netscape's rewrite, open-sourced as Mozilla in 1998),
  **KHTML** (KDE, 1998) → **WebKit** (Apple, Safari, 2003) →
  **Blink** (Google's fork, 2013; Chrome, Edge, Opera, Brave). **HTML5**
  (WHATWG, from 2004; Ian Hickson's parsing algorithm) finally wrote
  down what browsers do with tag soup, so that all engines build the
  same tree from the same bytes.
- **Servo** (Mozilla Research, 2012; Rust, parallel style and layout,
  parts of it now in Firefox) and **Ladybird** (Andreas Kling, from
  SerenityOS's browser, 2018; an independent project since 2024,
  reading the specs step by step and naming the code after them): new
  engines from scratch, and proof that one can still be written.

## Part 3: the small ones

- **Lynx**, **Links** (Mikuláš Patočka, 1999; graphics mode later),
  **w3m** (Akinori Ito, 1995; tables laid out in characters): text
  browsers.
- **Dillo** (1999; a few hundred kilobytes, its own engine),
  **NetSurf** (2002, from RISC OS; its own HTML and CSS libraries,
  clean C), **surf** (suckless, a WebKit window and little else).
- **Mothra** (Tom Duff, Plan 9, 1995) and **Abaco** (Federico
  Benavento, 2000s; Russ Cox's `libhtml`, tables included): see Part 5.
- **Gemini** clients (Lagrange, 2020): a protocol and a markup chosen
  small on purpose, which is this plan's premise taken as a movement.

## Part 4: the teaching lineage

- **Pavel Panchekha and Chris Harrelson, *Web Browser Engineering***
  (browser.engineering, since 2019): a browser in Python, grown a
  stage per chapter -- downloading, drawing, formatting text, the
  tree, layout, styles, links, then JavaScript, security, animations,
  accessibility. This plan follows its order for the first seven
  chapters, and stops where it adds scripts.
- **Matt Brubeck, "Let's build a browser engine!"** (2014): robinson,
  in Rust -- HTML, CSS, style tree, layout tree, painting to pixels --
  a thousand lines, no network.
- **Tali Garsiel and Paul Irish, "How Browsers Work"** (2011): the
  modern engine's pipeline explained from WebKit's and Gecko's
  sources; **Mariko Kosaka, "Inside look at modern web browser"**
  (Google, 2018): the processes, the compositor.
- **The specifications as textbooks**: the WHATWG's parsing chapter is
  an algorithm written to be implemented line by line; CSS 2.1's
  chapters 9 and 10 the same for layout.

## Part 5: in the house -- Mothra and MMM

The author's other repositories hold two browsers with literate books,
at opposite ends:

| | Mothra (`principia-softwarica/browsers/`) | MMM (`~/github/mmm`) |
|---|---|---|
| language, size | C, 4,850 lines (+ webfs 4,800) | OCaml, ~30,000 lines |
| book | `Browser.nw` (with webfs, hget, cookies) | `mmm.nw` |
| fetching | not its business: the webfs file server | its own HTTP, a cache, a scheduler for images |
| HTML | `rdhtml.c`: a stack of style states, no tree | ocamllex tokens, repaired by DTDs as OCaml data |
| layout | a flat list of `Rtext` runs, wrapped by libpanel | a *display machine*: per-tag handlers over a stack of formatters, into a Tk text widget |
| images | external converters, five processes at once | Tk photo images |
| tables | none (`<tr>` is a line break) | begun |
| its lesson | a browser composed of other programs | a browser as a typed, extensible program; applets sandboxed by capabilities |

Neither builds a tree -- both format as the tokens stream by, which
was the norm before CSS and scripts needed the whole document. TinyMosaic
builds one (the plan says why), and borrows MMM's DTD-as-data for tag
soup and Mothra's run list as what a paragraph is before its lines are
broken.

## Where TinyMosaic actually sits

Beside *Web Browser Engineering*, in scope: fetch, parse, style,
layout, paint, click, history, pictures, forms; and it stops before
JavaScript. It differs in being after one original (Mosaic's web,
1993, is small enough to do all of), in running natively and in a
browser from the same source, in its pieces being the house's own
(HTTP both ways and its server, the event loop, GIF, PNG and JPEG,
Hershey's strokes, Knuth-Plass), in a view per stage of the pipeline,
and in putting the simple and the better version of a stage side by
side (greedy and optimal lines, images reflowing and reserved). What
came after Mosaic -- pictures fetched several at once, threads, tables,
CSS -- is TinyNetscape's (plan_browser_teaching.md). It will not read
much of today's web: `https://` only through curl, no scripts, no
style sheets. That is the lesson's shape, not its failure -- the
notes' last section says what the missing half is for.
