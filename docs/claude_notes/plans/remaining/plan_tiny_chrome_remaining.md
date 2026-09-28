# Plan: what's left for TinyChrome

The TinyChrome plan is done: see
[`done/plan_tiny_chrome.md`](done/plan_tiny_chrome.md) -- its survey of
what the web asks, its phases C0 to C10 (CSS read, the cascade, the box
model, resources, flexbox, SVG, the browser, the ES5 core, video and
sound, speed), each phase's status, and "What it actually took", the
fixes the live pages forced that the survey did not predict -- and its
tutorial, [`notes_css_engine.md`](../tutorials/notes_css_engine.md).
About 5,200 lines in the new modules against the 5,800 budgeted (5,000,
and 800 for the ES5 core). What's left, roughly from most to least
worth doing. As before: a named site for each addition, the facts
first (the specification's section, a page measured), a worked
example in the `.mli` tested, the live page tried by hand and a page of
our own frozen as a golden frame.

## 1. What the plan decided and did not do

- **A saved Wikipedia article as a golden frame** (the plan's
  Decisions: "our own look-alike pages and one saved Wikipedia article
  as golden frames"): only our own pages are frozen; the article and
  its two sheets were probed by hand, never committed. Its
  license allows it (CC BY-SA, the attribution in the page); a few
  hundred kilobytes in `apps/internet/site/`, served by `about:`, would
  keep the cascade, the box model and the speed honest on a real page.
- **The per-site setting's other sites**: only Hacker News is on by
  default and tried; Project Gutenberg's menus (`addEventListener` on
  `window`, done), Craigslist (`window`'s size, `navigator`), the
  footers' `new Date()` -- each tried live, added to `default_allowed`
  if it works.
- **The survey's other sites**, not tried since C1: CNN Lite (its
  1,309 `var()`), BBC News (grid, 60 of them), Craigslist, Space Jam
  (1996, its tables and its GIFs); each a line in the plan's "What it
  actually took" when tried.

## 2. Speed: what C10 left

- **The page's shapes built lazily** (0.25 s of a Wikipedia relayout's
  0.3): `Browser_draw.drawn`'s shapes as `Lazy.t`, forced for what the
  window shows -- the four browsers' views change with it.
- **A relayout only where it changed**: a picture arriving moves what
  is below it; the boxes above could be kept (engines' "dirty bits").
- **Hershey's widths memoized** (`Browser_text.metrics`: 145,000 calls
  for an article before the measures' memo, not counted since): count
  again first.
- **The cascade's first run** (0.3 s for Wikipedia, 1 s for GitHub's
  4.9 MB of CSS): the index by rightmost key and the ancestor filter
  are done; next, sharing styles between siblings (WebKit's style
  sharing) and a Bloom filter for the ancestors.

## 3. Layout

- **Grid** (CSS Grid Layout Level 1's common part: `grid-template-
  columns` with `fr` and `repeat()`, `grid-area` by names, gaps):
  Wikipedia's contents beside its article, BBC News' page. The plan's
  exercise; Flex_layout's way -- the arithmetic apart
  (`Grid_layout`), Box_layout laying the items out.
- **An inline element's own box, finished**: done for backgrounds,
  borders and paddings (C4); not `vertical-align` on an empty
  inline-block (about:chrome's arrow and bell sit on the baseline), a
  picture's own border and padding (HN's logo's white frame),
  `position: relative` on an inline (the badge's `top: -6px`).
- **Tables**: `rowspan=`, `border-collapse`, `table-layout: fixed`, a
  column's `width=`.
- **Positioning**: `bottom` and `right` with `top` and `left` auto,
  fixed boxes staying on screen as the page scrolls, `z-index`, sticky.
- **Clipping by pixels**: `overflow` clips by culling what is drawn
  (whole lines and words); a clip shape in the playground would cut
  exactly (every backend's).
- **Flexbox's rest**: `order`, baseline alignment, `align-content`.

## 4. Style

- **Fonts**: Hershey's one face stands for serif, sans-serif and
  monospace; web fonts are out of scope, but the three families as
  three faces (Hershey has a serif set) would already change a page.
  The letters Hershey lacks (é, –, ©, ▶): drawn from their decomposition
  or a fallback.
- **Gradients** (`linear-gradient`: Wikipedia's fades, GitHub's
  buttons), `border-radius`, `box-shadow`, border styles (dashed,
  inset): the drawing's, `Browser_boxes`.
- **`::before` and `::after`** with a string `content` (C1 parses
  them; nothing draws them): icons in a font, quotes, counters.
- **`:hover`**: restyling under the pointer (the menus of every site);
  the cascade again at each move, or only the rules that name `:hover`.
- **Media queries' rest**: `prefers-color-scheme: dark` switched from
  the browser (Wikipedia's night mode), `min-resolution`.

## 5. JavaScript

- **Promises and `then`** (`fetch(...).then(r => r.json())`: HN's hide,
  every modern site's), microtasks run after each task;
  `XMLHttpRequest`'s answer (`onload`, `responseText`) -- the tab's
  message back to the page's world.
- **`class`**, template literals, `switch`, `for...in`, getters and
  setters, `JSON.parse`, spread and destructuring: ES2015's common
  part, for the next tier of sites.
- **The elements' methods for players**: `play()`, `pause()`,
  `currentTime`, `ended` (C9 plays by clicks only).
- **Cookies** (C5's decision): no site of the plan needed one yet;
  `Playground.Http.get` taking request headers first (Cmd, both
  platforms, the HTTP client), then a jar per tab (RFC 6265's parsing
  and matching, `libs/networking/protocols/Cookie`, pure).
- **Location changes**: `location.href = ...`, `history.pushState`.

## 6. The browser

- **The omnibox**: its suggestions (the history, the visited), the
  search engine chosen in a settings page; a bookmarks bar.
- **The developer tools' rest**: the computed values (not only the
  declarations), the box model's diagram (margin, border, padding,
  content), editing a declaration live, the console (TinyFirefox's) in
  the panel, a request's headers and body in the network view.
- **Tabs' rest**: dragging, a tab's own history of closed tabs, a
  background tab's timers slowed rather than stopped.
- **Players**: seeking by the bar, volume and muted, full screen; a
  sound deck per player (two playing at once).

## 7. Beyond: the real formats

Plans of their own, as the video plan's "remaining" says
(`plan_video_remaining.md`): the **MP4** container with **H.264
Baseline** and **AAC-LC** (YouTube's classic 360p stream, many phones'
files), or **WebM** with **VP8** and **Vorbis** (Wikimedia Commons');
**TLS** of our own, so that https:// needs no curl
(`plan_dependencies_remaining.md`); **PNG and JPEG progressive** as
the web serves them (the images plan's remaining).
