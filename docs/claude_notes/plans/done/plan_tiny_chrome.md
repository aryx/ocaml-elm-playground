# Plan: TinyChrome, a small browser for the real web

## Context

The browsers so far teach the web's history on pages of our own:
TinyMosaic (HTML 2.0), TinyNetscape (Netscape's extensions, tables,
CSS1, threads), TinyFirefox (the page's JavaScript, a panel after
Firebug) -- `plan_browser_teaching.md`, `plan_tiny_firefox.md`. None of
them would show today's web well: its style sheets are hundreds of
kilobytes, laid out with flexbox and custom properties, drawn with SVG.

**TinyChrome** is the family's complete one, as TinyOffice is the office
suites': the browser that tries to **work on real, famous websites** --
Hacker News, Wikipedia, Google's home page, GitHub, the text sites of
the news -- and render them **mostly correctly**. Not the Acid tests;
enough to be recognizable and readable, the layout roughly where the
authors put it. And still **small**: a budget in lines, set below, and
each piece a module one reads, as everywhere in this repository. Chrome
itself (2008: a process per tab, V8, the omnibox, tabs on top) is tens
of millions of lines; TinyChrome aims at about five thousand beyond
what the three before it built.

## What the web actually asks: a survey

Fetched on 2026-09-25 with a desktop user agent, what each page sends
and how much of it can be read without running its JavaScript (words
of text outside scripts and styles):

| site | HTML | words | style sheets | layout it uses | pictures | without JS |
|---|---|---|---|---|---|---|
| info.cern.ch | 1 KB | 90 | none | plain | none | all |
| Space Jam (1996) | 6 KB | 23 | 1 | tables | 15 GIFs | all |
| Berkshire Hathaway | 5 KB | 187 | inline | a table | 1 | all |
| text.npr.org | 6 KB | 266 | inline | plain | none | all |
| Craigslist | 60 KB | 743 | inline | flex (1) | none | all |
| Hacker News | 34 KB | 693 | 1 file, 109 rules | tables, `@media`, `[attr]` | SVG logo, GIF | all |
| gnu.org | 31 KB | 1,135 | 2 | plain, floats | 12 | all |
| Project Gutenberg | 26 KB | 456 | 1 | a table | 3, 1 SVG | all |
| lite.cnn.com | 333 KB | 1,363 | inline | flex (5), **`var()` (1,309)** | none | all |
| Wikipedia (an article) | 620 KB | 8,639 | 2 files + 16 inline, **1,391 rules** | flex (39), grid (4), floats (42), `calc()` (169), **`var()` (553)**, `:not()` (398), `>` (219), `::before/after` (179) | JPEG, PNG, **SVG** | all: designed so |
| Google (home) | 88 KB | 22 | inline, 219 rules | inline-block, flex, absolute, `var()` | PNG, inline SVG | the search form |
| GitHub (a repository) | 445 KB | 1,313 | **41 files** | flex, grid | **242 inline SVG icons** | nearly all |
| BBC News | 378 KB | 1,604 | inline | flex (30), **grid (60)** | 42, 32 SVG | most |
| old.reddit.com, Stack Overflow | -- | 7, 9 | -- | -- | -- | a bot check, not the page |
| X (Twitter) | 35 KB | 82 | -- | -- | -- | nothing: a JavaScript application |
| YouTube | 931 KB | **24** | -- | -- | -- | nothing: 896 KB of the 931 are script |

So three tiers, the targets of this plan:

- **Right** (as a browser shows them, near enough): info.cern.ch, Space
  Jam, Berkshire Hathaway, text.npr.org, Craigslist, Hacker News,
  gnu.org, Project Gutenberg -- tables, plain HTML, a little CSS.
- **Mostly right** (readable, recognizable, blocks where they belong,
  some misplaced): Wikipedia, lite.cnn.com, Google's home page, GitHub,
  BBC News -- they need a real CSS engine: `@media`, custom properties,
  `calc()`, modern selectors, the box model, flexbox; and SVG.
- **Out, and said so**: YouTube, X, Google Maps, Facebook (applications
  made of megabytes of modern JavaScript: our engine is a teaching
  subset), and the sites behind a bot check. Google's *search results*
  too: Google has required JavaScript for search since early 2025, so
  the omnibox searches with DuckDuckGo's HTML version
  (html.duckduckgo.com), made for browsers without it.

**What the other new engines found.** Servo and Ladybird, the two
engines written from scratch in the 2020s, test on real sites more than
on lists: Ladybird's monthly newsletters fix one named site after
another (Roundcube, Reddit, Google Sheets), and neither publishes a
list of simple sites. But a review of Servo (Corbin Davenport,
spacebar.news) found exactly our first choices working perfectly --
Wikipedia, CNN Lite, text-only NPR, a personal site -- while Google's
results overlapped and MacRumors crashed: the tiers above, confirmed.
The list is meant to **grow**: a site added to the survey when a phase
makes it work, with what it needed written next to it.

**GitHub, a named goal** (the author's wish). A repository page is
server-rendered -- its file list, its README (Markdown made HTML by
GitHub), a file's code as a table of lines -- so it reads without its
JavaScript (1,313 words). What it asks: 41 style sheets, flex rows,
`var()` and `prefers-color-scheme`, and **242 inline SVG icons**: SVG
written in the HTML itself, so the tree builder must take `<svg>` as
foreign content (its subtree kept whole, not parsed as HTML, then drawn
as a picture by `Svg`). The pages to reach, in order: a repository's
front page, its README, a file, an issue.

## What it takes

What the pages above use, in the order it matters, each with its
lines budgeted:

1. **A real CSS parser** (`Css_syntax`, ~400). N5's `Css.parse` splits
   on braces: it loses `@media` blocks (most of Wikipedia's sheet) and
   breaks on a string or a `url()` holding punctuation. CSS Syntax
   Level 3's tokenizer and its rules, as written.
2. **The cascade, grown** (`Selectors`, `Cascade`, `Css_values`,
   ~900): selectors of Level 3 (`>`, `+`, `~`, attributes, `:not()`,
   `:first-child`, `:last-child`, `:link`, `:visited`, `::before` and
   `::after` with a string `content`), specificity; `@media` evaluated
   against the window (width, `screen`, `prefers-color-scheme:
   light`); `!important`; **custom properties** and `var()` (without
   them, Wikipedia's and CNN's colours and sizes all fall back to
   nothing); `calc()` of lengths; the rules **indexed** by their
   rightmost part (id, class, name), as engines do -- 1,391 rules tried
   on every one of an article's thousands of elements is too slow.
3. **Computed styles** (`Computed`, ~400): a record per element --
   display, the box's four margins, borders and paddings, width,
   height, min and max, box-sizing, colour and background, font size,
   weight, style and family (serif, sans-serif, monospace: Hershey's
   faces), line height, text alignment, decoration and transform,
   white-space, vertical alignment, float, clear, position and its
   offsets, overflow, visibility, list style -- inherited or initial as
   the specification says, lengths resolved to pixels. And Looks' table
   (Mosaic's, TinyMosaic's and TinyNetscape's) written as a real
   **user-agent style sheet** in CSS (`ua.css`), N5's exercise made
   necessary: every look now comes through the cascade.
4. **A box-model layout** (`Box_layout`, ~900; `Flex_layout`, ~400),
   beside `Html_layout`, which stays as it is for the teaching
   browsers: CSS 2.1's block and inline formatting with margins,
   borders and paddings, widths and auto margins (centring),
   `inline-block`, lists, floats (TinyNetscape's, generalized), relative
   positioning and a rough absolute and fixed one (the box put where
   its offsets say, over the rest), `overflow: hidden` as a clip;
   tables through `Table_layout`, reused; **flexbox**'s common subset
   (a row or a column, wrap, grow and shrink, `justify-content`,
   `align-items`, `gap`). **Grid** is laid out as blocks, one below the
   other: on Wikipedia and the BBC that puts a sidebar above the
   article instead of beside it -- "mostly right", said so; the grid
   is an exercise.
5. **SVG, a small part** (`graphics/images/svg/`, ~500; and `<svg>`
   in HTML kept whole by `Html_tree`, the WHATWG's foreign content,
   ~50): what logos and
   icons use -- `rect`, `circle`, `ellipse`, `line`, `polyline`,
   `polygon`, and `path` with its commands (M L H V C S Q T A Z,
   absolute and relative), fill and stroke colours, `viewBox`,
   `transform`'s translate and scale -- rasterized by `graphics/2d`
   into an Rgba_image, so a picture like any other. For Hacker News'
   logo, Wikipedia's wordmark, GitHub's 242 icons, Google's logo.
   Gradients, text, filters and masks: not.
6. **Resources** (`Browser_resources`, ~300): a page's `<link
   rel=stylesheet>`s and `@import`s fetched like its pictures, the page
   laid out again as each arrives; compressed answers (gzip: our own
   `Inflate` for `http://`, curl's for `https://`); **cookies** (a jar
   per session, sent back to their site: Google's consent redirect in
   Europe needs one); `srcset`'s first picture; `<meta charset>`.
7. **JavaScript, honestly** (`Browser_script`): most real sites' code
   is ES2020 and more -- classes, promises, modules, bundles of
   megabytes -- which our engine (a teaching subset) refuses. So on
   `http(s)://` pages scripts are **off by default** (the author's
   decision, 2026-09-25), as in Chrome with JavaScript disabled, and
   `<noscript>`'s content is shown (Wikipedia and others write for
   that); the built-in pages keep theirs. And a few famous sites whose
   scripts are small and old-fashioned get theirs run (next section).
8. **The browser** (`TinyChrome.ml`, ~700): Chrome's window --
   **tabs** on top (a list of `Browser_tab`, finally: TinyFirefox's
   exercise), the **omnibox** (an address, or words searched), back,
   forward, reload; and the developer tools grown from TinyFirefox's
   panel: the console, the elements with an element's **computed
   styles** (where each value came from: which rule, which sheet), the
   network (each resource, its size, its time).

About 5,000 lines in all, and about 800 more for the ES5 core (below)
-- the most in the family, still a reading.

## Famous sites with simple scripts

Some famous pages carry small scripts of the old kind -- functions,
`var`, the DOM -- which a slightly larger engine runs. Measured
(2026-09-25):

| site | its script | what it does | what our engine lacks |
|---|---|---|---|
| Hacker News | `hn.js`, 5 KB, ES5: 43 functions, 26 `var` | folds a comment thread (`[-]`), votes | `Array.prototype.x.call(...)` (prototypes, `call`, `apply`), `getElementsByClassName` and `ByTagName`, one regular expression, `XMLHttpRequest` for the vote |
| Project Gutenberg | inline, 0.9 KB, `const`, arrows | its menus' clicks | `addEventListener` on `window` |
| Craigslist | inline, 1.7 KB | reads `window`'s size, a preference | `window`, `innerWidth`, `navigator` |
| Space Jam, Berkshire Hathaway | inline, 0.2 KB each | the year in a footer; analytics | `new Date()`, `getFullYear`; the analytics fail harmlessly |

So, for these, a **site's scripts on**: Chrome's per-site setting
("JavaScript allowed for news.ycombinator.com"), a list the omnibox's
page icon toggles, Hacker News and Gutenberg on it by default. And the
**ES5 core** the engine then needs (`libs/languages/javascript`,
~800 lines more, its own tests):

- `new`, constructor functions and **prototypes** (a property looked up
  on the object, then its prototype, up the chain),
  `Function.prototype.call` and `apply`, `Array.prototype`'s methods
  reachable so, `Object.create`; the strings' and arrays' methods moved
  onto their prototypes (`plan_tiny_firefox.md`'s exercise, done);
- `var` as it is (hoisted to its function), `==` with its conversions,
  `arguments`;
- **regular expressions**, a backtracking matcher of their common part
  (classes, `*` `+` `?` `{n,m}`, groups, alternation, `^` `$`, `\d`
  `\w` `\s`, the `g` and `i` flags) for `test`, `exec`, `match`,
  `replace`, `split` (~350);
- `Date` (the time of the page's clock, and its fields), `setTimeout`
  as it is, `window` (the global object, its size), `navigator`;
- the DOM's `getElementsByClassName`, `getElementsByTagName`,
  `classList`, and `XMLHttpRequest` / `fetch` for a GET over
  `Cmd.Http_get` (Hacker News' vote, with the session's cookies).

Hacker News' comment folding is the demo: a thread's `[-]` clicked, its
replies hidden by a class and the site's own style sheet, its `hn.js`
run by our engine as the site wrote it.

## Video and sound: `<video>`, `<audio>`, and a site of our own

(The real YouTube is ruled out -- the author, 2026-09-25 -- for the
reasons below; the elements and a site of our own stay.)

YouTube is the web's most visited page after Google, and the reason
many people think of a browser at all. The real one is out of reach
(above: an application of megabytes of JavaScript, its streams' URLs
signed by that code, H.264, VP9 or AV1 video and AAC or Opus sound,
and terms that forbid fetching them any other way). But what a video
site asks of a browser is a fine test of the rest of this repository,
which already reads video and sound:

- **`<video>` and `<audio>`** (`Browser_media`, ~300): the element's
  file fetched, recognized by its bytes (as TinyMediaPlayer does, over
  `media_player`'s `Media`), decoded by our own readers -- **MPEG-1**
  video with its **MP2** sound (`.mpg`, `graphics/videos/mpeg1` and
  `mpeg_system`), Motion JPEG in AVI, Y4M, FLI and FLC, animated GIF;
  **MP3**, MP2, WAV and MOD for `<audio>` (`audio/formats`) -- and
  played in the page's box: a frame drawn as a picture (`bitmap`),
  the sound through the Playground's audio, the controls (play, pause,
  the time bar) drawn by the browser, `autoplay`, `loop`, `muted`, and
  the element's methods for the scripts (`play()`, `pause()`,
  `currentTime`).
- **A video site of our own, `about:tube`** (the built-in site): a page
  of thumbnails -- each a frame of its clip -- in a grid of flexbox, a
  watch page with the player, a title and a description, a list of the
  next ones; its clips our own (`Our_media`'s, drawn by
  `graphics/2d`, encoded by our MPEG-1 encoder), so nothing is fetched
  and the golden frames are ours. It exercises at once the layout
  (flex, inline-block, the box model), the pictures, the codecs, the
  audio, and the Playground's frame clock.
- **Toward the real formats**, plans of their own
  (`plan_video_remaining.md`, the video plan's "a plan of their own"):
  the **MP4** container (ISO BMFF's boxes: IFF's chunks again, ~300)
  with **H.264 Baseline** (CAVLC, no B pictures, no CABAC: the
  profile of YouTube's classic 360p stream and of many phones' files;
  MPEG-1 is its skeleton, ~3,000) and **AAC-LC** sound (~1,500); or
  **WebM** with **VP8** and **Vorbis**, what Wikimedia Commons serves.
  Each would let `<video>` play files of the real web; none is in this
  plan.

## Where it goes

- `libs/web/css/` (new folder of `web_style`): `Css_syntax`,
  `Selectors`, `Css_values`, `Cascade`, `Computed`, `ua.css` (embedded
  by dune); N5's `Css` rewritten over them, `Looks` kept for the
  teaching browsers.
- `libs/web/layout/`: `Box_layout`, `Flex_layout`, beside
  `Html_layout` and `Table_layout`.
- `libs/graphics/images/svg/`: `Svg`, a library like `png/` and `gif/`,
  `Image_decode` recognizing SVG's text.
- `libs/networking/protocols/`: `Cookie` (RFC 6265's parsing and
  matching, pure); `Http_client` and `Http_request` asking for gzip.
- `appkits/browser/`: `Browser_resources`, `Browser_media`,
  `Browser_devtools` (the panel's three views).
- `apps/internet/TinyChrome.ml`, and `site/tube/` for `about:tube`.

## Phases

- **C0, this plan** and its companions: `notes_css_engine.md` (the
  cascade and the box model as an engine sees them, with the worked
  examples the tests will check) and a corpus: the survey's pages
  saved, for trying by hand.
- **C1, CSS read**: `Css_syntax` (tokens, rules, at-rules, blocks),
  `Selectors` (Level 3, specificity), N5's tests kept passing.
- **C2, the cascade**: `@media`, `!important`, custom properties,
  `calc()`, the rule index, `Computed`, `ua.css`; TinyChrome's first
  window over `Html_layout` fed by computed styles where it can.
- **C3, the box model**: `Box_layout` (block, inline, inline-block,
  margins, borders, paddings, widths, lists, floats, positioning,
  overflow), tables through `Table_layout`: the first tier right.
- **C4, resources**: style sheets fetched, gzip, cookies, `srcset`,
  JavaScript off by default and `<noscript>`: Hacker News and
  Wikipedia fetched live.
- **C5, flexbox**: `Flex_layout`: Google, CNN Lite, GitHub mostly
  right.
- **C6, SVG**: the logos and the icons, `<svg>` inline in HTML;
  GitHub's repository page recognizable.
- **C7, the browser**: tabs, the omnibox (DuckDuckGo's HTML search),
  the developer tools with computed styles and the network.
- **C8, the ES5 core**: prototypes and `new`, `call` and `apply`,
  `var` hoisted, `==`, regular expressions, `Date`, `window`; the DOM's
  `getElementsBy...`, `classList`, `XMLHttpRequest`; the per-site
  setting: Hacker News' comments folding with its own `hn.js`.
  Refined by reading `hn.js` (5 KB, 2026-09-25), in two steps:
  - *the engine* (`libs/languages/javascript`): `new`, a prototype
    chain on every object (a function's `prototype` made when first
    read), `instanceof`, `Function.prototype.call`, `apply`, `bind`
    (`Array.prototype.indexOf.call(a, x)`, `slice.call`,
    `forEach.call`), `Object.create`; `var` hoisted to its function;
    `==`'s conversions; `arguments`; regular expression literals and a
    backtracking matcher, `Js_regexp` (`s.match(/[0-9]+/)` runs at
    load: without it the script dies on its first page); `splice`,
    `lastIndexOf`, `replace`, `match`, `search`, `split` by a regex,
    `charCodeAt`, `encodeURIComponent`; `Date` on a clock the host
    gives;
  - *the page* (`Browser_script`): `getElementsByClassName` and
    `ByTagName` (the document's and an element's), `nextElementSibling`,
    `nextSibling`, `classList`, an `a`'s `href` resolved, `event.target`,
    `stopImmediatePropagation`, `scrollIntoView` (nothing to do),
    `window` (the global object) and `location`, `new URL(href, base)`
    and its `searchParams`, `XMLHttpRequest` (a GET queued for the tab,
    its answer dropped: HN's vote, which needs a login anyway) and
    `fetch` (a promise that never settles: HN's hide, no promises
    here); then TinyChrome's per-site setting (Hacker News on).
- **C9, video and sound**: `<video>`, `<audio>`, `about:tube`.
- **C10, speed**: a Wikipedia article read, styled and laid out in well
  under a second natively; what the web build can do (its fetches are
  limited: below).

## Status

- **C0 done** (2026-09-25): this plan (the survey, the tiers, the
  budget), and `notes_css_engine.md`, the engine's tutorial, each
  section with the worked example its tests check.
- **C1 done** (2026-09-25): `Css_syntax` (CSS Syntax Level 3's tokens,
  blocks by matching brackets, rules and at-rules, declarations with
  `!important`, errors skipped as the spec says; values written back as
  text), `Selectors` (Level 3: descendant, child and sibling
  combinators, attribute tests, `:not()`, `:first-child`,
  `:nth-child`, `:link`; specificity; matched right to left with
  backtracking; pseudo-elements set apart). N5's `Css` rewritten over
  them, its interface kept (a rule now knows its `!important`
  declarations, which the cascade puts over the rest): TinyNetscape's
  and TinyFirefox's frames unchanged. Tests: the notes' worked examples,
  `Unit_css_syntax` and `Unit_selectors`. About 700 lines, within C1's
  budget. The style modules stay in `libs/web/style/` (not a `css/`
  folder: one library, three modules more).
- **C2 done** (2026-09-25): `Css_values` (a length computed as pixels
  plus a percentage of the containing block, which `calc()` adds to;
  `min()`, `max()`, `clamp()`; the colours of CSS Color 4, the 148
  names; `var()` substituted with its fallback, cycles refused),
  `Cascade` (`@media` evaluated against the window, `@supports` taken
  as true, origins and importance, the rules indexed by their rightmost
  id, class or name, and WebKit's **ancestor filter**), `Computed` (a
  record per element, inherited or initial per property, `inherit`,
  `initial`, `unset`; the shorthands expanded; the custom properties
  inherited), `ua.css` (CSS 2.1's appendix D, embedded as `Ua_sheet`).
  On a saved Wikipedia article (5,637 elements, its 1,557 rules for this
  window): the body's text #202122 on #f8f9fa in sans-serif, the title
  in 28.8 px serif, links #36c through `var()` -- Wikipedia's own
  values; the cascade 0.54 s (0.95 before the ancestor filter), the
  computed styles 0.2 s more: C10 has the rest to win. Tests:
  `Unit_cascade`, the notes' examples. TinyChrome's first window moves
  to C3: shown through `Html_layout` it would need an adapter thrown
  away one phase later. About 900 lines.
- **C3 done** (2026-09-25): `Box_layout` (CSS 2.1's box model over
  `Computed`: the horizontal equation with its autos, max-width solved
  again, box-sizing; margins collapsing between siblings, through a
  parent, through an empty block; inline content on lines with each
  element's line-height and the block's strut, white-space, sub and
  super; inline-blocks and floats shrunk to fit; clear; a formatting
  context narrowed beside a float; relative, absolute and a rough
  fixed; tables through `Table_layout`, a row's background under its
  cells; list markers; HTML's "align descendants", a `<center>`
  centring its blocks), seen as an `Html_layout.box` for `Hit` and the
  controls; `Browser_boxes`, its drawing (backgrounds, borders, the
  words by `Browser_draw.glyphs`); `Browser_page`'s setting `boxes`;
  `Cascade`'s **presentational hints** (bgcolor=, width=, align=,
  cellpadding=, `<font>`...) and `Computed`'s **quirks mode** (a page
  without a DOCTYPE: a table's fonts and alignment not inherited). And
  **TinyChrome's first window**: one tab, the omnibox, the status
  bubble; `about:chrome`, a page in today's CSS, its golden frame.
  Tried live: Hacker News laid out as in Chrome, its 85% table centred
  in its `<center>`, rows and ranks in place -- what is missing is
  `news.css` (unfetched: C4: the spacing, black header links, the vote
  arrows) and the logo (SVG: C6). Speed: a frame costs the software
  rasterizer what TinyFirefox's does (0.08 s, Hershey's strokes), the
  layout a small part. Not done, in `Box_layout.mli`: an inline
  element's own margins, borders, paddings and background (the
  missing yellow of about:chrome's "C3" badge: a first thing for C4 or
  C5), rowspan=, z-index. About 1,000 lines with TinyChrome's 360.
- **C4 done** (2026-09-25): the page's **style sheets fetched**
  (`Browser_page.sheets_wanted`: its `<link rel=stylesheet>`s whose
  `media=` holds, not the alternates, then the `@import`s of those that
  have come, resolved against their sheet, four deep; `Browser_tab`
  queues them ahead of the pictures, in its `sheets` cache, the page
  laid out again as each arrives); **scripts** per URL (`config.scripts`
  a function: TinyChrome's run on the built-in pages only, `<noscript>`
  shown on the web as nothing hides it); `srcset=`'s first address
  (`Box_layout.picture_src`); an **inline element's box** (its margin,
  border and padding as spacer words, its background and border drawn
  under its words: `backdrops`, the badge's yellow); `overflow: auto`
  and `scroll` clipping, drawn by culling in `Browser_boxes` (no clip in
  the playground: a line or a word not wholly inside left out -- the
  screen-reader text in 1-pixel boxes gone, Wikipedia's table of
  contents cut at its column's bottom); `opacity: 0` not drawn (styled
  checkboxes); `overflow`'s two values; HN's spacer rows (a row's own
  height, an empty row counted); no `<center>` centring while measuring
  (HN's vote column had swallowed the page); curl's User-Agent
  (`Commands.ml`: Wikipedia refuses a request without one). Tried
  live: Hacker News as in Chrome but for its SVG logo and vote arrows
  (C6); a Wikipedia article right -- its text, links, infobox floated
  with its picture -- above it the header, the tabs and the contents
  stacked where flexbox and grid put them side by side (C5) and the
  icons (SVG masks, C6). `about:chrome` now gets its style from
  `chrome.css`, which `@import`s `chrome-colours.css`: the golden frame
  goes through the fetching. **Not done, and why**: gzip -- neither curl
  nor our client asks for it, so servers answer uncompressed (bandwidth,
  not correctness); cookies -- `Playground.Http.get` takes no request
  headers, a change of the playground's API (Cmd, both platforms, the
  client) that neither Hacker News nor Wikipedia needs: C5, with
  Google, whose consent page does. About 400 lines.
- **C5 done** (2026-09-25): `Flex_layout`, flexbox's arithmetic on
  numbers as `Table_layout` is a table's (the lines, section 9.7's
  resolving of flexible lengths with its freezing, the placing along by
  justify-content or the auto margins, across by align-items), and
  `Box_layout`'s flex containers over it (items blockified, a run of
  text an anonymous item, a base size measured, a minimum measured only
  when the line is too full; rows and columns, wrap, gaps, a single
  line as tall as its given container). What measuring does
  differently, found on the live pages and written in
  `Box_layout.mli`: a percentage width is auto (Wikipedia's
  `width: 100%` menus had made a toolbar half the page), a right float
  on the left, a flex row neither growing nor shrinking, its last
  margin counted; and `display: none` table cells out of the grid
  (GitHub's small-screen cells), min-height and max-height of the
  border box with `box-sizing: border-box` (Google's button), a
  hundredth of a pixel's slack in filling a line. `about:chrome`'s
  header a flex row, a card of tiles (`flex: 1, 2, 1`) and wrapping
  chips: the golden frame. Tried live: **Wikipedia**'s header, tabs and
  toolbar in their rows (its contents still above the article: grid);
  **Google**'s home page as in Chrome, its no-script version; a
  **GitHub** repository recognisable (its header the mobile one below
  1,012 pixels, as in Chrome; the files' messages and dates written by
  React, so missing without scripts). **Cookies not done, still**:
  Google's consent page never came (the address decides), and its
  search needs JavaScript whatever the cookies (its `<noscript>` sends
  to "enable JavaScript") -- no site of the plan needs one yet, so the
  playground's API keeps its shape until one does; the omnibox's
  searches will be DuckDuckGo's HTML version (C7). About 450 lines.
- **C6 done** (2026-09-25): `Svg` (`graphics/images/svg/`, library
  `graphics_svg`: a small XML reader; rect, circle, ellipse, line,
  polyline, polygon and path with all its commands, arcs by the spec's
  endpoint-to-centre conversion, curves by `Curve.flatten`;
  transforms, the viewBox, fill and stroke, their rule, opacities,
  `style=`, currentColor; each shape's coverage drawn white on black
  by `Fill.polygons_aa` -- the framebuffer has no alpha -- then laid
  "over" an RGBA picture). The author's decision: `graphics_core` and
  `graphics_2d` moved to the package `elm_playground` (their
  `(package)` field only), so that `graphics_svg` and the browsers
  (in a browser too) use them. In the browsers: an SVG file is a
  picture (`Browser_picture`); an inline `<svg>` a replaced element,
  kept whole by `Html_tree`'s **foreign content** (`/>` honoured, no
  HTML rule closing inside), drawn from its own tree in its text's
  colour, cached by element (`Browser_boxes`); **background-image**
  (fetched with the pictures, a sheet's `url()`s resolved against the
  sheet, queued again as each sheet arrives; drawn at its size, shrunk
  to fit) and **mask-image** (the picture tinted with the background's
  colour); **data: URLs** (`Browser_url.data_url`); `Selectors`'
  `:is()`, `:where()`, `:enabled` (Wikipedia's quiet buttons' rule had
  been dropped for one `:enabled` in its list); a hidden word's picture
  not drawn (GitHub's hidden menus' icons); no Mosaic frame round a
  picture in a link. Tried live: Hacker News' "Y" and vote arrows,
  Wikipedia's wordmark, tagline and icons, GitHub's octicons.
  `about:chrome`'s SVG card (Chrome's logo inline, a `data:` star, the
  arrow as a background, a bell as a mask) is a second golden frame
  (`TinyChrome_svg`, Space scrolling a screen). Not done: gradients,
  `<use>`, text in SVG, a picture's own border and padding (HN's
  logo's white frame), vertical-align on an empty inline-block. About
  700 lines.
- **C7 done** (2026-09-25): **tabs** (a list of `Browser_tab`, each
  with an id its answers carry, + and x in the strip); the **omnibox**
  (an address, or words searched: Wikipedia's search by default --
  DuckDuckGo's page without scripts worked, then soon answered a
  program asking again and again with its duck-picking challenge, as
  did Mojeek; `search=duckduckgo` keeps it; Google's needs
  JavaScript); `<meta http-equiv=refresh>` followed (a second or less;
  one in a `<noscript>` only when the page's scripts do not run --
  DuckDuckGo's links); the **developer tools** (F12 or the wrench):
  Elements -- Inspect, then an element of the page: its path, box
  (outlined), children, and its styles, each winning declaration with
  its rule's selector and its sheet's name (`Cascade.explain`,
  `Browser_page.explain`, the sheets named) -- and Network (the tab's
  log of requests, `Browser_tab.requests`, timed by TinyChrome's clock
  as the log changes), all as lines of text from `Browser_devtools`.
  And a positioned `<select>` stays a control (Wikipedia's search
  page). Golden frames: `TinyChrome_elements`, `TinyChrome_network`.
  About 600 lines.
- **C8 done** (2026-09-25): the ES5 core in the engine -- every object
  a prototype (`proto`), looked up the chain, a function's `prototype`
  made when first read; `new`, `instanceof`; `Function.prototype.call`,
  `apply`, `bind`, `Object.create`, `getPrototypeOf`, `assign`, the
  constructors' `prototype`s (`Array.prototype.indexOf.call(a, x)`);
  `var` hoisted to its function (a `for`'s `var` one for all its
  closures); `==`'s conversions; `arguments`; `Js_regexp`, a
  backtracking matcher (classes, groups, alternation, greedy and lazy
  repetition, anchors, `\b`, the `g`, `i`, `m` flags, a step budget),
  regular expression literals (the lexer telling a regex's `/` from a
  division's), `match`, `replace` (`$1`, a function), `search`,
  `split`, `test`, `exec`, `RegExp`; `splice`, `lastIndexOf`,
  `charCodeAt`, `substr`, `toFixed`, `encodeURIComponent` and its kin,
  `Date` on the page's clock, `Error` and its kinds as constructors;
  tests `Unit_js_es5`. In the page (`Browser_script`): the DOM's
  additions of the C8 bullet, `window`, `location`, `navigator`,
  `URL`, `XMLHttpRequest` and `fetch` (their GETs sent by the tab,
  their answers dropped); **scripts of their own file** fetched by the
  tab (`<script src>`: an exercise until now) and all run once the last
  has come; TinyChrome's **per-site setting** (a "JS" badge in the
  omnibox; Hacker News on by default; `scripts=`), a click on the page
  given to its scripts first, the shown tab's timers on the frame
  clock. **Hacker News' comments fold** live with its own `hn.js`, as
  the site wrote it: a thread's toggle clicked, `[8 more]`, its
  replies' rows hidden by `news.css`'s `.noshow` (which asked hidden
  rows of a table dropped). `about:threads` does the same with its own
  `threads.js` (ES5 of ours, as HN's is written), a golden frame
  (`TinyChrome_threads`). Not done: promises, `class`, template
  literals, `switch`, getters and setters, an XMLHttpRequest's answer.
  About 900 lines.
- **C9 done** (2026-09-25): `<video>` and `<audio controls>` as
  replaced boxes (their width= and height= hints; 320 by 240; an
  audio's bar 300 by 32; `audio:not([controls])` hidden by `ua.css`),
  drawn black by `Browser_boxes`; their files fetched last by the tab
  (`Browser_tab.media_sources`: src=, or a `<source>`; the `media`
  cache, kind `Media` in the network panel); `Browser_media`, the
  players, in `apps/internet` (the library `internet_media`: the
  media player's library, `media_player`, has no package, and so no
  appkit may use it): a file opened by `Media.open_` when first had,
  paused on its first frame, played by a click or by autoplay, looped
  by loop; one sound deck the mixer pulls (`Audio.instrument`), its
  samples the clock the picture follows, the frame clock without
  sound; the controls (play or pause, a progress bar, the time).
  `Tube`, **TinyTube**, `about:tube`: its index of thumbnails (a
  paused `<video>` each: the clip in MPEG-1 and MP2, Motion JPEG and
  PCM in an AVI, FLC, Y4M, an animated GIF) in a flex grid, an MP3 in
  an `<audio>`, and a watch page per clip (the player with controls and
  autoplay, a title, what its format is, the next ones); TinyChrome in
  a stanza of its own, the teaching browsers not linking the media
  player. Golden frames `TinyChrome_tube` and `TinyChrome_tube_watch`
  (the FLC's: no sound, so the frozen frame clock; a clip with sound
  would follow the mixer). Not done: seeking, volume and muted, the
  elements' methods for scripts, the real formats (MP4, H.264, AAC:
  plans of their own). About 400 lines.
- **C10 done** (2026-09-25): measured first (a probe timing each stage
  on the saved Wikipedia article and GitHub page, the best of 3 runs):
  a Wikipedia article took 7.7 s to read and lay out, and 8.2 s again
  for each relayout -- one per picture and sheet arriving, some two
  minutes a page. Three memos, each exact (the layouts' fragments
  compared before and after, identical): shrink-to-fit's measures per
  layout (89,903 blocks laid out, 98% of them measuring the same
  subtrees again: the layout 2.4 s to 0.1 s), the sheets parsed once per
  address and text, and the last page's computed styles kept while its
  tree and sheets stay the same (a picture's relayout skips the
  cascade). Now: the article read, styled and laid out in 0.7 s, a
  relayout 0.3 s; GitHub's relayout 0.05 s (0.9). `notes_opti_ocaml.md`
  section 11. Next, not done: the shapes of the whole page are built
  at each layout (0.25 s of the 0.3), lazily they would be the
  window's only (`Browser_draw.drawn`, all four browsers'); the
  software rasterizer's frame (0.08 s, Hershey's strokes) is the
  playground's business.

## What it actually took

The survey above predicted what the pages ask; the pages, fetched and
laid out, asked more. Each thing below was found on a live page and
fixed, the page it was found on in parentheses -- the part of the work
no survey showed. Added to as each phase finishes.

- **C2, the cascade**: rules indexed but still slow -- WebKit's ancestor
  filter (Wikipedia, 0.95 s to 0.54 s); `inherit` of a non-inherited
  property.
- **C3, the box model**: max-width solved again with auto margins (the
  column not centred); lines on the left while measuring shrink-to-fit
  (a float as wide as its card); `&nbsp;` joining words; quirks mode
  for a page without a DOCTYPE, a table not inheriting `<center>`'s
  alignment (Hacker News, centred text everywhere); HTML's "align
  descendants", a `<center>` centring its table (Hacker News'
  85% table on the left); the attributes as presentational hints
  (Hacker News is `bgcolor=`, `width=`, `cellpadding=`).
- **C4, resources**: a sheet's `@import`s queued also when it comes
  from the built-in site (the colours lost); no `<center>` centring
  while measuring (HN's vote column swallowing the page); a row's own
  height and empty rows counted (HN's spacers); a User-Agent for curl
  (Wikipedia refusing the request); `overflow: auto` and `scroll`
  clipping, drawn by culling (Wikipedia's contents over its title; the
  screen-reader texts); `overflow`'s two values (`hidden auto`);
  `opacity: 0` hiding (styled checkboxes); inline elements' margins,
  borders, paddings, backgrounds (the badge).
- **C5, flexbox**: while measuring, a percentage width as auto
  (Wikipedia's `width: 100%` menus making a toolbar half the page), a
  right float on the left (the search icon at the far end), a flex row
  neither growing nor shrinking and its last margin counted
  (Wikipedia's tabs); a single line as tall as its container
  (`align-items: center`); `display: none` cells out of a table's grid
  (GitHub's file list off the screen); min-height of the border box
  with border-box (Google's button); a hundredth of a pixel's slack
  in filling a line (a chip's two words on two lines).
- **C6, SVG**: `graphics_core` and `graphics_2d` moved to the package
  `elm_playground` (the author's decision) for the browsers to
  rasterize; Html_tree's foreign content (`<path/>` inside `<svg>`);
  background images queued again as each sheet arrives (HN's arrows
  never fetched); `mask-image` tinted (Wikipedia's icons as black
  squares); `data:` URLs (GitHub's sheets); `:is()`, `:where()`,
  `:enabled` (one unknown pseudo-class dropping Wikipedia's whole rule
  for its quiet buttons); a hidden word's picture not drawn (GitHub's
  hidden menus' icons scattered); no Mosaic frame round a picture in a
  link.
- **C8, the ES5 core**: `<script src>` loaded at all -- the teaching
  browsers ran only inline scripts, and `hn.js` is a file of its own
  (fetched like a sheet, the scripts run once the last has come, in
  order); only the scripts of a JavaScript `type=` (not JSON-LD, not
  modules); a table's rows of `display: none` dropped (HN's folded
  replies, hidden by class on their `<tr>`); the network panel's times
  of requests made before the clock's first tick.
- **C9, video and sound**: the players outside the appkit
  (`media_player` has no package); the golden frames of a clip without
  sound only (the mixer would move one with); the TinyTube logo's
  &#9654; drawn as an inline SVG (Hershey has no such letter).
- **C10, speed**: the plan budgeted speed for a cascade; it was the
  layout's measuring (98% of the blocks laid out) and a relayout per
  picture redoing everything; found only by counting, then kept exact
  by comparing the layouts before and after.
- **C7, the browser**: `<meta http-equiv=refresh>` followed
  (DuckDuckGo's result links); the omnibox searching Wikipedia, not
  DuckDuckGo (its page without scripts, then Mojeek's, soon answer a
  program with a challenge); a positioned `<select>` staying a control
  (Wikipedia's search page).

## Verification

- Each module's worked examples as tests (`libs/web/tests/`, the notes'
  numbers): a selector's specificity, a `var()` resolved, a `calc()`, a
  margin collapsed, a flex row's widths, an SVG path's pixels.
- **Golden frames of pages of our own** built to look like the targets
  -- a table of stories, a search box centred, an article with a
  sidebar and an infobox, a repository page of flex rows and icons --
  each feature frozen as it lands.
- **One real page saved**: a Wikipedia article (CC BY-SA 4.0: its
  attribution kept with it), its HTML and its style sheets as served on
  a date, the golden frame of the most complex page we render.
  Pages of other sites are not stored here: their text and pictures are
  their authors'.
- **The live sites by hand**, a checklist per phase: each target of the
  survey opened natively, a screenshot kept outside the repository, what
  is wrong written down in this plan's status.
- The web build: the built-in site only -- a page loaded from GitHub's
  pages may not fetch another site's (the browser's same-origin rules,
  CORS): TinyChrome is native first, said so.

## References

- Pavel Panchekha and Chris Harrelson, *Web Browser Engineering*: its
  later chapters (the box model, CSS's cascade and inheritance, the
  event loop), and Ladybird's and Servo's blogs for what real sites
  need.
- W3C: CSS Syntax Level 3, Selectors Level 3, CSS Cascading and
  Inheritance Level 4, CSS Custom Properties Level 1, CSS Values and
  Units Level 3 (`calc()`), CSS 2.1 chapters 8 to 10 (the box model,
  the visual formatting model), CSS Flexible Box Layout Level 1, SVG
  1.1's paths and basic shapes, RFC 6265 (cookies).
- Corbin Davenport, "I tried Servo, the undercover web browser engine
  made with Rust" (spacebar.news): the sites that work in a young
  engine.
- Ladybird's newsletters, "This Month in Ladybird" (ladybird.org): a
  site at a time.

## Out of scope

Real JavaScript applications (YouTube, X, Maps), the sites behind a
bot check, Google's search results; CSS grid (laid out as blocks),
animations and transitions, transforms, gradients, shadows, web fonts
(Hershey's faces only), `<canvas>`, `<iframe>`, forms beyond
TinyNetscape's, accessibility, printing; HTTP/2 and HTTP/3, caching,
service workers, WebAssembly; security beyond the same-origin rule
(a process per tab was Chrome's idea: here, one OCaml program).

## Decisions

The author agreed with the assessment and the proposals (2026-09-25):

- **The targets**: the survey's first two tiers, GitHub named among
  them (the author's wish); the real YouTube ruled out, `<video>` and
  `about:tube` in; the list to grow later.
- **The engine may grow** (the author: "but not too much"): each
  addition for a named site, budgeted.
- **JavaScript off** on `http(s)://` pages by default (noscript),
  `<noscript>` shown; the built-in pages' scripts on; and a few famous
  sites with simple scripts run (Hacker News, Gutenberg: the author's
  wish), with the ES5 core that takes.
- **The tests**: our own look-alike pages and one saved Wikipedia
  article as golden frames; the live sites by hand.
- **SVG** in (its ~500 lines): the logos and icons are what makes a
  site recognizable.
- **A new layout engine** (`Box_layout`) beside `Html_layout`, rather
  than growing it: the teaching browsers keep their small layout.
- **The budget**: about 5,000 lines, and 800 for the ES5 core; a phase
  over its budget is stopped and discussed, not grown.
