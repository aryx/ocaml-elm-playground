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
