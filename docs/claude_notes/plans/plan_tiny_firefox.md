# Plan: TinyFirefox, a browser with JavaScript, and libs/languages/

## Context

TinyMosaic (1993) and TinyNetscape (1994-1997) are done
(`plan_browser_teaching.md`): a page fetched, parsed into a tree,
styled, laid out and drawn, with Netscape's extensions to HTML marked
in the tree, threads for what blocks, tables and CSS1. What they cannot
do is what made the web an application platform: **run the page's own
program**. Netscape 2.0 (1995) added JavaScript (Brendan Eich's ten
days, May 1995: Mocha, then LiveScript, then JavaScript), and every
browser since is two things: a document engine and a language engine,
joined by the DOM.

This plan adds the second engine, small, and a third browser to show
it, **TinyFirefox** -- after Firefox (Mozilla, 1.0 in November 2004:
Netscape's code, opened in 1998, reborn), whose Web Console and whose
extension Firebug (Joe Hewitt, 2006) made the page's program and its
live tree things a person could watch.

**The tiny rule.** Not the whole of JavaScript, the DOM and HTML5: a
core small enough to read in an afternoon, in small libraries, a small
appkit and a small program. What is left out is listed, each item an
exercise or a "never", so the line is drawn on purpose.

## The four parts, and what each keeps

| Part | What it is | Kept |
|---|---|---|
| the language | lexer, parser, interpreter: values, objects, closures | a modern-looking core (below) |
| the DOM from a script | `document`, elements, their text and attributes | a dozen calls |
| events and the event loop | clicks, keys, timers, one task at a time | clicks, keys, input, timers |
| reflow | the tree changed, the page laid out again | once per task, reusing `Html_layout` |
| the looks | HTML's, Netscape's extensions, CSS | all of TinyNetscape's: `extensions` and `css` on, and a script's `style` |

### The language: a modern-looking core

Roughly the language of Robert Nystrom's Lox (*Crafting Interpreters*,
2021: JavaScript-like on purpose), with JavaScript's syntax and objects:

- values: numbers (floats), strings, booleans, `null`, `undefined`,
  objects `{a: 1}`, arrays `[1, 2]`, functions;
- `let`, `const` (block scope); `var` read as `let` -- no hoisting, the
  difference noted (hoisting is what makes `var` hard to explain);
- functions: declarations, `function` expressions, arrow functions
  `x => x + 1`; closures; `this` in a method call `o.f()`;
- `if`, `while`, `for (;;)`, `for (x of xs)`, `break`, `continue`,
  `return`; `throw`, `try`/`catch` if cheap;
- operators: arithmetic, comparison, `===` `!==` (`==` read as `===`,
  noted), `&&` `||` `!` `?:`, `typeof`, `+=` and friends, `++`/`--`;
  `+` with JavaScript's string/number coercion and truthiness, the
  famous "wat" cases (Gary Bernhardt, 2012) shown on purpose;
- a newline may end a statement (the simple half of automatic
  semicolon insertion);
- errors with their line: a syntax error, a `ReferenceError`, a
  `TypeError` ("x is not a function").

Built-ins: `console.log`, `Math.floor`/`random`/`max`/`min`/`abs`,
`String()`, `Number()`, `parseInt`, `JSON.stringify` (small: the
console needs to print objects anyway); strings' `length`,
`toUpperCase`, `toLowerCase`, `slice`, `indexOf`, `split`, `trim`;
arrays' `length`, `push`, `pop`, `join`, `indexOf`, `forEach`, `map`,
`filter`.

Left out (exercises, or never): prototypes and `new`, `class`, getters
and setters, `Symbol`, promises, `async`/`await`, `fetch`, regular
expressions, `eval`, `with`, modules, generators, `==`'s coercion
table, the rest of automatic semicolon insertion, template literals,
destructuring, spread; a garbage collector (OCaml's does it) and a JIT.

### The DOM, from a script

- `document.getElementById`, `document.querySelector` (and `All`), over
  `Css.matches` -- the selectors N5 wrote;
- `document.createElement`, `document.title`, `document.body`;
- an element's `textContent` (read, set), `innerHTML` (set: the string
  parsed by `Html_tree`, the same parser as the page's), `id`,
  `className`, `getAttribute`, `setAttribute`, `style.color = "red"`
  (written into its `style=`, which N5 honours), `value` and `checked`
  (a form's field), `parentNode`, `children`, `appendChild`,
  `removeChild`;
- `<script>` elements, inline, run in order when the page is read
  (`<script src=...>` an exercise).

Left out: the node types but elements and text, `Node`'s full
interface, `DocumentFragment`, ranges, selections, shadow DOM,
`<canvas>`, `localStorage`, cookies, `history.pushState`.

### Events and the event loop

- `onclick="..."` attributes (Netscape 2's way, "DOM level 0") and
  `addEventListener("click" | "keydown" | "input" | "change", f)`;
- `event.target`, `event.key`, `preventDefault()` (a link not
  followed, a form not sent); **bubbling**: from the element up its
  parents to the document -- HyperCard's message path (button, card,
  background, stack: `appkits/hypertalk`) under another name, and the
  notes put the two side by side;
- `setTimeout`, `setInterval`, `clearInterval`, on the frame clock, so
  that `-fixed-time` and the golden frames stay deterministic;
- `alert("...")`, drawn in the page as a dialog;
- **one task at a time**: an event's handlers, or a timer's function,
  run to their end (a script cannot be interrupted: the event loop of
  `notes_browser.md` section 14, JavaScript's single thread), then the
  page is laid out again if the tree changed -- once per task, which is
  why a loop of a hundred `appendChild`s reflows once, as in real
  browsers.

### CSS, as in TinyNetscape

TinyFirefox is a modern browser: it honours what TinyNetscape does --
Netscape's extensions (N3), tables (N4), CSS1 (N5: `Css`, the cascade
over Looks' table) -- with `Browser_page.settings` `extensions` and
`css` on. A script adds the two ways of changing the looks a page uses
most: `element.style.color = "red"` (the element's `style=`, the top of
the cascade) and `element.className = "done"` (another rule matches:
the to-do list's crossed-out items). Both change the tree, so both are
seen at the reflow after the task, with nothing more to write. The
panel's live tree shows the class and the style as they change.
`getComputedStyle` (the cascade's result, read back) is an exercise.

### Reflow: a mutable copy, frozen

`Dom` is a value, built once and only read, and everything since
TinyMosaic relies on it (the looks, the layout, the hit test, the
forms). A script needs to change the tree. So the script sees a
**mutable copy** (`Browser_script`'s nodes, with parents), and after
each task that changed it, the copy is frozen back into a `Dom.element`
and the page is laid out again from it -- the Elm architecture: the
model changes, the view is computed again. Real engines mutate one tree
and lay out incrementally (dirty bits: `notes_browser.md` exercise 5);
here the pages are small and the whole layout is milliseconds.

## Where it goes

- **`libs/languages/javascript/`** -- the engine, pure OCaml, no DOM,
  no Playground (the `libs/` rule): `Js_lexer`, `Js_ast`, `Js_parse` (a
  Pratt parser: operators by binding power, Vaughan Pratt, 1973),
  `Js_value` (values, objects as property tables, environments),
  `Js_eval` (a tree-walking interpreter), `Js_builtins`. About 1500
  lines with its tests. What the engine knows of the outside world is
  a **host**, a record of functions (HyperTalk's `world`, again), so a
  test runs a script against a fake page, and a future devtool against
  none. The prefix `Js_` because the libraries are unwrapped and
  js_of_ocaml's own module is `Js`: no module here may be named `Js`.
- **`appkits/browser/`** -- the binding, about 500 lines:
  `Browser_script` (the mutable tree, the host objects, the `<script>`s
  run, the task queue, timers), `Browser_console` (what `console.log`
  said, the errors with their line).
- **`apps/internet/TinyFirefox.ml`** -- about 800 lines: Firefox 1.0's
  window (toolbar, location bar, search box); a panel after Firebug,
  the lesson's centre -- the **console** (log and errors) and the
  **live DOM tree**, watched changing as the scripts run; built-in
  pages (`site/`): a counter, a to-do list, a clock, tic-tac-toe in a
  table.

The web build is the joke that teaches: a JavaScript interpreter,
written in OCaml, compiled to JavaScript, running in a browser.

## libs/languages/: the other languages of the house

The author's proposal (2026-09-25): a folder for the languages, each a
library, pure (text in, a program out, run against a host of
functions), JavaScript the first. What else could live there -- the
analysis, not a decision (the rule stands: no move without the
author's yes, each its own step):

| Language | Where now | Lines | Depends on | Verdict |
|---|---|---|---|---|
| HyperTalk | `appkits/hypertalk/Hypertalk` | 430 | nothing | stays (the author's call, 2026-09-25): movable as it is, but unlikely to be used outside HyperCard, the one program that speaks it |
| BASIC | `appkits/basic/` (`Basic_parse`, `Basic_run`, `Basic_session`, `Basic_disk`) | 1270 | `Basic_run` over the Playground's `Teletype` way | not now: another session is building it (three commits on 2026-09-25); later, `Basic_parse` (pure) could move, `Basic_run` only once split from `Teletype` (its conversation, a continuation, the language's; the terminal, the appkit's) |
| Spreadsheet formulas | `libs/languages/formula/Formula` | 252 | nothing | moved (the author's yes, 2026-09-25): a library of its own, `formula`, which `appkit_sheet`'s `Sheet` depends on -- a general expression evaluator, the first of the folder; its tests stay in `Unit_sheet` |
| Karel's language | inside `games/programming/TinyKarel.ml` | (part of 465) | `playground/ways/Karel` | an extraction, not a move: the parser to `libs/languages/karel/` with an AST of its own, the game translating it to the way's commands |
| Redcode | inside `games/programming/TinyCoreWar.ml` | (part of 417) | the game | an extraction: the assembler and the machine (MARS) as a library, the game its screen |
| Logo | `playground/ways/Logo` | 182 | the Playground | stays: it is an OCaml API (a program is a `command list`), a *way*, not a text language; a new `libs/languages/logo/`, Logo's text (`repeat 4 [fd 100 rt 90]`) read into those commands, would be an addition |
| PuzzleScript | `playground/ways/Puzzlescript` | 430 | the Playground | stays: rules as OCaml data, a way |

So the line: a *way* (`playground/ways/`) is an OCaml API that builds
an app; a *language* (`libs/languages/`) is text, parsed and run, with
no Playground. The formulas moved (`libs/languages/README.md`);
HyperTalk qualifies but stays by its app; BASIC could after a split,
Karel's and Redcode's after an extraction.

## Phases

- **J0, the plan** (this file), then `notes_javascript.md` (a tutorial:
  a language engine in five stages, with worked examples the tests will
  check). No related-work notes (the author's call: JavaScript is not
  the most elegant language to survey); the plan's references suffice.
- **J1, reading**: `Js_lexer` (tokens, a newline remembered for the
  statement's end), `Js_ast`, `Js_parse` (Pratt for expressions,
  recursive descent for statements), an AST printer; tests from the
  notes' worked examples (precedence, associativity, errors on their
  line).
- **J2, running**: `Js_value`, `Js_eval` (environments as chains of
  frames, closures capturing them, `return`/`break` as OCaml
  exceptions or results), `Js_builtins`; tests: closures (a counter), 
  recursion, the "wat" coercions, errors.
- **J3, the DOM**: `Browser_script` -- the mutable copy and its
  freezing, the host objects, `<script>` run at load, `innerHTML`,
  `querySelector`; tests on fake pages.
- **J4, events**: the task queue, `onclick` and `addEventListener`,
  bubbling, timers on the frame clock, the reflow after a task,
  `Browser_console`; tests.
- **J5, TinyFirefox**: the window, the panel (console, live tree),
  the built-in pages; golden frames (a page after its script, a click,
  a timer's tick under `-fixed-time`), CATALOG row, web page.
- Exercises: prototypes and `new`, `class`, template literals, a
  `TinyNode` REPL in `apps/devtools/` (the engine with no page), tabs
  (Firefox's signature), `<canvas>`, `fetch` over `Cmd.Http_get`,
  promises and the microtask queue, `localStorage`, a bytecode compiler
  (QuickJS's way) instead of the tree walker; `Formula` grown into the
  general expression evaluator it could be (comparisons, IF, strings:
  what the spreadsheets added after VisiCalc).

## Status

- **J0 done** (2026-09-25): this plan; the tutorial
  [`notes_javascript.md`](../tutorials/notes_javascript.md) (a language
  engine in stages, then the DOM, events and the browser, each section
  with the worked example its tests will check);
  `libs/languages/` opened with `Formula` moved there (its README says
  what belongs in the folder).

## Decisions

- **TinyFirefox**, the modern line (the author's choice, 2026-09-25,
  over a historical TinyNetscape2); the language a small modern-looking
  core, not JavaScript 1.0 -- `let` and arrow functions are what people
  write, and block scope is simpler than `var`'s hoisting.
- **`libs/languages/javascript/`** for the engine (the author's
  proposal, 2026-09-25), usable without a browser.
- **A mutable copy frozen after each task** rather than a mutable
  `Dom` (everything since TinyMosaic reads the immutable tree).
- **No standalone console app** for now: TinyFirefox's panel is the
  console; `TinyNode` an exercise.
- **CSS in TinyFirefox** (the author, 2026-09-25): TinyNetscape's
  looks, extensions and CSS1, all on, and a script's `style` and
  `className`.
- **`libs/languages/`**: `Formula` moved there (the author's yes);
  HyperTalk stays in `appkits/` (the author's call).
- **"Tiny"** measured: the engine about 1500 lines, the binding 500,
  the browser 800; what is out is listed above.

## Verification

- `libs/languages/javascript/tests/`: the notes' worked examples (a
  parse, a closure, a coercion, an error's line), each phase adding its
  own.
- `appkits/tests/`: `Browser_script` on fake pages (a click bubbling,
  a timer, `innerHTML` read back through `Html_tree`).
- Golden frames for TinyFirefox, deterministic (`-fixed-time`, timers
  on the frame clock, the `-script` of clicks and keys).
- `tests/catalog/`: the CATALOG row, the golden frame, the web page.

## References

- Brendan Eich, "JavaScript at Ten Years" (2005); Allen Wirfs-Brock and
  Brendan Eich, "JavaScript: The First 20 Years" (HOPL IV, 2020).
- ECMA-262, first edition (June 1997), about 100 pages: the size of a
  language worth citing; ES2015 for `let`, `const`, arrows.
- Robert Nystrom, *Crafting Interpreters* (2021): jlox, a tree-walking
  interpreter for a JavaScript-like language, the closest model.
- Vaughan Pratt, "Top Down Operator Precedence" (1973); Douglas
  Crockford's JavaScript version of it (2007).
- Small real engines to measure against: QuickJS (Fabrice Bellard,
  2019), Duktape, mJS and Elk (Cesanta), Espruino.
- W3C, DOM Level 1 (1998) and Level 2 Events (2000): the calls kept
  here, and bubbling.
- Firebug (Joe Hewitt, 2006) and Firefox's Web Console: the panel.
- Gary Bernhardt, "Wat" (CodeMash, 2012): the coercions.
