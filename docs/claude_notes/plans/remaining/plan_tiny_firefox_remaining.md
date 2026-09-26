# Plan: what's left for TinyFirefox and libs/languages/

The plan is done: see
[`done/plan_tiny_firefox.md`](../done/plan_tiny_firefox.md) -- the
JavaScript engine `libs/languages/javascript/` (`Js_lexer`, `Js_ast`,
`Js_parse`, a Pratt parser; `Js_value`; `Js_eval`, a tree walker with
a budget of steps; `Js_builtins`), the page's side in `appkits/browser`
(`Browser_script`: the mutable copy of the tree frozen after each
task, the DOM's calls, listeners and bubbling, timers on the frame
clock; `Browser_tab`, TinyNetscape's navigation taken out), and
TinyFirefox (Firefox 1.0, 2004) over them, its panel after Firebug
(the console and its command line, the live tree), its pages
`about:counter`, `about:todo`, `about:timer`, `about:tictactoe`; with
the tutorial [`notes_javascript.md`](../../tutorials/notes_javascript.md).
And `libs/languages/` opened, `Formula`, BASIC and HyperTalk moved in.
TinyChrome's C8 since added the ES5 core (prototypes and `new`, `var`
hoisted, `==`, regular expressions, `Date`, `<script src>`); the
language's next tier is in `plan_tiny_chrome_remaining.md`.

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli` and its test.

## 1. TinyFirefox's own exercises (its header)

- **Tabs**, Firefox's signature: a list of `Browser_tab.t`, the one
  shown chosen by a row of tabs. TinyChrome has them (C7), a model to
  follow, not to share yet.
- **The search box**, beside the location bar (Firefox 1.0's Google
  box): words sent to a search engine's page.
- **View Source**: the page's text, as TinyNetscape's `s` shows it.
- **The panel's tree scrolled**, and an element picked on the page
  shown in it -- TinyChrome's Inspect (`Browser_devtools`) does the
  picking.
- **The command line's history**: the console remembering its
  commands, the arrows going through them.

## 2. The language: what the plan left out

Promises, `class`, template literals, getters and setters are in
TinyChrome's remaining (its section 5); not repeated here. Left:

- **`async`/`await`**, after the promises; generators.
- **`getComputedStyle`**: the cascade's result read back by a script.
- **Strings counted in code points** (or UTF-16 units as JavaScript
  does): `"é".length` is 2 today, bytes (`Js_value.mli`).
- **The other half of semicolon insertion**: a line starting with `(`
  or `[` continuing the one before (`Js_parse.mli`).
- **The expressions in ocamlyacc**, compared with the Pratt parser
  (`Js_parse.mli`: the binding powers as `%left`/`%right`).
- Never, as the plan said: `eval`, `with`, modules, `Symbol`; a
  garbage collector of our own, a JIT.

## 3. The DOM and the browser's services

- **`<canvas>`**: its 2D context's calls (`fillRect`, paths, `fillText`)
  drawn into a picture of the page, over `graphics/2d`.
- **`fetch` over `Cmd.Http_get`** with its answer: C8's `fetch` and
  `XMLHttpRequest` send their GETs and drop the answers; the answer
  needs the promises (TinyChrome's section 5).
- **`localStorage`**: over `Playground_platform.store` and `fetch`
  (TinyEudora's store: files natively, the browser's own localStorage
  on the web), per site.
- `DocumentFragment`, node types other than elements and text; the rest
  of `Node`'s interface. (Cookies and `history.pushState` are
  TinyChrome's section 5.)

## 4. Other engines

- **`TinyNode`**, a REPL in `apps/devtools/`: the engine with no page,
  `console.log` to a Textmode screen, as TinyBasic is BASIC's.
- **A bytecode compiler** (QuickJS's way) instead of the tree walker,
  measured against it; the house has two models, Pascal's P-code
  (`Pcode`, `Pmachine`) and Smalltalk's bytecodes.

## 5. libs/languages/: the extractions

The plan's table, what it proposed and was not done:

- **Karel's language**: its parser out of `TinyKarel.ml` into
  `libs/languages/karel/`, with an AST of its own; the game translating
  it to the `Karel` way's commands.
- **Redcode**: the assembler and the machine (MARS) out of
  `TinyCoreWar.ml` into a library, the game its screen.
- **Logo's text**: `repeat 4 [fd 100 rt 90]` read into the `Logo`
  way's commands, a new `libs/languages/logo/` (the way stays: an
  OCaml API).
- **`Formula` grown** into a general expression evaluator: strings,
  comparisons and `IF` (what the spreadsheets added after VisiCalc;
  `Formula.mli` lists them as left out).

Each a move or a new library: the author's yes first, each its own
step.
